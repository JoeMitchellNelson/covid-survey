# calculate WTP using draws from mvnormal distributions

require(pacman)

p_load(rpart,rpart.plot,readr,dplyr,RCurl,rjson,lubridate,
       rvest,stringr,Hmisc,rattle,RColorBrewer,ddpcr,tidytext,tidyr,
       ggrepel,ggplot2,png,ggpubr,tidycensus,sf,plm,lmtest,stargazer,MASS,
       xtable,knitr,magick,purrr,ggthemes,gifski,extrafont,latex2exp,
       cowplot,mapproj,patchwork,remotes,tictoc,Hmisc,english,readstata13,margins,xtable)

p_load(clubSandwich,rms)

#################### READ IN DATA ################
{
  
  newdemean <- read.csv("C:/Users/joe/Dropbox (University of Oregon)/VSL-COVID-shared/intermediate-files/demeanrp-lasso.csv")[,-1]
  cmatch <- read.csv("C:/Users/joe/Dropbox (University of Oregon)/VSL-COVID-shared/intermediate-files/countymatch.csv")[,-1]
  dat <- read.csv("C:/Users/joe/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_nointx.csv") %>% dplyr::filter(rejectonly==0) 
  dat2 <- read.dta13("C:/Users/joe/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_nointx.dta")
  
  ethnic <- read.csv("~/covid-survey/data/countyethfrac.csv")[,-1]
  
  varlabs <- data.frame(variable = names(dat),label= attributes(dat2)$var.labels)
  varlabs$variable <- as.character(varlabs$variable)
  varlabs$label <- as.character(varlabs$label)
  varlabs <- varlabs %>% dplyr::filter(variable!="multi")
  
  dat$state <- ifelse(dat$CA==1,"CA",
                      ifelse(dat$OR==1,"OR","WA"))
  
  dat$prep <- ifelse(dat$prepover==1,"over",
                     ifelse(dat$prepreas==1,"reasonable","poor"))
  
  dat$rules <- ifelse(dat$rulesbad==1,"bad",
                      ifelse(dat$rulesgood==1,"good","insufficient"))
  
  dat$schoolage <- ifelse(dat$hhld0to1 ==1 | dat$hhld2to5==1 | dat$hhld6to12==1 | dat$hhld13to17==1,1,0)
  
  dat <- dat %>% group_by(ResponseId) %>% mutate(nchoice=length(unique(choice))) %>% ungroup %>% mutate(popwt2 = popwt/nchoice)
  
  dat$ideol <- ifelse(dat$ideolsconserv==1,"strongly conservative",
                      ifelse(dat$ideolsliberal==1,"strongly liberal",
                             ifelse(dat$ideolmliberal==1,"moderately liberal",
                                    ifelse(dat$ideolmconserv==1,"moderately conservative",
                                           ifelse(dat$ideolmod==1,"moderate","MISSING")))))
  dat$ideol <- as.factor(dat$ideol)
  dat <- within(dat, ideol <- relevel(ideol, ref = 2))
  
  dat$ideol2 <- ifelse(dat$ideol %in% c("strongly conservative","moderately conservative"),"conservative",
                       ifelse(dat$ideol %in% c("strongly liberal","moderately liberal"),"liberal",
                              ifelse(dat$ideol %in% c("moderate"),"moderate","MISSING")))
  dat$ideol2 <- as.factor(dat$ideol2)
  dat <- within(dat, ideol2 <- relevel(ideol2, ref = 4))
  
  dat <- dat %>% mutate(ethfrac = 1 - pziprasian^2 - pziprwhite^2 - pziprblack^2 - pziprothrace3^2 - pziprmulti^2)
  dat$ethfrac <- ifelse(dat$ethfrac==1,0,dat$ethfrac)
  dat <- dat %>% mutate(ethcheck = pziprasian + pziprwhite + pziprblack + pziprothrace3 + pziprmulti)
  
  
  dat <- left_join(dat,newdemean)
  dat <- left_join(dat,cmatch)
  
  dat$ideol <- dat$ideol %>% str_remove_all(" ") %>% as.factor()
  
  dat <- left_join(dat,ethnic)
  
  dat$months2 <- ifelse(dat$months==1,"one",
                        ifelse(dat$months==2,"two","three"))
  
  dat$months2 <- factor(dat$months2,levels=c("one","two","three"))
  
  dat <- dat %>% dplyr::filter(Durationinseconds>300)
  dat$all <- "Everyone" %>% as.factor()
  
}
{
  dat$ideol2 <- dat$ideol2 %>% factor(levels=c("liberal","moderate","conservative","MISSING"))
  
  
  dat$gender <- ifelse(dat$male==1,"Men",
                       ifelse(dat$female==1,"Women",
                              ifelse(dat$nonbinary==1,"NonBinary","MISSING")))
  
  dat$gender <- factor(dat$gender,levels=c("Women","Men","NonBinary","MISSING"))
  
  dat$age <- ifelse(dat$agebcont < 35, "18to34",
                    ifelse(dat$agebcont %in% c(40,50,60),"35to64","65andolder"))
  
  dat$age <- dat$age %>% factor(levels=c("18to34","35to64","65andolder"))
  
  dat$race <- ifelse(dat$white == 1,"White","NonWhite")
  dat$race <- as.factor(dat$race)
  
  dat$education <- ifelse(dat$scollorless==1,"Noncollege",
                          ifelse(dat$educpnts==1,"MISSING","College"))
  
  dat$education <- factor(dat$education,levels=c("Noncollege","College","MISSING"))
  
  dat$income <- ifelse(dat$inccont > 75,"Morethan75","Lessthan75")
  dat$income <- factor(dat$income)
  
  dat$threealts2 <- ifelse(dat$threealts==0,"Alts2","Alts3") %>% as.factor()
  
}

getsim <- function (n=100000,model) {
  x <- robcov(model)
  
  sigma <- x$var %>% as.matrix()
  mu <- x$coefficients %>% as.matrix()
  
  sims <- mvrnorm(n = n, mu=mu, Sigma=sigma,empirical=F) %>% as.data.frame()
  sims
}

getmrs <- function (sim,v1,v2) {
  
#  mrs <- getsim(n=n,model=model)
  a <- -1*sim[,which(names(sim)==v1)]/sim[,which(names(sim)==v2)]
  a <- a[order(a)]
  lb <- a[round(length(a)*0.025)] %>% signif(3)
  ub <- a[round(length(a)*0.975)] %>% signif(3)
  if ((ub < 0 & lb <0)|(ub > 0 & lb > 0)) {
    paste0("(",lb,", ",ub,")*")
  } else {
    paste0("(",lb,", ",ub,")")
  }
}


gettable <- function (fac,df,fedui="any") {
  
  df <- df %>% dplyr::filter(!is.na(eval(as.symbol(fac)))) %>% droplevels()
  
  df$feduinone <- (df$feduiany -1)^2
  
  levs <- df[,which(names(df)==fac)] %>%  unique()
  levs <- summary(levs[,1])[,1] %>% str_remove_all(":1") %>% trimws()
  levs <- levs %>% str_remove_all(" ")
  
  if (fac=="education") {
    levs <- c("Noncollege","College")
  }
  
  bv <- c("mabsdeaths","mabscases","avcost","unempl",
          "rule1","rule2","rule3","rule4","rule5","rule6",
          "rule7","rule8","rule9","rule10","statquo")
  
  newdf <- df %>% dplyr::select("best","choice",all_of(bv),all_of(fac),"popwt","popwt2","lassorpfl","feduiany","feduinone")
  
  allv <- c()
  for (i in 1:length(levs)) {
    allv <- c(allv,paste0(bv,"_",levs[i]))
  }
  
  for (i in 1:length(allv)) {
    a <- allv[i] %>% str_split_fixed("_",2)
    newdf$newvar <- (newdf[,which(names(newdf)==a[,1])] * ifelse(newdf[,which(names(newdf)==fac)]==a[,2],1,0)) %>% unlist %>%  as.numeric()
    names(newdf)[which(names(newdf)=="newvar")] <- allv[i]
  }
  
  form1 <- paste0(allv,"*lassorpfl")
  form2 <- paste(form1,collapse=" + ")
  
  if (fedui=="any") {
    form2 <- form2 %>% str_replace_all("unempl","feduiany*unempl") %>% str_replace_all("avcost","feduiany*avcost")
  } else if (fedui=="none") {
    form2 <- form2 %>% str_replace_all("unempl","feduinone*unempl") %>% str_replace_all("avcost","feduinone*avcost")
  } else {
    cat("fedui must be \"any\" or \"none\"")
  }
  form3 <- paste0("best ~",form2,"+ strata(choice)") %>% as.formula
  
  clogit(form3,data=newdf,weights=popwt,method="approximate")
}
 
fixnames <- function (x) {
  
  x <- paste(x,collapse="\n")
  
  
  for (i in nrow(varlabs):1) {
    if(str_detect(x,paste0("",varlabs$variable[i]))) {
      x <- str_replace_all(x,varlabs$variable[i],varlabs$label[i])
    }
  }
  
  x <- str_replace_all(x," numberernative","")
  x <- str_replace_all(x," w/ any attrib.","")
  x <- str_replace_all(x,"1=Non-zero federal UI policy","$\\\\quad \\\\times$ 1=Non-zero federal UI")
  x <- str_remove_all(x,":Unempl rate for county")
  x <- str_remove_all(x,"Avg. hhld cost for county:")
  # x <- str_replace_all(x," & & \\\\\\\\"," & & \\\\\\\\[-1.2ex]")
  # x <- str_replace_all(x,"\\[-1.8ex]","\\\\")
  
  print(str_count(x," & & \\\\\\\\"))
  x
  
}


# fac = "ideol2"
# df = dat[which(dat$choiceofperson %in% 1:2 & dat$ideol2 != "MISSING"),]
# fedui = "any"
# denom = "avcost"
# n=100000

makepretty <- function (fac,df,fedui="any",denom="avcost",n=100000) {
  
  v <- c("mabsdeaths","mabscases","rule1","rule2","rule3","rule4",
         "rule5","rule6","rule7","rule8","rule9","rule10","statquo")
  
  df <- df %>% dplyr::filter(!is.na(eval(as.symbol(fac)))) %>% droplevels()
  
  levs <- df[,which(names(df)==fac)] %>% unique()
  levs <- summary(levs[,1])[,1] %>% str_remove_all(":1") %>% trimws()
  levs <- levs %>% str_remove_all(" ")

  m <- gettable(fac=fac,df=df,fedui=fedui)
  
  sim1 <- getsim(n=n,model=m)
  
  res <- matrix(nrow=length(v)*2,ncol=length(levs)+1) %>% as.data.frame()
  names(res) <- c("Var",levs)
  res$Var <- rep(v,each=2)
  res$Var <-ifelse(row_number(res$Var) %% 2 == 0,"",res$Var)
  
  for (i in 1:length(levs)) {
    for (j in 1:length(v)) {
  v1 <- paste0(v[j],"_",levs[i])
  v2 <- paste0(denom,"_",levs[i])
  res[j*2-1,i+1] <- median(-1*sim1[,which(names(sim1)==v1)]/sim1[,which(names(sim1)==v2)]) %>% signif(3)
  res[j*2,i+1] <- getmrs(sim=sim1,v1=v1,v2=v2)
  
    }
  }
  
  res
}

makepretty(fac="ideol2",
           df=dat[which(dat$choiceofperson %in% 1:2 & dat$ideol2 != "MISSING"),],
           fedui="any",
           denom="avcost",
           n=10000)  %>% xtable(align=c("l","l","c","c","c")) %>% 
  print.xtable(include.rownames=FALSE) %>% fixnames() %>% 
  str_replace_all("Var","") %>% 
  str_replace_all("\\*","$^{**}$") %>% 
  str_replace_all("liberal","Liberal") %>% 
  str_replace_all("moderate","Moderate") %>% 
  str_replace_all("conservative","Conservative") %>% 
  cat()

makepretty(fac="age",
           df=dat[which(dat$choiceofperson %in% 1:2),],
           fedui="any",
           denom="avcost",
           n=10000)  %>% xtable(align=c("l","l","c","c","c")) %>% 
  print.xtable(include.rownames=FALSE) %>% fixnames() %>% 
  str_replace_all("Var","") %>% 
  str_replace_all("\\*","$^{**}$") %>% 
  cat()

makepretty(fac="income",
           df=dat[which(dat$choiceofperson %in% 1:2),],
           fedui="any",
           denom="avcost",
           n=10000)  %>% xtable(align=c("l","l","c","c")) %>% 
  print.xtable(include.rownames=FALSE) %>% fixnames() %>% 
  str_replace_all("Var","") %>% 
  str_replace_all("\\*","$^{**}$") %>%
  cat()

makepretty(fac="education",
           df=dat[which(dat$choiceofperson %in% 1:2 & dat$education != "MISSING"),],
           fedui="any",
           denom="avcost",
           n=10000)  %>% xtable(align=c("l","l","c","c")) %>% 
  print.xtable(include.rownames=FALSE) %>% fixnames() %>% 
  str_replace_all("Var","") %>% 
  str_replace_all("\\*","$^{**}$") %>%
  cat()

makepretty(fac="race",
           df=dat[which(dat$choiceofperson %in% 1:2),],
           fedui="any",
           denom="avcost",
           n=10000) %>% xtable(align=c("l","l","c","c")) %>% 
  print.xtable(include.rownames=FALSE) %>% fixnames() %>% 
  str_replace_all("Var","") %>% 
  str_replace_all("\\*","$^{**}$") %>% 
  cat()

makepretty(fac="all",
           df=dat[which(dat$choiceofperson %in% 1:2),],
           fedui="any",
           denom="avcost",
           n=10000)  %>% xtable(align=c("l","l","c")) %>% 
  print.xtable(include.rownames=FALSE) %>% fixnames() %>% 
  str_replace_all("Var","") %>% 
  str_replace_all("\\*","$^{**}$") %>% 
  cat()



m1 <- gettable("age",dat[which(dat$choiceofperson %in% 1:2),],fedui="any")
summary(m1)


a <- getsim(model=m1) %>% as.data.frame()
summary(a$avcost_18to34)
summary(a$rule4_18to34)
b <- (-1*a$rule4_18to34/a$avcost_18to34)
b <- b[order(b)]
b[100000-2500]
