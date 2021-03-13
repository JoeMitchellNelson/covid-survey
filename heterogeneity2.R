require(pacman)

p_load(rpart,rpart.plot,readr,dplyr,RCurl,rjson,lubridate,
       rvest,stringr,Hmisc,rattle,RColorBrewer,ddpcr,tidytext,tidyr,
       ggrepel,ggplot2,png,ggpubr,tidycensus,sf,plm,lmtest,stargazer,MASS,
       xtable,knitr,magick,purrr,ggthemes,gifski,extrafont,latex2exp,
       cowplot,mapproj,patchwork,remotes,tictoc,Hmisc,english,readstata13,margins)

#################### READ IN DATA ################
{

newdemean <- read.csv("~/covid-survey/demeanrp-lasso.csv")[,-1]
cmatch <- read.csv("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID-shared/intermediate-files/countymatch.csv")[,-1]
dat <- read.csv("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_nointx.csv") %>% dplyr::filter(rejectonly==0) 
dat2 <- read.dta13("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_nointx.dta")



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

}

dat$ideol <- dat$ideol %>% str_remove_all(" ") %>% as.factor()
####### functions to format data #############


getsummary <- function (fac,df,fedint=T) {
  
  levs <- df[,which(names(df)==fac)] %>%  unique()
  levs <- summary(levs[,1])[,1] %>% str_remove_all(":1") %>% trimws()
  levs <- levs %>% str_remove_all(" ")
  
  bv <- c("mabsdeaths","mabscases","avcost","unempl",
          "rule1","rule2","rule3","rule4","rule5","rule6",
          "rule7","rule8","rule9","rule10","statquo")
  
  newdf <- df %>% dplyr::select("best","choice",all_of(bv),all_of(fac),"popwt","lassorpfl","feduiany")
  
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
  
  if (fedint) {
    form2 <- form2 %>% str_replace_all("unempl","feduiany*unempl") %>% str_replace_all("avcost","feduiany*avcost")
  }
  form3 <- paste0("best ~",form2,"+ strata(choice)") %>% as.formula
  
  summary(clogit(form3,data=newdf,weights=popwt,method="approximate"))
}

maketable <- function (fac,df,fedint=T,drop=c("MISSING")) {
  
  s <- getsummary(fac,df,fedint)
  
  levs <- df[,which(names(df)==fac)] %>%  unique()
  levs <- summary(levs[,1])[,1] %>% str_remove_all(":1") %>% trimws()
  levs <- levs %>% str_remove_all(" ")
  
  t <- s$coefficients %>% as.data.frame() %>% rownames_to_column(var="term") %>% 
    dplyr::filter(! str_detect(term,"lassorpfl"))
  
  modelnames <- c()
  titles <- c()
  
  for (i in 1:length(levs)) {
    if (!levs[i] %in% drop) {
      tempcoef <- t %>% dplyr::filter(str_detect(term,levs[i]) )
      assign(paste0(levs[i],"coef"),tempcoef)
      modelnames <- c(modelnames,paste0(levs[i],"coef"))
      titles <- c(titles,levs[i])
    }
  }
  
  if (fedint) {
    fake <- lm(best ~ mabsdeaths + mabscases + avcost + avcost:feduiany + unempl + unempl:feduiany +
                 rule1 + rule2 + rule3 + rule4 + rule5 + rule6 +
                 rule7 + rule8 + rule9 + rule10 + statquo + 0,data = dat)
    
    vars.order <- c("mabsdeaths",  "mabscases",   "avcost","avcost:feduiany",  "unempl", 
                    "feduiany:unempl", "rule1", "rule2" , "rule3",  
                    "rule4",   "rule5" , "rule6" ,   "rule7",    "rule8"   ,   "rule9" , "rule10" ,   
                    "statquo" )
  } else {
    
    fake <- lm(best ~ mabsdeaths + mabscases + avcost + unempl + 
                 rule1 + rule2 + rule3 + rule4 + rule5 + rule6 +
                 rule7 + rule8 + rule9 + rule10 + statquo + 0,data = dat)
    
    vars.order <- vars.order <- c("mabsdeaths",  "mabscases",   "avcost",  "unempl", 
                                   "rule1", "rule2" , "rule3",  
                                  "rule4",   "rule5" , "rule6" ,   "rule7",    "rule8"   ,   "rule9" , "rule10" ,   
                                  "statquo" )
  }
  
  mods <- list()
  for (i in 1:length(modelnames)) {mods[[i]] <- fake}
  
  coeflist <- list()
  for (i in 1:length(modelnames)) {coeflist[[i]] <- (modelnames[i] %>% as.symbol() %>% eval())$coef}
  
  selist <- list()
  for (i in 1:length(modelnames)) {selist[[i]] <- (modelnames[i] %>% as.symbol() %>% eval())$`robust se`}
  
  stargazer(mods,type="latex",
            # omit=":lassorpfl",
            coef = coeflist,
            # standard errors
            se = selist,
            order=paste0("^", vars.order , "$"),
            column.labels = titles,
            keep.stat="n",
            t.auto = T)
  
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
  x <- str_replace_all(x," & & \\\\\\\\"," & & \\\\\\\\[-1.2ex]")
  
  print(str_count(x," & & \\\\\\\\"))
  x
  
}



maketable(fac="ideol", df=dat, fedint = T, drop="MISSING") %>% fixnames() %>% cat()

dat$gender <- ifelse(dat$male==1,"Men",
                     ifelse(dat$female==1,"Women",
                            ifelse(dat$nonbinary==1,"NonBinary","MISSING")))

dat$gender <- factor(dat$gender)

maketable(fac="gender", 
          df=dat, 
          fedint = T, 
          drop=c("MISSING","NonBinary")) %>% 
  fixnames() %>% cat()
