require(pacman)

p_load(rpart,rpart.plot,readr,dplyr,RCurl,rjson,lubridate,
       rvest,stringr,Hmisc,rattle,RColorBrewer,ddpcr,tidytext,tidyr,
       ggrepel,ggplot2,png,ggpubr,tidycensus,sf,plm,lmtest,stargazer,MASS,
       xtable,knitr,magick,purrr,ggthemes,gifski,extrafont,latex2exp,
       cowplot,mapproj,patchwork,remotes,tictoc,Hmisc,english,readstata13,margins)

#################### READ IN DATA ################
{

newdemean <- read.csv("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID-shared/intermediate-files/demeanrp-lasso.csv")[,-1]
cmatch <- read.csv("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID-shared/intermediate-files/countymatch.csv")[,-1]
dat <- read.csv("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_nointx.csv") %>% dplyr::filter(rejectonly==0) 
dat2 <- read.dta13("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_nointx.dta")

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
}


####### functions to format data #############

#df = dat
#fac="income"

getsummary <- function (fac,df,fedint=T) {
  
  df <- df %>% dplyr::filter(!is.na(eval(as.symbol(fac))))

  levs <- df[,which(names(df)==fac)] %>%  unique()
  levs <- summary(levs[,1])[,1] %>% str_remove_all(":1") %>% trimws()
  levs <- levs %>% str_remove_all(" ")
  
  if (fac=="education") {
  levs <- c("Noncollege","College")
  }
  
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
  
  df <- df %>% dplyr::filter(!is.na(eval(as.symbol(fac))))
  
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
                 rule7 + rule8 + rule9 + rule10 + statquo + 0,data = df)
    
    vars.order <- c("mabsdeaths",  "mabscases",   "avcost","avcost:feduiany",  "unempl", 
                    "feduiany:unempl", "rule1", "rule2" , "rule3",  
                    "rule4",   "rule5" , "rule6" ,   "rule7",    "rule8"   ,   "rule9" , "rule10" ,   
                    "statquo" )
  } else {
    
    fake <- lm(best ~ mabsdeaths + mabscases + avcost + unempl + 
                 rule1 + rule2 + rule3 + rule4 + rule5 + rule6 +
                 rule7 + rule8 + rule9 + rule10 + statquo + 0,data = df)
    
    vars.order <- c("mabsdeaths",  "mabscases",   "avcost",  "unempl", 
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
  
  stargazer(mods,type="latex",title=str_to_title(fac),
            # omit=":lassorpfl",
            coef = coeflist,
            # standard errors
            se = selist,
            order=paste0("^", vars.order , "$"),
            column.labels = titles,
            keep.stat="n",
           # digits.extra = 5,
            no.space = T,
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
 # x <- str_replace_all(x," & & \\\\\\\\"," & & \\\\\\\\[-1.2ex]")
 # x <- str_replace_all(x,"\\[-1.8ex]","\\\\")
  
  print(str_count(x," & & \\\\\\\\"))
  x
  
}


dat$ideol2 <- dat$ideol2 %>% factor(levels=c("liberal","moderate","conservative","MISSING"))


dat$gender <- ifelse(dat$male==1,"Men",
                     ifelse(dat$female==1,"Women",
                            ifelse(dat$nonbinary==1,"NonBinary","MISSING")))

dat$gender <- factor(dat$gender,levels=c("Women","Men","NonBinary","MISSING"))

dat$age <- ifelse(dat$agebcont < 35, "18to34",
                  ifelse(dat$agebcont %in% c(40,50,60),"35to64","65andolder"))

dat$age <- dat$age %>% factor(levels=c("18to34","35to64","65andolder"))

dat$race <- ifelse(dat$white == 1,"White","Other")
dat$race <- as.factor(dat$race)

dat$education <- ifelse(dat$scollorless==1,"Noncollege",
                        ifelse(dat$educpnts==1,"MISSING","College"))

dat$education <- factor(dat$education,levels=c("Noncollege","College","MISSING"))

dat$income <- ifelse(dat$inccont > 75,"Morethan75","Lessthan75")
dat$income <- factor(dat$income)

ideoltable <- maketable(fac="ideol2", 
          df=dat, 
          fedint = T, 
          drop="MISSING") %>% 
  fixnames() %>% 
  str_replace_all("liberal","Liberal") %>%
  str_replace_all("moderate","Moderate") %>% 
  str_replace_all("conservative","Conservative")
  

write(ideoltable,file="~/covid-survey/tables/ideologytable.bib")

gendertable <- maketable(fac="gender", 
          df=dat, 
          fedint = T, 
          drop=c("MISSING","NonBinary")) %>% 
  fixnames() 

write(gendertable,file="~/covid-survey/tables/gendertable.bib")


agetable <- maketable(fac="age", 
          df=dat, 
          fedint = T, 
          drop=NA) %>% 
  fixnames() %>% 
  str_replace_all("18to34","18 to 34") %>% 
  str_replace_all("35to64","35 to 64") %>% 
  str_replace_all("65andolder","65 +")

write(agetable,file="~/covid-survey/tables/agetable.bib")


racetable <- maketable(fac="race", 
          df=dat, 
          fedint = T, 
          drop=NA) %>% 
  fixnames()
write(racetable,file="~/covid-survey/tables/racetable.bib")

eductable <- maketable(fac="education", 
                         df=dat, 
                         fedint = T,
                       drop="MISSING") %>% 
  fixnames() 


incometable <- maketable(fac="income", 
          df=dat, 
          fedint = T, 
          drop=NA) %>% 
  fixnames()

write(incometable,file="~/covid-survey/tables/incometable.bib")

####################################################
##################### Main results #################
####################################################

fixnames2 <- function (x) {
  
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
  
  x <- str_replace_all(x,"factor\\(Federal UI per week\\)", "$\\\\quad \\\\times$ Federal UI per week = \\\\$")
  x <- str_replace_all(x,":Avg. hhld cost for county", "")
  x <- str_replace_all(x,"Unempl rate for county:","")
  
  x
  
}

#run regressions
{
main1 <- clogit(best ~
                          mabsdeaths*lassorpfl + mabscases*lassorpfl +
                          unempl*lassorpfl + avcost*lassorpfl +
                          
                          rule1*lassorpfl + rule2*lassorpfl +
                          rule3*lassorpfl + rule4*lassorpfl +
                          rule5*lassorpfl + rule6*lassorpfl +
                          rule7*lassorpfl + rule8*lassorpfl +
                          rule9*lassorpfl + rule10*lassorpfl +
                          
                          statquo*lassorpfl +
                          strata(choice),data=dat
                         ,weights=popwt,method="approximate"
)

main2 <- clogit(best ~
                  mabsdeaths*lassorpfl + mabscases*lassorpfl +
                  unempl*feduiany*lassorpfl + avcost*feduiany*lassorpfl +
                  
                  rule1*lassorpfl + rule2*lassorpfl +
                  rule3*lassorpfl + rule4*lassorpfl +
                  rule5*lassorpfl + rule6*lassorpfl +
                  rule7*lassorpfl + rule8*lassorpfl +
                  rule9*lassorpfl + rule10*lassorpfl +
                  
                  statquo*lassorpfl +
                  strata(choice),data=dat
                ,weights=popwt,method="approximate"
)

main3 <- clogit(best ~
                  mabsdeaths*lassorpfl + mabscases*lassorpfl +
                  unempl*factor(fedui)*lassorpfl + avcost*factor(fedui)*lassorpfl +
                  
                  rule1*lassorpfl + rule2*lassorpfl +
                  rule3*lassorpfl + rule4*lassorpfl +
                  rule5*lassorpfl + rule6*lassorpfl +
                  rule7*lassorpfl + rule8*lassorpfl +
                  rule9*lassorpfl + rule10*lassorpfl +
                  
                  statquo*lassorpfl +
                  strata(choice),data=dat
                ,weights=popwt,method="approximate"
)

}


# reorder table
{
vars.order <- c("mabsdeaths",  "mabscases",
                "avcost","feduiany:avcost",  "factor\\(fedui\\)100:avcost", "factor\\(fedui\\)200:avcost", "factor\\(fedui\\)300:avcost","factor\\(fedui\\)400:avcost",
                "unempl", "unempl:feduiany", "unempl:factor\\(fedui\\)100", "unempl:factor\\(fedui\\)200", "unempl:factor\\(fedui\\)300", "unempl:factor\\(fedui\\)400",
                
                 "rule1", "rule2" , "rule3",  
                "rule4",   "rule5" , "rule6" ,   "rule7",    "rule8"   ,   "rule9" , "rule10" ,   
                "statquo" )

var.omit <- c("lassorpfl","^feduiany$","^factor\\(fedui\\)100$","^factor\\(fedui\\)200$","^factor\\(fedui\\)300$","^factor\\(fedui\\)400$")

}

maintabs <- stargazer(main1,main2,main3,
                      type="latex",
                      omit=var.omit,
                      order=paste0("^", vars.order , "$"),
                      keep.stat="n",
                      digits.extra = 10,
                      no.space = T)
maintable <- fixnames2(maintabs)

write(maintable,file="~/covid-survey/tables/maintable.bib")


############ remove speeders, see what happens ##################


speedtest <- maketable(fac="education", 
                        df=dat[which(dat$pblazepage<.568),], 
                        fedint = T, 
                        drop=c("MISSING","NonBinary")) %>% 
  fixnames() 

cat(speedtest)


############# ETHNIC FRACTIONALIZATION #############################

fac = "ideol2"
df= dat

s <- getsummary2("ideol2",dat,T)

getsummary2 <- function (fac,df,ethint=T) {
  
  df <- df %>% dplyr::filter(!is.na(eval(as.symbol(fac))))
  
  levs <- df[,which(names(df)==fac)] %>%  unique()
  levs <- summary(levs[,1])[,1] %>% str_remove_all(":1") %>% trimws()
  levs <- levs %>% str_remove_all(" ")
  
  if (fac=="education") {
    levs <- c("Noncollege","College")
  }
  
  bv <- c("mabsdeaths","mabscases","avcost","unempl",
          "rule1","rule2","rule3","rule4","rule5","rule6",
          "rule7","rule8","rule9","rule10","statquo")
  
  newdf <- df %>% dplyr::select("best","choice",all_of(bv),all_of(fac),"popwt","lassorpfl","feduiany","ethfraccnty")
  
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
  
  if (ethint) {
    form2 <- form2 %>% str_replace_all("unempl","ethfraccnty*unempl") %>% str_replace_all("avcost","ethfraccnty*avcost") %>% 
      str_replace_all("mabsdeaths","ethfraccnty*mabsdeaths") %>% str_replace_all("mabscases","ethfraccnty*mabscases")
  }
  form3 <- paste0("best ~",form2,"+ strata(choice)") %>% as.formula
  
  summary(clogit(form3,data=newdf,weights=popwt,method="approximate"))
}

maketable2 <- function (fac,df,fedint=T,drop=c("MISSING")) {
  
  df <- df %>% dplyr::filter(!is.na(eval(as.symbol(fac))))
  
  s <- getsummary2(fac,df,fedint)
  
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
  
  if (ethint) {
    fake <- lm(best ~ mabsdeaths + mabsdeaths:ethfraccnty + mabscases + mabscases:ethfraccnty + 
                 avcost + avcost:ethfraccnty + unempl + unempl:ethfraccnty +
                 rule1 + rule2 + rule3 + rule4 + rule5 + rule6 +
                 rule7 + rule8 + rule9 + rule10 + statquo + 0,data = df)
    
    vars.order <- c("mabsdeaths", "mabsdeaths:ethfraccnty", "mabscases",  "ethfraccnty:mabscases",
                    "avcost","ethfraccnty:avcost",  "unempl", 
                    "ethfraccnty:unempl", "rule1", "rule2" , "rule3",  
                    "rule4",   "rule5" , "rule6" ,   "rule7",    "rule8"   ,   "rule9" , "rule10" ,   
                    "statquo" )
  } else {
    
    fake <- lm(best ~ mabsdeaths + mabscases + avcost + unempl + 
                 rule1 + rule2 + rule3 + rule4 + rule5 + rule6 +
                 rule7 + rule8 + rule9 + rule10 + statquo + 0,data = df)
    
    vars.order <- c("mabsdeaths",  "mabscases",   "avcost",  "unempl", 
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
  
  stargazer(mods,type="latex",title=str_to_title(fac),
            # omit=":lassorpfl",
            coef = coeflist,
            # standard errors
            se = selist,
            order=paste0("^", vars.order , "$"),
            column.labels = titles,
            keep.stat="n",
            # digits.extra = 5,
            no.space = T,
            t.auto = T)
  
}

fixnames2 <- function (x) {
  
  x <- paste(x,collapse="\n")
  
  x <- str_replace_all(x,"mabsdeaths:ethfraccnty","$\\\\quad \\\\times$ Ethnic fractionalization")
  x <- str_replace_all(x,"ethfraccnty:mabscases","$\\\\quad \\\\times$ Ethnic fractionalization")
  x <- str_replace_all(x,"ethfraccnty:avcost","$\\\\quad \\\\times$ Ethnic fractionalization")
  x <- str_replace_all(x,"ethfraccnty:unempl","$\\\\quad \\\\times$ Ethnic fractionalization")
  
  for (i in nrow(varlabs):1) {
    if(str_detect(x,paste0("",varlabs$variable[i]))) {
      x <- str_replace_all(x,varlabs$variable[i],varlabs$label[i])
    }
  }
  

  x
  
}


dat$ethfraccnty <- dat$ethfraccnty - mean(dat$ethfraccnty,na.rm=T)

ethfrac1 <- clogit(best ~
                  mabsdeaths*ethfraccnty*lassorpfl + mabscases*ethfraccnty*lassorpfl +
                  avcost*ethfraccnty*lassorpfl + unempl*ethfraccnty*lassorpfl + 
                  
                  rule1*lassorpfl + rule2*lassorpfl +
                  rule3*lassorpfl + rule4*lassorpfl +
                  rule5*lassorpfl + rule6*lassorpfl +
                  rule7*lassorpfl + rule8*lassorpfl +
                  rule9*lassorpfl + rule10*lassorpfl +
                  
                  statquo*ethfraccnty*lassorpfl +
                  strata(choice),data=dat[which(dat$ideol2=="conservative"),]
                ,weights=popwt,method="approximate"
)
a <- summary(ethfrac1)$coefficients %>% as.data.frame()
a$`robust se`

stargazer(ethfrac1,
          omit=c("lassorpfl"),
          digits=4,
          se=list(a$`robust se`))

ethfractable <- maketable2("ideol2",dat,T,"MISSING") %>% fixnames2()
write(ethfractable,file="~/covid-survey/tables/ethfractable.bib")

