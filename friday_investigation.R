require(pacman)

p_load(rpart,rpart.plot,readr,dplyr,RCurl,rjson,lubridate,
       rvest,stringr,Hmisc,rattle,RColorBrewer,ddpcr,tidytext,tidyr,
       ggrepel,ggplot2,png,ggpubr,tidycensus,sf,plm,lmtest,stargazer,MASS,
       xtable,knitr,magick,purrr,ggthemes,gifski,extrafont,latex2exp,
       cowplot,mapproj,patchwork,remotes,tictoc,Hmisc,english,readstata13,margins,lmtest,fastDummies)

#################### READ IN DATA ################
{
  
  newdemean <- read.csv("C:/Users/joe/Dropbox (University of Oregon)/VSL-COVID-shared/intermediate-files/demeanrp-lasso.csv")[,-1]
  cmatch <- read.csv("C:/Users/joe/Dropbox (University of Oregon)/VSL-COVID-shared/intermediate-files/countymatch.csv")[,-1]
  dat <- read.csv("C:/Users/joe/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_nointx.csv") %>% 
    # dplyr::filter(rejectonly==0) %>% 
    dplyr::filter(Durationinseconds > 360) %>% 
    dplyr::filter(choiceofperson %in% 1:2) %>% 
    # dplyr::filter(rejectonly==0) %>% 
    group_by(ResponseId) %>% 
    mutate(rejectever = sum(rejectonly)) %>% 
    ungroup() %>% 
    dplyr::filter(rejectever==0) 
  
  dat$owninc <- ifelse(dat$owninc==0,NA,dat$owninc)
  
  
  dat2 <- read.dta13("C:/Users/joe/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_nointx.dta")
  
  ethnic <- read.csv("~/covid-survey/data/countyethfrac.csv")[,-1]
  
  varlabs <- data.frame(variable = names(dat2),label= attributes(dat2)$var.labels)
  varlabs$variable <- as.character(varlabs$variable)
  varlabs$label <- as.character(varlabs$label)
  varlabs <- varlabs %>% dplyr::filter(variable!="multi")
  newvarlabs <- data.frame(variable=c("feduianyavcost","feduinoneavcost","feduianyunempl","feduinoneunempl",
                                      "fedui100avcost",  "fedui200avcost",  "fedui300avcost",  "fedui400avcost",
                                      "fedui100unempl",  "fedui200unempl",  "fedui300unempl",  "fedui400unempl"),
                           label=c("Avg. hhld cost for county (federal UI > 0)",
                                   "Avg. hhld cost for county (federal UI = 0)",
                                   "Unempl rate for county (federal UI > 0)",
                                   "Unempl rate for county (federal UI = 0)",
                                   "Avg. hhld cost for county (federal UI = 100)",
                                   "Avg. hhld cost for county (federal UI = 200)",
                                   "Avg. hhld cost for county (federal UI = 300)",
                                   "Avg. hhld cost for county (federal UI = 400)",
                                   "Unempl rate for county (federal UI = 100)",
                                   "Unempl rate for county (federal UI = 200)",
                                   "Unempl rate for county (federal UI = 300)",
                                   "Unempl rate for county (federal UI = 400)"))
  
  varlabs <- rbind(varlabs,newvarlabs)
  
  dat$mabscases <- dat$mabscases
  dat$avcost <- dat$avcost/100
  
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
  
  dat$feduianyavcost <- dat$feduiany*dat$avcost
  dat$feduinoneavcost <- dat$feduinone*dat$avcost
  
  dat$feduianyunempl <- dat$feduiany*dat$unempl
  dat$feduinoneunempl <- dat$feduinone*dat$unempl
  
  
  dat$fedui100avcost <- dat$fedui100*dat$avcost
  dat$fedui200avcost <- dat$fedui200*dat$avcost
  dat$fedui300avcost <- dat$fedui300*dat$avcost
  dat$fedui400avcost <- dat$fedui400*dat$avcost
  
  dat$fedui100unempl <- dat$fedui100*dat$unempl
  dat$fedui200unempl <- dat$fedui200*dat$unempl
  dat$fedui300unempl <- dat$fedui300*dat$unempl
  dat$fedui400unempl <- dat$fedui400*dat$unempl
  
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
  
  dat$threealts2 <- ifelse(dat$threealts==0,"Alts2","Alts3") %>% as.factor()
  
  
  dat <- dummy_cols(dat,
                    select_columns=c("rule1","rule2","rule3","rule4","rule5","rule6","rule7","rule8","rule9","rule10"),
                    remove_first_dummy = T)
  
  names(dat) <- names(dat) %>% str_replace_all("_",".")
  
  dat <- dat %>% mutate(worstunempl = pmax(unempr20201,unempr20202,unempr20203,unempr20204,unempr20205,unempr20206,
                                           unempr20207,unempr20208,unempr20209,unempr202010,unempr202011,unempr202012))
  
  dat$relunemp <- ifelse(dat$worstunempl > median(dat$worstunempl),"WorseUnemp","BetterUnemp") %>% as.factor()
  
}


tdat <- dat

tdat$feduinoneavcost_noloss <- tdat$feduinoneavcost*I(tdat$incloss==0)
tdat$feduinoneavcost_yesloss <- tdat$feduinoneavcost*I(tdat$incloss==1)
tdat$feduianyavcost_noloss <- tdat$feduianyavcost*I(tdat$incloss==0)
tdat$feduianyavcost_yesloss <- tdat$feduianyavcost*I(tdat$incloss==1)
tdat$feduinoneunempl_noloss <- tdat$feduinoneunempl*I(tdat$incloss==0)
tdat$feduinoneunempl_yesloss <- tdat$feduinoneunempl*I(tdat$incloss==1)
tdat$feduianyunempl_noloss <- tdat$feduianyunempl*I(tdat$incloss==0)
tdat$feduianyunempl_yesloss <- tdat$feduianyunempl*I(tdat$incloss==1)

tdat$feduinoneavcost_poor <- tdat$feduinoneavcost*I(tdat$inccont<tdat$zipmhhinc)
tdat$feduinoneavcost_rich <- tdat$feduinoneavcost*I(tdat$inccont>=tdat$zipmhhinc)
tdat$feduianyavcost_poor <- tdat$feduianyavcost*I(tdat$inccont<tdat$zipmhhinc)
tdat$feduianyavcost_rich <- tdat$feduianyavcost*I(tdat$inccont>=tdat$zipmhhinc)
tdat$feduinoneunempl_poor <- tdat$feduinoneunempl*I(tdat$inccont<tdat$zipmhhinc)
tdat$feduinoneunempl_rich <- tdat$feduinoneunempl*I(tdat$inccont>=tdat$zipmhhinc)
tdat$feduianyunempl_poor <- tdat$feduianyunempl*I(tdat$inccont<tdat$zipmhhinc)
tdat$feduianyunempl_rich <- tdat$feduianyunempl*I(tdat$inccont>=tdat$zipmhhinc)

reg1 <- clogit(best ~
                   mabsdeaths*lassorpfl + mabscases*lassorpfl +
                   feduinoneavcost_poor*lassorpfl + feduianyavcost_poor*lassorpfl + 
                   feduinoneunempl_poor*lassorpfl + feduianyunempl_poor*lassorpfl + 
                 feduinoneavcost_rich*lassorpfl + feduianyavcost_rich*lassorpfl + 
                 feduinoneunempl_rich*lassorpfl + feduianyunempl_rich*lassorpfl + 
                   
                   factor(rule1)*lassorpfl + factor(rule2)*lassorpfl +
                   factor(rule3)*lassorpfl + factor(rule4)*lassorpfl +
                   factor(rule5)*lassorpfl + factor(rule6)*lassorpfl +
                   factor(rule7)*lassorpfl + factor(rule8)*lassorpfl +
                   factor(rule9)*lassorpfl + factor(rule10)*lassorpfl +
                   
                   statquo*lassorpfl +
                   strata(choice),data=tdat[which(dat$choiceofperson %in% 1:2),]
                 ,weights=popwt,method="approximate"
)

summary(reg1)

stargazer(reg1,type="text")

xtabs(~inccont+incloss,data=dat)

summary(lm(inccont ~ ideol2 + R0subjmean + 0,data=dat))

incplot <- dat %>% group_by(inccont) %>% summarise(loss=mean(incloss,na.rm=T))

########### clustered SEs ####################

reg1 <- clogit(best ~
                 mabsdeaths*lassorpfl + mabscases*lassorpfl +
                 avcost*lassorpfl + unempl*lassorpfl + 
                 avcost*feduiany*lassorpfl + unempl*feduiany*lassorpfl +
                 
                 
                 factor(rule1)*lassorpfl + factor(rule2)*lassorpfl +
                 factor(rule3)*lassorpfl + factor(rule4)*lassorpfl +
                 factor(rule5)*lassorpfl + factor(rule6)*lassorpfl +
                 factor(rule7)*lassorpfl + factor(rule8)*lassorpfl +
                 factor(rule9)*lassorpfl + factor(rule10)*lassorpfl +
                 
                 statquo*lassorpfl +
                 strata(choice),data=dat[which(dat$choiceofperson %in% 1:2),]
               ,weights=popwt,method="approximate",cluster=ResponseId
)

summary(reg1)

reg2 <- coxph(formula = Surv(rep(1, 3972L), best) ~ mabsdeaths * lassorpfl + 
        mabscases * lassorpfl + feduinoneavcost * lassorpfl + feduianyavcost * 
        lassorpfl + feduinoneunempl * lassorpfl + feduianyunempl * 
        lassorpfl + factor(rule1) * lassorpfl + factor(rule2) * lassorpfl + 
        factor(rule3) * lassorpfl + factor(rule4) * lassorpfl + factor(rule5) * 
        lassorpfl + factor(rule6) * lassorpfl + factor(rule7) * lassorpfl + 
        factor(rule8) * lassorpfl + factor(rule9) * lassorpfl + factor(rule10) * 
        lassorpfl + statquo * lassorpfl + strata(choice) + cluster(ResponseId), data = dat[which(dat$choiceofperson %in% 
                                                                             1:2), ], weights = popwt, method = "breslow")
summary(reg2)

main3 <- clogit(best ~
                  mabsdeaths*lassorpfl + mabscases*lassorpfl +
                  avcost*lassorpfl + unempl*lassorpfl + 
                  avcost*feduiany*lassorpfl + unempl*feduiany*lassorpfl +
                  
                  rule1*lassorpfl + rule2*lassorpfl +
                  rule3*lassorpfl + rule4*lassorpfl +
                  rule5*lassorpfl + rule6*lassorpfl +
                  rule7*lassorpfl + rule8*lassorpfl +
                  rule9*lassorpfl + rule10*lassorpfl +
                  
                  statquo*lassorpfl +
                  strata(choice),data=dat[which(dat$choiceofperson %in% 1:2),]
                ,weights=popwt,method="approximate",cluster=ResponseId
)

summary(main3)
