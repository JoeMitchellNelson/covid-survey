require(pacman)

p_load(rpart,rpart.plot,readr,dplyr,RCurl,rjson,lubridate,
       rvest,stringr,Hmisc,rattle,RColorBrewer,ddpcr,tidytext,tidyr,
       ggrepel,ggplot2,png,ggpubr,tidycensus,sf,plm,lmtest,stargazer,MASS,
       xtable,knitr,magick,purrr,ggthemes,gifski,extrafont,latex2exp,
       cowplot,mapproj,patchwork,remotes,tictoc,Hmisc,english,readstata13,gmnl,poLCA)

{
  
  newdemean <- read.csv("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID-shared/intermediate-files/demeanrp-lasso.csv")[,-1]
  cmatch <- read.csv("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID-shared/intermediate-files/countymatch.csv")[,-1]
  dat <- read.csv("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_nointx.csv") %>% 
    # dplyr::filter(rejectonly==0) %>% 
    dplyr::filter(Durationinseconds > 360) %>% 
    dplyr::filter(choiceofperson %in% 1:2) %>% 
    # dplyr::filter(rejectonly==0) %>% 
    group_by(ResponseId) %>% 
    mutate(rejectever = sum(rejectonly)) %>% 
    ungroup() %>% 
    dplyr::filter(rejectever==0) 
  
  dat$owninc <- ifelse(dat$owninc==0,NA,dat$owninc)
  dat$caseid <- as.numeric(dat2$ResponseId)
  
  
  dat2 <- read.dta13("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_nointx.dta")
  
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
  
  dat$mabscases <- dat$mabscases/1000
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
  
  
  
  
}


set.seed(123)

dat2 <- dat[sample(1:nrow(dat)),] %>% arrange(ResponseId,choiceofperson)
dat2 <- dat2 %>% dplyr::select(ResponseId,choiceofperson,choice,
                               best,popwt,
                               mabsdeaths, mabscases,
                               feduinoneavcost, feduianyavcost, 
                               feduinoneunempl, feduianyunempl, 
                               rule1, rule2,
                               rule3, rule4,
                               rule5, rule6,
                               rule7, rule8,
                               rule9, rule10,
                               statquo,
                               lassorpfl,
                               choice,
                               alt,
                               caseid,
                               female,ideolcon,ideollib)

#dat2$choice <- 1:nrow(dat2)
dat2$caseid <- as.numeric(dat2$ResponseId)


write.csv(dat,"~/covid-survey/data/for_stata_lclogit.csv")

install.packages("https://cran.r-project.org/src/contrib/Archive/mlogit/mlogit_1.0-2.tar.gz", repos=NULL,type="source")
library(mlogit)

tcodes <- read.csv("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/number_non-varying.csv")

#dat3 <- read.csv("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/forR_VSL-COVID_two-alt_long.csv")
dat <- read.dta13("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/forR_VSL-COVID_two-alt_long.dta")

dat2$alt <- ifelse(dat2$alt==3,2,1)

dat3 <- mlogit.data(dat2, chid.var = "choice", id.var="caseid", alt.var="alt", choice = "best", varying = tcodes[,3]:tcodes[,4], shape = "long", sep = "")

dat3 <- mlogit.data(dat2, chid.var = "choice", id.var="caseid", alt.var="alt", choice = "best", varying = c(3,6:ncol(dat2)), shape = "long", sep = "")


lcmodel <- gmnl(best ~ 
                  mabsdeaths + 
                  mabscases +
                   feduinoneavcost + feduianyavcost + 
                   feduinoneunempl + feduianyunempl + 
                   
                   rule1 + rule2 +
                   rule3 + rule4 +
                   rule5 + rule6 +
                   rule7 + rule8 +
                   rule9 + rule10 +
                  
                 statquo 
                
                | 0 | 0 | 0 | 0, data = dat3, model = "mixl",
                panel = T, Q = 3,seed=123,method="bfgs",ranp=ranp1)
summary(lcmodel)
s <- shares(lcmodel)
s

res <- lcmodel$prob.alt
summary(res)

dat2 <- dat2 %>% group_by(ResponseId,choiceofperson) %>% 
  summarise(ResponseId = first(ResponseId),
            choiceofperson = first(choiceofperson),
            best=first(best),
            popwt=first(popwt),
            lassorpfl = first(lassorpfl),
            statquo = first(statquo) - last(statquo),
            mabsdeaths = first(mabsdeaths)-last(mabsdeaths),
            mabscases = first(mabscases)-last(mabscases),
            feduinoneavcost = first(feduinoneavcost) - last(feduinoneavcost), 
            feduianyavcost = first(feduianyavcost) - last(feduianyavcost), 
            feduinoneunempl = first(feduinoneunempl) - last(feduinoneunempl), 
            feduianyunempl = first(feduianyunempl) - last(feduianyunempl), 
            rule1 = first(rule1) - last(rule1),
            rule2 = first(rule2) - last(rule2), 
            rule3 = first(rule3) - last(rule3), 
            rule4 = first(rule4) - last(rule4), 
            rule5 = first(rule5) - last(rule5), 
            rule6 = first(rule6) - last(rule6), 
            rule7 = first(rule7) - last(rule7), 
            rule8 = first(rule8) - last(rule8), 
            rule9 = first(rule9) - last(rule9), 
            rule10 = first(rule10) - last(rule10),
            female=first(female),
            ideolcon=first(ideolcon),
            ideollib=first(ideollib))



summary(glm(best ~ mabsdeaths + mabscases +
              feduinoneavcost + feduianyavcost + 
              feduinoneunempl + feduianyunempl + 
              
              rule1 + rule2 +
              rule3 + rule4 +
              rule5 + rule6 +
              rule7 + rule8 +
              rule9 + rule10 +
              
              statquo +
              
              mabsdeaths:lassorpfl + mabscases:lassorpfl +
              feduinoneavcost:lassorpfl + feduianyavcost:lassorpfl + 
              feduinoneunempl:lassorpfl + feduianyunempl:lassorpfl + 
              
              rule1:lassorpfl + rule2:lassorpfl +
              rule3:lassorpfl + rule4:lassorpfl +
              rule5:lassorpfl + rule6:lassorpfl +
              rule7:lassorpfl + rule8:lassorpfl +
              rule9:lassorpfl + rule10:lassorpfl +
              
              statquo:lassorpfl + 0, weights=popwt, family = binomial(link = "logit"), 
            data = dat2))


dat2$idk <- rep(lcmodel$prob.ind,each=2)

summary(lm(idk ~ female + ideolcon + ideollib,data=dat2))


plot_ci_lc(lcmodel, var = c("feduinoneavcost"))

s %*% lcmodel$coefficients[which(str_detect(names(lcmodel$coefficients),"feduinoneavcost"))]

WTPs <- -lcmodel$coefficients[which(str_detect(names(lcmodel$coefficients),"mabsdeaths"))]/lcmodel$coefficients[which(str_detect(names(lcmodel$coefficients),"feduinoneavcost"))]
WTPs %*% s
WTPs
