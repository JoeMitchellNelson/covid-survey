require(pacman)

p_load(rpart,rpart.plot,readr,dplyr,RCurl,rjson,lubridate,
       rvest,stringr,Hmisc,rattle,RColorBrewer,ddpcr,tidytext,tidyr,
       ggrepel,ggplot2,png,ggpubr,tidycensus,sf,plm,lmtest,stargazer,MASS,
       xtable,knitr,magick,purrr,ggthemes,gifski,extrafont,latex2exp,
       cowplot,mapproj,patchwork,remotes,tictoc,Hmisc,english,readstata13)

dat <- read.dta13("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_intx.dta")
#dat <- read.dta13("C:/Users/Joe/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_intx.dta")
raw <- read.csv("~/covid-survey/data/qualtrics_raw.csv")
raw2 <- raw %>% dplyr::filter(ResponseID=="R_daKqb5ILM3r0XE5")

#testcase <- dat %>% dplyr::filter(ResponseId=="R_daKqb5ILM3r0XE5")
testcase <- dat %>% dplyr::filter(Durationinseconds==5140 & WA==1 & female==0 & zip==98593) %>% 
  dplyr::select(best,choice,alt,months,caseswo,deathswo,deldeaths,delcases,absdeaths,abscases,mabsdeaths,mabscases,rule1,rule2,rule3,rule4, rule5,
                rule6, rule7, rule8, rule9, rule10,
                avcost,unempl) %>% t()
write.csv(testcase,"~/covid-survey/R_daKqb5ILM3r0XE5_rows.csv")
write.csv(raw2,"~/covid-survey/R_daKqb5ILM3r0XE5_raw_from_qualtrics.csv")

###############

dat2 <- dat %>% group_by(ResponseId) %>% summarise(n=n(),nopolicies=sum(alt==3 & best==1 & threealts==1))
sum(dat2$n==14 & dat2$nopolicies==1)

dat3 <- dat %>% group_by(ResponseId) %>% summarise()

dat3 <- dat %>% group_by(ResponseId) %>% summarise(n_on_third = sum(choiceofperson==3 & alt==3 & best==1),
                                                   n_on_fifth = sum(choiceofperson %in% 6 & threealts==1 & alt==3 & best==1))
small <- dat %>% dplyr::select(ResponseId,best,alt,choiceofperson)

dat4 <- dat %>% group_by(ResponseId) %>% summarise(n_on_CDN = sum(alt==3 & best==1 & choiceofperson == 3 & threealts==1),
                                                   ans4or5 = sum(choiceofperson %in% 4:5)/2,
                                                   n_on_EFN = sum(alt==3 & best==1 & choiceofperson == 6 & threealts==1),
                                                   ans7or8 = sum(choiceofperson %in% 7:8)/2)

dat5 <- dat %>% group_by(ResponseId) %>% summarise(pickedC = sum(alt==1 & best==1 & choiceofperson==3))

ggplot(dat[which(dat$alt!=3),]) +
  geom_point(aes(x=log(deldeaths),y=log(delcases)),color="purple")
