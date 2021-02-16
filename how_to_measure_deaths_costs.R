require(pacman)

p_load(rpart,rpart.plot,readr,dplyr,RCurl,rjson,lubridate,
       rvest,stringr,Hmisc,rattle,RColorBrewer,ddpcr,tidytext,tidyr,
       ggrepel,ggplot2,png,ggpubr,tidycensus,sf,plm,lmtest,stargazer,MASS,
       xtable,knitr,magick,purrr,ggthemes,gifski,extrafont,latex2exp,
       cowplot,mapproj,patchwork,remotes,tictoc,Hmisc,english,readstata13)

dat <- read.dta13("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_intx.dta")
#dat <- read.dta13("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/stacked_covid_responses.dta")
#dat <- read.dta13("C:/Users/Joe/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_intx.dta")

dat$pzipwhite <- 1 - (dat$pzipasian+dat$pzipblack+dat$pzipothrace3+dat$pzipmulti)

dat <- dat %>% mutate(racehhi = ((pzipasian*100)^2 + (pzipwhite*100)^2 + (pzipblack*100)^2 +(pzipothrace3*100)^2 + (pzipmulti*100)^2)/10000)
dat <- dat %>% mutate(racehhi_c = (racehhi-mean(racehhi))/sd(racehhi))
dat <- dat %>% mutate(pzipwhite_c = (pzipwhite-mean(pzipwhite))/sd(pzipwhite))
dat <- dat %>% mutate(pzipblack_c = (pzipblack-mean(pzipblack))/sd(pzipblack))
dat <- dat %>% mutate(hhldinc_c = (hhldinc-mean(hhldinc))/sd(hhldinc))
dat$white <- ifelse(dat$black + dat$asian + dat$multi ==0,1,0)

dat <- dat %>% mutate(con = 5*sconserv + 5*mconserv + 3*moderate + 1*mliberal + 1*sliberal)
dat$con <- ifelse(dat$con==0,9,dat$con)
dat$con_ <- as.factor(dat$con)
dat <- within(dat, con_ <- relevel(con_, ref = 2))
dat$schoolage <- ifelse(dat$hhld0to1 ==1 | dat$hhld2to5==1 | dat$hhld6to12==1 | dat$hhld13to17==1,1,0)


dat <- dat %>% mutate(cases=mabscases/(countypop/100000),deaths=mabsdeaths/(countypop/100000))

dat <- dat %>% mutate(mcaseswo = caseswo/months, mdeathswo=deathswo/months)

dat$countypopbins <- cut(dat$countypop,10)

a <- (clogit(best ~
                  cases*demeanrp + deaths*demeanrp + unempl*demeanrp + avcost*demeanrp +
                 
                  rule1*demeanrp + rule2*demeanrp + rule3*demeanrp + rule4*demeanrp + rule5*demeanrp +
                  rule6*demeanrp + rule7*demeanrp + rule8*demeanrp + rule9*demeanrp + rule10*demeanrp +
                 
                  statquo*demeanrp +
                  strata(choice),data=dat[which(dat$reject==0),],
             weights=popwt,method="approximate"))

b <- (clogit(best ~
               mabscases*demeanrp + mabsdeaths*demeanrp + unempl*demeanrp + avcost*demeanrp +

               rule1*demeanrp + rule2*demeanrp + rule3*demeanrp + rule4*demeanrp + rule5*demeanrp +
               rule6*demeanrp + rule7*demeanrp + rule8*demeanrp + rule9*demeanrp + rule10*demeanrp +
               
               statquo*demeanrp +
               strata(choice),data=dat[which(dat$reject==0),],
             weights=popwt,method="approximate"))




heuristics <- dat %>% group_by(choice) %>% mutate(max_deaths = max(deldeaths),max_cases=max(delcases),
                                                  rv_deaths = var(deldeaths)/mean(deldeaths),
                                                  rv_cases = var(delcases)/mean(delcases))
heuristics$max_deaths <- ifelse(heuristics$max_deaths==heuristics$deldeaths,1,0)
heuristics$max_cases <- ifelse(heuristics$max_cases==heuristics$delcases,1,0)
heuristics <- heuristics %>% group_by(choice) %>% mutate(most_expensive=ifelse(avcost==max(avcost),1,0)) %>% ungroup()
heuristics <- heuristics %>% group_by(choice) %>% mutate(max_unempl=ifelse(unempl==max(unempl),1,0)) %>% ungroup()

c <- (clogit(best ~
               
               max_cases*demeanrp + max_deaths*demeanrp + max_unempl*demeanrp + most_expensive*demeanrp +
               
               rule1*demeanrp + rule2*demeanrp + rule3*demeanrp + rule4*demeanrp + rule5*demeanrp +
               rule6*demeanrp + rule7*demeanrp + rule8*demeanrp + rule9*demeanrp + rule10*demeanrp +
               
               statquo*demeanrp +
               strata(choice),data=heuristics[which(heuristics$reject==0),],
             weights=popwt,method="approximate"
             ))
a$loglik[2]
b$loglik[2]
c$loglik[2]

tidyc <- tidy(c) %>% dplyr::filter(str_detect(term,"cases|deaths|avcost|unempl|rule|expensive") & !str_detect(term,"demeanrp"))

#########################################################################

