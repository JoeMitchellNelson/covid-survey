require(pacman)

p_load(rpart,rpart.plot,readr,dplyr,RCurl,rjson,lubridate,
       rvest,stringr,Hmisc,rattle,RColorBrewer,ddpcr,tidytext,tidyr,
       ggrepel,ggplot2,png,ggpubr,tidycensus,sf,plm,lmtest,stargazer,MASS,
       xtable,knitr,magick,purrr,ggthemes,gifski,extrafont,latex2exp,
       cowplot,mapproj,patchwork,remotes,tictoc,Hmisc,english,readstata13)

#dat <- read.dta13("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_intx.dta")
dat <- read.dta13("C:/Users/Joe/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_intx.dta")




dat <- dat %>% mutate(con = 5*sconserv + 4*mconserv + 3*moderate + 2*mliberal + 1*sliberal)
dat$con <- ifelse(dat$con==0,9,dat$con)
dat$con_ <- as.factor(dat$con)

fixfed <- dat %>% group_by(choice) %>% summarise(fedui2=max(fedui))

dat <- left_join(dat,fixfed)
dat$state <- ifelse(dat$CA==1,"CA",
                    ifelse(dat$OR==1,"OR","WA"))

dat$prep <- ifelse(dat$prepover==1,"over",
                   ifelse(dat$prepreas==1,"reasonable","poor"))

dat$rules <- ifelse(dat$rulesbad==1,"bad",
                    ifelse(dat$rulesgood==1,"good","insufficient"))

dat$schoolage <- ifelse(dat$hhld0to1 ==1 | dat$hhld2to5==1 | dat$hhld6to12==1 | dat$hhld13to17==1,1,0)

summary(clogit(best ~
                  delcases*con_ + deldeaths*con_ + unempl*con_ + avcost*con_ +

                  rule1*con_ + rule2*con_ + rule3*con_ + rule4*con_ + rule5*con_ +
                  rule6*con_ + rule7*con_ + rule8*con_ + rule9*con_ + rule10*con_ +

                 # lirule1*rule1 + lirule2*rule2 + lirule3*rule3 + lirule4*rule4 + lirule5*rule5 +
                 # lirule6*rule6 + lirule7*rule7 + lirule8*rule8 + lirule9*rule9 + lirule10*rule10 +
                 #
                 # larule1*rule1 + larule2*rule2 + larule3*rule3 + larule4*rule4 + larule5*rule5 +
                 # larule6*rule6 + larule7*rule7 + larule8*rule8 + larule9*rule9 + larule10*rule10 +

                 statquo*con_ +
                 strata(choice),data=dat[which(dat$reject==0),]))


heuristics <- dat %>% group_by(choice) %>% mutate(max_deaths = max(deldeaths),max_cases=max(delcases))
heuristics$max_deaths <- ifelse(heuristics$max_deaths==heuristics$deldeaths,1,0)
heuristics$max_cases <- ifelse(heuristics$max_cases==heuristics$delcases,1,0)


summary(clogit(best ~
                 max_deaths + max_cases +
                 unempl + avcost +

                 factor(rule1) + factor(rule2) + factor(rule3) + factor(rule4) + factor(rule5) +
                 factor(rule6) + factor(rule7) + factor(rule8) + factor(rule9) + factor(rule10) +
                 
                 # lirule1*rule1 + lirule2*rule2 + lirule3*rule3 + lirule4*rule4 + lirule5*rule5 +
                 # lirule6*rule6 + lirule7*rule7 + lirule8*rule8 + lirule9*rule9 + lirule10*rule10 +
                 # 
                 # larule1*rule1 + larule2*rule2 + larule3*rule3 + larule4*rule4 + larule5*rule5 +
                 # larule6*rule6 + larule7*rule7 + larule8*rule8 + larule9*rule9 + larule10*rule10 +

                 statquo +
                 strata(choice),data=heuristics[which(dat$reject==0),]
              # ,weights=popwt,method="approximate"
               ))


######## look at data on individuals #############

people <- dat %>% group_by(caseid) %>% summarise(con=mean(con),
                                                 lirule9=mean(lirule9),
                                                 larule9=mean(larule9),
                                                 rnought = mean(rnoughtmean),
                                                 reject=mean(reject),
                                                 govstayout=mean(govstayout),
                                                 vulnnonwhite=mean(vulnnonwhite),
                                                 vulnrural=mean(vulnrural),
                                                 vulnnoneng=mean(vulnnoneng),
                                                 female=mean(female),
                                                 black=mean(black),
                                                 asian=mean(asian),
                                                 multi=mean(multi),
                                                 hhldinc=mean(hhldinc),
                                                 state=first(state),
                                                 prep=first(prep),
                                                 rules = first(rules),
                                                 w_uneven=first(w_uneven),
                                                 schoolage=first(schoolage),
                                                 idrule1 =first(idrule1),
                                                 idrule2 =first(idrule2),
                                                 idrule3 =first(idrule3),
                                                 idrule4 =first(idrule4),
                                                 idrule5 =first(idrule5),
                                                 idrule6 =first(idrule6),
                                                 idrule7 =first(idrule7),
                                                 idrule8 =first(idrule8),
                                                 idrule9 =first(idrule9),
                                                 idrule10 =first(idrule10),
                                                 popwt=first(popwt))

people$race = ifelse(people$black==1,"black",
                     ifelse(people$multi==1,"multi",
                            ifelse(people$asian==1,"asian","white/hisp")))

summary(lm(idrule3 ~ I(female==0)*schoolage + factor(con)*schoolage, data=people,weights=popwt))

summary(polr(factor(con) ~ govstayout + rules + prep + female + factor(state) +  
              multi +  asian + black + log(hhldinc), data=people[which(people$con<9),],
     method = "logistic"))


treemodel <- rpart(factor(con) ~ govstayout + rules + prep + female + race + hhldinc,
                   data=people[which(people$con<9),which(names(people)!="caseid")],
                   weights=popwt,
                   control=rpart.control(cp=0.007))
treemodel %>% rpart.plot(fallen.leaves=F,box.palette=0,tweak=1.1)
treemodel %>% summary()

############## idrules distributions plot ##############

idplot1 <- ggplot(people) + geom_histogram(aes(x=idrule1),bins=4,fill="forestgreen",color="black") +labs(x="Grocery/essential",y="") + theme_minimal()
idplot2 <- ggplot(people) + geom_histogram(aes(x=idrule2),bins=4,fill="forestgreen",color="black") +labs(x="Non-essential retail",y="") + theme_minimal()
idplot3 <- ggplot(people) + geom_histogram(aes(x=idrule3),bins=4,fill="forestgreen",color="black") +labs(x="Schools",y="") + theme_minimal()
idplot4 <- ggplot(people) + geom_histogram(aes(x=idrule4),bins=4,fill="forestgreen",color="black") +labs(x="Colleges",y="") + theme_minimal()
idplot5 <- ggplot(people) + geom_histogram(aes(x=idrule5),bins=4,fill="forestgreen",color="black") +labs(x="Parks",y="") + theme_minimal()
idplot6 <- ggplot(people) + geom_histogram(aes(x=idrule6),bins=4,fill="forestgreen",color="black") +labs(x="Gyms",y="") + theme_minimal()
idplot7 <- ggplot(people) + geom_histogram(aes(x=idrule7),bins=4,fill="forestgreen",color="black") +labs(x="Theaters",y="") + theme_minimal()
idplot8 <- ggplot(people) + geom_histogram(aes(x=idrule8),bins=4,fill="forestgreen",color="black") +labs(x="Restaurants",y="") + theme_minimal()
idplot9 <- ggplot(people) + geom_histogram(aes(x=idrule9),bins=4,fill="forestgreen",color="black") +labs(x="Religious/Social",y="") + theme_minimal()
idplot10 <- ggplot(people) + geom_histogram(aes(x=idrule10),bins=4,fill="forestgreen",color="black") +labs(x="Institutions",y="") + theme_minimal()

(idplot1 + idplot2) / (idplot3 + idplot4) / (idplot5 + idplot6) / (idplot7 + idplot8) / (idplot9 + idplot10)
