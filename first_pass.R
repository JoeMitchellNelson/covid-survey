require(pacman)

p_load(rpart,rpart.plot,readr,dplyr,RCurl,rjson,lubridate,
       rvest,stringr,Hmisc,rattle,RColorBrewer,ddpcr,tidytext,tidyr,
       ggrepel,ggplot2,png,ggpubr,tidycensus,sf,plm,lmtest,stargazer,MASS,
       xtable,knitr,magick,purrr,ggthemes,gifski,extrafont,latex2exp,
       cowplot,mapproj,patchwork,remotes,tictoc,Hmisc,english,readstata13)

dat <- read.dta13("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_intx.dta")
#dat <- read.dta13("C:/Users/Joe/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_intx.dta")

# below is replaced by ownclinear
# dat$costbelief <- ifelse(dat$owncmuchhi==1,2,
#                          ifelse(dat$owncsomehi==1,1,
#                                 ifelse(dat$owncsomelo==1,-1,
#                                        ifelse(dat$owncmuchlo==1,-2,0))))


dat$costbelief <- factor(dat$costbelief)
dat <- within(dat, costbelief <- relevel(costbelief, ref = 3))

dat$state <- ifelse(dat$CA==1,"CA",
                    ifelse(dat$OR==1,"OR","WA"))

dat$prep <- ifelse(dat$prepover==1,"over",
                   ifelse(dat$prepreas==1,"reasonable","poor"))

dat$rules <- ifelse(dat$rulesbad==1,"bad",
                    ifelse(dat$rulesgood==1,"good","insufficient"))

dat$schoolage <- ifelse(dat$hhld0to1 ==1 | dat$hhld2to5==1 | dat$hhld6to12==1 | dat$hhld13to17==1,1,0)

dat <- dat %>% group_by(choice) %>% mutate(months2=max(months)) %>% ungroup()

summary(clogit(best ~
                  mabsdeaths + mabsnfcases + unempl*fedui +
                  I(log(owninc-avcost+1)) +

                  rule1 + rule2 + rule3 + rule4 + rule5 +
                  rule6 + rule7 + rule8 + rule9 + rule10 +

                 # lirule1*rule1 + lirule2*rule2 + lirule3*rule3 + lirule4*rule4 + lirule5*rule5 +
                 # lirule6*rule6 + lirule7*rule7 + lirule8*rule8 + lirule9*rule9 + lirule10*rule10 +
                 # 
                 # larule1*rule1 + larule2*rule2 + larule3*rule3 + larule4*rule4 + larule5*rule5 +
                 # larule6*rule6 + larule7*rule7 + larule8*rule8 + larule9*rule9 + larule10*rule10 +

                 statquo +
                 strata(choice),data=dat[which(dat$months==1),]))


heuristics <- dat %>% group_by(choice) %>% mutate(max_deaths = max(deldeaths),max_cases=max(delcases))
heuristics$max_deaths <- ifelse(heuristics$max_deaths==heuristics$deldeaths,1,0)
heuristics$max_cases <- ifelse(heuristics$max_cases==heuristics$delcases,1,0)

new <- dat[which(dat$statquo==0),] %>% group_by(caseid) %>% summarise(rule2 = mean(rule2),
                                                                      costb = first(costbelief),
                                                                      li2 = first(lirule2))
summary(lm(as.numeric(costb) ~ rule2*li2,data=new))

summary(clogit(best ~
                 max_deaths + max_cases +
                # absdeaths*factor(months) + abscases*factor(months) +
                 unempl + I(owninc-avcost) +

                 I(rule1) + I(rule2) +
                 I(rule3) + I(rule4) +
                 I(rule5) + I(rule6) +
                 I(rule7) + I(rule8) +
                 I(rule9) + I(rule10) +
                 
                 # lirule1*rule1 + lirule2*rule2 + lirule3*rule3 + lirule4*rule4 + lirule5*rule5 +
                 # lirule6*rule6 + lirule7*rule7 + lirule8*rule8 + lirule9*rule9 + lirule10*rule10 +
                 # 
                 # larule1*rule1 + larule2*rule2 + larule3*rule3 + larule4*rule4 + larule5*rule5 +
                 # larule6*rule6 + larule7*rule7 + larule8*rule8 + larule9*rule9 + larule10*rule10 +

                 statquo*factor(costbelief) +
                 strata(choice),data=heuristics
               ,weights=popwt,method="approximate"
               ))


ggplot(dat[which(dat$statquo==0),]) +
  geom_point(aes(x=delcases,y=avcost,color=log(countypop)),alpha=.3) +
  scale_color_viridis_c() +
  geom_smooth(aes(x=delcases,y=avcost),method="lm",se=T)

ggplot(dat[which(dat$statquo==0),]) +
  geom_point(aes(x=delcases/(months*countypop),y=avcost,color=log(countypop)),alpha=.3) +
  scale_color_viridis_c() +
  geom_smooth(aes(x=delcases/(months*countypop),y=avcost),method="lm",se=T)

ggplot(dat[which(dat$statquo==0 & dat$countypop > 100000),]) +
  geom_point(aes(x=delcases/(months*countypop),y=avcost,color=log(countypop)),alpha=.3) +
  scale_color_viridis_c() +
  geom_smooth(aes(x=delcases/(months*countypop),y=avcost),method="lm",se=T)

summary(lm(avcost ~ fedui + unempl, data=dat[which(dat$statquo==0 & dat$months==1),]))

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

summary(lm(hhldinc ~ prep, data=people,weights=popwt))

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
