require(pacman)

p_load(rpart,rpart.plot,readr,dplyr,RCurl,rjson,lubridate,
       rvest,stringr,Hmisc,rattle,RColorBrewer,ddpcr,tidytext,tidyr,
       ggrepel,ggplot2,png,ggpubr,tidycensus,sf,plm,lmtest,stargazer,MASS,
       xtable,knitr,magick,purrr,ggthemes,gifski,extrafont,latex2exp,
       cowplot,mapproj,patchwork,remotes,tictoc,Hmisc,english,readstata13,margins)

#dat <- read.dta13("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_intx.dta")
#dat <- read.dta13("C:/Users/Joe/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_intx.dta")

# below is replaced by ownclinear
# dat$costbelief <- ifelse(dat$owncmuchhi==1,2,
#                          ifelse(dat$owncsomehi==1,1,
#                                 ifelse(dat$owncsomelo==1,-1,
#                                        ifelse(dat$owncmuchlo==1,-2,0))))

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

###### VAR GROUPS ###########




basevars <- c("mabsdeaths","mabsnfcases","avcost","unempl")
basefedui <- c("mabsdeaths","mabsnfcases","avcost*fedui","unempl*fedui")
rules <- c("rule1","rule2","rule3","rule4","rule5","rule6","rule7","rule8","rule9","rule10")
rulesfactors <- paste("factor(",rules,")")
rulesany <- paste("I(",rules,">1)")
larules <- paste0("la",rules) %>% paste(rules,sep="*")
lirules <- paste0("li",rules) %>% paste(rules,sep="*")
ideolbase <- paste(basevars,"ideol",sep="*")
ideolbasefedui <- paste(basefedui,"ideol",sep="*")
ideolrules <- paste(rules,"ideol",sep="*")

dat <- dat %>% dplyr::mutate(nocommute = pzipiconstr + pzipiagric + pzipiartent + pzipimanuf + pzipitransp + pzipiwholes + pzipiretail + pzipiothserv)

#######################################


summary(main <- clogit(best ~
                         mabsdeaths*lassorpfl + mabscases*lassorpfl +
                         avcost*lassorpfl + 
                         unempl*lassorpfl +
                         
                         rule1*lassorpfl + rule2*lassorpfl +
                         rule3*lassorpfl + rule4*lassorpfl +
                         rule5*lassorpfl + rule6*lassorpfl +
                         rule7*lassorpfl + rule8*lassorpfl +
                         rule9*lassorpfl + rule10*lassorpfl +
                         
                         
                         statquo*lassorpfl +
                         strata(choice),data=dat
                       ,weights=popwt,method="approximate"
))




summary(main2 <- clogit(best ~
                          mabsdeaths*lassorpfl + mabscases*lassorpfl +
                          unempl*lassorpfl +
                          
                          rule1*lassorpfl + rule2*lassorpfl +
                          rule3*lassorpfl + rule4*lassorpfl +
                          rule5*lassorpfl + rule6*lassorpfl +
                          rule7*lassorpfl + rule8*lassorpfl +
                          rule9*lassorpfl + rule10*lassorpfl +
                          
                          
                          statquo*lassorpfl +
                         strata(choice),data=dat
                       ,weights=popwt2,method="approximate"
))

summary(main3 <- clogit(best ~
                          mabsdeaths*lassorpfl + mabscases*lassorpfl +
                          avcost*lassorpfl + 
                          
                          rule1*lassorpfl + rule2*lassorpfl +
                          rule3*lassorpfl + rule4*lassorpfl +
                          rule5*lassorpfl + rule6*lassorpfl +
                          rule7*lassorpfl + rule8*lassorpfl +
                          rule9*lassorpfl + rule10*lassorpfl +
                          
                          
                          statquo*lassorpfl +
                          strata(choice),data=dat
                        ,weights=popwt2,method="approximate"
))


t1 <- stargazer(main,main2,main3,keep.stat = "n",type="latex") %>% paste(collapse="\n")

for (i in 1:nrow(varlabs)) {
  if(str_detect(t1,paste0("",varlabs$variable[i]))) {
   t1 <- str_replace_all(t1,varlabs$variable[i],varlabs$label[i])
  }
}

cat(t1)

res <- broom::tidy(main) %>% 
  dplyr::filter(!str_detect(term,"lassorp") & !is.na(estimate)) %>% 
  mutate(star=ifelse(p.value<0.05,"*","")) %>% 
  dplyr::select(star,everything())

res$term <- str_remove_all(res$term,"ideol2")

View(res)
stargazer(main2,type="text")

# CAN WE COMMUTE? ***

summary(a <- clogit(best ~
                 mabsdeaths + mabscases +
                 unempl*nocommute*havzipindus + 
                 
                 rule1 + rule2 +
                 rule3 + rule4 +
                 rule5 + rule6 +
                 rule7 + rule8 +
                 rule9 + rule10 +
                 
                 # lirule1*rule1 + lirule2*rule2 + lirule3*rule3 + lirule4*rule4 + lirule5*rule5 +
                 # lirule6*rule6 + lirule7*rule7 + lirule8*rule8 + lirule9*rule9 + lirule10*rule10 +
                 # 
                 # larule1*rule1 + larule2*rule2 + larule3*rule3 + larule4*rule4 + larule5*rule5 +
                 # larule6*rule6 + larule7*rule7 + larule8*rule8 + larule9*rule9 + larule10*rule10 +
                 
                 statquo +
                 strata(choice),data=dat
                ,weights=popwt,method="approximate"
))

t1 <- stargazer(a,keep.stat = "n") %>% paste(collapse="\n")

for (i in 1:nrow(varlabs)) {
  if(str_detect(t1,paste0("",varlabs$variable[i]))) {
    t1 <- str_replace_all(t1,varlabs$variable[i],varlabs$label[i])
  }
}

cat(t1)

# POLITICAL IDEOLOGY ***

summary(b <- clogit(best ~
                 mabsdeaths*ideol + mabscases*ideol +
                 avcost*feduiany*ideol + 
                 
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
                 
                 statquo +
                 strata(choice),data=dat
               ,weights=popwt,method="approximate"
))




# RETAIL x RULE 2

summary(c <- clogit(best ~
                      mabsdeaths + mabsnfcases +
                      avcost + unempl +
                      
                      rule1 + rule2*pzipiretail +
                      rule3 + rule4 +
                      rule5 + rule6 +
                      rule7 + rule8 +
                      rule9 + rule10 +
                      
                      # lirule1*rule1 + lirule2*rule2 + lirule3*rule3 + lirule4*rule4 + lirule5*rule5 +
                      # lirule6*rule6 + lirule7*rule7 + lirule8*rule8 + lirule9*rule9 + lirule10*rule10 +
                      # 
                      # larule1*rule1 + larule2*rule2 + larule3*rule3 + larule4*rule4 + larule5*rule5 +
                      # larule6*rule6 + larule7*rule7 + larule8*rule8 + larule9*rule9 + larule10*rule10 +
                      
                      statquo +
                      strata(choice),data=dat
                    ,weights=popwt,method="approximate"
))

t1 <- stargazer(c,keep.stat = "n") %>% paste(collapse="\n")

for (i in 1:nrow(varlabs)) {
  if(str_detect(t1,paste0(" ",varlabs$variable[i]))) {
    t1 <- str_replace_all(t1,varlabs$variable[i],varlabs$label[i])
  }
}

cat(t1)

# ED/HEALTH on Rule 3

summary(c <- clogit(best ~
                      mabsdeaths + mabsnfcases +
                      avcost + unempl +
                      
                      rule1 + rule2 +
                      rule3*pzipiedserv + rule4 +
                      rule5 + rule6 +
                      rule7 + rule8 +
                      rule9 + rule10 +
                      
                      # lirule1*rule1 + lirule2*rule2 + lirule3*rule3 + lirule4*rule4 + lirule5*rule5 +
                      # lirule6*rule6 + lirule7*rule7 + lirule8*rule8 + lirule9*rule9 + lirule10*rule10 +
                      # 
                      # larule1*rule1 + larule2*rule2 + larule3*rule3 + larule4*rule4 + larule5*rule5 +
                      # larule6*rule6 + larule7*rule7 + larule8*rule8 + larule9*rule9 + larule10*rule10 +
                      
                      statquo +
                      strata(choice),data=dat
                    ,weights=popwt,method="approximate"
))

# ED/HEALTH on cases/deaths

summary(c <- clogit(best ~
                      mabsdeaths*pzipiedserv  + mabsnfcases*pzipiedserv  +
                      avcost + unempl +
                      
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
                      
                      statquo +
                      strata(choice),data=dat
                    ,weights=popwt,method="approximate"
))

# IDEOLOGY on rule 8

summary(c <- clogit(best ~
                      mabsdeaths  + mabsnfcases  +
                      avcost + unempl +
                      
                      I(rule1) + I(rule2) +
                      I(rule3) + I(rule4)*ideol +
                      I(rule5) + I(rule6) +
                      I(rule7) + I(rule8)*ideol +
                      I(rule9) + I(rule10) +
                      
                      # lirule1*rule1 + lirule2*rule2 + lirule3*rule3 + lirule4*rule4 + lirule5*rule5 +
                      # lirule6*rule6 + lirule7*rule7 + lirule8*rule8 + lirule9*rule9 + lirule10*rule10 +
                      # 
                      # larule1*rule1 + larule2*rule2 + larule3*rule3 + larule4*rule4 + larule5*rule5 +
                      # larule6*rule6 + larule7*rule7 + larule8*rule8 + larule9*rule9 + larule10*rule10 +
                      
                      statquo +
                      strata(choice),data=dat
                    ,weights=popwt,method="approximate"
))


# IDEOLOGY on everything

summary(c <- clogit(best ~
                      mabsdeaths*ideol  + mabsnfcases*ideol  +
                      avcost*ideol + unempl*ideol +
                      
                      I(rule1)*ideol + I(rule2)*ideol +
                      I(rule3)*ideol + I(rule4)*ideol +
                      I(rule5)*ideol + I(rule6)*ideol +
                      I(rule7)*ideol + I(rule8)*ideol +
                      I(rule9)*ideol + I(rule10)*ideol +
                      
                      # lirule1*rule1 + lirule2*rule2 + lirule3*rule3 + lirule4*rule4 + lirule5*rule5 +
                      # lirule6*rule6 + lirule7*rule7 + lirule8*rule8 + lirule9*rule9 + lirule10*rule10 +
                      # 
                      # larule1*rule1 + larule2*rule2 + larule3*rule3 + larule4*rule4 + larule5*rule5 +
                      # larule6*rule6 + larule7*rule7 + larule8*rule8 + larule9*rule9 + larule10*rule10 +
                      
                      statquo*ideol +
                      strata(choice),data=dat
                    ,weights=popwt,method="approximate"
))



# IDEOLOGY on 4,8 and costs

summary(c <- clogit(best ~
                      mabsdeaths*ideol  + mabscases*ideol  +
                      avcost*ideol + unempl*ideol +
                      
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
                      
                      statquo*ideol +
                      strata(choice),data=dat
                    ,weights=popwt,method="approximate"
))

# age

dat$agebins <- as.factor(dat$agebcont)
summary(dat$agebins)
dat <- within(dat, agebins <- relevel(agebins, ref = 4))


summary(c <- clogit(best ~ 
                      mabsdeaths*agebins + mabscases*agebins  +
                      avcost + unempl +
                      
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
                      
                      statquo +
                      strata(choice),data=dat
                    ,weights=popwt,method="approximate"
))


# own comorbid



summary(c <- clogit(best ~ 
                      mabsdeaths*owncomorbid  + mabscases*owncomorbid  +
                      avcost + unempl +
                      
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
                      
                      statquo +
                      strata(choice),data=dat
                    ,weights=popwt,method="approximate"
))

# ff comorbid

summary(c <- clogit(best ~ 
                      mabsdeaths*ffcomorbid  + mabscases*ffcomorbid  +
                      avcost + unempl +
                      
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
                      
                      statquo +
                      strata(choice),data=dat
                    ,weights=popwt,method="approximate"
))


# had covid?

summary(c <- clogit(best ~ 
                      mabsdeaths*owncomcovid  + mabscases*owncomcovid  +
                      avcost + unempl +
                      
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
                      
                      statquo +
                      strata(choice),data=dat
                    ,weights=popwt,method="approximate"
))

# ff had covid

summary(c <- clogit(best ~ 
                      mabsdeaths*ffcomcovid  + mabscases*ffcomcovid  +
                      avcost + unempl +
                      
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
                      
                      statquo +
                      strata(choice),data=dat
                    ,weights=popwt,method="approximate"
))


# LA/LI ***

summary(a <- clogit(best ~
                      mabsdeaths + mabscases +
                      unempl + avcost +
                      
                      rule1 + rule2 +
                      rule3 + rule4 +
                      rule5 + rule6 +
                      rule7 + rule8 +
                      rule9 + rule10 +
                      
                      lirule1*rule1 + lirule2*rule2 + lirule3*rule3 + lirule4*rule4 + lirule5*rule5 +
                      lirule6*rule6 + lirule7*rule7 + lirule8*rule8 + lirule9*rule9 + lirule10*rule10 +

                      larule1*rule1 + larule2*rule2 + larule3*rule3 + larule4*rule4 + larule5*rule5 +
                      larule6*rule6 + larule7*rule7 + larule8*rule8 + larule9*rule9 + larule10*rule10 +
                      
                      statquo +
                      strata(choice),data=dat
                    ,weights=popwt,method="approximate"
))


############# 3 separate ideology regressions



ideols <- unique(as.character(dat$ideol))



whomst <- ideols[1:2]

summary(conservatives <- clogit(best ~
                      mabsdeaths  + mabscases  +
                      avcost + unempl +
                      
                      rule1 + rule2 +
                      rule3 + rule4 +
                      rule5 + rule6 +
                      rule7 + rule8 +
                      rule9 + rule10 +
                      
                      
                      statquo +
                      strata(choice),data=dat[which(dat$ideol %in% whomst),]
                    ,weights=popwt,method="approximate"
))

whomst <- ideols[3]

summary(moderates <- clogit(best ~
                                  mabsdeaths  + mabscases  +
                                  avcost + unempl +
                                  
                                  rule1 + rule2 +
                                  rule3 + rule4 +
                                  rule5 + rule6 +
                                  rule7 + rule8 +
                                  rule9 + rule10 +
                                  
                                  
                                  statquo +
                                  strata(choice),data=dat[which(dat$ideol %in% whomst),]
                                ,weights=popwt,method="approximate"
))

whomst <- ideols[4:5]

summary(liberals <- clogit(best ~
                              mabsdeaths  + mabscases  +
                              avcost + unempl +
                              
                              rule1 + rule2 +
                              rule3 + rule4 +
                              rule5 + rule6 +
                              rule7 + rule8 +
                              rule9 + rule10 +
                              
                              
                              statquo +
                              strata(choice),data=dat[which(dat$ideol %in% whomst),]
                            ,weights=popwt,method="approximate"
))

t1 <- stargazer(liberals,moderates,conservatives,keep.stat = "n") %>% paste(collapse="\n")

for (i in 1:nrow(varlabs)) {
  if(str_detect(t1,paste0(" ",varlabs$variable[i]))) {
    t1 <- str_replace_all(t1,varlabs$variable[i],varlabs$label[i])
  }
}

cat(t1)

### learn margins package
margins(c)

d <- lm(rule1 ~ rule2, data= dat[which(dat$alt==2 & dat$choiceofperson==2),])

cor(dat[which(dat$alt==2 & dat$choiceofperson==2),6:15])

summary(d)


margins(d)


# main with ideol dummies

ideoldat <- dat %>% mutate(conservative_deaths = mabsdeaths*I(ideol2=="conservative"),
                             conservative_cases = mabscases*I(ideol2=="conservative"),
                           conservative_cost = avcost*I(ideol2=="conservative"),
                           conservative_unempl = unempl*I(ideol2=="conservative"),
                           conservative_rule1 = rule1*I(ideol2=="conservative"),
                           conservative_rule2 = rule2*I(ideol2=="conservative"),
                           conservative_rule3 = rule3*I(ideol2=="conservative"),
                           conservative_rule4 = rule4*I(ideol2=="conservative"),
                           conservative_rule5 = rule5*I(ideol2=="conservative"),
                           conservative_rule6 = rule6*I(ideol2=="conservative"),
                           conservative_rule7 = rule7*I(ideol2=="conservative"),
                           conservative_rule8 = rule8*I(ideol2=="conservative"),
                           conservative_rule9 = rule9*I(ideol2=="conservative"),
                           conservative_rule10 = rule10*I(ideol2=="conservative"),
                           conservative_statquo = statquo*I(ideol2=="conservative"))

ideoldat <- ideoldat %>% mutate(moderate_deaths = mabsdeaths*I(ideol2=="moderate"),
                           moderate_cases = mabscases*I(ideol2=="moderate"),
                           moderate_cost = avcost*I(ideol2=="moderate"),
                           moderate_unempl = unempl*I(ideol2=="moderate"),
                           moderate_rule1 = rule1*I(ideol2=="moderate"),
                           moderate_rule2 = rule2*I(ideol2=="moderate"),
                           moderate_rule3 = rule3*I(ideol2=="moderate"),
                           moderate_rule4 = rule4*I(ideol2=="moderate"),
                           moderate_rule5 = rule5*I(ideol2=="moderate"),
                           moderate_rule6 = rule6*I(ideol2=="moderate"),
                           moderate_rule7 = rule7*I(ideol2=="moderate"),
                           moderate_rule8 = rule8*I(ideol2=="moderate"),
                           moderate_rule9 = rule9*I(ideol2=="moderate"),
                           moderate_rule10 = rule10*I(ideol2=="moderate"),
                           moderate_statquo = statquo*I(ideol2=="moderate"))

ideoldat <- ideoldat %>% mutate(liberal_deaths = mabsdeaths*I(ideol2=="liberal"),
                           liberal_cases = mabscases*I(ideol2=="liberal"),
                           liberal_cost = avcost*I(ideol2=="liberal"),
                           liberal_unempl = unempl*I(ideol2=="liberal"),
                           liberal_rule1 = rule1*I(ideol2=="liberal"),
                           liberal_rule2 = rule2*I(ideol2=="liberal"),
                           liberal_rule3 = rule3*I(ideol2=="liberal"),
                           liberal_rule4 = rule4*I(ideol2=="liberal"),
                           liberal_rule5 = rule5*I(ideol2=="liberal"),
                           liberal_rule6 = rule6*I(ideol2=="liberal"),
                           liberal_rule7 = rule7*I(ideol2=="liberal"),
                           liberal_rule8 = rule8*I(ideol2=="liberal"),
                           liberal_rule9 = rule9*I(ideol2=="liberal"),
                           liberal_rule10 = rule10*I(ideol2=="liberal"),
                           liberal_statquo = statquo*I(ideol2=="liberal"))

ideoldat <- ideoldat %>% mutate(MISSING_deaths = mabsdeaths*I(ideol2=="MISSING"),
                           MISSING_cases = mabscases*I(ideol2=="MISSING"),
                           MISSING_cost = avcost*I(ideol2=="MISSING"),
                           MISSING_unempl = unempl*I(ideol2=="MISSING"),
                           MISSING_rule1 = rule1*I(ideol2=="MISSING"),
                           MISSING_rule2 = rule2*I(ideol2=="MISSING"),
                           MISSING_rule3 = rule3*I(ideol2=="MISSING"),
                           MISSING_rule4 = rule4*I(ideol2=="MISSING"),
                           MISSING_rule5 = rule5*I(ideol2=="MISSING"),
                           MISSING_rule6 = rule6*I(ideol2=="MISSING"),
                           MISSING_rule7 = rule7*I(ideol2=="MISSING"),
                           MISSING_rule8 = rule8*I(ideol2=="MISSING"),
                           MISSING_rule9 = rule9*I(ideol2=="MISSING"),
                           MISSING_rule10 = rule10*I(ideol2=="MISSING"),
                           MISSING_statquo = statquo*I(ideol2=="MISSING"))




summary(ideolmodelrp <- clogit(best ~
                         
                          conservative_deaths*lassorpfl + conservative_cases*lassorpfl + conservative_cost*lassorpfl + conservative_unempl*lassorpfl + 
                          conservative_rule1*lassorpfl + conservative_rule2*lassorpfl + conservative_rule3*lassorpfl + conservative_rule4*lassorpfl + 
                          conservative_rule5*lassorpfl + conservative_rule6*lassorpfl + conservative_rule7*lassorpfl + conservative_rule8*lassorpfl + 
                          conservative_rule9*lassorpfl + conservative_rule10*lassorpfl + conservative_statquo*lassorpfl + 
                          
                          moderate_deaths*lassorpfl + moderate_cases*lassorpfl + moderate_cost*lassorpfl + moderate_unempl*lassorpfl + 
                          moderate_rule1*lassorpfl + moderate_rule2*lassorpfl + moderate_rule3*lassorpfl + moderate_rule4*lassorpfl + 
                          moderate_rule5*lassorpfl + moderate_rule6*lassorpfl + moderate_rule7*lassorpfl + moderate_rule8*lassorpfl + 
                          moderate_rule9*lassorpfl + moderate_rule10*lassorpfl + moderate_statquo*lassorpfl + 
                          
                          liberal_deaths*lassorpfl + liberal_cases*lassorpfl + liberal_cost*lassorpfl + liberal_unempl*lassorpfl + 
                          liberal_rule1*lassorpfl + liberal_rule2*lassorpfl + liberal_rule3*lassorpfl + liberal_rule4*lassorpfl + 
                          liberal_rule5*lassorpfl + liberal_rule6*lassorpfl + liberal_rule7*lassorpfl + liberal_rule8*lassorpfl + 
                          liberal_rule9*lassorpfl + liberal_rule10*lassorpfl + liberal_statquo*lassorpfl + 
                          
                          MISSING_deaths*lassorpfl + MISSING_cases*lassorpfl + MISSING_cost*lassorpfl + MISSING_unempl*lassorpfl + 
                          MISSING_rule1*lassorpfl + MISSING_rule2*lassorpfl + MISSING_rule3*lassorpfl + MISSING_rule4*lassorpfl + 
                          MISSING_rule5*lassorpfl + MISSING_rule6*lassorpfl + MISSING_rule7*lassorpfl + MISSING_rule8*lassorpfl + 
                          MISSING_rule9*lassorpfl + MISSING_rule10*lassorpfl + MISSING_statquo*lassorpfl +
                          
                         strata(choice),data=ideoldat
                       ,weights=popwt,method="approximate"
))


summary(ideolmodel <- clogit(best ~
                                 
                                 conservative_deaths + conservative_cases + conservative_cost + conservative_unempl + 
                                 conservative_rule1 + conservative_rule2 + conservative_rule3 + conservative_rule4 + 
                                 conservative_rule5 + conservative_rule6 + conservative_rule7 + conservative_rule8 + 
                                 conservative_rule9 + conservative_rule10 + conservative_statquo + 
                                 
                                 moderate_deaths + moderate_cases + moderate_cost + moderate_unempl + 
                                 moderate_rule1 + moderate_rule2 + moderate_rule3 + moderate_rule4 + 
                                 moderate_rule5 + moderate_rule6 + moderate_rule7 + moderate_rule8 + 
                                 moderate_rule9 + moderate_rule10 + moderate_statquo + 
                                 
                                 liberal_deaths + liberal_cases + liberal_cost + liberal_unempl + 
                                 liberal_rule1 + liberal_rule2 + liberal_rule3 + liberal_rule4 + 
                                 liberal_rule5 + liberal_rule6 + liberal_rule7 + liberal_rule8 + 
                                 liberal_rule9 + liberal_rule10 + liberal_statquo + 
                                 
                                 MISSING_deaths + MISSING_cases + MISSING_cost + MISSING_unempl + 
                                 MISSING_rule1 + MISSING_rule2 + MISSING_rule3 + MISSING_rule4 + 
                                 MISSING_rule5 + MISSING_rule6 + MISSING_rule7 + MISSING_rule8 + 
                                 MISSING_rule9 + MISSING_rule10 + MISSING_statquo +
                                 
                                 strata(choice),data=ideoldat
                               ,weights=popwt,method="approximate"
))


summary(ideolmodelrpfedui <- clogit(best ~
                                 
                                 conservative_deaths*lassorpfl + conservative_cases*lassorpfl + conservative_cost*feduiany*lassorpfl + conservative_unempl*feduiany*lassorpfl + 
                                 conservative_rule1*lassorpfl + conservative_rule2*lassorpfl + conservative_rule3*lassorpfl + conservative_rule4*lassorpfl + 
                                 conservative_rule5*lassorpfl + conservative_rule6*lassorpfl + conservative_rule7*lassorpfl + conservative_rule8*lassorpfl + 
                                 conservative_rule9*lassorpfl + conservative_rule10*lassorpfl + conservative_statquo*feduiany*lassorpfl + 
                                 
                                 moderate_deaths*lassorpfl + moderate_cases*lassorpfl + moderate_cost*feduiany*lassorpfl + moderate_unempl*feduiany*lassorpfl + 
                                 moderate_rule1*lassorpfl + moderate_rule2*lassorpfl + moderate_rule3*lassorpfl + moderate_rule4*lassorpfl + 
                                 moderate_rule5*lassorpfl + moderate_rule6*lassorpfl + moderate_rule7*lassorpfl + moderate_rule8*lassorpfl + 
                                 moderate_rule9*lassorpfl + moderate_rule10*lassorpfl + moderate_statquo*feduiany*lassorpfl + 
                                 
                                 liberal_deaths*lassorpfl + liberal_cases*lassorpfl + liberal_cost*feduiany*lassorpfl + liberal_unempl*feduiany*lassorpfl + 
                                 liberal_rule1*lassorpfl + liberal_rule2*lassorpfl + liberal_rule3*lassorpfl + liberal_rule4*lassorpfl + 
                                 liberal_rule5*lassorpfl + liberal_rule6*lassorpfl + liberal_rule7*lassorpfl + liberal_rule8*lassorpfl + 
                                 liberal_rule9*lassorpfl + liberal_rule10*lassorpfl + liberal_statquo*feduiany*lassorpfl + 
                                 
                                 MISSING_deaths*lassorpfl + MISSING_cases*lassorpfl + MISSING_cost*feduiany*lassorpfl + MISSING_unempl*feduiany*lassorpfl + 
                                 MISSING_rule1*lassorpfl + MISSING_rule2*lassorpfl + MISSING_rule3*lassorpfl + MISSING_rule4*lassorpfl + 
                                 MISSING_rule5*lassorpfl + MISSING_rule6*lassorpfl + MISSING_rule7*lassorpfl + MISSING_rule8*lassorpfl + 
                                 MISSING_rule9*lassorpfl + MISSING_rule10*lassorpfl + MISSING_statquo*lassorpfl +
                                 
                                 strata(choice),data=ideoldat
                               ,weights=popwt,method="approximate"
))

res <- broom

summary(ideolmodelfedui <- clogit(best ~
                               
                               conservative_deaths + conservative_cases + conservative_cost*feduiany + conservative_unempl*feduiany + 
                               conservative_rule1 + conservative_rule2 + conservative_rule3 + conservative_rule4 + 
                               conservative_rule5 + conservative_rule6 + conservative_rule7 + conservative_rule8 + 
                               conservative_rule9 + conservative_rule10 + conservative_statquo + 
                               
                               moderate_deaths + moderate_cases + moderate_cost*feduiany + moderate_unempl*feduiany + 
                               moderate_rule1 + moderate_rule2 + moderate_rule3 + moderate_rule4 + 
                               moderate_rule5 + moderate_rule6 + moderate_rule7 + moderate_rule8 + 
                               moderate_rule9 + moderate_rule10 + moderate_statquo + 
                               
                               liberal_deaths + liberal_cases + liberal_cost*feduiany + liberal_unempl*feduiany + 
                               liberal_rule1 + liberal_rule2 + liberal_rule3 + liberal_rule4 + 
                               liberal_rule5 + liberal_rule6 + liberal_rule7 + liberal_rule8 + 
                               liberal_rule9 + liberal_rule10 + liberal_statquo + 
                               
                               MISSING_deaths + MISSING_cases + MISSING_cost*feduiany + MISSING_unempl*feduiany + 
                               MISSING_rule1 + MISSING_rule2 + MISSING_rule3 + MISSING_rule4 + 
                               MISSING_rule5 + MISSING_rule6 + MISSING_rule7 + MISSING_rule8 + 
                               MISSING_rule9 + MISSING_rule10 + MISSING_statquo +
                               
                               strata(choice),data=ideoldat
                             ,weights=popwt,method="approximate"
))

############### ideol without fedui table ###############

ideolmodelrp.s <- summary(ideolmodelrp)$coefficients %>% as.data.frame() %>% rownames_to_column(var="term") %>% 
  dplyr::filter(! str_detect(term,"lassorpfl"))



concoef <- ideolmodelrp.s %>% dplyr::filter(str_detect(term,"conservative") )
modcoef <- ideolmodelrp.s %>% dplyr::filter(str_detect(term,"moderate") )
libcoef <- ideolmodelrp.s %>% dplyr::filter(str_detect(term,"liberal") )



fake <- lm(best ~ mabsdeaths + mabscases + avcost + unempl + 
                     rule1 + rule2 + rule3 + rule4 + rule5 + rule6 +
                     rule7 + rule8 + rule9 + rule10 + statquo + 0,data =ideoldat
                   )

t2 <- stargazer(fake,fake,fake,type="latex",
         # omit=":lassorpfl",
          coef = list(
            libcoef$coef,
            modcoef$coef,
            concoef$coef
          ),
          # standard errors
          se = list(
            libcoef$`robust se`,
            modcoef$`robust se`,
            concoef$`robust se`
          ),
          column.labels = c("Liberal", "Moderate","Conservative"),
          keep.stat="n",
          t.auto = T)

t2 <- paste(t2,collapse="\n")

for (i in nrow(varlabs):1) {
  if(str_detect(t2,paste0("",varlabs$variable[i]))) {
    t2 <- str_replace_all(t2,varlabs$variable[i],varlabs$label[i])
  }
}

cat(t2)


############## ideol with fedui table ###########################

ideolmodelrpfedui.s <- summary(ideolmodelrpfedui)$coefficients %>% as.data.frame() %>% rownames_to_column(var="term") %>% 
  dplyr::filter(! str_detect(term,"lassorpfl"))



concoeffedui <- ideolmodelrpfedui.s %>% dplyr::filter(str_detect(term,"conservative") )
modcoeffedui <- ideolmodelrpfedui.s %>% dplyr::filter(str_detect(term,"moderate") )
libcoeffedui <- ideolmodelrpfedui.s %>% dplyr::filter(str_detect(term,"liberal") )



fakefedui <- lm(best ~ mabsdeaths + mabscases + avcost + avcost:feduiany + unempl + unempl:feduiany + 
             rule1 + rule2 + rule3 + rule4 + rule5 + rule6 +
             rule7 + rule8 + rule9 + rule10 + statquo + 0,data =ideoldat
)

t3 <- stargazer(fakefedui,fakefedui,fakefedui,type="latex",
                 omit="^feduiany$",
                coef = list(
                  libcoeffedui$coef,
                  modcoeffedui$coef,
                  concoeffedui$coef
                ),
                # standard errors
                se = list(
                  libcoeffedui$`robust se`,
                  modcoeffedui$`robust se`,
                  concoeffedui$`robust se`
                ),
                column.labels = c("Liberal", "Moderate","Conservative"),
                keep.stat="n",
                t.auto = T)

t3 <- paste(t3,collapse="\n")

for (i in nrow(varlabs):1) {
  if(str_detect(t3,paste0("",varlabs$variable[i]))) {
    t3 <- str_replace_all(t3,varlabs$variable[i],varlabs$label[i])
  }
}

cat(t3)



ideolrp <- dat %>% group_by(ideol) %>% summarise(meanrplasso = mean(lassorp),
                                                 meanrplassofl = mean(lassorpfl),
                                                 trudy = mean(demeanrp))

######## Ethnic fractionalization by zipcode #################


summary(fracreg <- clogit(best ~ 
                            conservative_deaths*lassorpfl + conservative_cases*lassorpfl + conservative_cost*I((1-havzipvars)^2)*ethfrac*lassorpfl + conservative_unempl*I((1-havzipvars)^2)*ethfrac*lassorpfl + 
                            conservative_rule1*lassorpfl + conservative_rule2*lassorpfl + conservative_rule3*lassorpfl + conservative_rule4*lassorpfl + 
                            conservative_rule5*lassorpfl + conservative_rule6*lassorpfl + conservative_rule7*lassorpfl + conservative_rule8*lassorpfl + 
                            conservative_rule9*lassorpfl + conservative_rule10*lassorpfl + conservative_statquo*I((1-havzipvars)^2)*ethfrac*lassorpfl + 
                            
                            moderate_deaths*lassorpfl + moderate_cases*lassorpfl + moderate_cost*ethfrac*I((1-havzipvars)^2)*lassorpfl + moderate_unempl*ethfrac*I((1-havzipvars)^2)*lassorpfl + 
                            moderate_rule1*lassorpfl + moderate_rule2*lassorpfl + moderate_rule3*lassorpfl + moderate_rule4*lassorpfl + 
                            moderate_rule5*lassorpfl + moderate_rule6*lassorpfl + moderate_rule7*lassorpfl + moderate_rule8*lassorpfl + 
                            moderate_rule9*lassorpfl + moderate_rule10*lassorpfl + moderate_statquo*ethfrac*I((1-havzipvars)^2)*lassorpfl + 
                            
                            liberal_deaths*lassorpfl + liberal_cases*lassorpfl + liberal_cost*ethfrac*I((1-havzipvars)^2)*lassorpfl + liberal_unempl*ethfrac*I((1-havzipvars)^2)*lassorpfl + 
                            liberal_rule1*lassorpfl + liberal_rule2*lassorpfl + liberal_rule3*lassorpfl + liberal_rule4*lassorpfl + 
                            liberal_rule5*lassorpfl + liberal_rule6*lassorpfl + liberal_rule7*lassorpfl + liberal_rule8*lassorpfl + 
                            liberal_rule9*lassorpfl + liberal_rule10*lassorpfl + liberal_statquo*ethfrac*I((1-havzipvars)^2)*lassorpfl + 
                            
                            MISSING_deaths*lassorpfl + MISSING_cases*lassorpfl + MISSING_cost*ethfrac*I((1-havzipvars)^2)*lassorpfl + MISSING_unempl*ethfrac*I((1-havzipvars)^2)*lassorpfl + 
                            MISSING_rule1*lassorpfl + MISSING_rule2*lassorpfl + MISSING_rule3*lassorpfl + MISSING_rule4*lassorpfl + 
                            MISSING_rule5*lassorpfl + MISSING_rule6*lassorpfl + MISSING_rule7*lassorpfl + MISSING_rule8*lassorpfl + 
                            MISSING_rule9*lassorpfl + MISSING_rule10*lassorpfl + MISSING_statquo*ethfrac*I((1-havzipvars)^2)*lassorpfl +
                            
                            strata(choice),data=ideoldat[which(ideoldat$havzipvars==1),]
                          ,weights=popwt,method="approximate"
))



ethfrac.s <- summary(fracreg)$coefficients %>% as.data.frame() %>% rownames_to_column(var="term") %>% 
  dplyr::filter(! str_detect(term,"lassorpfl") & ! str_detect(term,"havzip"))



concoeffrac <- ethfrac.s %>% dplyr::filter(str_detect(term,"conservative") )
modcoeffrac <- ethfrac.s %>% dplyr::filter(str_detect(term,"moderate") )
libcoeffrac <- ethfrac.s %>% dplyr::filter(str_detect(term,"liberal") )


fakefedui <- lm(best ~ mabsdeaths  + mabscases  + avcost + avcost:ethfrac + unempl + unempl:ethfrac + 
                  rule1 + rule2 + rule3 + rule4 + rule5 + rule6 +
                  rule7 + rule8 + rule9 + rule10 + statquo + 0,data =ideoldat
)

t4 <- stargazer(fakefedui,fakefedui,fakefedui,type="latex",
                omit=c("^ethfrac$","havzipvars"),
                coef = list(
                  libcoeffrac$coef,
                  modcoeffrac$coef,
                  concoeffrac$coef
                ),
                # standard errors
                se = list(
                  libcoeffrac$`robust se`,
                  modcoeffrac$`robust se`,
                  concoeffrac$`robust se`
                ),
                column.labels = c("Liberal", "Moderate","Conservative"),
                keep.stat="n",
                t.auto = T)

t4 <- paste(t4,collapse="\n")

for (i in nrow(varlabs):1) {
  if(str_detect(t4,paste0("",varlabs$variable[i]))) {
    t4 <- str_replace_all(t4,varlabs$variable[i],varlabs$label[i])
  }
}

cat(t4)

res <- broom::tidy(fracreg) %>% dplyr::filter(str_detect(term,"ethfrac") & estimate!=0)



################# gender het ####################

genderdat <- dat %>% mutate(female_deaths = mabsdeaths*I(female==1),
                           female_cases = mabscases*I(female==1),
                           female_cost = avcost*I(female==1),
                           female_unempl = unempl*I(female==1),
                           female_rule1 = rule1*I(female==1),
                           female_rule2 = rule2*I(female==1),
                           female_rule3 = rule3*I(female==1),
                           female_rule4 = rule4*I(female==1),
                           female_rule5 = rule5*I(female==1),
                           female_rule6 = rule6*I(female==1),
                           female_rule7 = rule7*I(female==1),
                           female_rule8 = rule8*I(female==1),
                           female_rule9 = rule9*I(female==1),
                           female_rule10 = rule10*I(female==1),
                           female_statquo = statquo*I(female==1))

genderdat <- genderdat %>% mutate(male_deaths = mabsdeaths*I(male==1),
                            male_cases = mabscases*I(male==1),
                            male_cost = avcost*I(male==1),
                            male_unempl = unempl*I(male==1),
                            male_rule1 = rule1*I(male==1),
                            male_rule2 = rule2*I(male==1),
                            male_rule3 = rule3*I(male==1),
                            male_rule4 = rule4*I(male==1),
                            male_rule5 = rule5*I(male==1),
                            male_rule6 = rule6*I(male==1),
                            male_rule7 = rule7*I(male==1),
                            male_rule8 = rule8*I(male==1),
                            male_rule9 = rule9*I(male==1),
                            male_rule10 = rule10*I(male==1),
                            male_statquo = statquo*I(male==1))

genderdat <- genderdat %>% mutate(nonbinary_deaths = mabsdeaths*I(nonbinary==1),
                                  nonbinary_cases = mabscases*I(nonbinary==1),
                                  nonbinary_cost = avcost*I(nonbinary==1),
                                  nonbinary_unempl = unempl*I(nonbinary==1),
                                  nonbinary_rule1 = rule1*I(nonbinary==1),
                                  nonbinary_rule2 = rule2*I(nonbinary==1),
                                  nonbinary_rule3 = rule3*I(nonbinary==1),
                                  nonbinary_rule4 = rule4*I(nonbinary==1),
                                  nonbinary_rule5 = rule5*I(nonbinary==1),
                                  nonbinary_rule6 = rule6*I(nonbinary==1),
                                  nonbinary_rule7 = rule7*I(nonbinary==1),
                                  nonbinary_rule8 = rule8*I(nonbinary==1),
                                  nonbinary_rule9 = rule9*I(nonbinary==1),
                                  nonbinary_rule10 = rule10*I(nonbinary==1),
                                  nonbinary_statquo = statquo*I(nonbinary==1))


summary(gendreg <- clogit(best ~
                            female_deaths*lassorpfl + female_cases*lassorpfl + female_cost*lassorpfl + female_unempl*lassorpfl + 
                            female_rule1*lassorpfl + female_rule2*lassorpfl + female_rule3*lassorpfl + female_rule4*lassorpfl + 
                            female_rule5*lassorpfl + female_rule6*lassorpfl + female_rule7*lassorpfl + female_rule8*lassorpfl + 
                            female_rule9*lassorpfl + female_rule10*lassorpfl + female_statquo*lassorpfl + 
                            
                            male_deaths*lassorpfl + male_cases*lassorpfl + male_cost*lassorpfl + male_unempl*lassorpfl + 
                            male_rule1*lassorpfl + male_rule2*lassorpfl + male_rule3*lassorpfl + male_rule4*lassorpfl + 
                            male_rule5*lassorpfl + male_rule6*lassorpfl + male_rule7*lassorpfl + male_rule8*lassorpfl + 
                            male_rule9*lassorpfl + male_rule10*lassorpfl + male_statquo*lassorpfl + 
                            
                            nonbinary_deaths*lassorpfl + nonbinary_cases*lassorpfl + nonbinary_cost*lassorpfl + nonbinary_unempl*lassorpfl + 
                            nonbinary_rule1*lassorpfl + nonbinary_rule2*lassorpfl + nonbinary_rule3*lassorpfl + nonbinary_rule4*lassorpfl + 
                            nonbinary_rule5*lassorpfl + nonbinary_rule6*lassorpfl + nonbinary_rule7*lassorpfl + nonbinary_rule8*lassorpfl + 
                            nonbinary_rule9*lassorpfl + nonbinary_rule10*lassorpfl + nonbinary_statquo*lassorpfl +
                            
                            strata(choice),data=genderdat
                          ,weights=popwt,method="approximate"
))






gender.s <- summary(gendreg)$coefficients %>% as.data.frame() %>% rownames_to_column(var="term") %>% 
  dplyr::filter(! str_detect(term,"lassorpfl") & ! str_detect(term,"havzip"))



femcoeffrac <- gender.s %>% dplyr::filter(str_detect(term,"female") )
malecoeffrac <- gender.s %>% dplyr::filter(str_detect(term,"male") & !str_detect(term,"female"))


fakegender <- lm(best ~ mabsdeaths  + mabscases  + avcost + avcost + unempl + unempl + 
                  rule1 + rule2 + rule3 + rule4 + rule5 + rule6 +
                  rule7 + rule8 + rule9 + rule10 + statquo + 0,data =ideoldat
)

t4 <- stargazer(fakegender,fakegender,type="latex",
               # omit=c("^ethfrac$","havzipvars"),
                coef = list(
                  femcoeffrac$coef,
                  malecoeffrac$coef
                ),
                # standard errors
                se = list(
                  femcoeffrac$`robust se`,
                  malecoeffrac$`robust se`
                ),
                column.labels = c("Women", "Men"),
                keep.stat="n",
                t.auto = T)

t4 <- paste(t4,collapse="\n")

for (i in nrow(varlabs):1) {
  if(str_detect(t4,paste0("",varlabs$variable[i]))) {
    t4 <- str_replace_all(t4,varlabs$variable[i],varlabs$label[i])
  }
}

cat(t4)


################### SVI ##############################

svidat <- dat %>% mutate(svi1hi_deaths = mabsdeaths*I(svi1hi==1),
                          svi1hi_cases = mabscases*I(svi1hi==1),
                          svi1hi_cost = avcost*I(svi1hi==1),
                          svi1hi_unempl = unempl*I(svi1hi==1),
                          svi1hi_rule1 = rule1*I(svi1hi==1),
                          svi1hi_rule2 = rule2*I(svi1hi==1),
                          svi1hi_rule3 = rule3*I(svi1hi==1),
                          svi1hi_rule4 = rule4*I(svi1hi==1),
                          svi1hi_rule5 = rule5*I(svi1hi==1),
                          svi1hi_rule6 = rule6*I(svi1hi==1),
                          svi1hi_rule7 = rule7*I(svi1hi==1),
                          svi1hi_rule8 = rule8*I(svi1hi==1),
                          svi1hi_rule9 = rule9*I(svi1hi==1),
                          svi1hi_rule10 = rule10*I(svi1hi==1),
                          svi1hi_statquo = statquo*I(svi1hi==1))

svidat <- svidat %>% mutate(svi1lo_deaths = mabsdeaths*I(svi1lo==1),
                              svi1lo_cases = mabscases*I(svi1lo==1),
                              svi1lo_cost = avcost*I(svi1lo==1),
                              svi1lo_unempl = unempl*I(svi1lo==1),
                              svi1lo_rule1 = rule1*I(svi1lo==1),
                              svi1lo_rule2 = rule2*I(svi1lo==1),
                              svi1lo_rule3 = rule3*I(svi1lo==1),
                              svi1lo_rule4 = rule4*I(svi1lo==1),
                              svi1lo_rule5 = rule5*I(svi1lo==1),
                              svi1lo_rule6 = rule6*I(svi1lo==1),
                              svi1lo_rule7 = rule7*I(svi1lo==1),
                              svi1lo_rule8 = rule8*I(svi1lo==1),
                              svi1lo_rule9 = rule9*I(svi1lo==1),
                              svi1lo_rule10 = rule10*I(svi1lo==1),
                              svi1lo_statquo = statquo*I(svi1lo==1))

svidat <- svidat %>% mutate(svi2hi_deaths = mabsdeaths*I(svi2hi==1),
                         svi2hi_cases = mabscases*I(svi2hi==1),
                         svi2hi_cost = avcost*I(svi2hi==1),
                         svi2hi_unempl = unempl*I(svi2hi==1),
                         svi2hi_rule1 = rule1*I(svi2hi==1),
                         svi2hi_rule2 = rule2*I(svi2hi==1),
                         svi2hi_rule3 = rule3*I(svi2hi==1),
                         svi2hi_rule4 = rule4*I(svi2hi==1),
                         svi2hi_rule5 = rule5*I(svi2hi==1),
                         svi2hi_rule6 = rule6*I(svi2hi==1),
                         svi2hi_rule7 = rule7*I(svi2hi==1),
                         svi2hi_rule8 = rule8*I(svi2hi==1),
                         svi2hi_rule9 = rule9*I(svi2hi==1),
                         svi2hi_rule10 = rule10*I(svi2hi==1),
                         svi2hi_statquo = statquo*I(svi2hi==1))

svidat <- svidat %>% mutate(svi2lo_deaths = mabsdeaths*I(svi2lo==1),
                            svi2lo_cases = mabscases*I(svi2lo==1),
                            svi2lo_cost = avcost*I(svi2lo==1),
                            svi2lo_unempl = unempl*I(svi2lo==1),
                            svi2lo_rule1 = rule1*I(svi2lo==1),
                            svi2lo_rule2 = rule2*I(svi2lo==1),
                            svi2lo_rule3 = rule3*I(svi2lo==1),
                            svi2lo_rule4 = rule4*I(svi2lo==1),
                            svi2lo_rule5 = rule5*I(svi2lo==1),
                            svi2lo_rule6 = rule6*I(svi2lo==1),
                            svi2lo_rule7 = rule7*I(svi2lo==1),
                            svi2lo_rule8 = rule8*I(svi2lo==1),
                            svi2lo_rule9 = rule9*I(svi2lo==1),
                            svi2lo_rule10 = rule10*I(svi2lo==1),
                            svi2lo_statquo = statquo*I(svi2lo==1))

svidat <- svidat %>% mutate(svi3hi_deaths = mabsdeaths*I(svi3hi==1),
                         svi3hi_cases = mabscases*I(svi3hi==1),
                         svi3hi_cost = avcost*I(svi3hi==1),
                         svi3hi_unempl = unempl*I(svi3hi==1),
                         svi3hi_rule1 = rule1*I(svi3hi==1),
                         svi3hi_rule2 = rule2*I(svi3hi==1),
                         svi3hi_rule3 = rule3*I(svi3hi==1),
                         svi3hi_rule4 = rule4*I(svi3hi==1),
                         svi3hi_rule5 = rule5*I(svi3hi==1),
                         svi3hi_rule6 = rule6*I(svi3hi==1),
                         svi3hi_rule7 = rule7*I(svi3hi==1),
                         svi3hi_rule8 = rule8*I(svi3hi==1),
                         svi3hi_rule9 = rule9*I(svi3hi==1),
                         svi3hi_rule10 = rule10*I(svi3hi==1),
                         svi3hi_statquo = statquo*I(svi3hi==1))

svidat <- svidat %>% mutate(svi3lo_deaths = mabsdeaths*I(svi3lo==1),
                            svi3lo_cases = mabscases*I(svi3lo==1),
                            svi3lo_cost = avcost*I(svi3lo==1),
                            svi3lo_unempl = unempl*I(svi3lo==1),
                            svi3lo_rule1 = rule1*I(svi3lo==1),
                            svi3lo_rule2 = rule2*I(svi3lo==1),
                            svi3lo_rule3 = rule3*I(svi3lo==1),
                            svi3lo_rule4 = rule4*I(svi3lo==1),
                            svi3lo_rule5 = rule5*I(svi3lo==1),
                            svi3lo_rule6 = rule6*I(svi3lo==1),
                            svi3lo_rule7 = rule7*I(svi3lo==1),
                            svi3lo_rule8 = rule8*I(svi3lo==1),
                            svi3lo_rule9 = rule9*I(svi3lo==1),
                            svi3lo_rule10 = rule10*I(svi3lo==1),
                            svi3lo_statquo = statquo*I(svi3lo==1))

svidat <- svidat %>% mutate(svi4hi_deaths = mabsdeaths*I(svi4hi==1),
                         svi4hi_cases = mabscases*I(svi4hi==1),
                         svi4hi_cost = avcost*I(svi4hi==1),
                         svi4hi_unempl = unempl*I(svi4hi==1),
                         svi4hi_rule1 = rule1*I(svi4hi==1),
                         svi4hi_rule2 = rule2*I(svi4hi==1),
                         svi4hi_rule3 = rule3*I(svi4hi==1),
                         svi4hi_rule4 = rule4*I(svi4hi==1),
                         svi4hi_rule5 = rule5*I(svi4hi==1),
                         svi4hi_rule6 = rule6*I(svi4hi==1),
                         svi4hi_rule7 = rule7*I(svi4hi==1),
                         svi4hi_rule8 = rule8*I(svi4hi==1),
                         svi4hi_rule9 = rule9*I(svi4hi==1),
                         svi4hi_rule10 = rule10*I(svi4hi==1),
                         svi4hi_statquo = statquo*I(svi4hi==1))

svidat <- svidat %>% mutate(svi4lo_deaths = mabsdeaths*I(svi4lo==1),
                            svi4lo_cases = mabscases*I(svi4lo==1),
                            svi4lo_cost = avcost*I(svi4lo==1),
                            svi4lo_unempl = unempl*I(svi4lo==1),
                            svi4lo_rule1 = rule1*I(svi4lo==1),
                            svi4lo_rule2 = rule2*I(svi4lo==1),
                            svi4lo_rule3 = rule3*I(svi4lo==1),
                            svi4lo_rule4 = rule4*I(svi4lo==1),
                            svi4lo_rule5 = rule5*I(svi4lo==1),
                            svi4lo_rule6 = rule6*I(svi4lo==1),
                            svi4lo_rule7 = rule7*I(svi4lo==1),
                            svi4lo_rule8 = rule8*I(svi4lo==1),
                            svi4lo_rule9 = rule9*I(svi4lo==1),
                            svi4lo_rule10 = rule10*I(svi4lo==1),
                            svi4lo_statquo = statquo*I(svi4lo==1))


############### svi1 ##############

summary(svi1reg <- clogit(best ~
                            svi1hi_deaths*lassorpfl + svi1hi_cases*lassorpfl + svi1hi_cost*feduiany*lassorpfl + svi1hi_unempl*feduiany*lassorpfl + 
                            svi1hi_rule1*lassorpfl + svi1hi_rule2*lassorpfl + svi1hi_rule3*lassorpfl + svi1hi_rule4*lassorpfl + 
                            svi1hi_rule5*lassorpfl + svi1hi_rule6*lassorpfl + svi1hi_rule7*lassorpfl + svi1hi_rule8*lassorpfl + 
                            svi1hi_rule9*lassorpfl + svi1hi_rule10*lassorpfl + svi1hi_statquo*lassorpfl + 
                            
                            svi1lo_deaths*lassorpfl + svi1lo_cases*lassorpfl + svi1lo_cost*feduiany*lassorpfl + svi1lo_unempl*feduiany*lassorpfl + 
                            svi1lo_rule1*lassorpfl + svi1lo_rule2*lassorpfl + svi1lo_rule3*lassorpfl + svi1lo_rule4*lassorpfl + 
                            svi1lo_rule5*lassorpfl + svi1lo_rule6*lassorpfl + svi1lo_rule7*lassorpfl + svi1lo_rule8*lassorpfl + 
                            svi1lo_rule9*lassorpfl + svi1lo_rule10*lassorpfl + svi1lo_statquo*lassorpfl + 
                            
                            strata(choice),data=svidat
                          ,weights=popwt,method="approximate"
))

svi1.s <- summary(svi1reg)$coefficients %>% as.data.frame() %>% rownames_to_column(var="term") %>% 
  dplyr::filter(! str_detect(term,"lassorpfl"))

svi1hicoef <- svi1.s %>% dplyr::filter(str_detect(term,"svi1hi"))
svi1locoef <- svi1.s %>% dplyr::filter(str_detect(term,"svi1lo"))

fakesvi <- lm(best ~ mabsdeaths  + mabscases  + avcost + avcost:feduiany + unempl + unempl:feduiany + 
                   rule1 + rule2 + rule3 + rule4 + rule5 + rule6 +
                   rule7 + rule8 + rule9 + rule10 + statquo + 0,data =svidat
)

tsvi1 <- stargazer(fakesvi,fakesvi,type="latex",
                # omit=c("^ethfrac$","havzipvars"),
                coef = list(
                  svi1locoef$coef,
                  svi1hicoef$coef
                ),
                # standard errors
                se = list(
                  svi1locoef$`robust se`,
                  svi1hicoef$`robust se`
                ),
                column.labels = c("SVI 1 Low", "SVI 1 High"),
                keep.stat="n",
                t.auto = T)

tsvi1 <- paste(tsvi1,collapse="\n")

for (i in nrow(varlabs):1) {
  if(str_detect(tsvi1,paste0("",varlabs$variable[i]))) {
    tsvi1 <- str_replace_all(tsvi1,varlabs$variable[i],varlabs$label[i])
    tsvi1 <- str_replace_all(tsvi1," numberernative","")
  }
}

cat(tsvi1)

############### svi2 ##############

summary(svi2reg <- clogit(best ~
                            svi2hi_deaths*lassorpfl + svi2hi_cases*lassorpfl + svi2hi_cost*feduiany*lassorpfl + svi2hi_unempl*feduiany*lassorpfl + 
                            svi2hi_rule1*lassorpfl + svi2hi_rule2*lassorpfl + svi2hi_rule3*lassorpfl + svi2hi_rule4*lassorpfl + 
                            svi2hi_rule5*lassorpfl + svi2hi_rule6*lassorpfl + svi2hi_rule7*lassorpfl + svi2hi_rule8*lassorpfl + 
                            svi2hi_rule9*lassorpfl + svi2hi_rule10*lassorpfl + svi2hi_statquo*lassorpfl + 
                            
                            svi2lo_deaths*lassorpfl + svi2lo_cases*lassorpfl + svi2lo_cost*feduiany*lassorpfl + svi2lo_unempl*feduiany*lassorpfl + 
                            svi2lo_rule1*lassorpfl + svi2lo_rule2*lassorpfl + svi2lo_rule3*lassorpfl + svi2lo_rule4*lassorpfl + 
                            svi2lo_rule5*lassorpfl + svi2lo_rule6*lassorpfl + svi2lo_rule7*lassorpfl + svi2lo_rule8*lassorpfl + 
                            svi2lo_rule9*lassorpfl + svi2lo_rule10*lassorpfl + svi2lo_statquo*lassorpfl + 
                            
                            strata(choice),data=svidat
                          ,weights=popwt,method="approximate"
))

svi2.s <- summary(svi2reg)$coefficients %>% as.data.frame() %>% rownames_to_column(var="term") %>% 
  dplyr::filter(! str_detect(term,"lassorpfl"))

svi2hicoef <- svi2.s %>% dplyr::filter(str_detect(term,"svi2hi"))
svi2locoef <- svi2.s %>% dplyr::filter(str_detect(term,"svi2lo"))



tsvi2 <- stargazer(fakesvi,fakesvi,type="latex",
                   # omit=c("^ethfrac$","havzipvars"),
                   coef = list(
                     svi2locoef$coef,
                     svi2hicoef$coef
                   ),
                   # standard errors
                   se = list(
                     svi2locoef$`robust se`,
                     svi2hicoef$`robust se`
                   ),
                   column.labels = c("SVI 2 Low", "SVI 2 High"),
                   keep.stat="n",
                   t.auto = T)

tsvi2 <- paste(tsvi2,collapse="\n")

for (i in nrow(varlabs):1) {
  if(str_detect(tsvi2,paste0("",varlabs$variable[i]))) {
    tsvi2 <- str_replace_all(tsvi2,varlabs$variable[i],varlabs$label[i])
    tsvi2 <- str_replace_all(tsvi2," numberernative","")
  }
}

cat(tsvi2)

############### svi3 ##############

summary(svi3reg <- clogit(best ~
                            svi3hi_deaths*lassorpfl + svi3hi_cases*lassorpfl + svi3hi_cost*feduiany*lassorpfl + svi3hi_unempl*feduiany*lassorpfl + 
                            svi3hi_rule1*lassorpfl + svi3hi_rule2*lassorpfl + svi3hi_rule3*lassorpfl + svi3hi_rule4*lassorpfl + 
                            svi3hi_rule5*lassorpfl + svi3hi_rule6*lassorpfl + svi3hi_rule7*lassorpfl + svi3hi_rule8*lassorpfl + 
                            svi3hi_rule9*lassorpfl + svi3hi_rule10*lassorpfl + svi3hi_statquo*lassorpfl + 
                            
                            svi3lo_deaths*lassorpfl + svi3lo_cases*lassorpfl + svi3lo_cost*feduiany*lassorpfl + svi3lo_unempl*feduiany*lassorpfl + 
                            svi3lo_rule1*lassorpfl + svi3lo_rule2*lassorpfl + svi3lo_rule3*lassorpfl + svi3lo_rule4*lassorpfl + 
                            svi3lo_rule5*lassorpfl + svi3lo_rule6*lassorpfl + svi3lo_rule7*lassorpfl + svi3lo_rule8*lassorpfl + 
                            svi3lo_rule9*lassorpfl + svi3lo_rule10*lassorpfl + svi3lo_statquo*lassorpfl + 
                            
                            strata(choice),data=svidat
                          ,weights=popwt,method="approximate"
))

svi3.s <- summary(svi3reg)$coefficients %>% as.data.frame() %>% rownames_to_column(var="term") %>% 
  dplyr::filter(! str_detect(term,"lassorpfl"))

svi3hicoef <- svi3.s %>% dplyr::filter(str_detect(term,"svi3hi"))
svi3locoef <- svi3.s %>% dplyr::filter(str_detect(term,"svi3lo"))



tsvi3 <- stargazer(fakesvi,fakesvi,type="latex",
                   # omit=c("^ethfrac$","havzipvars"),
                   coef = list(
                     svi3locoef$coef,
                     svi3hicoef$coef
                   ),
                   # standard errors
                   se = list(
                     svi3locoef$`robust se`,
                     svi3hicoef$`robust se`
                   ),
                   column.labels = c("SVI 3 Low", "SVI 3 High"),
                   keep.stat="n",
                   t.auto = T)

tsvi3 <- paste(tsvi3,collapse="\n")

for (i in nrow(varlabs):1) {
  if(str_detect(tsvi3,paste0("",varlabs$variable[i]))) {
    tsvi3 <- str_replace_all(tsvi3,varlabs$variable[i],varlabs$label[i])
    tsvi3 <- str_replace_all(tsvi3," numberernative","")
  }
}

cat(tsvi3)

############### svi4 ##############

summary(svi4reg <- clogit(best ~
                            svi4hi_deaths*lassorpfl + svi4hi_cases*lassorpfl + svi4hi_cost*feduiany*lassorpfl + svi4hi_unempl*feduiany*lassorpfl + 
                            svi4hi_rule1*lassorpfl + svi4hi_rule2*lassorpfl + svi4hi_rule3*lassorpfl + svi4hi_rule4*lassorpfl + 
                            svi4hi_rule5*lassorpfl + svi4hi_rule6*lassorpfl + svi4hi_rule7*lassorpfl + svi4hi_rule8*lassorpfl + 
                            svi4hi_rule9*lassorpfl + svi4hi_rule10*lassorpfl + svi4hi_statquo*lassorpfl + 
                            
                            svi4lo_deaths*lassorpfl + svi4lo_cases*lassorpfl + svi4lo_cost*feduiany*lassorpfl + svi4lo_unempl*feduiany*lassorpfl + 
                            svi4lo_rule1*lassorpfl + svi4lo_rule2*lassorpfl + svi4lo_rule3*lassorpfl + svi4lo_rule4*lassorpfl + 
                            svi4lo_rule5*lassorpfl + svi4lo_rule6*lassorpfl + svi4lo_rule7*lassorpfl + svi4lo_rule8*lassorpfl + 
                            svi4lo_rule9*lassorpfl + svi4lo_rule10*lassorpfl + svi4lo_statquo*lassorpfl + 
                            
                            strata(choice),data=svidat
                          ,weights=popwt,method="approximate"
))

svi4.s <- summary(svi4reg)$coefficients %>% as.data.frame() %>% rownames_to_column(var="term") %>% 
  dplyr::filter(! str_detect(term,"lassorpfl"))

svi4hicoef <- svi4.s %>% dplyr::filter(str_detect(term,"svi4hi"))
svi4locoef <- svi4.s %>% dplyr::filter(str_detect(term,"svi4lo"))



tsvi4 <- stargazer(fakesvi,fakesvi,type="latex",
                   # omit=c("^ethfrac$","havzipvars"),
                   coef = list(
                     svi4locoef$coef,
                     svi4hicoef$coef
                   ),
                   # standard errors
                   se = list(
                     svi4locoef$`robust se`,
                     svi4hicoef$`robust se`
                   ),
                   column.labels = c("SVI 4 Low", "SVI 4 High"),
                   keep.stat="n",
                   t.auto = T)

tsvi4 <- paste(tsvi4,collapse="\n")

for (i in nrow(varlabs):1) {
  if(str_detect(tsvi4,paste0("",varlabs$variable[i]))) {
    tsvi4 <- str_replace_all(tsvi4,varlabs$variable[i],varlabs$label[i])
    tsvi4 <- str_replace_all(tsvi4," numberernative","")
  }
}


cat(tsvi1)
cat(tsvi2)
cat(tsvi3)
cat(tsvi4)


########### individual level analysis #################

people <- ideoldat %>% dplyr::select(ResponseId) %>% unique()
partial <- ideoldat %>% dplyr::select(ResponseId,ethfrac,ideol2,havzipvars,pziprwhite,white,male,female,nonbinary,agebcont,
                                      idrule1,idrule2,idrule3,idrule4,idrule5,idrule6,idrule7,idrule8,idrule9,idrule10,
                                      hhld0to1,hhld2to5,hhld6to12,hhld13to17,svi1lo,svi1hi)

people <- left_join(people,partial) %>% unique() 

summary(lm(svi1lo ~ ideol2 + 0,data=people))

ggplot(people[which(people$ideol2 %in% c("liberal", "conservative")),]) + 
  geom_density(aes(x=ethfrac,group=ideol2,fill=ideol2),alpha=.5) +
  scale_fill_manual(labels = c("Conservative", "Liberal"), values = c("red", "blue")) +
  labs(x="Ethnic fractionalization",y="",fill="Ideology") +
  theme_minimal()

ggplot(people[which(people$ideol2 %in% c("liberal", "conservative")),]) + 
  geom_density(aes(x=pziprwhite,group=ideol2,fill=ideol2),alpha=.5) +
  scale_fill_manual(labels = c("Conservative", "Liberal"), values = c("red", "blue")) +
  labs(x="Proportion white",y="",fill="Ideology") +
  theme_minimal()


ggplot(people[which(people$ideol2 %in% c("liberal", "conservative")),]) + 
  geom_density(aes(x=agebcont,group=ideol2,fill=ideol2),alpha=.5) +
  scale_fill_manual(labels = c("Conservative", "Liberal"), values = c("red", "blue")) +
  labs(x="Age",y="",fill="Ideology") +
  theme_minimal()

ggplot(people[which(people$ideol2 %in% c("liberal", "conservative")),]) + 

cor.test(as.numeric(people$agebcont),as.numeric(I(people$ideol2=="conservative")))

summary(lm(agebcont ~ ideol2 + 0,data=people))

summary(lm(I(idrule3<2) ~ hhld0to1 + hhld2to5 + hhld6to12 + hhld2to5*hhld13to17,data=people))

############ PLAYGROUND ##########

raw <- read.csv("~/covid-survey/data/qualtricsraw.csv")[-c(1:2),] %>% dplyr::select(StartDate,ResponseId)
raw$StartDate <- raw$StartDate %>% ymd_hms()
dat <- left_join(dat,raw)

dat$afternb <- ifelse(dat$StartDate > ymd_hms("2021-02-05 14:36:15"),1,0)

nbdat <- dat %>% mutate(afternb_deaths = mabsdeaths*I(afternb==1),
                            afternb_cases = mabscases*I(afternb==1),
                            afternb_cost = avcost*I(afternb==1),
                            afternb_unempl = unempl*I(afternb==1),
                            afternb_rule1 = rule1*I(afternb==1),
                            afternb_rule2 = rule2*I(afternb==1),
                            afternb_rule3 = rule3*I(afternb==1),
                            afternb_rule4 = rule4*I(afternb==1),
                            afternb_rule5 = rule5*I(afternb==1),
                            afternb_rule6 = rule6*I(afternb==1),
                            afternb_rule7 = rule7*I(afternb==1),
                            afternb_rule8 = rule8*I(afternb==1),
                            afternb_rule9 = rule9*I(afternb==1),
                            afternb_rule10 = rule10*I(afternb==1),
                            afternb_statquo = statquo*I(afternb==1))

nbdat <- nbdat %>% mutate(sawnb_deaths = mabsdeaths*I(afternb==0),
                                  sawnb_cases = mabscases*I(afternb==0),
                                  sawnb_cost = avcost*I(afternb==0),
                                  sawnb_unempl = unempl*I(afternb==0),
                                  sawnb_rule1 = rule1*I(afternb==0),
                                  sawnb_rule2 = rule2*I(afternb==0),
                                  sawnb_rule3 = rule3*I(afternb==0),
                                  sawnb_rule4 = rule4*I(afternb==0),
                                  sawnb_rule5 = rule5*I(afternb==0),
                                  sawnb_rule6 = rule6*I(afternb==0),
                                  sawnb_rule7 = rule7*I(afternb==0),
                                  sawnb_rule8 = rule8*I(afternb==0),
                                  sawnb_rule9 = rule9*I(afternb==0),
                                  sawnb_rule10 = rule10*I(afternb==0),
                                  sawnb_statquo = statquo*I(afternb==0))

summary(main <- clogit(best ~
                         

                         mabsdeaths*lassorpfl + mabscases*lassorpfl +
                         unempl*countymatch*lassorpfl + avcost*countymatch*lassorpfl + 
                         
                         rule1*lassorpfl + rule2*lassorpfl +
                         rule3*lassorpfl + rule4*lassorpfl +
                         rule5*lassorpfl + rule6*lassorpfl +
                         rule7*lassorpfl + rule8*lassorpfl +
                         rule9*lassorpfl + rule10*lassorpfl +
                         
                         
                         
                         statquo*countymatch*lassorpfl +

                         strata(choice),data=dat
                       ,weights=popwt,method="approximate"
))
 
res <- broom::tidy(main) %>% dplyr::filter(!str_detect(term,"lasso")) %>% mutate(star=ifelse(p.value<0.05,"*",""))

summary(lm(lassorpfl ~ afternb,data=dat))
