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

dat <- read.csv("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_nointx.csv") %>% dplyr::filter(rejectonly==0,owninc>0) 
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


dat$ideol <- ifelse(dat$ideolsconserv==1,"strongly conservative",
                    ifelse(dat$ideolsliberal==1,"strongly liberal",
                           ifelse(dat$ideolmliberal==1,"moderately liberal",
                                  ifelse(dat$ideolmconserv==1,"moderately conservative",
                           ifelse(dat$ideolmoderate==1,"moderate","MISSING")))))
dat$ideol <- as.factor(dat$ideol)
dat <- within(dat, ideol <- relevel(ideol, ref = 2))



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
                      mabsdeaths + mabscases +
                      avcost + 
                      
                      rule1 + rule2 +
                      rule3 + rule4 +
                      rule5 + rule6 +
                      rule7 + rule8 +
                      rule9 + rule10 +
                      
                      
                      statquo +
                      strata(choice),data=dat
                    ,weights=popwt,method="approximate"
))




summary(main2 <- clogit(best ~
                         mabsdeaths + mabscases +
                         unempl + 
                         
                         rule1 + rule2 +
                         rule3 + rule4 +
                         rule5 + rule6 +
                         rule7 + rule8 +
                         rule9 + rule10 +
                         
                         
                         statquo +
                         strata(choice),data=dat
                       ,weights=popwt,method="approximate"
))

summary(main3 <- clogit(best ~
                          mabsdeaths + mabscases +
                          avcost + unempl + 
                          
                          rule1 + rule2 +
                          rule3 + rule4 +
                          rule5 + rule6 +
                          rule7 + rule8 +
                          rule9 + rule10 +
                          
                          
                          statquo +
                          strata(choice),data=dat
                        ,weights=popwt,method="approximate"
))


t1 <- stargazer(main,main2,main3,keep.stat = "n") %>% paste(collapse="\n")

for (i in 1:nrow(varlabs)) {
  if(str_detect(t1,paste0(" ",varlabs$variable[i]))) {
   t1 <- str_replace_all(t1,varlabs$variable[i],varlabs$label[i])
  }
}

cat(t1)

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
  if(str_detect(t1,paste0(" ",varlabs$variable[i]))) {
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
                      mabsdeaths*agebins  + mabscases*agebins  +
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
                      mabsdeaths*ffcomcovid*ageb65up  + mabscases*ffcomcovid*ageb65up  +
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
### emphasize the survey writing process



### talk about history, at least enough to say *that* it matters
### looking for help thinking about how and why it matters
### serial correlation? 
