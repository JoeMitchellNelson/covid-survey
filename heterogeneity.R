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


dat <- left_join(dat,newdemean)


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
                         avcost*feduiany*lassorpfl + 
                         unempl*feduiany*lassorpfl +
                         
                         I(rule1)*lassorpfl + I(rule2)*lassorpfl +
                         I(rule3)*lassorpfl + I(rule4)*lassorpfl +
                         I(rule5)*lassorpfl + I(rule6)*lassorpfl +
                         I(rule7)*lassorpfl + I(rule8)*lassorpfl +
                         I(rule9)*lassorpfl + I(rule10)*lassorpfl +
                         
                         
                         statquo*lassorpfl +
                         strata(choice),data=dat
                       ,weights=popwt,method="approximate"
))




summary(main2 <- clogit(best ~
                          mabsdeaths*lassorpfl + mabscases*lassorpfl +
                          unempl*lassorpfl +
                          
                          factor(rule1)*lassorpfl + factor(rule2)*lassorpfl +
                          factor(rule3)*lassorpfl + factor(rule4)*lassorpfl +
                          factor(rule5)*lassorpfl + factor(rule6)*lassorpfl +
                          factor(rule7)*lassorpfl + factor(rule8)*lassorpfl +
                          factor(rule9)*lassorpfl + factor(rule10)*lassorpfl +
                          
                          
                          statquo*lassorpfl +
                         strata(choice),data=dat
                       ,weights=popwt2,method="approximate"
))

summary(main3 <- clogit(best ~
                          mabsdeaths*lassorpfl + mabscases*lassorpfl +
                          avcost*lassorpfl + 
                          
                          factor(rule1)*lassorpfl + factor(rule2)*lassorpfl +
                          factor(rule3)*lassorpfl + factor(rule4)*lassorpfl +
                          factor(rule5)*lassorpfl + factor(rule6)*lassorpfl +
                          factor(rule7)*lassorpfl + factor(rule8)*lassorpfl +
                          factor(rule9)*lassorpfl + factor(rule10)*lassorpfl +
                          
                          
                          statquo*lassorpfl +
                          strata(choice),data=dat
                        ,weights=popwt2,method="approximate"
))


# t1 <- stargazer(main,main2,main3,keep.stat = "n",type="text") %>% paste(collapse="\n")
# 
# for (i in 1:nrow(varlabs)) {
#   if(str_detect(t1,paste0(" ",varlabs$variable[i]))) {
#    t1 <- str_replace_all(t1,varlabs$variable[i],varlabs$label[i])
#   }
# }
# 
# cat(t1)

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
                                 conservative_rule9*lassorpfl + conservative_rule10*lassorpfl + conservative_statquo*lassorpfl + 
                                 
                                 moderate_deaths*lassorpfl + moderate_cases*lassorpfl + moderate_cost*feduiany*lassorpfl + moderate_unempl*feduiany*lassorpfl + 
                                 moderate_rule1*lassorpfl + moderate_rule2*lassorpfl + moderate_rule3*lassorpfl + moderate_rule4*lassorpfl + 
                                 moderate_rule5*lassorpfl + moderate_rule6*lassorpfl + moderate_rule7*lassorpfl + moderate_rule8*lassorpfl + 
                                 moderate_rule9*lassorpfl + moderate_rule10*lassorpfl + moderate_statquo*lassorpfl + 
                                 
                                 liberal_deaths*lassorpfl + liberal_cases*lassorpfl + liberal_cost*feduiany*lassorpfl + liberal_unempl*feduiany*lassorpfl + 
                                 liberal_rule1*lassorpfl + liberal_rule2*lassorpfl + liberal_rule3*lassorpfl + liberal_rule4*lassorpfl + 
                                 liberal_rule5*lassorpfl + liberal_rule6*lassorpfl + liberal_rule7*lassorpfl + liberal_rule8*lassorpfl + 
                                 liberal_rule9*lassorpfl + liberal_rule10*lassorpfl + liberal_statquo*lassorpfl + 
                                 
                                 MISSING_deaths*lassorpfl + MISSING_cases*lassorpfl + MISSING_cost*feduiany*lassorpfl + MISSING_unempl*feduiany*lassorpfl + 
                                 MISSING_rule1*lassorpfl + MISSING_rule2*lassorpfl + MISSING_rule3*lassorpfl + MISSING_rule4*lassorpfl + 
                                 MISSING_rule5*lassorpfl + MISSING_rule6*lassorpfl + MISSING_rule7*lassorpfl + MISSING_rule8*lassorpfl + 
                                 MISSING_rule9*lassorpfl + MISSING_rule10*lassorpfl + MISSING_statquo*lassorpfl +
                                 
                                 strata(choice),data=ideoldat
                               ,weights=popwt,method="approximate"
))


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

stargazer(ideolmodel,ideolmodelrp,ideolmodelfedui,ideolmodelrpfedui,type="text",omit="lassorpfl")

res <- broom::tidy(main2) %>% 
  dplyr::filter(!str_detect(term,"lassorp") & !is.na(estimate)) %>% 
  mutate(star=ifelse(p.value<0.05,"*","")) %>% 
  dplyr::select(star,everything())

res$term <- str_remove_all(res$term,"ideol2")

View(res)
stargazer(main2,type="text")

ideolrp <- dat %>% group_by(ideol) %>% summarise(meanrplasso = mean(lassorp),
                                                 meanrplassofl = mean(lassorpfl),
                                                 trudy = mean(demeanrp))

### emphasize the survey writing process



### talk about history, at least enough to say *that* it matters
### looking for help thinking about how and why it matters
### serial correlation? 
