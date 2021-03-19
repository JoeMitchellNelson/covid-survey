# the 2 and 3 alt problem

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


a <- dat %>% group_by(ResponseId) %>% summarise(n1=sum(choiceofperson==1),
                                                n2=sum(choiceofperson==2),
                                                n3=sum(choiceofperson==3),
                                                n6=sum(choiceofperson==6))

b <- a %>% dplyr::filter(n1==0 | n2==0 | n3==0 | n6==0)

write.csv(b,"~/covid-survey/missing-a-choice.csv")

huh <- dat %>% dplyr::filter(ResponseId %in% b$ResponseId)

########## DOES THE FIRST FEDUI YOU SAW MATTER FOR THE REST OF THE THINGS? ############

firstfed <- dat %>% dplyr::filter(choiceofperson==1) %>% dplyr::select(ResponseId,fedui,feduiany) %>% unique()
names(firstfed) <- c("ResponseId","feduifirst","feduianyfirst")
dat <- left_join(dat,firstfed,by="ResponseId")

main1 <- clogit(best ~
                  mabsdeaths*lassorpfl + mabscases*lassorpfl +
                  unempl*feduianyfirst*lassorpfl + avcost*feduianyfirst*lassorpfl +
                  unempl*feduiany*lassorpfl + avcost*feduiany*lassorpfl +
                  
                  rule1*lassorpfl + rule2*lassorpfl +
                  rule3*lassorpfl + rule4*lassorpfl +
                  rule5*lassorpfl + rule6*lassorpfl +
                  rule7*lassorpfl + rule8*lassorpfl +
                  rule9*lassorpfl + rule10*lassorpfl +
                  
                  statquo*lassorpfl +
                  strata(choice),data=dat[which(dat$choiceofperson %in% c(1,2)),]
                ,weights=popwt,method="approximate"
)
a <- summary(main1)
a <- a$coefficients %>% as.data.frame() %>% rownames_to_column("term") %>% dplyr::filter(!str_detect(term,"lasso")) %>% dplyr::select(term,coef,`robust se`,z,`Pr(>|z|)`)
a

stargazer(main1,omit="lasso")
