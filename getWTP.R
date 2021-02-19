require(pacman)

p_load(rpart,rpart.plot,readr,dplyr,RCurl,rjson,lubridate,
       rvest,stringr,Hmisc,rattle,RColorBrewer,ddpcr,tidytext,tidyr,
       ggrepel,ggplot2,png,ggpubr,tidycensus,sf,plm,lmtest,stargazer,MASS,
       xtable,knitr,magick,purrr,ggthemes,gifski,extrafont,latex2exp,
       cowplot,mapproj,patchwork,remotes,tictoc,Hmisc,english,readstata13)


p_load(glmnet,tibble,broom)

dat <- read.dta13("C:/Users/Joe/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_intx.dta")

##### "Standard" bundle of attributes #######

mabsdeaths_w <- median(dat$mabsdeaths[which(dat$statquo==0)]/dat$countypop[which(dat$statquo==0)]) * median(dat$countypop[which(dat$statquo==0)])
mabsdeaths_wo <- median(dat$mabsdeaths[which(dat$statquo==1)]/dat$countypop[which(dat$statquo==1)]) * median(dat$countypop[which(dat$statquo==1)])
mabsnfcases_w <- median(dat$mabsnfcases[which(dat$statquo==0)]/dat$countypop[which(dat$statquo==0)]) * median(dat$countypop[which(dat$statquo==0)])
mabsnfcases_wo <- median(dat$mabsnfcases[which(dat$statquo==1)]/dat$countypop[which(dat$statquo==1)]) * median(dat$countypop[which(dat$statquo==1)])
avcost_w <- median(dat$avcost[which(dat$statquo==0)])
avcost_wo <- 0
unempl_w <- median(dat$unempl[which(dat$statquo==0)])
unempl_wo <- median(dat$unempl[which(dat$statquo==1)])
rule1_w <- 2
rule2_w <- 2
rule3_w <- 2
rule4_w <- 2
rule5_w <- 2
rule6_w <- 2
rule7_w <- 2
rule8_w <- 2
rule9_w <- 2
rule10_w <- 2

stbundle <- data.frame(mabsdeaths= mabsdeaths_w-mabsdeaths_wo,
                       mabsnfcases=mabsnfcases_w - mabsnfcases_wo,
                       avcost=avcost_w-avcost_wo,
                       unempl=unempl_w-unempl_wo,
                       rule1=rule1_w,
                       rule2=rule2_w,
                       rule3=rule3_w,
                       rule4=rule4_w,
                       rule5=rule5_w,
                       rule6=rule6_w,
                       rule7=rule7_w,
                       rule8=rule8_w,
                       rule9=rule9_w,
                       rule10=rule10_w,
                       statquo=-1) %>% t() %>% as.data.frame() %>% rownames_to_column("term")
names(stbundle)[2] <- "val"

######### "Standard" county/zip characteristics #########

stcounty <- data.frame(OR=0,              
                       WA=0,
                       demeanrp=0,
                       female=.5,
                       owninc = median(dat$owninc),
                       pvoterep=median(dat$pvoterep),
                       pvoteother= median(dat$pvoteother),     
                       pzipageunder6=median(dat$pzipageunder6), 
                       pzipage6to11=median(dat$pzipage6to11),    
                       pzipage12to17=median(dat$pzipage12to17),   
                       pzipage18to24=median(dat$pzipage18to24),
                       pzipage25to34=median(dat$pzipage25to34),
                       pzipage25to44=median(dat$pzipage25to44),
                       pzipage55to64=median(dat$pzipage55to64),
                       pzipage65to74=median(dat$pzipage65to74),
                       pziprblack= median(dat$pziprblack),
                       pziprasian=median(dat$pziprasian),
                       pziprothrace3=median(dat$pziprothrace3),
                       pziprmulti=median(dat$pziprmulti),
                       pziphealthinsno = median(dat$pziphealthinsno),
                       pzipiagric= median(dat$pzipiagric),
                       pzipiconstr=     median(dat$pzipiconstr),
                       pzipimanuf=       median(dat$pzipimanuf),
                       pzipiwholes=      median(dat$pzipiwholes),
                       pzipiretail=      median(dat$pzipiretail),
                       pzipitransp=      median(dat$pzipitransp),
                       pzipiinfor=       median(dat$pzipiinfor),
                       pzipifinan=       median(dat$pzipifinan),
                       pzipiprofsci=    median(dat$pzipiprofsci),
                       pzipiartent=      median(dat$pzipiartent),
                       pzipiothserv=     median(dat$pzipiothserv),
                       pzipipubadm=    median(dat$pzipipubadm),
                       countypop=    median(dat$countypop),
                       fedui=          300,
                       reject=        0,
                       monthstwo=     0,  
                       monthsthr=     0) %>% t() %>% as.data.frame() %>% rownames_to_column("term")
names(stcounty)[2] <- "val"

stand <- rbind(stbundle,stcounty)

getWTP <- left_join(restidy,stand,by=c("term1"="term"))
getWTP <- left_join(getWTP,stand,by=c("term2"="term"))
getWTP$val.y <- ifelse(getWTP$term2=="",1,getWTP$val.y)
getWTP$iscost <- ifelse(str_detect(getWTP$term,"avcost"),1,0)

denom <- sum(getWTP$estimate * (getWTP$iscost) * getWTP$val.x * getWTP$val.y)

num <- sum(getWTP$estimate * getWTP$val.x * getWTP$val.y * (1-getWTP$iscost))

totalWTP <- -num/denom

