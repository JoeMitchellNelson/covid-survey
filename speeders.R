require(pacman)

p_load(rpart,rpart.plot,readr,dplyr,RCurl,rjson,lubridate,
       rvest,stringr,Hmisc,rattle,RColorBrewer,ddpcr,tidytext,tidyr,
       ggrepel,ggplot2,png,ggpubr,tidycensus,sf,plm,lmtest,stargazer,MASS,
       xtable,knitr,magick,purrr,ggthemes,gifski,extrafont,latex2exp,evd,
       cowplot,mapproj,patchwork,remotes,tictoc,Hmisc,english,readstata13,margins)

#################### READ IN DATA ################
{
  
  newdemean <- read.csv("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID-shared/intermediate-files/demeanrp-lasso.csv")[,-1]
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
  
}


raw <- read.csv("~/covid-survey/data/qualtricsraw.csv")
raw <- raw %>%  dplyr::filter(ResponseId %in% dat$ResponseId)

times <- raw[,which(str_detect(names(raw),"Page\\.Submit"))]

plot(density(as.numeric(times[6,]),na.rm=T))

clicks <- raw[,which(str_detect(names(raw),"Click\\.Count"))]

fton <- function (x) {
  x %>% as.character() %>% as.numeric()
}

clicks <- clicks[,1:ncol(clicks)] %>% sapply(fton) %>% unlist() %>% as.data.frame()
times <- times[,1:ncol(times)] %>% sapply(fton) %>% unlist() %>% as.data.frame()

rmax <- function (x) {
  max(x,na.rm=T)
}

rsd <- function (x) {
  sd(x,na.rm=T)
}

rmean <- function (x) {
  mean(x,na.rm=T)
}

maxtime <- apply(times,1,rmax) 
sdtime <- apply(times,1,rsd)
meantime <- apply(times,1,rmean)
raw$maxtime <- maxtime
raw$sdtime <- sdtime
raw$meantime <- meantime

clicksum <- clicks %>% rowSums(na.rm=T)

raw$clicks <- clicksum

clicks <- raw %>% dplyr::select(ResponseId,clicks,maxtime,sdtime,meantime,Q224)

dat <- left_join(dat,clicks)

raw$neverpolicy <- ifelse(raw$Q78=="",1,0)
never <- raw %>% dplyr::select(ResponseId,neverpolicy)

dat <- left_join(dat,never)

summary(lm(pblazepage ~ firstbutton, data=dat))

ggplot(dat) +
  geom_point(aes(x=blazepage,y=clicks,color=neverpolicy)) +
  geom_smooth(aes(x=blazepage,y=clicks),method="lm")

dat$education <- ifelse(dat$scollorless==1,"Noncollege",
                        ifelse(dat$educpnts==1,"MISSING","College"))

dat$education <- factor(dat$education,levels=c("Noncollege","College","MISSING"))

summary(clogit(best ~ I(alt==3)*clicks + strata(choice),data=dat))

main1 <- clogit(best ~
                  mabsdeaths*I(maxtime<45) + mabscases*I(maxtime<45) +
                  unempl*I(maxtime<45) + avcost*I(maxtime<45) +
                  
                  rule1*I(maxtime<45) + rule2*I(maxtime<45) +
                  rule3*I(maxtime<45) + rule4*I(maxtime<45) +
                  rule5*I(maxtime<45) + rule6*I(maxtime<45) +
                  rule7*I(maxtime<45) + rule8*I(maxtime<45) +
                  rule9*I(maxtime<45) + rule10*I(maxtime<45) +
                  
                  statquo*I(maxtime<45) +
                  strata(choice),data=dat
                ,weights=popwt,method="approximate"
)
summary(main1)

people <- dat %>% dplyr::select(ResponseId,ideol,fedgood,fedbad,fedgreat,fedterrible,
                                agebcont,maxtime,clicks,neverpolicy,male,sdtime,lthighsch,
                                maxtime,meantime,pblazepage,firstbutton,Q224,defcon,deffor,countyname,countypop,pspeedpage,hispanic,ideolsconserv) %>% unique()

people$newadmingood <- ifelse(people$Q224=="Much improved",1,0)

people$alpha <- ifelse(str_detect(people$countyname,"^A"),1,0)
people$omega <- ifelse(str_detect(people$countyname,"^T|^U|^V|^W|^Y"),1,0)

summary(lm(pblazepage ~ newadmingood*ideolsconserv,data=people))
summary(lm(pblazepage ~ hispanic,data=people))

rcorr(as.matrix(people[which(names(people) %in% c("fedgreat","defcon","alpha","pblazepage","ideolsconserv","newadmingood"))]))



alphacounty <- people$countyname %>% unique()
alphacounty <- alphacounty[order(alphacounty)]
alphacounty <- data.frame(countyname=alphacounty,countyorder=1:length(alphacounty))

people <- left_join(people,alphacounty)

byc <- people %>% group_by(countyname) %>% summarise(meanblaze= mean(pblazepage),n=n(),order=mean(countyorder),countypop=mean(countypop))
head(byc)

ggplot(byc) +
  geom_point(aes(x=order,y=meanblaze)) +
  geom_smooth(aes(x=order,y=meanblaze))

ggplot(byc) +
  geom_point(aes(x=order,y=countypop)) +
  geom_smooth(aes(x=order,y=countypop))



ggplot(people[which(people$sdtime<15),]) +
  geom_point(aes(x=sdtime,y=meantime,color=I(maxtime<50)),size=1.5,alpha=1) +
  scale_color_viridis_d() +
  theme_minimal()
 # geom_smooth(aes(x=sdtime,y=meantime),method="lm")
  
summary(people$sdtime)

### mean of log normal is exp(mu + (sigma^2)/2)
### variance of log normal is (exp(sigma^2)-1) * exp(2*mu + sigma^2)


