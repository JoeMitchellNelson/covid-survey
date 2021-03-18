require(pacman)

p_load(rpart,rpart.plot,readr,dplyr,RCurl,rjson,lubridate,readxl,
       rvest,stringr,Hmisc,rattle,RColorBrewer,ddpcr,tidytext,tidyr,
       ggrepel,ggplot2,png,ggpubr,tidycensus,sf,plm,lmtest,stargazer,MASS,
       xtable,knitr,magick,purrr,ggthemes,gifski,extrafont,latex2exp,evd,
       cowplot,mapproj,patchwork,remotes,tictoc,Hmisc,english,readstata13,margins)

# data is from Raifman et al. (2020)

pol <- read.csv("~/covid-survey/data/covidrestrictions.csv")[-c(1:4),] %>% dplyr::filter(STATE != "")
polvars <- read.csv("~/covid-survey/data/covidrestrictions.csv")[c(1:4),] %>% t() %>% as.data.frame() %>% rownames_to_column()
#######################################################
##### indicators for restriction severity #############
#######################################################

# indicator list is from Armstrong 2020 "...75 US/Canadian cities..."

# state of emergency declared STEMERG
# school closures CLSCHOOL, 
# day care closures CLDAYCR,	OPNCLDCR
# limits on nursing home visits CLNURSHM
# shelter-in-place rules STAYHOME	STAYHOMENOGP	END_STHM
# non-essential business closures CLBSNS	END_BSNS
# public mask wearing
# restaurant closures CLREST	ENDREST
# fitness centre closures CLGYM	ENDGYM
# cinema closures  CLMOVIE  END_MOV

# others

# religious gatherings exempt RELIGEX


pols <- pol %>% dplyr::select(STATE,FIPS,CLREST,ENDREST,CLRST2,ENDREST2,CLRST3,END_CLRST3)

pols$CLREST <- mdy(pols$CLREST)
pols$ENDREST <- mdy(pols$ENDREST)
pols$CLRST2 <- mdy(pols$CLRST2)
pols$ENDREST2 <- mdy(pols$ENDREST2)
pols$CLRST3 <- mdy(pols$CLRST3)
pols$END_CLRST3 <- mdy(pols$END_CLRST3)


ts <- expand.grid(pols$STATE,min(pols$CLREST,na.rm=T) + ddays(1:330))
names(ts) <- c("STATE","DATE")

statelist <- unique(pols$STATE)


ts$REST <- 1

for (i in 1:51) {
  
  if (!is.na(pols$ENDREST[which(pols$STATE==statelist[i])])) {
    int1 <- interval(pols$CLREST[which(pols$STATE==statelist[i])],pols$ENDREST[which(pols$STATE==statelist[i])]) 
  } else  {
    int1 <- interval(pols$CLREST[which(pols$STATE==statelist[i])],today())
  }
  
  
  if (!is.na(int1)) {
    
    ts$REST <- ifelse(ts$STATE==statelist[i] & 
                        (ts$DATE %within% int1),
                      0,ts$REST)
    
  }
  
  
  if (!is.na(pols$ENDREST2[which(pols$STATE==statelist[i])])) {
    int2 <- interval(pols$CLRST2[which(pols$STATE==statelist[i])],pols$ENDREST2[which(pols$STATE==statelist[i])]) 
  } else  {
    int2 <- interval(pols$CLRST2[which(pols$STATE==statelist[i])],today())
  }
  

  if (!is.na(int2)) {
    
    ts$REST <- ifelse(ts$STATE==statelist[i] & 
                        (ts$DATE %within% int2),
                      0,ts$REST)
  }
  
  if (!is.na(pols$END_CLRST3[which(pols$STATE==statelist[i])])) {
    int3 <- interval(pols$CLRST3[which(pols$STATE==statelist[i])],pols$END_CLRST3[which(pols$STATE==statelist[i])]) 
  } else  {
    int3 <- interval(pols$CLRST3[which(pols$STATE==statelist[i])],today())
  }
  

  if (!is.na(int3)) {
    
    ts$REST <- ifelse(ts$STATE==statelist[i] & 
                        (ts$DATE %within% int3),
                      0,ts$REST)
  }
  
  
  
}
head(ts)


blues <- c("Washington","Oregon","California","New York","District of Columbia","Maine","New Hampshire","Delaware","New Jersey",
           "Colorado","Connecticutt","Georgia","Hawaii","Illinois","Maine","Rhode Island","Pennsylvania", "Maryland",
           "New Mexico","Nevada", "Wisconsin","Virginina","Vermont","New Hampshire","Massachusetts")

ts$POL <- ifelse(ts$STATE %in% blues,"BLUE","RED")

ts2 <- ts %>% group_by(DATE,POL) %>% summarise(RESTSTATE=sum(REST),n=n())
ts2 <- ts2 %>% mutate(RESTSTATE= RESTSTATE/n)


ggplot(ts2) +
  geom_point(aes(x=DATE,y=RESTSTATE,color=POL)) +
  labs(y="Proportion with restaurants open",x="",color="") +
  geom_vline(xintercept=ymd("2020-07-26")) +
  geom_vline(xintercept=ymd("2020-12-26")) +
  scale_color_manual(values=c("blue","red"))

