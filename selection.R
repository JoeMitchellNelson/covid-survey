require(pacman)

p_load(rpart,rpart.plot,readr,dplyr,RCurl,rjson,lubridate,
       rvest,stringr,Hmisc,rattle,RColorBrewer,ddpcr,tidytext,tidyr,
       ggrepel,ggplot2,png,ggpubr,tidycensus,sf,plm,lmtest,stargazer,MASS,
       xtable,knitr,magick,purrr,ggthemes,gifski,extrafont,latex2exp,
       cowplot,mapproj,patchwork,remotes,tictoc,Hmisc,english,readstata13,lfe)

dat <- read.csv("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_nointx.csv")

#dat <- dat %>% dplyr::filter(rejectonly==0)

dat$ideol <- ifelse(dat$ideolsconserv==1,"strong conservative",
                    ifelse(dat$ideolsliberal==1,"strong liberal",
                           ifelse(dat$ideolmconserv==1,"moderately conservative",
                                  ifelse(dat$ideolmliberal==1,"moderately liberal",
                           ifelse(dat$ideolmoderate==1,"moderate","missing")))))

dat$ideol <- as.factor(dat$ideol)
dat$ideol <- factor(dat$ideol,levels=c("strong liberal","moderately liberal","moderate","moderately conservative","strong conservative","missing"))

dat2 <- dat %>% dplyr::filter(statquo==0 & choiceofperson %in% c(1,2)) %>% 
  group_by(ideol) %>% 
  summarise(meancost = mean(avcost),
            sdcost=sd(avcost),
            n=n())

dat2 <- dat2 %>% mutate(se=sdcost/sqrt(n)) %>% mutate(conflo = meancost - 1.96*se, confhi=meancost + 1.96*se)

ggplot(dat2,aes(x=ideol,y=meancost)) + 
  geom_point(aes(color=ideol),show.legend = F) +
  geom_errorbar(aes(ymin=conflo,ymax=confhi,color=ideol),width=.3,show.legend = F) + 
  scale_color_manual(values=c( "#0000FF","#3300FF","#AA00AA", "#FF0033","#FF0000","grey50")) +
  labs(x="",y="Mean cost for policies A and B (excluding stat quo)") +
  coord_flip() +
  theme_minimal()

dat3 <- dat %>% dplyr::filter(statquo==0 & choiceofperson %in% c(1,2)) %>% group_by(ResponseId) %>% 
  summarise(cost=mean(avcost),ideol=first(ideol),
            pvotedem=first(pvotedem),pvoterep=first(pvoterep),
            cases=mean(delcases),deaths=mean(deldeaths),
            countyname=first(countyname),
            inc = first(countyhhldinc))

summary(felm(cost ~ ideol + inc,data=within(dat3, ideol <- relevel(ideol, ref = 3))))
#
summary(lm(cost ~ I(ideol %in% c("moderately liberal","strong liberal")),data=dat3))

summary(lm(I(ideol %in% c("moderately liberal","strong liberal")) ~ cost,data=dat3))

dat4 <- dat %>% dplyr::filter(statquo==0 & choiceofperson %in% c(1,2)) %>% 
  group_by(ResponseId) %>% summarise(cost=mean(avcost),
                                     idrule1=first(idrule1),
                                     idrule2=first(idrule2),
                                     idrule3=first(idrule3),
                                     idrule4=first(idrule4),
                                     idrule5=first(idrule5),
                                     idrule6=first(idrule6),
                                     idrule7=first(idrule7),
                                     idrule8=first(idrule8),
                                     idrule9=first(idrule9),
                                     idrule10=first(idrule10))

summary(lm(cost ~ idrule3 + idrule8,data=dat4))

