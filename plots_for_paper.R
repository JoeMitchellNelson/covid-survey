#plots for paper

require(pacman)

p_load(rpart,readr,dplyr,RCurl,rjson,lubridate,
       rvest,stringr,Hmisc,rattle,RColorBrewer,ddpcr,tidytext,tidyr,
       ggrepel,ggplot2,png,ggpubr,tidycensus,sf,plm,lmtest,stargazer,MASS,
       xtable,knitr,magick,purrr,ggthemes,gifski,extrafont,latex2exp,
       cowplot,mapproj,patchwork,remotes)


font_import()
fonts()
loadfonts()

windowsFonts(Garamond=windowsFont("LM Roman 10"))

newdemean <- read.csv("~/covid-survey/demeanrp-lasso.csv")[,-1]
newdemean$complete <- ifelse(newdemean$complete==1,"Responders","Non-responders")

people <- dat %>% dplyr::select(ResponseId,lassorpfl)

ggplot(newdemean) +
  geom_vline(xintercept=0,linetype="dashed",color="grey50") +
  geom_density(aes(x=lassorpfl,group=factor(complete),fill=factor(complete)),alpha=.5,color=NA,bw=.2) +
  scale_fill_manual(values=c("blue","forestgreen")) +
  geom_density(aes(x=lassorpfl),fill="NA",color="black",bw=.2) +
  labs(x="Demeaned response propensity",y="Smoothed density",fill="") +
  guides(color=guide_legend(order=1)) +
  theme_minimal() +
  theme(text = element_text(size=10, family="Garamond"),
        axis.line = element_line(size = 1, colour = "grey50", linetype=1),
        legend.position="bottom", legend.box = "horizontal") 

ggsave("~/covid-survey/lasso-rp-plot.png",last_plot(),width=5,height=3,units="in")


ggplot(dat[which(dat$statquo!=1),]) +
  geom_point(aes(x=avcost*100,y=unempl,color=factor(fedui))) +
  scale_color_viridis_d(option="C") +
  labs(x="Lost income per month ($)",y="Unemployment (%)", color="Federal UI\n($/week)") +
  theme_minimal() +
  theme(panel.background = element_rect(fill="grey60",color="white"))
