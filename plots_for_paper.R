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
  geom_density(aes(x=lassorpfl,group=factor(complete),fill=factor(complete)),alpha=.5) +
  scale_fill_manual(values=c("blue","forestgreen")) +
  labs(x="Demeaned response propensity",y="Smoothed density",fill="") +
  theme_minimal() +
  theme(text = element_text(size=10, family="Garamond"),
        axis.line = element_line(size = 1, colour = "grey50", linetype=1)) 

ggsave("~/covid-survey/lasso-rp-plot.png",last_plot(),width=5,height=3,units="in")
             