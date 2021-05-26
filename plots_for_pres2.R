require(pacman)
p_load(tidyverse,ggplot2,lubridate)

#start 600 (PUC): April 5, 2020
#end 600 (PUC): July 31, 2020
#start 300 (Consolidated appropriations act: Dec 26, 2020
#scheduled end 300 (Consolidated appropriations act): March 14, 2021

int600 <- interval(ymd("2020-4-5"),ymd("2020-7-31"))
int300 <- interval(ymd("2020-12-26"),ymd("2021-3-14"))

field <- dat %>% dplyr::select(ResponseId,fromlaunch) %>% unique()
field$Date <- ymd("2021-1-13") + ddays(field$fromlaunch)
field$Week <- week(field$Date)

bene <- data.frame(Date=0:400,Benefit=0) 
bene$Date <- ymd("2020-03-20") + ddays(bene$Date)
bene$Benefit <- ifelse(bene$Date %within% int600,600,bene$Benefit)
bene$Benefit <- ifelse(bene$Date %within% int300,300,bene$Benefit)
bene <- bene %>% dplyr::filter(Date<=ymd("2021-3-15"))



tl1 <- ggplot(bene) +
  geom_line(aes(x=Date,y=Benefit)) +
  labs(x="",y="Federal\nunemployment\nbenefits\n(weekly)") +
  # geom_density(data=field,aes(x=Date,y=..scaled..*270),color=NA,fill="blue",alpha=0.5) +
  annotate(x=ymd("2020-6-3"),y=400,label="Pandemic\nUnemployment\nCompensation",geom="text") +
  annotate(x=ymd("2021-2-4"),y=400,label="Consolidated\nAppropriations\nAct",geom="text") +
  
 # annotate(x=ymd("2020-11-1"),y=250,label="Survey\nresponses\nreceived",geom="text",color="blue",alpha=0.8) +
#  geom_segment(x=ymd("2020-11-10"),xend=ymd("2021-1-19"),y=170,yend=100,color="blue",alpha=0.8) +
  
  annotate(x=ymd("2021-5-1"),y=345,label="Scheduled\nbenefits cliff\n(March 14)",geom="text",alpha=0.8) +
  geom_segment(x=ymd("2021-5-1"),xend=ymd("2021-3-14"),y=260,yend=200,alpha=0.8) +
  lims(x=c(ymd("2020-3-20"), ymd("2021-6-1"))) +
  scale_y_continuous(labels=scales::dollar_format()) +
  theme_minimal() +
  theme(axis.title.y = element_text(angle = 0,vjust=0.5),
        panel.background = element_rect(fill="#FAFAFA",color="#FAFAFA"),
        plot.background = element_rect(fill="#FAFAFA",color="#FAFAFA"))

tl2 <- ggplot(bene) +
  lims(x=c(ymd("2020-3-20"), ymd("2021-6-1")),y=c(-140,0)) +
  
 # annotate(x=ymd("2020-12-1"),y=-50,label="Biden proposes\nAmerican Rescue Plan\n(Jan 14)",geom="text") +
#  geom_segment(x=ymd("2020-12-26"),xend=ymd("2021-1-1"),y=-20,yend=0,alpha=0.8) +
  
 # annotate(x=ymd("2021-4-1"),y=-110,label="ARP signed\ninto law\n(March 11)",geom="text") +
#  geom_segment(x=ymd("2021-3-20"),xend=ymd("2021-3-15"),y=-75,yend=0,alpha=0.8) +
  
  labs(x="",y="Federal\nunemployment\nbenefits\n(weekly)",color="white") +
  theme_void() +
  theme(axis.title.y = element_text(angle = 0,vjust=0.5,color="white"),
        axis.ticks=element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill="#FAFAFA",color="#FAFAFA"),
        plot.background = element_rect(fill="#FAFAFA",color="#FAFAFA"))

tl1 / tl2 +plot_layout(heights=c(3,1.7))

ggsave("~/covid-survey/presentations/figures/timeline1.png",last_plot(),width=11*.85,height=8*.85,units="in")

tl3 <- ggplot(bene) +
  geom_line(aes(x=Date,y=Benefit)) +
  labs(x="",y="Federal\nunemployment\nbenefits\n(weekly)") +
  # geom_density(data=field,aes(x=Date,y=..scaled..*270),color=NA,fill="blue",alpha=0.5) +
  annotate(x=ymd("2020-6-3"),y=400,label="Pandemic\nUnemployment\nCompensation",geom="text") +
  annotate(x=ymd("2021-2-4"),y=400,label="Consolidated\nAppropriations\nAct",geom="text") +
  
  # annotate(x=ymd("2020-11-1"),y=250,label="Survey\nresponses\nreceived",geom="text",color="blue",alpha=0.8) +
  #  geom_segment(x=ymd("2020-11-10"),xend=ymd("2021-1-19"),y=170,yend=100,color="blue",alpha=0.8) +
  
  annotate(x=ymd("2021-5-1"),y=345,label="Scheduled\nbenefits cliff\n(March 14)",geom="text",alpha=0.8) +
  geom_segment(x=ymd("2021-5-1"),xend=ymd("2021-3-14"),y=260,yend=200,alpha=0.8) +
  lims(x=c(ymd("2020-3-20"), ymd("2021-6-1"))) +
  scale_y_continuous(labels=scales::dollar_format()) +
  theme_minimal() +
  theme(axis.title.y = element_text(angle = 0,vjust=0.5),
        panel.background = element_rect(fill="#FAFAFA",color="#FAFAFA"),
        plot.background = element_rect(fill="#FAFAFA",color="#FAFAFA"))

tl4 <- ggplot(bene) +
  lims(x=c(ymd("2020-3-20"), ymd("2021-6-1")),y=c(-140,0)) +
  
   annotate(x=ymd("2020-12-1"),y=-50,label="Biden proposes\nAmerican Rescue Plan\n(Jan 14)",geom="text") +
    geom_segment(x=ymd("2020-12-26"),xend=ymd("2021-1-1"),y=-20,yend=0,alpha=0.8) +
  
   annotate(x=ymd("2021-4-1"),y=-110,label="ARP signed\ninto law\n(March 11)",geom="text") +
    geom_segment(x=ymd("2021-3-20"),xend=ymd("2021-3-15"),y=-75,yend=0,alpha=0.8) +
  
  labs(x="",y="Federal\nunemployment\nbenefits\n(weekly)",color="#FAFAFA") +
  theme_void() +
  theme(axis.title.y = element_text(angle = 0,vjust=0.5,color="#FAFAFA"),
        axis.ticks=element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill="#FAFAFA",color="#FAFAFA"),
        plot.background = element_rect(fill="#FAFAFA",color="#FAFAFA"))

tl3 / tl4 +plot_layout(heights=c(3,1.7))

ggsave("~/covid-survey/presentations/figures/timeline2.png",last_plot(),width=11*.85,height=8*.85,units="in")


tl5 <- ggplot(bene) +
  geom_line(aes(x=Date,y=Benefit)) +
  labs(x="",y="Federal\nunemployment\nbenefits\n(weekly)") +
   geom_density(data=field,aes(x=Date,y=..scaled..*270),color=NA,fill="blue",alpha=0.5) +
  annotate(x=ymd("2020-6-3"),y=400,label="Pandemic\nUnemployment\nCompensation",geom="text") +
  annotate(x=ymd("2021-2-4"),y=400,label="Consolidated\nAppropriations\nAct",geom="text") +
  
   annotate(x=ymd("2020-11-1"),y=250,label="Survey\nresponses\nreceived",geom="text",color="blue",alpha=0.8) +
    geom_segment(x=ymd("2020-11-10"),xend=ymd("2021-1-19"),y=170,yend=100,color="blue",alpha=0.8) +
  
  annotate(x=ymd("2021-5-1"),y=345,label="Scheduled\nbenefits cliff\n(March 14)",geom="text",alpha=0.8) +
  geom_segment(x=ymd("2021-5-1"),xend=ymd("2021-3-14"),y=260,yend=200,alpha=0.8) +
  lims(x=c(ymd("2020-3-20"), ymd("2021-6-1"))) +
  scale_y_continuous(labels=scales::dollar_format()) +
  theme_minimal() +
  theme(axis.title.y = element_text(angle = 0,vjust=0.5),
        panel.background = element_rect(fill="#FAFAFA",color="#FAFAFA"),
        plot.background = element_rect(fill="#FAFAFA",color="#FAFAFA"))

tl6 <- ggplot(bene) +
  lims(x=c(ymd("2020-3-20"), ymd("2021-6-1")),y=c(-140,0)) +
  
  annotate(x=ymd("2020-12-1"),y=-50,label="Biden proposes\nAmerican Rescue Plan\n(Jan 14)",geom="text") +
  geom_segment(x=ymd("2020-12-26"),xend=ymd("2021-1-1"),y=-20,yend=0,alpha=0.8) +
  
  annotate(x=ymd("2021-4-1"),y=-110,label="ARP signed\ninto law\n(March 11)",geom="text") +
  geom_segment(x=ymd("2021-3-20"),xend=ymd("2021-3-15"),y=-75,yend=0,alpha=0.8) +
  
  labs(x="",y="Federal\nunemployment\nbenefits\n(weekly)",color="#FAFAFA") +
  theme_void() +
  theme(axis.title.y = element_text(angle = 0,vjust=0.5,color="#FAFAFA"),
        axis.ticks=element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill="#FAFAFA",color="#FAFAFA"),
        plot.background = element_rect(fill="#FAFAFA",color="#FAFAFA"))

tl5 / tl6 +plot_layout(heights=c(3,1.7))

ggsave("~/covid-survey/presentations/figures/timeline3.png",last_plot(),width=11*.85,height=8*.85,units="in")

