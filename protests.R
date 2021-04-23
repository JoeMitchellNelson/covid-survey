require(pacman)
p_load(tidyverse,lubridate)

acled <- read.csv("~/covid-survey/data/coronavirus_Apr16.csv")
acled$date <- dmy(acled$EVENT_DATE)
us <- acled %>% dplyr::filter(ISO==840)
restr <- us %>% dplyr::filter((str_detect(NOTES,"restriction") | str_detect(NOTES,"re-opening the economy")) & !str_detect(NOTES,"loose"))
reds <- scan("~/covid-survey/data/red2020.txt",what="character",sep="\n")

econ <- restr <- us %>% dplyr::filter(str_detect(NOTES,"econom"))


byday <- restr %>% group_by(date) %>% summarise(n=n())
biggest <- restr %>% dplyr::filter(date==byday$date[which(byday$n==max(byday$n))])

bystate <- restr %>% group_by(ADMIN1,date) %>% summarise(n=n())
bystate$ideol <- ifelse(bystate$ADMIN1 %in% reds,"R","D")

ggplot(restr,aes(x=date)) +
 # geom_rect(aes(xmin=ymd("2020-04-05"),xmax=ymd("2020-07-31"),ymin=0,ymax=.025),alpha=.01,fill="green") +
  geom_bar(fill="blue",alpha=0.5) +
  theme_minimal()
summary(restr$ADMIN1)

restr$NOTES[which(restr$date < ymd("2020-11-23") & restr$date > ymd("2020-11-19"))]

ggplot(bystate) +
  geom_point(aes(y=n,x=date,group=ideol,color=ideol),size=1,alpha=.7) +
  scale_color_manual(values=c("blue","red"))

length(unique(restr$ADMIN1))

