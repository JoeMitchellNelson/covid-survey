require(pacman)

p_load(rpart,rpart.plot,readr,dplyr,RCurl,rjson,lubridate,
       rvest,stringr,Hmisc,rattle,RColorBrewer,ddpcr,tidytext,tidyr,
       ggrepel,ggplot2,png,ggpubr,tidycensus,sf,plm,lmtest,stargazer,MASS,
       xtable,knitr,magick,purrr,ggthemes,gifski,extrafont,latex2exp,
       cowplot,mapproj,patchwork,remotes,tictoc,Hmisc,english,readstata13,margins)


states <- get_acs(geography = "state",
                       year=2018,
                       cache_table=T,
                       variables=c(
                         ziptotal="B01003_001"),
                       geometry=T,
                       shift_geo=T)

votes <- read.csv("~/covid-survey/data/1976-2020-president.csv") %>% dplyr::filter(year==2020 & party_detailed %in% c("DEMOCRAT","REPUBLICAN")) 
votes <- votes %>% group_by(state) %>% dplyr::filter(candidatevotes==max(candidatevotes))
votes <- votes %>% dplyr::select(state,party_simplified)
votes$statelower <- tolower(votes$state)
votes$`2020 winner` <- votes$party_simplified %>% as.factor()

nomask <- c("Idaho", "Montana", "North Dakota", "South Dakota",
            "Wyoming","Arizona","Nebraska","Iowa","Kansas",
            "Missouri","Oklahoma","Arkansas","Mississippi","Texas",
            "Tennessee","Georgia","Florida","South Carolina","Alaska")

states$`Masks required` <- ifelse(states$NAME %in% nomask,"No","Yes") %>% as.factor()
states$statelower <- tolower(states$NAME)

states <- left_join(states,votes)
states$`2020 winner` %>% summary
states <- within(states, `2020 winner` <- relevel(`2020 winner`, ref = 1))
states$`Masks required` <- factor(states$`Masks required`,levels=c("Yes","No"))


head(states)
a <- ggplot(states) +
  geom_sf(aes(fill=`Masks required`),color="white") +
  scale_fill_manual(values=c("#0000FF","#FF0000")) +
  theme_map()

b <- ggplot(states) +
  geom_sf(aes(fill=`2020 winner`),color="white") +
  scale_fill_manual(values=c("#0000FF","#FF0000")) +
  theme_map()

c <- a / b
c
#ggsave("~/covid-survey/masks2020comparison.png",c)

####################################################################

dat <- read.csv("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_nointx.csv")

dat$ideol <- ifelse(dat$ideolsconserv==1,"Strongly conservative",
                    ifelse(dat$ideolsliberal==1,"Strongly liberal",
                           ifelse(dat$ideolmliberal==1,"Moderately liberal",
                                  ifelse(dat$ideolmconserv==1,"Moderately conservative",
                                         ifelse(dat$ideolmoderate==1,"Moderate","MISSING")))))

people <- dat %>% group_by(ResponseId) %>% summarise(
                                                 ideology=first(ideol),
                                                 idrule1 =first(idrule1),
                                                 idrule2 =first(idrule2),
                                                 idrule3 =first(idrule3),
                                                 idrule4 =first(idrule4),
                                                 idrule5 =first(idrule5),
                                                 idrule6 =first(idrule6),
                                                 idrule7 =first(idrule7),
                                                 idrule8 =first(idrule8),
                                                 idrule9 =first(idrule9),
                                                 idrule10 =first(idrule10)
                                                 )
############## idrules distributions plot ##############

for (i in 1:6) {

colorlist <- data.frame(ideol=unique(people$ideology),color=c("#CC0033","#3300CC","#FF0000","#0000FF","#990099","grey50"))

idplot1 <- ggplot(people[which(people$ideology==colorlist$ideol[i]),]) + geom_histogram(aes(x=idrule1),bins=4,fill=colorlist$color[i],color="black") +labs(x="Grocery/essential",y="") + theme_minimal()
idplot2 <- ggplot(people[which(people$ideology==colorlist$ideol[i]),]) + geom_histogram(aes(x=idrule2),bins=4,fill=colorlist$color[i],color="black") +labs(x="Non-essential retail",y="") + theme_minimal()
idplot3 <- ggplot(people[which(people$ideology==colorlist$ideol[i]),]) + geom_histogram(aes(x=idrule3),bins=4,fill=colorlist$color[i],color="black") +labs(x="Schools",y="") + theme_minimal()
idplot4 <- ggplot(people[which(people$ideology==colorlist$ideol[i]),]) + geom_histogram(aes(x=idrule10),bins=4,fill=colorlist$color[i],color="black") +labs(x="Colleges",y="") + theme_minimal()
idplot5 <- ggplot(people[which(people$ideology==colorlist$ideol[i]),]) + geom_histogram(aes(x=idrule4),bins=4,fill=colorlist$color[i],color="black") +labs(x="Parks",y="") + theme_minimal()
idplot6 <- ggplot(people[which(people$ideology==colorlist$ideol[i]),]) + geom_histogram(aes(x=idrule5),bins=4,fill=colorlist$color[i],color="black") +labs(x="Gyms",y="") + theme_minimal()
idplot7 <- ggplot(people[which(people$ideology==colorlist$ideol[i]),]) + geom_histogram(aes(x=idrule6),bins=4,fill=colorlist$color[i],color="black") +labs(x="Theaters",y="") + theme_minimal()
idplot8 <- ggplot(people[which(people$ideology==colorlist$ideol[i]),]) + geom_histogram(aes(x=idrule7),bins=4,fill=colorlist$color[i],color="black") +labs(x="Restaurants",y="") + theme_minimal()
idplot9 <- ggplot(people[which(people$ideology==colorlist$ideol[i]),]) + geom_histogram(aes(x=idrule8),bins=4,fill=colorlist$color[i],color="black") +labs(x="Religious/Social",y="") + theme_minimal()
idplot10 <- ggplot(people[which(people$ideology==colorlist$ideol[i]),]) + geom_histogram(aes(x=idrule9),bins=4,fill=colorlist$color[i],color="black") +labs(x="Institutions",y="") + theme_minimal()

(idplot1 + idplot2) / (idplot3 + idplot4) / (idplot5 + idplot6) / (idplot7 + idplot8) / (idplot9 + idplot10) + plot_annotation(
  title = as.character(colorlist$ideol[i]))

ggsave(paste0("~/covid-survey/idealrules",as.character(colorlist$ideol[i]),".png"),last_plot())

}

############# counties #######################

nyt <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

nyt$date <- ymd(nyt$date)

nyt <- nyt %>% group_by(fips) %>% summarise(firstcase=min(date))
nyt$fips <- ifelse(str_length(nyt$fips)==4,paste0("0",nyt$fips),as.character(nyt$fips))

counties <- get_acs(geography = "county",
                  year=2018,
                  cache_table=T,
                  variables=c(
                    ziptotal="B01003_001"),
                  geometry=T,
                  shift_geo=T)

counties <- left_join(counties,nyt,by=c("GEOID"="fips"))

counties$`Any Covid-19 cases?` <- ifelse(counties$NAME=="Los Angeles County, California", "No","Yes")

counties$`Any Covid-19 cases?` <- as.factor(counties$`Any Covid-19 cases?`)

ggplot(counties) +
  geom_sf(aes(fill=(18691-as.numeric(firstcase))),color=NA,size=.001,show.legend=F) +
  scale_fill_viridis_c(direction=1) +
  theme_map()

ggsave("~/covid-survey/presentations/figures/whencovidcame.png",last_plot())


#ggsave("~/covid-survey/presentations/figures/countieswithcovid.png",last_plot())
