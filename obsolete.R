require(pacman)

p_load(tidyverse,tidycensus,ggthemes,ggplot2,Hmisc,english,censusr,mclogit,mlogit,broom,foreign)

#dat <- read.csv("~/covid-survey/data/VSL-COVID-php-obsolete_January 6, 2021_06.00_numeric-cleaned.csv")

vars <- load_variables(year=2018,dataset="acs5")



indiv_data <- get_acs(geography = "block group",
                      state=c("OR","WA","CA"),
                      variables=c(
                        #denominator
                        total_pop="B01001_001",
                        
                        #gender
                        male="B01001_002",
                        
                        #race
                        white="B02001_002",
                        black="B02001_003",
                        native="B02001_004",
                        asian="B02001_005",
                        latino= "B03002_012",
                        hawaiian_pi="B02001_006",
                        other_race="B02001_007",
                        two_more_races="B02001_008",
                        
                        #median income
                        med_income="B07011_001",
                        
                        #age
                        # age_u6 = "B05009_002",
                        # age_6_17 = "B05009_020",
                        # age_18_34 = "B21005_002",
                        # age_35_55 = "B21005_013",
                        # age_55_64 = "B21005_024",
                        # age_18_64 = "B21005_001",
                        age_o64 = "B27010_051",
                        
                        # #education
                        # less_than_HS = "B06009_002",
                        # HS = "B06009_003",
                        # some_college = "B06009_004",
                        # bachelors = "B06009_005",
                        # advanced_degree = "B06009_006",
                        # 
                        # # income
                        # income_0 = "B06010_002",
                        # income_1_10k = "B06010_004",
                        # income_10k_15k = "B06010_005",
                        # income_15k_25k = "B06010_006",
                        # income_25k_35k = "B06010_007",
                        # income_35k_50k = "B06010_008",
                        # income_50k_65k = "B06010_009",
                        # income_65k_75k = "B06010_010",
                        # income_over75k = "B06010_011"
                        
                        hh_income = "B19013_001"
                      ),
                      geometry=F,
                      output="wide")

indiv_data <- indiv_data %>% dplyr::select(which(str_detect(names(indiv_data),"E")))
names(indiv_data) <- str_remove_all(names(indiv_data),"E$")
names(indiv_data)[2] <- "NAME"
indiv_data$age_0_17 <- indiv_data$age_u6 + indiv_data$age_6_17

indiv_data$over18_pop <- indiv_data$total_pop - indiv_data$age_0_17

head(indiv_data)

for (i in 1:nrow(dat)) {

temp <- censusr::call_geolocator_latlon(lat=dat$LocationLatitude[i], lon=dat$LocationLongitude[i])
temp <- temp %>% str_remove("...$")

dat$GEOID[i] <- temp

}

dat <- left_join(dat,indiv_data,by="GEOID")

#write.csv(dat,"~/covid-survey/obsolete_Jan6_with_census_block_groups.csv")

dat2 <- dat %>% group_by(choice) %>% summarise(fedui2 = max(fedui))

dat <- left_join(dat,dat2)


res <- tidy(clogit(best ~ months + delcases + deldeaths + fedui +
                  rule1 + rule2 + rule3 + rule4 + rule5 + rule6 + rule7 + rule8 + rule9 + rule10 + 
                  unempl + avcost + strata(caseid),data=dat))

summary(clogit(best ~ deldeaths + delcases + rule1*larule1 + rule2*larule2 +rule3*larule3 + rule4*larule4 + 
                 rule5*larule5 + rule6*larule6 +rule7*larule7 + rule8*larule8 + rule9*larule9 + rule10*larule10 + 
                 unempl + avcost + statusquo + strata(choice),data=dat))

summary(clogit(best ~ deldeaths + delcases +
                 unempl*fedui2 + avcost*fedui2 + statusquo + strata(choice),data=dat))

res$estimate <- res$estimate/res$estimate[which(res$term=="avcost")]

summary(clogit(best ~ fedui + strata(choice),data=dat2))

small <- dat %>% dplyr::select(best,choice,alt,fedui)
dat2 <- dat %>% ungroup %>% as.data.frame

dat$statquo <- ifelse(dat$statusquo==T,1,0)
write.csv(dat,"~/covid-survey/for_stata.csv",na=".")



# fedui  months  delcases  deldeaths   rule1  rule2  rule3  rule4  rule5  rule6  rule7  rule8  rule9  rule10   unempl  avcost  statusquo


######## across all policies, could you please check what proportions of people say yes at each household cost, 
######## and what proportions say yes at each unemployment level.  Those two variables are not independent. 

dat <- read.csv("~/covid-survey/data/main_vars_intx.csv")

summary(clogit(best ~ delcases + deldeaths + 
                     avcost + statquo + strata(choice),data=dat))

small <- dat %>% dplyr::select(best,caseid,choice,alt,avcost,unempl,fedui)
small2 <- small %>% group_by(caseid) %>% summarise(cost = mean(avcost))
small2 <- small2 %>% dplyr::filter(!is.na(cost))

small <- small %>% dplyr::filter(caseid %in% small2$caseid)

small$costbin <- cut(small$avcost,7)
small$costbin <- ifelse(small$avcost==0,"0",small$costbin)
small$costbin <- ifelse(small$costbin=="(-1.05,151]","(0,151]",small$costbin)

small3 <- small %>% group_by(costbin) %>% summarise(prop_yes = mean(best))

small$unempbin <- cut(small$unempl,7)

small4 <- small %>% group_by(unempbin) %>% summarise(prop_yes=mean(best))

small5 <- small %>% group_by(unempbin,costbin) %>% summarise(prop_yes=mean(best),n=n())

ggplot(small5) +
  geom_tile(aes(x=unempbin,y=costbin,fill=prop_yes)) +
  scale_fill_viridis_c()
