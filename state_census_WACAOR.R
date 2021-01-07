require(pacman)

p_load(rpart,readr,dplyr,RCurl,rjson,lubridate,
       rvest,stringr,Hmisc,rattle,RColorBrewer,ddpcr,tidytext,tidyr,
       ggrepel,ggplot2,png,ggpubr,tidycensus,sf,plm,lmtest,stargazer,MASS,
       xtable,knitr,magick,purrr,ggthemes,gifski,extrafont,latex2exp,
       cowplot,mapproj,patchwork,remotes,tictoc,Hmisc,english)

font_import(paths="C:/Users/Joe/AppData/Local/Microsoft/Windows/Fonts")
loadfonts(device="win")
chosen_font <- "CMU Serif"

vars <- load_variables(year=2018,dataset="acs5")

##############################################
######### CENSUS COUNTIES ####################
##############################################

census_data <- get_acs(geography = "state",
                       state=c("OR","WA","CA"),
                       year=2018,
                       variables=c(
                         total="B01003_001",
                         median_hh_inc = "B19013_001",
                         hh_size= "B08202_001"), 
                       geometry=F,                
                       shift_geo=F,
                       output="wide")

census_data <- census_data[,c(1,2,3,5)]
names(census_data) <- c("fips","state","population2018","med_hh_inc2018")



indiv_data <- get_acs(geography = "state",
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
                        age_u6 = "B05009_002",
                        age_6_17 = "B05009_020",
                        age_18_34 = "B21005_002",
                        age_35_55 = "B21005_013",
                        age_55_64 = "B21005_024",
                        age_18_64 = "B21005_001",
                        age_o64 = "B27010_051",
                        
                        #education
                        less_than_HS = "B06009_002",
                        HS = "B06009_003",
                        some_college = "B06009_004",
                        bachelors = "B06009_005",
                        advanced_degree = "B06009_006",
                        
                        # income
                        income_0 = "B06010_002",
                        income_1_10k = "B06010_004",
                        income_10k_15k = "B06010_005",
                        income_15k_25k = "B06010_006",
                        income_25k_35k = "B06010_007",
                        income_35k_50k = "B06010_008",
                        income_50k_65k = "B06010_009",
                        income_65k_75k = "B06010_010",
                        income_over75k = "B06010_011"
                      ),
                      geometry=F,
                      output="wide")

indiv_data <- indiv_data %>% dplyr::select(which(str_detect(names(indiv_data),"E")))
names(indiv_data) <- str_remove_all(names(indiv_data),"E$")
names(indiv_data)[2] <- "NAME"
indiv_data$age_0_17 <- indiv_data$age_u6 + indiv_data$age_6_17

indiv_data$over18_pop <- indiv_data$total_pop - indiv_data$age_0_17

#### divide everything but income by total population ###

indiv2 <- indiv_data

indiv2[,which(!names(indiv2) %in% c("GEOID","NAME","total_pop","med_income"))] <- indiv_data[,which(!names(indiv_data) %in% c("GEOID","NAME","total_pop","med_income"))]/indiv_data$total_pop


indiv_data <- rbind(indiv_data,indiv2)

write.csv(indiv_data,"~/covid-survey/state_data_for_trudy.csv")

indiv_data <- get_acs(geography = "state",
                      state=c("OR","WA","CA"),
                      variables=c(
                        #denominator
                        total_pop="B01001_001",
                        
                        #gender
                        total_educ="B06009_001",
                        
                        
                        #education
                        less_than_HS = "B06009_002",
                        HS = "B06009_003",
                        some_college = "B06009_004",
                        bachelors = "B06009_005",
                        advanced_degree = "B06009_006",
                        age_18_64 = "B21005_001",
                        age_o64 = "B27010_051"

                      ),
                      geometry=F,
                      output="wide")

