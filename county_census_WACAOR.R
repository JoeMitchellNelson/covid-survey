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
vars2 <- load_variables(year=2010,dataset="sf1")

##############################################
######### CENSUS COUNTIES ####################
##############################################

census_data <- get_acs(geography = "county",
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
names(census_data) <- c("fips","county","population2018","med_hh_inc2018")

#write.csv(census_data,"~/WA_census_vars_for_trudy.csv")

hh_vars <- vars %>% dplyr::filter(str_detect(name,"B08202"))

hh_data <- get_acs(geography = "county",
                   state=c("OR","WA","CA"),
                   year=2018,
                   variables=hh_vars$name, 
                   geometry=F,                      
                   shift_geo=F,
                   output="wide")

hh_data <- hh_data %>% dplyr::select(which(str_detect(names(hh_data),"E")))
names(hh_data) <- c("fips","county",hh_vars$label)
names(hh_data) <- str_remove_all(names(hh_data),"!!")
names(hh_data) <- str_remove_all(names(hh_data),"EstimateTotal")
names(hh_data) <- str_remove_all(names(hh_data)," ")
names(hh_data)[3] <- "TotalHH"
#write.csv(hh_data,"~/WA_census_vars_for_trudy2.csv")




indiv_data <- get_acs(geography = "county",
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
                        age_o64 = "B27010_051"
                        
                        ),
                      geometry=F,
                      output="wide")

indiv_data <- indiv_data %>% dplyr::select(which(str_detect(names(indiv_data),"E")))
names(indiv_data) <- str_remove_all(names(indiv_data),"E$")
names(indiv_data)[2] <- "NAME"
indiv_data$age_0_17 <- indiv_data$age_u6 + indiv_data$age_6_17

#### divide everything but income by total population ###

indiv_data[,which(!names(indiv_data) %in% c("GEOID","NAME","total_pop","med_income"))] <- indiv_data[,which(!names(indiv_data) %in% c("GEOID","NAME","total_pop","med_income"))]/indiv_data$total_pop

###### make binned plots #####################

neg_to_zero <- function (x)
{
  ifelse(x<0,0,x)
}

indiv_bins <- indiv_data %>% as.data.frame()

numbins <- 5

for (i in 4:21) {
  indiv_bins$NEWCOL <- cut(indiv_bins[,i],numbins)
  indiv_bins$NEWCOL <- str_remove_all(indiv_bins$NEWCOL,"\\(|\\)|\\[|\\]")
  indiv_bins$NEWCOL2 <- NA
  indiv_bins[,which(names(indiv_bins) %in% c("NEWCOL","NEWCOL2"))] <- str_split_fixed(indiv_bins$NEWCOL,",",2) %>% as.numeric() %>% round(3) %>% neg_to_zero()
  indiv_bins$NEWCOL <- paste0(indiv_bins$NEWCOL,"\nto\n",indiv_bins$NEWCOL2)
  names(indiv_bins)[which(names(indiv_bins)=="NEWCOL")] <- paste0(names(indiv_bins[i]),"_bins")
  indiv_bins <- indiv_bins %>% dplyr::select(-NEWCOL2)
  
}



##### race plots #######

black <- ggplot(indiv_bins) +
  geom_bar(aes(x=black_bins),fill="forestgreen",alpha=.5) +
  geom_hline(yintercept=0) +
  labs(y="Number of counties",x="",title="Black") +
  lims(y=c(0,110)) +
  theme_minimal() +
  theme(axis.line.y = element_line()) +
  theme(text=element_text(family=chosen_font))

white <- ggplot(indiv_bins) +
  geom_bar(aes(x=white_bins),fill="forestgreen",alpha=.5) +
  geom_hline(yintercept=0) +
  labs(y="",x="",title="White") +
  lims(y=c(0,110)) +
  theme_minimal() +
  theme(axis.line.y = element_line()) +
  theme(text=element_text(family=chosen_font))

latino <- ggplot(indiv_bins) +
  geom_bar(aes(x=latino_bins),fill="forestgreen",alpha=.5) +
  geom_hline(yintercept=0) +
  labs(y="",x="",title="Latino") +
  lims(y=c(0,110)) +
  theme_minimal() +
  theme(axis.line.y = element_line()) +
  theme(text=element_text(family=chosen_font))

black + latino + white

#### age plots #####

minors <- ggplot(indiv_bins) +
  geom_bar(aes(x=age_0_17_bins),fill="orange",alpha=.5) +
  geom_hline(yintercept=0) +
  labs(y="Number of counties",x="",title="Age under 18") +
  lims(y=c(0,68)) +
  theme_minimal() +
  theme(axis.line.y = element_line()) +
  theme(text=element_text(family=chosen_font))

working <- ggplot(indiv_bins) +
  geom_bar(aes(x=age_18_64_bins),fill="orange",alpha=.5) +
  geom_hline(yintercept=0) +
  labs(y="",x="\nBinned proportion",title="Age 18 to 64") +
  lims(y=c(0,68)) +
  theme_minimal() +
  theme(axis.line.y = element_line()) +
  theme(text=element_text(family=chosen_font))

old <- ggplot(indiv_bins) +
  geom_bar(aes(x=age_o64_bins),fill="orange",alpha=.5) +
  geom_hline(yintercept=0) +
  labs(y="",x="",title="Age 65 and older") +
  lims(y=c(0,68)) +
  theme_minimal() +
  theme(axis.line.y = element_line()) +
  theme(text=element_text(family=chosen_font))

(black + latino + white) / (minors + working + old)

ggsave("~/covid-survey/demo_plots.png",last_plot(),width=8,height=6,units = "in")


##### median income plot ##############

medinc <- ggplot(indiv_bins) +
  geom_bar(aes(x=med_income_bins),fill="#5927b8",alpha=.5) +
  geom_hline(yintercept=0) +
  labs(y="",x="",title="Median income") +
  lims(y=c(0,72)) +
  theme_minimal() +
  theme(axis.line.y = element_line()) +
  theme(text=element_text(family=chosen_font))

medinc

##############################################
############## VOTES COUNTY ##################
##############################################

# pull county voting records directly from MIT election lab

votes <- read.delim("https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/VOQCHQ/HEIJCQ")

votes <- votes %>% dplyr::filter(state_po %in% c("WA","OR","CA") & year == 2016)
votes$party <- if_else(is.na(votes$party),"other",as.character(votes$party))

votes <- votes %>% dplyr::select(-c("year","version","office","candidate"))

votes <- votes %>% pivot_wider(names_from=party,values_from=c(candidatevotes))

votes$FIPS <- ifelse(str_length(votes$FIPS)==4,paste0(0,votes$FIPS),votes$FIPS)

partial <- indiv_bins %>% dplyr::select(GEOID,total_pop,age_u6,age_6_17)

votes <- left_join(votes,partial,by=c("FIPS"="GEOID"))
rm(partial)

votes$voter_prop <- votes$totalvotes / (votes$total_pop - votes$total_pop*votes$age_u6 - votes$total_pop*votes$age_6_17)
votes$dem_prop <- votes$democrat / votes$totalvotes
votes$rep_prop <- votes$republican / votes$totalvotes
votes$other_prop <- votes$other/votes$totalvotes

votes$gop_bins <- cut(votes$republican / votes$totalvotes,5) %>% str_remove_all("\\(|\\)|\\[|\\]") %>% str_replace(",","\nto\n")
votes$dem_bins <- cut(votes$democrat / votes$totalvotes,5) %>% str_remove_all("\\(|\\)|\\[|\\]") %>% str_replace(",","\nto\n")
votes$voter_bins <- cut(votes$voter_prop,5) %>% str_remove_all("\\(|\\)|\\[|\\]") %>% str_replace(",","\nto\n")

vote_all <- ggplot(votes) +
  geom_bar(aes(x=voter_bins),fill="purple",alpha=.5) +
  geom_hline(yintercept=0) +
  labs(y="Number of counties",x="",title="Voters") +
  lims(y=c(0,53)) +
  theme_minimal() +
  theme(axis.line.y = element_line()) +
  theme(text=element_text(family=chosen_font))

vote_blue <- ggplot(votes) +
  geom_bar(aes(x=dem_bins),fill="blue",alpha=.5) +
  geom_hline(yintercept=0) +
  labs(y="",x="",title="Clinton") +
  lims(y=c(0,53)) +
  theme_minimal() +
  theme(axis.line.y = element_line()) +
  theme(text=element_text(family=chosen_font))

vote_red <- ggplot(votes) +
  geom_bar(aes(x=gop_bins),fill="red",alpha=.5) +
  geom_hline(yintercept=0) +
  labs(y="",x="\nBinned proportion",title="Trump") +
  lims(y=c(0,53)) +
  theme_minimal() +
  theme(axis.line.y = element_line()) +
  theme(text=element_text(family=chosen_font))

vote_all + vote_red + vote_blue

ggsave("~/covid-survey/politics_bars.png",last_plot(),width=8,height=3,units="in")


#############################################
############## JOBS COUNTY ##################
#############################################
jvarnames <- c(paste0("C24070_00",1:9), paste0("C24070_0",10:14))

job_vars <- vars %>% dplyr::filter(name %in% jvarnames) 

jobs <- get_acs(geography="county",
                state=c("OR","CA","WA"),
                year=2018,
                variables = job_vars$name,
                geometry=F,
                shift_geo = F,
                output="wide")
jobs <- jobs %>% dplyr::select(which(str_detect(names(jobs),"E")))

names(jobs) <- c("fips","county",job_vars$label)
names(jobs) <- str_remove_all(names(jobs),"!!")
names(jobs) <- str_remove_all(names(jobs),"EstimateTotal")
names(jobs) <- str_replace_all(names(jobs)," ",".")
names(jobs) <- str_replace_all(names(jobs),",","")
names(jobs)[3] <- "Totalworkers"

############# divide everything by total workers ###################
jobs[,which(!names(jobs) %in% c("fips","county","Totalworkers"))] <- jobs[,which(!names(jobs) %in% c("fips","county","Totalworkers"))]/jobs$Totalworkers


########### make bins ###################

neg_to_zero <- function (x)
{
  ifelse(x<0,0,x)
}

jobs_bins <- jobs %>% as.data.frame()

numbins <- 5

for (i in 4:16) {
  jobs_bins$NEWCOL <- cut(jobs_bins[,i],numbins)
  jobs_bins$NEWCOL <- str_remove_all(jobs_bins$NEWCOL,"\\(|\\)|\\[|\\]")
  jobs_bins$NEWCOL2 <- NA
  jobs_bins[,which(names(jobs_bins) %in% c("NEWCOL","NEWCOL2"))] <- str_split_fixed(jobs_bins$NEWCOL,",",2) %>% as.numeric() %>% round(3) %>% neg_to_zero()
  jobs_bins$NEWCOL <- paste0(jobs_bins$NEWCOL,"\nto\n",jobs_bins$NEWCOL2)
  names(jobs_bins)[which(names(jobs_bins)=="NEWCOL")] <- paste0("bins_",names(jobs_bins[i]))
  jobs_bins <- jobs_bins %>% dplyr::select(-NEWCOL2)
  
}

for (i in 17:29) {
  
  titl <- job_vars$label[i-15] %>% str_remove("Estimate!!Total!!")
  titl <- ifelse(str_length(titl)>22,paste0(str_extract(titl,".{22}"),"..."),titl)
  
  x <- names(jobs_bins)[i]
 a <- ggplot(jobs_bins, aes(x = !!ensym(x))) +
    geom_bar(alpha = 0.5) +
    labs(x="",y="",title=titl) +
    geom_hline(yintercept=0) +
    theme_minimal() +
    theme(axis.line.y = element_line()) +
    theme(text=element_text(family=chosen_font))
 
 assign(paste0("jobs_plot",i-16),a)
  
}

(jobs_plot1 + jobs_plot2 + jobs_plot3) /
  (jobs_plot4 + jobs_plot5 + jobs_plot6) /
  (jobs_plot7+jobs_plot8+jobs_plot9) /
  (jobs_plot10 + jobs_plot11 + jobs_plot12) /
  (plot_spacer() + jobs_plot13 + plot_spacer()) + plot_layout()

ggsave("~/covid-survey/jobs_plots.png",last_plot(),width=8,height=11,units="in")

###############################################
############### ECONOMIC ######################
###############################################

econvars <- vars %>% dplyr::filter(name %in% c("B28007_001","B28007_002","B28007_003","B28007_009","B28007_015","B07011_001"))

econ <- get_acs(geography="county",
                state=c("OR","CA","WA"),
                year=2018,
                variables = econvars$name,
                geometry=F,
                shift_geo = F,
                output="wide")
econ <- econ %>% dplyr::select(which(str_detect(names(econ),"E")))
names(econ)[3:8] <- econvars$label %>% str_remove_all("!!|Estimate")

econ[,c(5:8)] <- econ[,c(5:8)]/econ$Total


###############################################
################# TABLES ######################
###############################################


######### jobs ######################

jobs_table <- summary(jobs$Agriculture.forestry.fishing.and.hunting.and.mining) %>% names() %>% str_flatten(" & ")
empty_line <- "\n & & & & & & \\\\"
jobs_table <- paste0(" & ",jobs_table,"\\\\ \\hline\\hline\\[.3ex]")

i <- 4

for (i in 4:16) {
  rowi <- summary(jobs[,i][[1]]) %>% 
    round(3) %>% 
    as.character %>% 
    str_flatten(" & ")
  
  titl <- job_vars$label[i-2] %>% str_remove("Estimate!!Total!!")
  titl <- ifelse(str_length(titl)>30,paste0(str_extract(titl,".{22}"),"..."),titl)
  rowi <- paste0(rowi," \\\\")
  rowi <- paste0("\n",titl," & ",rowi,empty_line)
  jobs_table <- paste0(jobs_table,rowi)
}
cat(jobs_table)

###### demographics #################

demo_table <-  summary(jobs$Agriculture.forestry.fishing.and.hunting.and.mining) %>% names() %>% str_flatten(" & ")
empty_line <- "\n & & & & & & \\\\"
demo_table <- paste0(" & ",demo_table,"\\\\ \\hline\\hline\\[.3ex]")

for (i in 4:21) {
  rowi <- summary(indiv_data[,i][[1]]) %>% 
    round(3) %>% 
    as.character %>% 
    str_flatten(" & ")
  
  titl <- names(indiv_data)[i]
  rowi <- paste0(rowi," \\\\")
  rowi <- paste0("\n",titl," & ",rowi,empty_line)
  demo_table <- paste0(demo_table,rowi)
}
cat(demo_table)

######## votes #################

votes_table <-  summary(c(0)) %>% names() %>% str_flatten(" & ")
empty_line <- "\n & & & & & & \\\\"
votes_table <- paste0(" & ",votes_table,"\\\\ \\hline\\hline\\\\[.3ex]")

for (i in 12:15) {
  rowi <- summary(votes[,i][[1]]) %>% 
    round(3) %>% 
    as.character %>% 
    str_flatten(" & ")
  
  titl <- names(votes)[i]
  rowi <- paste0(rowi," \\\\")
  rowi <- paste0("\n",titl," & ",rowi,empty_line,empty_line)
  votes_table <- paste0(votes_table,rowi)
}
cat(votes_table)

############### econ ###########################

econ_table <-  summary(c(0)) %>% names() %>% str_flatten(" & ")
empty_line <- "\n & & & & & & \\\\"
econ_table <- paste0(" & ",econ_table,"\\\\ \\hline\\hline\\\\[.3ex]")

for (i in c(3,6:8)) {
  rowi <- summary(econ[,i][[1]]) %>% 
    round(3) %>% 
    as.character %>% 
    str_flatten(" & ")
  
  titl <- names(econ)[i]
  rowi <- paste0(rowi," \\\\")
  rowi <- paste0("\n",titl," & ",rowi,empty_line,empty_line)
  econ_table <- paste0(econ_table,rowi)
}
cat(econ_table)

####################################################


vars2 <- load_variables(year=2010,dataset="sf1")


census_data <- get_decennial(geography = "county",
                       state=c("OR","WA","CA"),
                       year=2010,
                       variables=c(
                         total="H002001",
                         urban="H002002",
                         rural="H002005"), 
                       geometry=F,                
                       shift_geo=F,
                       output="wide")

write.csv(census_data,"~/covid-survey/urbanicity_county.csv")

census_data <- get_decennial(geography = "zcta",
                             year=2010,
                             variables=c(
                               total="H002001",
                               urban="H002002",
                               rural="H002005"), 
                             geometry=F,                
                             shift_geo=F,
                             output="wide")

census_data <- census_data %>% dplyr::filter(GEOID > 90000)

write.csv(census_data,"~/covid-survey/urbanicity_zcta.csv")
