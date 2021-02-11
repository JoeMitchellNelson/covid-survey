require(pacman)

p_load(tidycensus,tidyverse,censusr,sf,readstata13,survival)

#### read in data

dat <- read.dta13("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_intx.dta")
#dat <- read.dta13("C:/Users/Joe/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_intx.dta")

raw <- read.csv("~/covid-survey/data/qualtrics_raw.csv")
raw <- raw[3:nrow(raw),]

#### get everyone's county

counties <- raw %>% dplyr::select(ResponseId,Q7,Q9,Q169,Q171,Q174,Q176)
counties <- counties %>% mutate_if(is.factor,as.character)
counties$CAcounty <- ifelse(counties$Q9=="",counties$Q7,counties$Q9)
counties$ORcounty <- ifelse(counties$Q171=="",counties$Q169,counties$Q171)
counties$WAcounty <- ifelse(counties$Q176=="",counties$Q174,counties$Q176)
counties$county <- paste0(counties$CAcounty,counties$ORcounty,counties$WAcounty)
counties$county <- counties$county %>% str_remove("\\(.{1,100}\\)") %>% trimws()
counties <- counties %>% dplyr::select(ResponseId,county)

dat <- left_join(dat,counties)

small <- dat %>% dplyr::select(caseid,CA,WA,OR,northerly,easterly,ResponseId,zipcode,Durationinseconds) %>% unique()

census_tracts <- st_read("~/covid-survey/data/zipcodes/cb_2018_us_zcta510_500k.shp", quiet = TRUE)
states <- st_read("~/covid-survey/data/states/cb_2018_us_state_500k.shp", quiet = TRUE)

###### match lat/long to zip codes

latlong_sf <- small %>%
  filter(!is.na(northerly), !is.na(easterly)) %>%
  st_as_sf(coords = c("easterly", "northerly"), crs = st_crs(census_tracts))



intersected <- st_intersects(latlong_sf, census_tracts)
intersected2 <- st_intersects(latlong_sf,states)


latlong_final <- latlong_sf %>%
  mutate(intersection = as.integer(intersected),
         intersection2 = as.integer(intersected2),
         zip = if_else(is.na(intersection), "",
                        as.character(census_tracts$GEOID10[intersection])),
         state=if_else(is.na(intersection2), "",
                       as.character(states$NAME[intersection2])))

latlong_final$zip <- as.numeric(as.character(latlong_final$zip))

problems <- latlong_final %>% dplyr::filter(!state %in% c("Oregon","Washington","California")) %>% dplyr::select(!intersection,intersection2)

write.csv(problems,"~/covid-survey/problem_obs.csv")
# ggplot() +
#   geom_sf(data=states) +
#   geom_sf(data=latlong_final,alpha=.3,size=1,color="red") +
#   coord_sf(xlim = c(-130.15, -65.12), ylim = c(23, 50), expand = FALSE) +
#   labs(x="CA=475, WA=258, OR=149, Other US = 164, Int'l/Unknown = 11") +
#   theme_bw()

###### get census variables #######

# HEALTH INSURANCE COVERAGE STATUS
# hospitals in the county (Shan has this maybe)
# income measures?
# employment in various industries (NAICS), whatever I did for the prospectus
# 

vars <- load_variables(year=2018,dataset="acs5")

census_data <- get_acs(geography = "zcta",
                       year=2018,
                       cache_table=T,
                       variables=c(
                         ziptotal="B01003_001",
                         zipmedianhhinc = "B19013_001",
                         ziphhsize= "B08202_001",
                         
                         ### race ###
                         zipracewhite="C02003_003",
                         zipraceblack="C02003_004",
                         zipracenaai="C02003_005",
                         zipraceasian="C02003_006",
                         zipracepacisl="C02003_007",
                         zipraceother="C02003_008",
                         zipracemulti="C02003_009",
                         
                         ### age ###
                         zipageunder6 = "B17024_002",
                         zipage6to11 = "B17024_015",
                         zipage12to17 = "B17024_028",
                         zipage18to24="B17024_041",
                         zipage25to34="B17024_054",
                         zipage25to44="B17024_067",
                         zipage45to54="B17024_080",
                         zipage55to64 = "B17024_093",
                         zipage65to74="B17024_106",
                         zipageto75up = "B17024_119",
                         
                         ### insurance ###
                         healthinstotal = "C27021_001",
                         healthinsyes1 = "C27021_004",
                         healthinsyes2 = "C27021_008",
                         healthinsyes3 = "C27021_011",
                         healthinsyes4 = "C27021_014",
                         
                         healthinsno1 = "C27021_005",
                         healthinsno2 = "C27021_009",
                         healthinsno3 = "C27021_012",
                         healthinsno4 = "C27021_015"),
                       geometry=F,                
                       shift_geo=F,
                       output="wide")

census_data <- census_data[,which(str_detect(names(census_data),"E"))]
names(census_data)[3:length(names(census_data))] <- names(census_data)[3:length(names(census_data))] %>% str_remove("E")

census <- census_data %>% mutate(ziphealthinsyes = healthinsyes1 + healthinsyes2 + healthinsyes3 + healthinsyes4)
census <- census %>% mutate(ziphealthinsno = healthinsno1 + healthinsno2 + healthinsno3 + healthinsno4)
census <- census %>% dplyr::select(-healthinsyes1,-healthinsyes2,-healthinsyes3,-healthinsyes4,-healthinsno1,-healthinsno2,-healthinsno3,-healthinsno4)

census$GEOID <- as.numeric(census$GEOID)

census <- census %>% dplyr::filter(GEOID %in% latlong_final$zip | GEOID %in% latlong_final$zipcode)

#######################
##### VOTING 2016 #####
#######################

votes <- read.delim("https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/VOQCHQ/HEIJCQ")

votes <- votes %>% dplyr::filter(state_po %in% c("WA","OR","CA") & year == 2016)
votes$party <- if_else(is.na(votes$party),"other",as.character(votes$party))

votes <- votes %>% dplyr::select(-c("year","version","office","candidate"))

votes <- votes %>% pivot_wider(names_from=party,values_from=c(candidatevotes))

votes$FIPS <- ifelse(str_length(votes$FIPS)==4,paste0(0,votes$FIPS),votes$FIPS)

names(votes)[5:8] <- c("votestotal","votesdem","votesgop","votesother")

#############################################
############## JOBS COUNTY ##################
#############################################
jvarnames <- c(paste0("C24070_00",1:9), paste0("C24070_0",10:14))

job_vars <- vars %>% dplyr::filter(name %in% jvarnames) 

jobs <- get_acs(geography="zcta",
               # state=c("OR","CA","WA"),
                year=2018,
                variables = job_vars$name,
                geometry=F,
                shift_geo = F,
                output="wide")
jobs <- jobs %>% dplyr::select(which(str_detect(names(jobs),"E")))

names(jobs) <- c("GEOID","county",job_vars$label)
names(jobs) <- str_remove_all(names(jobs),"!!")
names(jobs) <- str_remove_all(names(jobs),"EstimateTotal")
names(jobs) <- str_replace_all(names(jobs)," ",".")
names(jobs) <- str_replace_all(names(jobs),",","")
names(jobs)[3] <- "Totalworkers"

jobs$GEOID <- as.numeric(jobs$GEOID)

jobs2 <- jobs %>% dplyr::filter(GEOID %in% latlong_final$zip | GEOID %in% latlong_final$zipcode)

write.csv(jobs2,"~/covid-survey/data/zipcodes_jobs.csv")
write.csv(census,"~/covid-survey/data/zipcodes_demo.csv")
write.csv(votes,"~/covid-survey/data/county_votes.csv")
