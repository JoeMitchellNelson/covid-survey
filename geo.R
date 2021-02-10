require(pacman)

p_load(tidycensus,tidyverse,censusr,sf,readstata13)


#dat <- read.dta13("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_intx.dta")
dat <- read.dta13("C:/Users/Joe/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_intx.dta")
small <- dat %>% dplyr::select(caseid,CA,WA,OR,northerly,easterly) %>% unique()

census_tracts <- st_read("~/covid-survey/data/zipcodes/cb_2018_us_zcta510_500k.shp", quiet = TRUE)
states <- st_read("~/covid-survey/data/states/cb_2018_us_state_500k.shp", quiet = TRUE)


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



ggplot() +
  geom_sf(data=states) +
  geom_sf(data=latlong_final,alpha=.3) +
  coord_sf(xlim = c(-130.15, -65.12), ylim = c(23, 50), expand = FALSE) +
  theme_bw()
