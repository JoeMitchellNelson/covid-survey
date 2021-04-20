require(pacman)

p_load(tidyverse,sf)

# read in shape files for states and counties
# downloaded: https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html

zips <- st_read("~/covid-survey/data/zipcodes/cb_2018_us_zcta510_500k.shp", quiet = TRUE)
counties <- st_read("~/covid-survey/data/counties/cb_2018_us_county_500k.shp", quiet = TRUE)

states <- c("41","06","53") # fips for OR, CA, WA resp.

counties <- counties %>% dplyr::filter(STATEFP %in% states) # only keep counties in OR, CA, WA

# This line is doing 99% of the work in this file
# It'll throw a warning that it's not accounting for the curvature of the earth
a <- st_intersection(zips,counties)

# calculate the area in m^2 of every overlap
a$area <- st_area(a) %>% as.numeric()

# drop non-overlaps
a <- a %>% dplyr::filter(area > 0)

# calculate zip code areas for denominator
zips$area_total <- st_area(zips) %>% as.numeric()

# make a smaller data frame for the sake of neatness only
zipareas <- zips %>% dplyr::select(ZCTA5CE10,area_total) %>% as.data.frame()

# merge zip areas back into main dataframe
a <- left_join(a,zipareas,by="ZCTA5CE10")

# calculate proportions
a$prop_in_county <- a$area / a$area_total

# quick sanity check
test <- 97448 # this zip code is mostly but not entirely in Lane County according to a map I found on google

# looks good
a[which(a$ZCTA5CE10==test),c(1,11,19)] %>% as.matrix

# clean up
b <- a %>% dplyr::select(ZCTA5CE10,NAME,STATEFP,COUNTYFP,prop_in_county) %>% st_drop_geometry()
names(b) <- c("zipcode","countyname","statefips","countyfips","prop_in_county")

# what if we want to assign every zip code to exactly one county?
c <- b %>% group_by(zipcode) %>% dplyr::filter(prop_in_county==max(prop_in_county)) %>% ungroup()

write.csv(b,"~/covid-survey/data/zip-proportions-in-counties.csv")
write.csv(c,"~/covid-survey/data/zip-county-most-overlap.csv")
