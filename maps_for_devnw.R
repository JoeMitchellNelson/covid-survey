require(pacman)
p_load(tidycensus,tidyverse,sf,ggmap,scales)

vars <- load_variables(year=2019,dataset="acs1")

vars2 <- load_variables(year=2010,dataset="sf1")

googleapi <- "AIzaSyCHCmR6RMCoHEm0oqznBgHTMRbUUW7zOmQ"

register_google(googleapi)

dat <- get_acs(geography = "tract",
               state="OR",
               county=c("Benton","Lincoln", "Lane","Linn","Marion","Polk"),
               variables=c(totalpop = "B01001_001",
                           speakspanish = "B06007_003",
                           englishwell = "B06007_004",
                           englishnotwell = "B06007_005",
                           totalHH = "B07013_001",
                           ownerHH = "B07013_002",
                           renterHH = "B07013_003"),
               year=2019,
               survey="acs5",
               geometry=T,
               output="wide")

dat$spanishprop <- dat$englishnotwellE/dat$totalpopE * 100
dat$renterprop <- dat$renterHHE/dat$totalHHE * 100


landmarks <- tribble(
  ~event, ~lat, ~lon,
  "Eugene", 44.0521, -123.0868,
  "Salem", 44.9429, -123.0351,
)

landmarks <- landmarks %>%
  st_as_sf(coords = c("lon", "lat"), crs = "NAD83")


ggplot(dat) +
  geom_sf(aes(fill=spanishprop),alpha=0.9,color=NA) +
  geom_sf(data=landmarks,shape=18,size=1,color="red") +
  scale_fill_viridis_c(option="B") +
  theme_minimal()



us <- c(left = -124.212944, bottom = 43.332245, right = -121.533914, top = 45.325035)
terrain <- get_stamenmap(us, zoom = 9, maptype = "terrain",color="bw", crs = 4326)
ggmap(terrain) +
  geom_sf(data=dat,inherit.aes = F,alpha=.5,mapping=aes(fill=renterprop)) +
  scale_fill_viridis_c(option="B") +
  geom_sf(data=landmarks,inherit.aes=F,shape=18,size=2,color="red")


dat2 <- get_decennial(geography = "block",
               state="OR",
               county=c("Lane"),
               variables=c(
                           totalHH = "H004001",
                           mortgageHH = "H004002",
                           ownedHH = "H004003",
                           rentHH= "H004004"),
               year=2010,
              # survey="acs5",
               geometry=T,
               output="wide")

dat2$renterprop <- dat2$rentHH/dat2$totalHH * 100


ggplot(dat2) +
  geom_sf(aes(fill=renterprop),color=NA,alpha=.8) +
  scale_fill_viridis_c(option="D")

dat2$area <- st_area(dat2)  
centers <- st_centroid(dat2)
centers$dist <- st_distance(centers,landmarks[1,])
centers <- centers[order(centers$dist),]
centers <- centers[1:1400,]

dat2 <- dat2 %>% dplyr::filter(GEOID %in% centers$GEOID)

dat2$density <- (dat2$totalHH/dat2$area * 10000) %>% as.numeric()

ggplot(dat2) +
  geom_sf(aes(fill=renterprop),color="white",alpha=.8) +
  scale_fill_viridis_c(option="D",na.value="white")+
  theme_minimal()

dat2 <- dat2 %>% dplyr::filter(!is.na(renterprop))

us <- c(left = -123.125, bottom = 44.023, right = -123.047, top = 44.0816)
terrain <- get_stamenmap(us, zoom = 15, maptype = "terrain",color="bw", crs = 4326)
ggmap(terrain) +
  geom_sf(data=dat2,inherit.aes = F,alpha=.5,mapping=aes(fill=renterprop),color=NA) +
  scale_fill_viridis_c(option="D",na.value="white") +
  labs(x="",y="",fill="% Renters")

ggsave("~/EUGENE_renters.png",last_plot(),width=8,height=7,units="in")
