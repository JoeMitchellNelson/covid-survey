require(pacman)

p_load(rpart,rpart.plot,readr,dplyr,RCurl,rjson,lubridate,
       rvest,stringr,Hmisc,rattle,RColorBrewer,ddpcr,tidytext,tidyr,
       ggrepel,ggplot2,png,ggpubr,tidycensus,sf,plm,lmtest,stargazer,MASS,
       xtable,knitr,magick,purrr,ggthemes,gifski,extrafont,latex2exp,
       cowplot,mapproj,patchwork,remotes,tictoc,Hmisc,english,readstata13,gmnl)

install.packages("https://cran.r-project.org/src/contrib/Archive/mlogit/mlogit_1.0-2.tar.gz", repos=NULL,type="source")
library(mlogit)

tcodes <- read.csv("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/number_non-varying.csv")

#dat3 <- read.csv("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/forR_VSL-COVID_two-alt_long.csv")
dat <- read.dta13("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/forR_VSL-COVID_two-alt_long.dta")


dat2 <- mlogit.data(dat, chid.var = "choice", id.var="caseid", alt.var="alt", choice = "best", varying = tcodes[,3]:tcodes[,4], shape = "long", sep = "")

lcmodel <- gmnl(best ~ 
                  mabsdeaths + mabsnfcases + avcost + unempl + 
                  rule1 + rule2 + rule3 + rule4 + rule5 +
                  rule6 + rule7 + rule8 + rule9 + rule10 +
                  statquo
                
                  # avcost:pvotedem + avcost:pvoteother
                
                | 0 | 0 | 0 | female, data = dat2, model = "lc", 
                panel = T, Q = 5,method="bhhh",seed=1245)
summary(lcmodel)


plot_ci_lc(lcmodel, var = c("avcost"))
s <- shares(lcmodel)

s %*% lcmodel$coefficients[which(str_detect(names(lcmodel$coefficients),"avcost"))]

WTPs <- -lcmodel$coefficients[which(str_detect(names(lcmodel$coefficients),"mabsdeaths"))]/lcmodel$coefficients[which(str_detect(names(lcmodel$coefficients),"avcost"))]
WTPs %*% s
WTPs
