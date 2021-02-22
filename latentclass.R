require(pacman)

p_load(rpart,rpart.plot,readr,dplyr,RCurl,rjson,lubridate,
       rvest,stringr,Hmisc,rattle,RColorBrewer,ddpcr,tidytext,tidyr,
       ggrepel,ggplot2,png,ggpubr,tidycensus,sf,plm,lmtest,stargazer,MASS,
       xtable,knitr,magick,purrr,ggthemes,gifski,extrafont,latex2exp,
       cowplot,mapproj,patchwork,remotes,tictoc,Hmisc,english,readstata13,gmnl)

install.packages("https://cran.r-project.org/src/contrib/Archive/mlogit/mlogit_1.0-2.tar.gz", repos=NULL,type="source")
library(mlogit)

dat <- read.csv("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_nointx.csv")
#dat <- read.dta13("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_nointx.dta")


dat2 <- mlogit.data(dat[which(dat$threealts==1),], chid.var = "choice", id.var="caseid", alt.var="alt", choice = "choice", varying = c(4:17,377:378,382,392:395,399), shape = "long", sep = "")
lcmodel <- gmnl(best ~ mabsdeaths + mabsnfcases + avcost + unempl + countrestr1 + countrestr2 + countrestr3 + statquo
                | 0 | 0 | 0 | black + female, data = dat2, model = "lc", 
                panel = T, Q = 3,method="bhhh",seed=12345)
summary(lcmodel)
cat("class 1 share is: ", exp(0) / (exp(0) + exp(coef(lcmodel)["(class)2"]) + exp(coef(lcmodel)["(class)3"])))
cat("class 2 share is: ", exp(coef(lcmodel)["(class)2"]) / (exp(0) + exp(coef(lcmodel)["(class)2"]) + exp(coef(lcmodel)["(class)3"])))
cat("class 3 share is: ",exp(coef(lcmodel)["(class)3"]) / (exp(0) + exp(coef(lcmodel)["(class)2"]) + exp(coef(lcmodel)["(class)3"])))

plot_ci_lc(lcmodel, var = c("avcost"))
