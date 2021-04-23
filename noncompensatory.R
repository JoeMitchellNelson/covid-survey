require(pacman)

p_load(rpart,rpart.plot,readr,dplyr,RCurl,rjson,lubridate,data.table,
       rvest,stringr,Hmisc,rattle,RColorBrewer,ddpcr,tidytext,tidyr,
       ggrepel,ggplot2,png,ggpubr,tidycensus,sf,plm,lmtest,stargazer,MASS,
       xtable,knitr,magick,purrr,ggthemes,gifski,extrafont,latex2exp,
       cowplot,mapproj,patchwork,remotes,tictoc,Hmisc,english,readstata13,plotly,randomForest,class)

#dat <- read.dta13("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_intx.dta")
dat <- read.csv("C:/Users/Joe/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_nointx.csv") %>% dplyr::filter(rejectonly==0)

dat$firstcasedate <- ymd(dat$firstcasedate) - ymd("2021-03-02")

dat$ideol <- ifelse(dat$ideolsconserv==1,"strong conservative",
                    ifelse(dat$ideolsliberal==1,"strong liberal",
                           ifelse(dat$ideolmconserv==1,"moderately conservative",
                                  ifelse(dat$ideolmliberal==1,"moderately liberal",
                                         ifelse(dat$ideolmoderate==1,"moderate","missing")))))
summary(dat$ideol %>% as.factor())
dat$ideol <- as.factor(dat$ideol)
dat <- within(dat, ideol <- relevel(ideol, ref = 2))

kinku <- function (a,x) {
  ifelse(x<a,0,abs(a-x))
}

kinkl <- function (a,x) {
  ifelse(x>a,0,abs(a-x))
}

u1 = 85
l1 = 25
u2 = 1800
l2 = 300
u3 = 850
l3 = 300
check <- dat
noncompens <- function (p,ll=T) {
  
  
  u3 <- p[1]
  l3 <- p[2]
  
  a <- clogit(best ~  
                
              #  kinku(u1,mabsdeaths) + 
                #   kinkl(l1,mabsdeaths) +
                mabsdeaths + 
               # kinku(u2,mabscases) +
                # kinkl(l2,mabscases) +
                mabscases +
                
              #  kinku(u3,avcost)*I(chhldinc-55) +
                kinkl(l3,avcost) +
                avcost +
                
                
                I(rule1) + I(rule2) +
                I(rule3) + I(rule4) +
                I(rule5) + I(rule6) +
                I(rule7) + I(rule8) +
                I(rule9) + I(rule10) +
                
                
                
                statquo  +
                
                
                strata(choice),data=check
              ,weights=popwt
              ,method="approximate"
  )
  
  if (u3 <= l3) { 
    100000000
  } else {
  
    if (ll) {
  
  a$loglik[2]*-1
    } else {
      a
    }
  }
  
}


p <- c(787.6734,240.0106)
optim(p,noncompens,F)
res2 <- broom::tidy(noncompens(p,ll=F))
res2



choicenums <- unique(dat$choice)


n <- 100
results <- data.frame(u3 = rep(NA,n),l3=NA)

for (i in 8:n) {
  
  cuts <- sample.int(n = length(choicenums),size = length(choicenums),replace=T)
  cuts2 <- cuts %>% as.data.frame()
  names(cuts2) <- "v1"
  cuts2 <- cuts2 %>% group_by(v1) %>% summarise(n=n())
  
  check <- NULL
  for (j in 1:max(cuts2$n)) {
    temp2 <- dat %>% dplyr::filter(choice %in% cuts2$v1[which(cuts2$n >= j)])
    check <- rbind(check,temp2)
  }
  
  res <- optim(c(800,200),noncompens,ll=T)
  p <- res$par
  
  results[i,] <- p
  
  cat(p,"\n")
}



mean(results$u3,na.rm=T)
sd(results$u3,na.rm=T)
mean(results$l3,na.rm=T)
sd(results$l3,na.rm=T)

### regress county income on cases deaths unemployment over last year

counties <- dat[,which(str_detect(names(dat),"y2020|unempr|pcurban|chhldinc|firstcasedate|countypop|countyname"))] %>% unique()

unemps <- counties[,which(str_detect(names(counties),"^unempr"))] %>% as.matrix()

cavcost <- unemps %*% unempcoefs + res$estimate[which(res$term=="avcost:I(chhldinc - 55)")]*(counties$chhldinc - 55) + res$estimate[which(res$term=="avcost")]

mean(cavcost * counties$countypop)
mean(cavcost)
sd(cavcost)

counties$firstcasedate <- ymd(counties$firstcasedate) 

summary(lm(avcost ~ 
             
             unempr201912 + unempr20201 +  unempr20202 +  unempr20203  + unempr20204  + unempr20205 +  unempr20206  + unempr20207  +
           unempr20208  + unempr20209  + unempr202010 + unempr202011+  unempr202012
            #  
            #   y2020m2deaths  +  y2020m3deaths  +  y2020m4deaths  +  y2020m5deaths  +  y2020m6deaths +   y2020m7deaths  +  y2020m8deaths  +
            # y2020m9deaths  +  y2020m10deaths  + y2020m11deaths  + y2020m12deaths
             
            firstcasedate + pcurban 
           
           ,data=dat))



names(dat)[which(str_detect(names(dat),"y2020"))]

check$avcostbins <- cut(check$avcost,10)
check <- check %>% group_by(choice) %>% mutate(costmax=max(avcost)) %>% ungroup() %>% dplyr::filter(costmax<1000)


a <- clogit(best ~  
              
              #  kinku(u1,mabsdeaths) + 
              #   kinkl(l1,mabsdeaths) +
              mabsdeaths + 
              # kinku(u2,mabscases) +
              # kinkl(l2,mabscases) +
              mabscases +
              
              #  kinku(u3,avcost)*I(chhldinc-55) +

              avcost*I(chhldinc-55) +
              unempl*I(chhldinc-55) +
              
              
              I(rule1) + I(rule2) +
              I(rule3) + I(rule4) +
              I(rule5) + I(rule6) +
              I(rule7) + I(rule8) +
              I(rule9) + I(rule10) +
              
              
              
              statquo  +
              
              
              strata(choice),data=check
            ,weights=popwt
            ,method="approximate"
)
summary(a)
