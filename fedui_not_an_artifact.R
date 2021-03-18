require(pacman)

p_load(rpart,rpart.plot,readr,dplyr,RCurl,rjson,lubridate,
       rvest,stringr,Hmisc,rattle,RColorBrewer,ddpcr,tidytext,tidyr,
       ggrepel,ggplot2,png,ggpubr,tidycensus,sf,plm,lmtest,stargazer,MASS,
       xtable,knitr,magick,purrr,ggthemes,gifski,extrafont,latex2exp,
       cowplot,mapproj,patchwork,remotes,tictoc,Hmisc,english,readstata13,margins,evd)

#################### READ IN DATA ################
{
  
  newdemean <- read.csv("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID-shared/intermediate-files/demeanrp-lasso.csv")[,-1]
  cmatch <- read.csv("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID-shared/intermediate-files/countymatch.csv")[,-1]
  dat <- read.csv("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_nointx.csv") %>% dplyr::filter(rejectonly==0) 
  dat2 <- read.dta13("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_nointx.dta")
  
  
  
  varlabs <- data.frame(variable = names(dat),label= attributes(dat2)$var.labels)
  varlabs$variable <- as.character(varlabs$variable)
  varlabs$label <- as.character(varlabs$label)
  varlabs <- varlabs %>% dplyr::filter(variable!="multi")
  
  dat$state <- ifelse(dat$CA==1,"CA",
                      ifelse(dat$OR==1,"OR","WA"))
  
  dat$prep <- ifelse(dat$prepover==1,"over",
                     ifelse(dat$prepreas==1,"reasonable","poor"))
  
  dat$rules <- ifelse(dat$rulesbad==1,"bad",
                      ifelse(dat$rulesgood==1,"good","insufficient"))
  
  dat$schoolage <- ifelse(dat$hhld0to1 ==1 | dat$hhld2to5==1 | dat$hhld6to12==1 | dat$hhld13to17==1,1,0)
  
  dat <- dat %>% group_by(ResponseId) %>% mutate(nchoice=length(unique(choice))) %>% ungroup %>% mutate(popwt2 = popwt/nchoice)
  
  dat$ideol <- ifelse(dat$ideolsconserv==1,"strongly conservative",
                      ifelse(dat$ideolsliberal==1,"strongly liberal",
                             ifelse(dat$ideolmliberal==1,"moderately liberal",
                                    ifelse(dat$ideolmconserv==1,"moderately conservative",
                                           ifelse(dat$ideolmod==1,"moderate","MISSING")))))
  dat$ideol <- as.factor(dat$ideol)
  dat <- within(dat, ideol <- relevel(ideol, ref = 2))
  
  dat$ideol2 <- ifelse(dat$ideol %in% c("strongly conservative","moderately conservative"),"conservative",
                       ifelse(dat$ideol %in% c("strongly liberal","moderately liberal"),"liberal",
                              ifelse(dat$ideol %in% c("moderate"),"moderate","MISSING")))
  dat$ideol2 <- as.factor(dat$ideol2)
  dat <- within(dat, ideol2 <- relevel(ideol2, ref = 4))
  
  dat <- dat %>% mutate(ethfrac = 1 - pziprasian^2 - pziprwhite^2 - pziprblack^2 - pziprothrace3^2 - pziprmulti^2)
  dat$ethfrac <- ifelse(dat$ethfrac==1,0,dat$ethfrac)
  dat <- dat %>% mutate(ethcheck = pziprasian + pziprwhite + pziprblack + pziprothrace3 + pziprmulti)
  
  
  dat <- left_join(dat,newdemean)
  dat <- left_join(dat,cmatch)
  
}

dat$ideol <- dat$ideol %>% str_remove_all(" ") %>% as.factor()

sim <- dat %>% dplyr::select(best,choice,popwt,mabsdeaths,mabscases,
                             avcost, unempl,
                             rule1,rule2, rule3, rule4, rule5,
                             rule6, rule7, rule8, rule9, rule10,
                             statquo,feduiany)

sim$costfed <- sim$avcost*sim$feduiany
sim$unemplfed <- sim$unempl*sim$feduiany

sim <- sim %>% dplyr::select(best,choice,popwt,mabsdeaths,mabscases,
                             avcost, unempl,
                             rule1,rule2, rule3, rule4, rule5,
                             rule6, rule7, rule8, rule9, rule10,
                             costfed,unemplfed,
                             statquo,feduiany)

a <- clogit(best ~ mabsdeaths + mabscases + avcost + unempl +
            rule1+rule2+ rule3+ rule4+ rule5+
            rule6+ rule7+ rule8+ rule9+ rule10 + costfed + unemplfed +
            statquo + strata(choice),data=sim,weights=popwt,method="approximate")

summary(a)
pars <- a$coefficients
#pars <- c(-1,-2,-3,-4,-1,-3,-2,-1,0,0,0,0,0,0,-1)/100
#pars[4] <- -1*pars[4]
#pars[15:16] <- 0


res <- rbind(pars,pars) %>% as.data.frame()

for (i in 1:100) {

sim$u <- as.numeric(as.matrix(sim[,4:20]) %*% pars) + rgumbel(nrow(sim))
sim <- sim %>% group_by(choice) %>% mutate(best2 = ifelse(u==max(u),1,0)) %>% ungroup()

b <- clogit(best2 ~ mabsdeaths + mabscases + avcost + unempl +
              rule1+rule2+ rule3+ rule4+ rule5+
              rule6+ rule7+ rule8+ rule9+ rule10+
              costfed + unemplfed +
              statquo + strata(choice),data=sim,weights=popwt,method="approximate")
#summary(b)

res[i,] <- b$coefficients


}

ggplot(res) +
  geom_density(aes(x=unempl)) +
  geom_vline(xintercept=pars[4])


ggplot(res) +
  geom_density(aes(x=avcost)) +
  geom_vline(xintercept=pars[3])


ggplot(res) +
  geom_density(aes(x=costfed)) +
  geom_vline(xintercept=pars[15])
sum(res$costfed < 0)

ggplot(res) +
  geom_density(aes(x=unemplfed)) +
  geom_vline(xintercept=pars[16])

sum(res$unemplfed > 0)
