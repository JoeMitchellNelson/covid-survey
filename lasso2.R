require(pacman)

p_load(rpart,rpart.plot,readr,dplyr,RCurl,rjson,lubridate,
       rvest,stringr,Hmisc,rattle,RColorBrewer,ddpcr,tidytext,tidyr,
       ggrepel,ggplot2,png,ggpubr,tidycensus,sf,plm,lmtest,stargazer,MASS,
       xtable,knitr,magick,purrr,ggthemes,gifski,extrafont,latex2exp,
       cowplot,mapproj,patchwork,remotes,tictoc,Hmisc,english,readstata13)

p_load(glmnet,tibble,broom)

source('makedum.R')

# NOTES
# do rules as factors
# 
# nonvarying <- agebins and agebcont, y2020 variables all, demeanrp, larules, lirules, 
# male/female, income as bins, incloss, wrongs (but not wrongmix), rulesbad/good,
# careful with education, careful with employment, careful with ffcom and owncom (drop owncomorbid and ffcomorbid), all pzips,
# vulns, pvotestuff, 
# 
# as factors <- state, age, countyfutr, countypast, gender, income, incloss (yes/no), certainty,
# rules (bad/good/insuff), prep (poor, good,whatever), education, employment, "R0subjmean"   "R0subjdnk"     "R0subjrange", 
# transit, monthstwo/three, task (not chset), 
# 
# ??? layoff (0 thru 3+), layoffret(0 thru 3+)  make layoff permanent and layoff return
# 
# ??? avcostshi, avcostmhi
# 
# NOWHERE <- fac stuff, idrules, tch, cch



dat <- read.dta13("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_nointx.dta")
pair <- read.dta13("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/forR_VSL-COVID_two-alt_long.dta")

pair$gender <- makedum(pair,c("male","female","nonbinary")) %>% as.factor()
pair$race <- makedum(pair,c("white","black","multi","asian","racepnts")) %>% as.factor()
pair$agebin <- makedum(pair,c("ageb18to24","ageb25to34","ageb35to44","ageb45to54","ageb55to64","ageb65to74","ageb75up"),"ageb") %>% as.factor()
pair$income <- as.factor(pair$owninc)
pair$countyfuture <- makedum(pair,c("countyfutr0to2",  "countyfutr3to5",  "countyfutr6to10", "countyfutr10up",  "countyfutruncer"),"countyfutr") %>% as.factor()
pair$countypast <- makedum(pair,c("countypast0to2","countypast3to5"  ,"countypast6to10", "countypast10up" ),"countypast") %>% as.factor()
pair$employment <- makedum(pair,c("empstatself" ,"empstatfull","empstatpart",   "empstatlook",     
                                  "empstatnotlook",  "empstatretired",  "empstatdisabled", "empstatstufull",  "empstatstupart"),"empstat") %>% as.factor()

pair$rulesjudgment <- makedum(pair,c("rulesbad", "rulesgood", "rulesinsuff", "rulesdnk"),"rules") %>% as.factor()

pair$prepjudgment <- makedum(pair,c("prepover", "prepreas", "preppoor"),"prep") %>% as.factor()
pair$state <- makedum(pair,c("CA","OR","WA")) %>% as.factor()
pair$ideology <- makedum(pair,c("ideolsconserv", "ideolmconserv", "ideolmoderate", "ideolmliberal", "ideolsliberal"),"ideol") %>% as.factor()
pair$education <- makedum(pair,c("lthighsch",         "hischgrad",         "somecoll",         
                                 "bachelors",         "masters",           "doctoral",          "tradetech", "educpnts")) %>% as.factor()

pair$certainty <- makedum(pair,c("vcertain","scertain","ncertain")) %>% as.factor()
pair$tasknum <- makedum(pair,c("task1","task2","task3","task4","task5","task6"),"task") %>% as.factor()
pair$transit <- makedum(pair,c("transityes","transitsome"),"transit")
pair$transit <- ifelse(pair$transit=="MISSING","no",pair$transit) %>% as.factor()

pair <- pair %>% mutate(layoffstart = layoffnum1*1 + layoffnum2*3 + layoffnum3up*3)
pair <- pair %>% mutate(layofftemp = layoffret1*1 + layoffret2*3 + layoffret3up*3)
pair <- pair %>% mutate(layoffperm = layoffstart - layofftemp)

pair$months <- as.factor(pair$months)

pair$frule1 <- as.factor(pair$rule1)
pair$frule2 <- as.factor(pair$rule2)
pair$frule3 <- as.factor(pair$rule3)
pair$frule4 <- as.factor(pair$rule4)
pair$frule5 <- as.factor(pair$rule5)
pair$frule6 <- as.factor(pair$rule6)
pair$frule7 <- as.factor(pair$rule7)
pair$frule8 <- as.factor(pair$rule8)
pair$frule9 <- as.factor(pair$rule9)
pair$frule10 <- as.factor(pair$rule10)




varying <- c(
  "avcost",
  "unempl",
  "mabscases",
  "mabsdeaths",
  "frule1","frule2","frule3","frule4","frule5",
  "frule6","frule7","frule8","frule9","frule10",
  "statquo"
)

nonvarying <- c(
                "gender",    "agebin",    "income",   "countyfuture",  "countypast" ,  
                "employment",    "rulesjudgment", "prepjudgment",  "state",  "ideology", "education",  "layofftemp",   
                "layoffperm", "incloss", "pvoterep", "pvotedem",
                stem2vars(pair,"y2020"),
                stem2vars(pair,"lirule[:digit:]"),
                stem2vars(pair,"larule[:digit:]"),
                stem2vars(pair,"^vuln"),
                "ffcomheart", "ffcomdiab",  "ffcomresp",  "ffcomcanc",  "ffcomage",   "ffcomother", "ffcompreg",  "ffcomcovid",
                "ffcomnovax", "owncomheart", "owncomdiab",  "owncomresp",  "owncomcanc",  "owncomage",
                "owncomother", "owncompreg",  "owncomcovid", "owncomnovax",
                stem2vars(pair,"pzip"), "tasknum","wrongexactnum",  "wrongpetstores", "wrongdaycare",   
                "wrongnopopup",   "wronguneven",    "wrongsamecost", "demeanrp", "reject",
                "R0subjmean",   "R0subjdnk",     "R0subjrange","transit","months","chhldinc"
                )

pdat <- pair %>% dplyr::filter(rejectonly==0)
pdat <- pair[,which(names(pdat) %in% c("best","weight",varying,nonvarying))]

v <- which(names(pdat) %in% varying)
nv <- which(names(pdat) %in% nonvarying)
bestind <- which(names(pdat)=="best")
weightind <- which(names(pdat)=="weight")

temp <- c()
for (i in v) {
  for (j in nv) {
    temp <- c(temp,paste0(names(pdat)[i],"*",names(pdat)[j]))
  }
}

lassoformula <- paste0("best ~ ",paste(varying,collapse=" + ")," + ", paste(temp,collapse=" + ")) %>% as.formula


pm <- model.matrix(lassoformula,pair)[,-1]
bestweight <- pdat %>% dplyr::select(best,weight) %>% as.matrix()

##################### DO THE LASSO ##########################

cv.lasso <- cv.glmnet(x = pm,y = bestweight[,1], alpha = 1, family = "binomial",weights=bestweight[,2], lambda = NULL)


model <- glmnet(x = pm,y = bestweight[,1], alpha = 1, family = "binomial",weights=bestweight[,2],
                lambda = 0.003634935)

# cv.lasso$lambda.min = 0.003634935

coef(model)
plot(cv.lasso)

keepers <- model$beta %>% as.matrix %>% as.data.frame() %>% rownames_to_column("vars") %>% dplyr::filter(s0!=0)
keepers <- keepers$vars 
keepers <- keepers[which(!keepers %in% "statquo:reject")]

frules <- c(paste0(c("frule1"),c("1","2")), paste0(c("frule2"),c("1","2","3")),paste0(c("frule3"),c("1","2","3")),
            paste0(c("frule4"),c("1","2","3")),paste0(c("frule5"),c("1","2","3")),paste0(c("frule6"),c("1","2","3")),
            paste0(c("frule7"),c("1","2","3")),paste0(c("frule8"),c("1","2","3")),paste0(c("frule9"),c("1","2","3")),paste0(c("frule10"),c("1","2","3")))

keepers <- c("mabscases","mabsdeaths","avcost","unempl",frules,"statquo",keepers) %>% unique()

keepers

lassoformula2 <- paste0("best ~ ", paste(keepers,collapse=" + ")," + strata(choice)") %>% as.formula


#################################################################



dat$gender <- makedum(dat,c("male","female","nonbinary")) %>% as.factor()
dat$race <- makedum(dat,c("white","black","multi","asian","racepnts")) %>% as.factor()
dat$agebin <- makedum(dat,c("ageb18to24","ageb25to34","ageb35to44","ageb45to54","ageb55to64","ageb65to74","ageb75up"),"ageb") %>% as.factor()
dat$income <- as.factor(dat$owninc)
dat$countyfuture <- makedum(dat,c("countyfutr0to2",  "countyfutr3to5",  "countyfutr6to10", "countyfutr10up",  "countyfutruncer"),"countyfutr") %>% as.factor()
dat$countypast <- makedum(dat,c("countypast0to2","countypast3to5"  ,"countypast6to10", "countypast10up" ),"countypast") %>% as.factor()
dat$employment <- makedum(dat,c("empstatself" ,"empstatfull","empstatpart",   "empstatlook",     
                                  "empstatnotlook",  "empstatretired",  "empstatdisabled", "empstatstufull",  "empstatstupart"),"empstat") %>% as.factor()

dat$rulesjudgment <- makedum(dat,c("rulesbad", "rulesgood", "rulesinsuff", "rulesdnk"),"rules") %>% as.factor()

dat$prepjudgment <- makedum(dat,c("prepover", "prepreas", "preppoor"),"prep") %>% as.factor()
dat$state <- makedum(dat,c("CA","OR","WA")) %>% as.factor()
dat$ideology <- makedum(dat,c("ideolsconserv", "ideolmconserv", "ideolmoderate", "ideolmliberal", "ideolsliberal"),"ideol") %>% as.factor()
dat$education <- makedum(dat,c("lthighsch",         "hischgrad",         "somecoll",         
                                 "bachelors",         "masters",           "doctoral",          "tradetech", "educpnts")) %>% as.factor()

dat$certainty <- makedum(dat,c("vcertain","scertain","ncertain")) %>% as.factor()
dat$tasknum <- makedum(dat,c("task1","task2","task3","task4","task5","task6"),"task") %>% as.factor()
dat$transit <- makedum(dat,c("transityes","transitsome"),"transit")
dat$transit <- ifelse(dat$transit=="MISSING","no",dat$transit) %>% as.factor()

dat <- dat %>% mutate(layoffstart = layoffnum1*1 + layoffnum2*3 + layoffnum3up*3)
dat <- dat %>% mutate(layofftemp = layoffret1*1 + layoffret2*3 + layoffret3up*3)
dat <- dat %>% mutate(layoffperm = layoffstart - layofftemp)

dat$months <- as.factor(dat$months)

dat$frule1 <- as.factor(dat$rule1)
dat$frule2 <- as.factor(dat$rule2)
dat$frule3 <- as.factor(dat$rule3)
dat$frule4 <- as.factor(dat$rule4)
dat$frule5 <- as.factor(dat$rule5)
dat$frule6 <- as.factor(dat$rule6)
dat$frule7 <- as.factor(dat$rule7)
dat$frule8 <- as.factor(dat$rule8)
dat$frule9 <- as.factor(dat$rule9)
dat$frule10 <- as.factor(dat$rule10)


varying <- c(
  "avcost",
  "unempl",
  "mabscases",
  "mabsdeaths",
  "frule1","frule2","frule3","frule4","frule5",
  "frule6","frule7","frule8","frule9","frule10",
  "statquo"
)

nonvarying <- c(
  "gender",    "agebin",    "income",   "countyfuture",  "countypast" ,  
  "employment",    "rulesjudgment", "prepjudgment",  "state",  "ideology", "education",  "layofftemp",   
  "layoffperm", "incloss", "pvoterep", "pvotedem",
  stem2vars(pair,"y2020"),
  stem2vars(pair,"lirule[:digit:]"),
  stem2vars(pair,"larule[:digit:]"),
  stem2vars(pair,"^vuln"),
  "ffcomheart", "ffcomdiab",  "ffcomresp",  "ffcomcanc",  "ffcomage",   "ffcomother", "ffcompreg",  "ffcomcovid",
  "ffcomnovax", "owncomheart", "owncomdiab",  "owncomresp",  "owncomcanc",  "owncomage",
  "owncomother", "owncompreg",  "owncomcovid", "owncomnovax",
  stem2vars(pair,"pzip"), "tasknum","wrongexactnum",  "wrongpetstores", "wrongdaycare",   
  "wrongnopopup",   "wronguneven",    "wrongsamecost", "demeanrp", "reject",
  "R0subjmean",   "R0subjdnk",     "R0subjrange","transit","months","chhldinc"
)

pdat2 <- dat %>% dplyr::filter(rejectonly==0)
pdat2 <- pdat2[,which(names(pdat2) %in% c("best","alt","popwt","choice","choiceofperson", varying,nonvarying))]
bw <- pdat2 %>% dplyr::select(best,popwt,choice,choiceofperson,alt)
pm2 <- model.matrix(lassoformula,pdat2)
pm2 <- cbind(bw,as.data.frame(pm2))


summary(res <- clogit(lassoformula2,data=pm2,weights=popwt,method="approximate"))
a <- broom::tidy(res)

#write.csv(a,"~/covid-survey/lasso-results.csv")

costvars <- a %>% dplyr::filter(str_detect(term,"avcost"))
costmax <- pm2[,c(which(names(pm2) %in% c(costvars$term,"alt","choiceofperson")))]
costmax <- costmax %>% dplyr::filter(choiceofperson==2,alt==2)

head(costmax)
costmax2 <- costmax[,3:ncol(costmax)]/costmax$avcost
costmax2$`avcost:reject` <- 0
costparams <- (as.matrix(costmax2) %*% as.matrix(costvars$estimate))

costpar <- costparams %>% as.data.frame()


ggplot(costpar) +
  geom_density(aes(x=V1)) +
  geom_vline(xintercept=mean(costpar$V1))


#########

a <- read.csv("~/covid-survey/lasso-results.csv")[,-1]

costvars <- a %>% dplyr::filter(str_detect(term,"avcost"))
costvars$term <- as.character(costvars$term)
costmax <- pm2[,c(which(names(pm2) %in% c(costvars$term,"alt","choiceofperson")))]

costmax <- costmax %>% dplyr::filter(choiceofperson==2,alt==2)

head(costmax)
costmax2 <- costmax[,3:ncol(costmax)]/costmax$avcost
costmax2$`avcost:reject` <- 0
costparams <- (as.matrix(costmax2) %*% as.matrix(costvars$estimate))

costpar <- costparams %>% as.data.frame()


ggplot(costpar) +
  geom_density(aes(x=V1),fill="#660066",alpha=.4) +
  geom_vline(xintercept=mean(costpar$V1)) +
  geom_vline(xintercept=0,linetype="dashed") +
  labs(x="Total effect of avcost, by person",
       y="Density",
       caption="Solid vertical line is mean; dashed line is 0.") +
  theme_minimal()

mean(costpar$V1)
sd(costpar$V1)/nrow(costpar)

mean(costpar$V1) + 2.5*sd(costpar$V1)/sqrt(nrow(costpar))
mean(costpar$V1) - 2.5*sd(costpar$V1)/sqrt(nrow(costpar))
