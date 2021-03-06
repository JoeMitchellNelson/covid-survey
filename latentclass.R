require(pacman)

p_load(rpart,rpart.plot,readr,dplyr,RCurl,rjson,lubridate,
       rvest,stringr,Hmisc,rattle,RColorBrewer,ddpcr,tidytext,tidyr,
       ggrepel,ggplot2,png,ggpubr,tidycensus,sf,plm,lmtest,stargazer,MASS,
       xtable,knitr,magick,purrr,ggthemes,gifski,extrafont,latex2exp,fastDummies,
       cowplot,mapproj,patchwork,remotes,tictoc,Hmisc,english,readstata13,gmnl,poLCA,haven,nnet)

install.packages("https://cran.r-project.org/src/contrib/Archive/mlogit/mlogit_1.0-2.tar.gz", repos=NULL,type="source")
library(mlogit)

#################### READ IN DATA ################
{
  
  newdemean <- read.csv("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID-shared/intermediate-files/demeanrp-lasso.csv")[,-1]
  cmatch <- read.csv("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID-shared/intermediate-files/countymatch.csv")[,-1]
  dat <- read.csv("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_nointx.csv") %>% 
    # dplyr::filter(rejectonly==0) %>% 
    dplyr::filter(Durationinseconds > 360) %>% 
    dplyr::filter(choiceofperson %in% 1:2) %>% 
    # dplyr::filter(rejectonly==0) %>% 
    group_by(ResponseId) %>% 
    mutate(rejectever = sum(rejectonly)) %>% 
    ungroup() %>% 
    dplyr::filter(rejectever==0) 
  

  dat$owninc <- ifelse(dat$owninc==0,NA,dat$owninc)
  
  
  dat2 <- read.dta13("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_nointx.dta")
  
  ethnic <- read.csv("~/covid-survey/data/countyethfrac.csv")[,-1]
  
  varlabs <- data.frame(variable = names(dat2),label= attributes(dat2)$var.labels)
  varlabs$variable <- as.character(varlabs$variable)
  varlabs$label <- as.character(varlabs$label)
  varlabs <- varlabs %>% dplyr::filter(variable!="multi")
  newvarlabs <- data.frame(variable=c("feduianyavcost","feduinoneavcost","feduianyunempl","feduinoneunempl",
                                      "fedui100avcost",  "fedui200avcost",  "fedui300avcost",  "fedui400avcost",
                                      "fedui100unempl",  "fedui200unempl",  "fedui300unempl",  "fedui400unempl"),
                           label=c("Avg. hhld cost for county (federal UI > 0)",
                                   "Avg. hhld cost for county (federal UI = 0)",
                                   "Unempl rate for county (federal UI > 0)",
                                   "Unempl rate for county (federal UI = 0)",
                                   "Avg. hhld cost for county (federal UI = 100)",
                                   "Avg. hhld cost for county (federal UI = 200)",
                                   "Avg. hhld cost for county (federal UI = 300)",
                                   "Avg. hhld cost for county (federal UI = 400)",
                                   "Unempl rate for county (federal UI = 100)",
                                   "Unempl rate for county (federal UI = 200)",
                                   "Unempl rate for county (federal UI = 300)",
                                   "Unempl rate for county (federal UI = 400)"))
  
  varlabs <- rbind(varlabs,newvarlabs)
  
  dat$mabscases <- dat$mabscases
  dat$avcost <- dat$avcost/100
  
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
  
  dat$ideol <- dat$ideol %>% str_remove_all(" ") %>% as.factor()
  
  dat <- left_join(dat,ethnic)
  
  dat$feduianyavcost <- dat$feduiany*dat$avcost
  dat$feduinoneavcost <- dat$feduinone*dat$avcost
  
  dat$feduianyunempl <- dat$feduiany*dat$unempl
  dat$feduinoneunempl <- dat$feduinone*dat$unempl
  
  
  dat$fedui100avcost <- dat$fedui100*dat$avcost
  dat$fedui200avcost <- dat$fedui200*dat$avcost
  dat$fedui300avcost <- dat$fedui300*dat$avcost
  dat$fedui400avcost <- dat$fedui400*dat$avcost
  
  dat$fedui100unempl <- dat$fedui100*dat$unempl
  dat$fedui200unempl <- dat$fedui200*dat$unempl
  dat$fedui300unempl <- dat$fedui300*dat$unempl
  dat$fedui400unempl <- dat$fedui400*dat$unempl
  
  dat$ideol2 <- dat$ideol2 %>% factor(levels=c("liberal","moderate","conservative","MISSING"))
  
  
  dat$gender <- ifelse(dat$male==1,"Men",
                       ifelse(dat$female==1,"Women",
                              ifelse(dat$nonbinary==1,"NonBinary","MISSING")))
  
  dat$gender <- factor(dat$gender,levels=c("Women","Men","NonBinary","MISSING"))
  
  dat$age <- ifelse(dat$agebcont < 35, "18to34",
                    ifelse(dat$agebcont %in% c(40,50,60),"35to64","65andolder"))
  
  dat$age <- dat$age %>% factor(levels=c("18to34","35to64","65andolder"))
  
  dat$race <- ifelse(dat$white == 1,"White","Other")
  dat$race <- as.factor(dat$race)
  
  dat$education <- ifelse(dat$scollorless==1,"Noncollege",
                          ifelse(dat$educpnts==1,"MISSING","College"))
  
  dat$education <- factor(dat$education,levels=c("Noncollege","College","MISSING"))
  
  dat$income <- ifelse(dat$inccont > 75,"Morethan75","Lessthan75")
  dat$income <- factor(dat$income)
  
  dat$threealts2 <- ifelse(dat$threealts==0,"Alts2","Alts3") %>% as.factor()
  
  
  dat <- dummy_cols(dat,
                    select_columns=c("rule1","rule2","rule3","rule4","rule5","rule6","rule7","rule8","rule9","rule10"),
                    remove_first_dummy = T)
  
  names(dat) <- names(dat) %>% str_replace_all("_",".")
  
  
}


################ CREATE RP INTERACTIONS ############

BV <- c("mabsdeaths", "mabscases",
        "feduinoneavcost", "feduianyavcost", 
        "feduinoneunempl", "feduianyunempl", 
        "rule1", "rule2",
        "rule3", "rule4",
        "rule5", "rule6",
        "rule7", "rule8",
        "rule9", "rule10",
        "statquo")

for (i in 1:length(BV)) {
  
  dat$newvar <- as.numeric(as.matrix(dat[,which(names(dat)==BV[i])])) * as.numeric(dat$lassorpfl) 
  names(dat)[which(names(dat)=="newvar")] <- paste0("RP",BV[i])
  
}


set.seed(123)

# dat2$mabsdeaths <- dat2$mabsdeaths * -1
# dat2$mabscases <- dat2$mabscases * -1
dat2 <- dat[sample(1:nrow(dat)),] %>% arrange(ResponseId,choiceofperson)
dat2 <- dat2 %>% dplyr::select(ResponseId,choiceofperson,choice,
                               best,popwt,
                               mabsdeaths, mabscases,
                               feduinoneavcost, feduianyavcost, 
                               feduinoneunempl, feduianyunempl, 
                               rule1, rule2,
                               rule3, rule4,
                               rule5, rule6,
                               rule7, rule8,
                               rule9, rule10,
                               statquo,
                               
                               RPmabsdeaths, RPmabscases,
                               RPfeduinoneavcost, RPfeduianyavcost, 
                               RPfeduinoneunempl, RPfeduianyunempl, 
                               RPrule1, RPrule2,
                               RPrule3, RPrule4,
                               RPrule5, RPrule6,
                               RPrule7, RPrule8,
                               RPrule9, RPrule10,
                               RPstatquo,
                               
                               lassorpfl,
                               choice,
                               alt,
                               caseid)

dat2$caseid <- as.numeric(as.factor(dat2$ResponseId))
dat2$alt <- ifelse(dat2$alt==3,2,1)

#labels for export to stata
{
labsdat <- read.dta13("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/variables_for_selection_models.dta")

labs <- data.frame(variable = names(labsdat),label=attributes(labsdat)$var.labels)
labs$variable <- as.character(labs$variable)
labs$label <- as.character(labs$label)

fort <- data.frame(variable = names(dat2),v1=NA,v2=NA)
fort$v1 <- fort$variable %>% str_remove_all("RP")
fort$v2 <- ifelse(str_detect(fort$variable,"RP"),"$\\\\times$ Response propensity","")
fort <- left_join(fort,varlabs,by=c("v1"="variable"))

fort$label <- paste0(fort$label," ",fort$v2)

statacode <- ""

for (i in 1:nrow(fort)) {
  temp <- paste0("label variable ",fort$variable[i]," \"",fort$label[i],"\"","\n")
  statacode <- paste0(statacode,temp)
}


fort$variable <- as.character(fort$variable)
cat(statacode)

#write_dta(dat2,"~/covid-survey/data/for_stata_lclogit.dta")

}




#tcodes <- read.csv("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/number_non-varying.csv")

#dat3 <- read.csv("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/forR_VSL-COVID_two-alt_long.csv")
#dat <- read.dta13("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/forR_VSL-COVID_two-alt_long.dta")


#dat3 <- mlogit.data(dat2, chid.var = "choice", id.var="caseid", alt.var="alt", choice = "best", varying = tcodes[,3]:tcodes[,4], shape = "long", sep = "")

dat3 <- mlogit.data(dat2, chid.var = "choice", id.var="caseid", alt.var="alt", choice = "best", varying = c(3,6:ncol(dat2)), shape = "long", sep = "")

ranp1 = c(
  mabsdeaths = "n", 
    mabscases = "n",
    feduinoneavcost = "n", feduianyavcost = "n", 
    feduinoneunempl = "n", feduianyunempl = "n", 
    # 
    # rule1 = "n", rule2 = "n",
    # rule3 = "n", rule4 = "n",
    # rule5 = "n", rule6 = "n",
    # rule7 = "n", rule8 = "n",
    # rule9 = "n", rule10 = "n",
    # 
     statquo = "n"
  
  # RPmabsdeaths= "n", RPmabscases= "n",
  # RPfeduinoneavcost= "n", RPfeduianyavcost= "n", 
  # RPfeduinoneunempl= "n", RPfeduianyunempl= "n", 
  # RPrule1= "n", RPrule2= "n",
  # RPrule3= "n", RPrule4= "n",
  # RPrule5= "n", RPrule6= "n",
  # RPrule7= "n", RPrule8= "n",
  # RPrule9= "n", RPrule10= "n",
  # RPstatquo= "n"
)

lcmodel <- gmnl(best ~ 
                  mabsdeaths + 
                  mabscases +
                   feduinoneavcost + feduianyavcost + 
                   feduinoneunempl + feduianyunempl + 
                   
                   rule1 + rule2 +
                   rule3 + rule4 +
                   rule5 + rule6 +
                   rule7 + rule8 +
                   rule9 + rule10 +
                  
                 statquo +
                  
                  RPmabsdeaths +  RPmabscases + 
                RPfeduinoneavcost +  RPfeduianyavcost +  
                RPfeduinoneunempl +  RPfeduianyunempl +  
                RPrule1 +  RPrule2 + 
                RPrule3 +  RPrule4 + 
                RPrule5 +  RPrule6 + 
                RPrule7 +  RPrule8 + 
                RPrule9 +  RPrule10 + 
                RPstatquo
                
                | 0 , data = dat3, model = "mixl",weights=popwt,
                panel = T, seed=123,ranp=ranp1,R=100)


summary(lcmodel)


ranp2 = c(
  mabsdeathsnegative = "ln", 
  mabscasesnegative = "ln",
  feduinoneavcost = "n", feduianyavcost = "n",
  feduinoneunempl = "n", feduianyunempl = "n",

  # rule1 = "n", rule2 = "n",
  # rule3 = "n", rule4 = "n",
  # rule5 = "n", rule6 = "n",
  # rule7 = "n", rule8 = "n",
  # rule9 = "n", rule10 = "n",
  # 
   statquo = "n"

  # RPmabsdeaths= "n", RPmabscases= "n",
  # RPfeduinoneavcost= "n", RPfeduianyavcost= "n",
  # RPfeduinoneunempl= "n", RPfeduianyunempl= "n",
  # RPrule1= "n", RPrule2= "n",
  # RPrule3= "n", RPrule4= "n",
  # RPrule5= "n", RPrule6= "n",
  # RPrule7= "n", RPrule8= "n",
  # RPrule9= "n", RPrule10= "n",
  # RPstatquo= "n"
)

dat2$mabscasesnegative <- dat2$mabscases * -1
dat2$mabsdeathsnegative <- dat2$mabsdeaths * -1
dat2$RPmabscasesnegative <- dat2$RPmabscases * -1
dat2$RPmabsdeathsnegative <- dat2$RPmabsdeaths * -1

dat4 <- mlogit.data(dat2, chid.var = "choice", id.var="caseid", alt.var="alt", choice = "best", varying = c(3,6:ncol(dat2)), shape = "long", sep = "")


lcmodel2 <- gmnl(best ~ 
                  mabsdeathsnegative + 
                  mabscasesnegative +
                  feduinoneavcost + feduianyavcost + 
                  feduinoneunempl + feduianyunempl + 
                  
                  rule1 + rule2 +
                  rule3 + rule4 +
                  rule5 + rule6 +
                  rule7 + rule8 +
                  rule9 + rule10 +
                  
                  statquo +
                  
                  RPmabsdeathsnegative +  RPmabscasesnegative + 
                  RPfeduinoneavcost +  RPfeduianyavcost +  
                  RPfeduinoneunempl +  RPfeduianyunempl +  
                  RPrule1 +  RPrule2 + 
                  RPrule3 +  RPrule4 + 
                  RPrule5 +  RPrule6 + 
                  RPrule7 +  RPrule8 + 
                  RPrule9 +  RPrule10 + 
                  RPstatquo
                
                | 0 , data = dat4, model = "mixl",weights=popwt,
                panel = T, seed=132,ranp=ranp2,R=100,
                start=startpars,
                print.init=T)


summary(lcmodel2)

startpars <- lcmodel2$coefficients

# sample993 <- dat2 %>% dplyr::select(ResponseId) %>% unique()
# write.csv(sample993,"~/covid-survey/sample993.csv")

##### FORMAT LATEX TABLES ############

{
fake1 <- lm(best ~ 
              mabsdeaths + 
              mabscases +
              feduinoneavcost + feduianyavcost + 
              feduinoneunempl + feduianyunempl + 
              
              rule1 + rule2 +
              rule3 + rule4 +
              rule5 + rule6 +
              rule7 + rule8 +
              rule9 + rule10 +
              
              statquo +
              
              RPmabsdeaths +  RPmabscases + 
              RPfeduinoneavcost +  RPfeduianyavcost +  
              RPfeduinoneunempl +  RPfeduianyunempl +  
              RPrule1 +  RPrule2 + 
              RPrule3 +  RPrule4 + 
              RPrule5 +  RPrule6 + 
              RPrule7 +  RPrule8 + 
              RPrule9 +  RPrule10 + 
              RPstatquo + 0,data=dat3)

fake2 <- lm(best ~ 
              mabsdeaths + 
              mabscases +
              feduinoneavcost + feduianyavcost + 
              feduinoneunempl + feduianyunempl + 
              
              statquo +
              0,data=dat3)

fake3 <- lm(best ~ 
              mabsdeathsnegative + 
              mabscasesnegative +
              feduinoneavcost + feduianyavcost + 
              feduinoneunempl + feduianyunempl + 
              
              rule1 + rule2 +
              rule3 + rule4 +
              rule5 + rule6 +
              rule7 + rule8 +
              rule9 + rule10 +
              
              statquo +
              
              RPmabsdeathsnegative +  RPmabscasesnegative + 
              RPfeduinoneavcost +  RPfeduianyavcost +  
              RPfeduinoneunempl +  RPfeduianyunempl +  
              RPrule1 +  RPrule2 + 
              RPrule3 +  RPrule4 + 
              RPrule5 +  RPrule6 + 
              RPrule7 +  RPrule8 + 
              RPrule9 +  RPrule10 + 
              RPstatquo + 0,data=dat4)

fake4 <- lm(best ~ 
              mabsdeathsnegative + 
              mabscasesnegative +
              feduinoneavcost + feduianyavcost + 
              feduinoneunempl + feduianyunempl + 
              
              statquo +
              0,data=dat4)
}

fixnames3 <- function (x) {
  
  x <- paste(x,collapse="\n")
  
  
  for (i in nrow(fort):1) {
    if(str_detect(x,paste0("",fort$variable[i]))) {
      x <- str_replace_all(x,fort$variable[i],fort$label[i])
    }
  }
  
  x <- str_replace_all(x," numberernative","")
  x <- str_replace_all(x," w/ any attrib.","")
  x <- str_replace_all(x,"1=Non-zero federal UI policy","$\\\\quad \\\\times$ 1=Non-zero federal UI")
  x <- str_remove_all(x,":Unempl rate for county")
  x <- str_remove_all(x,"Avg. hhld cost for county:")
  # x <- str_replace_all(x," & & \\\\\\\\"," & & \\\\\\\\[-1.2ex]")
  # x <- str_replace_all(x,"\\[-1.8ex]","\\\\")
  
  x <- str_replace_all(x,"factor\\(Federal UI per week\\)", "$\\\\quad \\\\times$ Federal UI per week = \\\\$")
  x <- str_replace_all(x,":Avg. hhld cost for county", "")
  x <- str_replace_all(x,"Unempl rate for county:","")
  
  x
  
}

summary(fake2)

summary(lcmodel2)

a <- summary(lcmodel)
b <- a$CoefTable %>% as.data.frame()

a2 <- summary(lcmodel2)
b2 <- a2$CoefTable %>% as.data.frame()

d <- b[which(str_detect( row.names(b), "^sd" )),]
row.names(d) <- row.names(d) %>% str_remove_all("sd\\.")

d2 <- b2[which(str_detect( row.names(b2), "^sd" )),]
row.names(d2) <- row.names(d2) %>% str_remove_all("sd\\.") 

coefs <- list(lcmodel$coefficients)
ses <- list(setNames(b$`Std. Error`,row.names(b)))

coefs[[2]] <- setNames(d$Estimate,row.names(d))
ses[[2]] <- setNames(d$`Std. Error`,row.names(d))

coefs[[3]] <- lcmodel2$coefficients
ses[[3]] <- setNames(b2$`Std. Error`,row.names(b2))

coefs[[4]] <- setNames(d2$Estimate,row.names(d2))
ses[[4]] <- setNames(d2$`Std. Error`,row.names(d2))

mixltable <- stargazer(fake1,fake2,fake3,fake4,
                       coef=coefs,se=ses,
                       column.labels = c("Estimate","SD","Estimate","SD"),
                       model.numbers = F)
mixltable %>% fixnames3() %>% str_replace_all("negative","$\\\\times -1$") %>%  cat()






dat2 <- dat2 %>% group_by(ResponseId,choiceofperson) %>% 
  summarise(ResponseId = first(ResponseId),
            choiceofperson = first(choiceofperson),
            best=first(best),
            popwt=first(popwt),
            lassorpfl = first(lassorpfl),
            statquo = first(statquo) - last(statquo),
            mabsdeaths = first(mabsdeaths)-last(mabsdeaths),
            mabscases = first(mabscases)-last(mabscases),
            feduinoneavcost = first(feduinoneavcost) - last(feduinoneavcost), 
            feduianyavcost = first(feduianyavcost) - last(feduianyavcost), 
            feduinoneunempl = first(feduinoneunempl) - last(feduinoneunempl), 
            feduianyunempl = first(feduianyunempl) - last(feduianyunempl), 
            rule1 = first(rule1) - last(rule1),
            rule2 = first(rule2) - last(rule2), 
            rule3 = first(rule3) - last(rule3), 
            rule4 = first(rule4) - last(rule4), 
            rule5 = first(rule5) - last(rule5), 
            rule6 = first(rule6) - last(rule6), 
            rule7 = first(rule7) - last(rule7), 
            rule8 = first(rule8) - last(rule8), 
            rule9 = first(rule9) - last(rule9), 
            rule10 = first(rule10) - last(rule10),
            female=first(female),
            ideolcon=first(ideolcon),
            ideollib=first(ideollib))



summary(glm(best ~ mabsdeaths + mabscases +
              feduinoneavcost + feduianyavcost + 
              feduinoneunempl + feduianyunempl + 
              
              rule1 + rule2 +
              rule3 + rule4 +
              rule5 + rule6 +
              rule7 + rule8 +
              rule9 + rule10 +
              
              statquo +
              
              mabsdeaths:lassorpfl + mabscases:lassorpfl +
              feduinoneavcost:lassorpfl + feduianyavcost:lassorpfl + 
              feduinoneunempl:lassorpfl + feduianyunempl:lassorpfl + 
              
              rule1:lassorpfl + rule2:lassorpfl +
              rule3:lassorpfl + rule4:lassorpfl +
              rule5:lassorpfl + rule6:lassorpfl +
              rule7:lassorpfl + rule8:lassorpfl +
              rule9:lassorpfl + rule10:lassorpfl +
              
              statquo:lassorpfl + 0, weights=popwt, family = binomial(link = "logit"), 
            data = dat2))


dat2$idk <- rep(lcmodel$prob.ind,each=2)

summary(lm(idk ~ female + ideolcon + ideollib,data=dat2))


plot_ci_lc(lcmodel, var = c("feduinoneavcost"))

s %*% lcmodel$coefficients[which(str_detect(names(lcmodel$coefficients),"feduinoneavcost"))]

WTPs <- -lcmodel$coefficients[which(str_detect(names(lcmodel$coefficients),"mabsdeaths"))]/lcmodel$coefficients[which(str_detect(names(lcmodel$coefficients),"feduinoneavcost"))]
WTPs %*% s
WTPs

summary(lm(avcost ~ unempl*countyname,data=dat[which(dat$alt!=3),]))

############## Ordered logit, politics ~ other sociodemos #####################

forolo <- dat %>% group_by(ResponseId) %>% summarise(ideol2=first(ideol2),
                                                     gender=first(gender),
                                                     age=first(age),
                                                     income=first(income),
                                                     race=first(race),
                                                     education=first(education))
forolo$ideol2 <- relevel(forolo$ideol2, ref = "moderate")
forolo$age <- relevel(forolo$age, ref = "35to64")
forolo$race <- relevel(forolo$race, ref = "White")



a <- multinom(ideol2 ~ age + gender + race + income + race + education,data=forolo)

summary(a)
summary(multinom(ideol2 ~ age,data=forolo))
summary(lm(I(ideol2=="conservative") ~ gender,data=forolo))

stargazer(a,type="text")

forolo %>% group_by(income,ideol2) %>% summarise(n=n())
