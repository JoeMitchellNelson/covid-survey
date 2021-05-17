require(pacman)

p_load(rpart,rpart.plot,readr,dplyr,RCurl,rjson,lubridate,
       rvest,stringr,Hmisc,rattle,RColorBrewer,ddpcr,tidytext,tidyr,
       ggrepel,ggplot2,png,ggpubr,tidycensus,sf,plm,lmtest,stargazer,MASS,
       xtable,knitr,magick,purrr,ggthemes,gifski,extrafont,latex2exp,
       cowplot,mapproj,patchwork,remotes,tictoc,Hmisc,english,readstata13,margins,lmtest,fastDummies)

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
  
  dat$mabscases <- dat$mabscases/10
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

# take 200 wpm to be average reading speed
# this page had the following text (152 words):
"During the first part of the current pandemic, there was an extra unemployment benefit of $600 per week
from the Federal government under the CARES Act. These benefits made the pandemic's “Average
$/month lost” much lower than they would normally be, for any given level of unemployment.
The $600/week extra benefit ended July 31. A $300/week extra benefit was then provided in December. The
incoming Administration is proposing $400/week. It is not yet clear whether extra unemployment benefits will
continue to be available, at what level, or for how long, as the pandemic drags on.
You will be shown policies that assume different levels of extra Federal unemployment benefits. These extra
benefits explain why policies with similar levels of unemployment can result in different “Average $/month
lost” in your county. Assume that any Federal unemployment benefits, as described, will be in place
regardless of any pandemic rules that apply in Solano County." 

### % of average reading speed | time to read ####
###          50                |    22.8 s
###         100                |    45.6 s
###         200                |    91.2 s

# censor at 91.2 seconds (likely indicates respondent was distracted)
dat$tQ171_censored <- ifelse(dat$tQ171 > 91.2,91.2,dat$tQ171)

fixnames2 <- function (x) {
  
  x <- paste(x,collapse="\n")
  
  x <- str_replace_all(x,"tQ171_censored","Time on page (censored)")
  
  x <- str_replace_all(x,"tQ171","Time on page")

  for (i in nrow(varlabs):1) {
    if(str_detect(x,paste0("",varlabs$variable[i]))) {
      x <- str_replace_all(x,varlabs$variable[i],varlabs$label[i])
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

# preferred specification for comparison
main3b <- clogit(best ~
                   mabsdeaths*lassorpfl + mabscases*lassorpfl +
                   feduinoneavcost*lassorpfl + feduianyavcost*lassorpfl + 
                   feduinoneunempl*lassorpfl + feduianyunempl*lassorpfl + 
                   
                   factor(rule1)*lassorpfl + factor(rule2)*lassorpfl +
                   factor(rule3)*lassorpfl + factor(rule4)*lassorpfl +
                   factor(rule5)*lassorpfl + factor(rule6)*lassorpfl +
                   factor(rule7)*lassorpfl + factor(rule8)*lassorpfl +
                   factor(rule9)*lassorpfl + factor(rule10)*lassorpfl +
                   
                   statquo*lassorpfl +
                   strata(choice),data=dat[which(dat$choiceofperson %in% 1:2),]
                 ,weights=popwt,method="approximate"
)

# interactions with no censoring, binning, etc.
reg1 <- clogit(best ~
                   mabsdeaths*lassorpfl + mabscases*lassorpfl +
                   feduinoneavcost*lassorpfl*tQ171 + feduianyavcost*lassorpfl*tQ171 + 
                   feduinoneunempl*lassorpfl*tQ171 + feduianyunempl*lassorpfl*tQ171 + 
                   
                   factor(rule1)*lassorpfl + factor(rule2)*lassorpfl +
                   factor(rule3)*lassorpfl + factor(rule4)*lassorpfl +
                   factor(rule5)*lassorpfl + factor(rule6)*lassorpfl +
                   factor(rule7)*lassorpfl + factor(rule8)*lassorpfl +
                   factor(rule9)*lassorpfl + factor(rule10)*lassorpfl +
                   
                   statquo*lassorpfl +
                   strata(choice),data=dat[which(dat$choiceofperson %in% 1:2),]
                 ,weights=popwt,method="approximate"
)

# top censored at 91.2 seconds
reg2 <- clogit(best ~
                 mabsdeaths*lassorpfl + mabscases*lassorpfl +
                 feduinoneavcost*lassorpfl*tQ171_censored + feduianyavcost*lassorpfl*tQ171_censored + 
                 feduinoneunempl*lassorpfl*tQ171_censored + feduianyunempl*lassorpfl*tQ171_censored + 
                 
                 factor(rule1)*lassorpfl + factor(rule2)*lassorpfl +
                 factor(rule3)*lassorpfl + factor(rule4)*lassorpfl +
                 factor(rule5)*lassorpfl + factor(rule6)*lassorpfl +
                 factor(rule7)*lassorpfl + factor(rule8)*lassorpfl +
                 factor(rule9)*lassorpfl + factor(rule10)*lassorpfl +
                 
                 statquo*lassorpfl +
                 strata(choice),data=dat[which(dat$choiceofperson %in% 1:2),]
               ,weights=popwt,method="approximate"
)


# binned above and below 22.8 seconds (less than 22.8 may indicate "skimming" or faster)
reg3 <- clogit(best ~
                 mabsdeaths*lassorpfl + mabscases*lassorpfl +
                 feduinoneavcost*lassorpfl*I(tQ171<22.8) + feduianyavcost*lassorpfl*I(tQ171<22.8) + 
                 feduinoneunempl*lassorpfl*I(tQ171<22.8) + feduianyunempl*lassorpfl*I(tQ171<22.8) + 
                 
                 factor(rule1)*lassorpfl + factor(rule2)*lassorpfl +
                 factor(rule3)*lassorpfl + factor(rule4)*lassorpfl +
                 factor(rule5)*lassorpfl + factor(rule6)*lassorpfl +
                 factor(rule7)*lassorpfl + factor(rule8)*lassorpfl +
                 factor(rule9)*lassorpfl + factor(rule10)*lassorpfl +
                 
                 statquo*lassorpfl +
                 strata(choice),data=dat[which(dat$choiceofperson %in% 1:2),]
               ,weights=popwt,method="approximate"
)

# binned above and below 11.519 seconds (the median time for this page)
reg4 <- clogit(best ~
                 mabsdeaths*lassorpfl + mabscases*lassorpfl +
                 feduinoneavcost*lassorpfl*I(tQ171<11.519) + feduianyavcost*lassorpfl*I(tQ171<11.519) + 
                 feduinoneunempl*lassorpfl*I(tQ171<11.519) + feduianyunempl*lassorpfl*I(tQ171<11.519) + 
                 
                 factor(rule1)*lassorpfl + factor(rule2)*lassorpfl +
                 factor(rule3)*lassorpfl + factor(rule4)*lassorpfl +
                 factor(rule5)*lassorpfl + factor(rule6)*lassorpfl +
                 factor(rule7)*lassorpfl + factor(rule8)*lassorpfl +
                 factor(rule9)*lassorpfl + factor(rule10)*lassorpfl +
                 
                 statquo*lassorpfl +
                 strata(choice),data=dat[which(dat$choiceofperson %in% 1:2),]
               ,weights=popwt,method="approximate"
)

# binned above and below 2 seconds (not plausible)
reg5 <- clogit(best ~
                 mabsdeaths*lassorpfl + mabscases*lassorpfl +
                 feduinoneavcost*lassorpfl*I(tQ171<2) + feduianyavcost*lassorpfl*I(tQ171<2) + 
                 feduinoneunempl*lassorpfl*I(tQ171<2) + feduianyunempl*lassorpfl*I(tQ171<2) + 
                 
                 factor(rule1)*lassorpfl + factor(rule2)*lassorpfl +
                 factor(rule3)*lassorpfl + factor(rule4)*lassorpfl +
                 factor(rule5)*lassorpfl + factor(rule6)*lassorpfl +
                 factor(rule7)*lassorpfl + factor(rule8)*lassorpfl +
                 factor(rule9)*lassorpfl + factor(rule10)*lassorpfl +
                 
                 statquo*lassorpfl +
                 strata(choice),data=dat[which(dat$choiceofperson %in% 1:2),]
               ,weights=popwt,method="approximate"
)

reg6 <- clogit(best ~
                 mabsdeaths*lassorpfl + mabscases*lassorpfl +
                 feduinoneavcost*lassorpfl*log(tQ171) + feduianyavcost*lassorpfl*log(tQ171) + 
                 feduinoneunempl*lassorpfl*log(tQ171) + feduianyunempl*lassorpfl*log(tQ171) + 
                 
                 factor(rule1)*lassorpfl + factor(rule2)*lassorpfl +
                 factor(rule3)*lassorpfl + factor(rule4)*lassorpfl +
                 factor(rule5)*lassorpfl + factor(rule6)*lassorpfl +
                 factor(rule7)*lassorpfl + factor(rule8)*lassorpfl +
                 factor(rule9)*lassorpfl + factor(rule10)*lassorpfl +
                 
                 statquo*lassorpfl +
                 strata(choice),data=dat[which(dat$choiceofperson %in% 1:2),]
               ,weights=popwt,method="approximate"
)

reg7 <- clogit(best ~
                 mabsdeaths*lassorpfl + mabscases*lassorpfl +
                 feduinoneavcost*lassorpfl*log(tQ171_censored) + feduianyavcost*lassorpfl*log(tQ171_censored) + 
                 feduinoneunempl*lassorpfl*log(tQ171_censored) + feduianyunempl*lassorpfl*log(tQ171_censored) + 
                 
                 factor(rule1)*lassorpfl + factor(rule2)*lassorpfl +
                 factor(rule3)*lassorpfl + factor(rule4)*lassorpfl +
                 factor(rule5)*lassorpfl + factor(rule6)*lassorpfl +
                 factor(rule7)*lassorpfl + factor(rule8)*lassorpfl +
                 factor(rule9)*lassorpfl + factor(rule10)*lassorpfl +
                 
                 statquo*lassorpfl +
                 strata(choice),data=dat[which(dat$choiceofperson %in% 1:2),]
               ,weights=popwt,method="approximate"
)

small <- dat %>% dplyr::select(ResponseId,tQ171,tQ171_censored) %>% unique()

stargazer(reg1,type="text",keep="fedui") # looks good
stargazer(reg2,type="text",keep="fedui") # looks good
stargazer(reg3,type="text",keep="fedui") # possible issues
stargazer(reg4,type="text",keep="fedui") # possible issues
stargazer(reg5,type="text",keep="fedui") # looks good
stargazer(reg6,type="text",keep="fedui") # looks good
stargazer(reg7,type="text",keep="fedui") # looks good


vars.order <- c("mabsdeaths",  "mabscases", "avcost", 
                "feduinoneavcost", "feduianyavcost",  "fedui100avcost", "fedui200avcost", "fedui300avcost","fedui400avcost",
                "unempl",
                "feduinoneunempl", "feduianyunempl", "fedui100unempl", "fedui200unempl", "fedui300unempl", "fedui400unempl",
                "tQ171","tQ171_censored",
                
                "statquo" )

var.omit <- c("lassorpfl","^feduiany$","^factor\\(fedui\\)100$","^factor\\(fedui\\)200$",
              "^factor\\(fedui\\)300$","^factor\\(fedui\\)400$","rule","^tQ171$","^tQ171_censored$")

stargazer(main3b,reg1,reg2,type="latex",
               omit=var.omit,
               order=paste0("^", vars.order , "$"))


ggplot(small) +
  geom_density(aes(x=tQ171_censored))
