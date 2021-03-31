require(pacman)

p_load(rpart,rpart.plot,readr,dplyr,RCurl,rjson,lubridate,
       rvest,stringr,Hmisc,rattle,RColorBrewer,ddpcr,tidytext,tidyr,
       ggrepel,ggplot2,png,ggpubr,tidycensus,sf,plm,lmtest,stargazer,MASS,
       xtable,knitr,magick,purrr,ggthemes,gifski,extrafont,latex2exp,
       cowplot,mapproj,patchwork,remotes,tictoc,Hmisc,english,readstata13,margins)

#################### READ IN DATA ################
{
  
  newdemean <- read.csv("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID-shared/intermediate-files/demeanrp-lasso.csv")[,-1]
  cmatch <- read.csv("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID-shared/intermediate-files/countymatch.csv")[,-1]
  dat <- read.csv("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_nointx.csv") %>% 
    # dplyr::filter(rejectonly==0) %>% 
    dplyr::filter(Durationinseconds > 360) %>% 
    dplyr::filter(choiceofperson %in% 1:2) %>% 
    group_by(ResponseId) %>% 
    mutate(rejectever = sum(rejectonly)) %>% 
    ungroup() %>% 
    dplyr::filter(rejectever==0) %>% 
    dplyr::select(-rejectever)
  
  dat2 <- read.dta13("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_nointx.dta")
  
  ethnic <- read.csv("~/covid-survey/data/countyethfrac.csv")[,-1]
  
  varlabs <- data.frame(variable = names(dat),label= attributes(dat2)$var.labels)
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
  
  dat$mabscases <- dat$mabscases/1000
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
  
  
  
}


####################################################
##################### Main results #################
####################################################




fixnames2 <- function (x) {
  
  x <- paste(x,collapse="\n")
  
  
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


#run regressions
{
  
  mainwith <- clogit(best ~
                    mabsdeaths*lassorpfl + mabscases*lassorpfl +
                    feduinoneavcost*lassorpfl + feduianyavcost*lassorpfl + 
                    feduinoneunempl*lassorpfl + feduianyunempl*lassorpfl + 
                    
                    rule1*lassorpfl + rule2*lassorpfl +
                    rule3*lassorpfl + rule4*lassorpfl +
                    rule5*lassorpfl + rule6*lassorpfl +
                    rule7*lassorpfl + rule8*lassorpfl +
                    rule9*lassorpfl + rule10*lassorpfl +
                    
                    statquo*lassorpfl +
                    strata(choice),data=dat[which(dat$choiceofperson %in% 1:2),]
                  ,weights=popwt,method="approximate"
  )
  
  mainwithout <- clogit(best ~
                          mabsdeaths + mabscases +
                          feduinoneavcost + feduianyavcost + 
                          feduinoneunempl + feduianyunempl + 
                          
                          rule1 + rule2 +
                          rule3 + rule4 +
                          rule5 + rule6 +
                          rule7 + rule8 +
                          rule9 + rule10 +
                          
                          statquo +
                          strata(choice),data=dat[which(dat$choiceofperson %in% 1:2),]
                        ,weights=popwt,method="approximate"
  )
  
}



# reorder table
{
  vars.order <- c("mabsdeaths", "mabsdeaths:lassorpfl", 
                  "mabscases", "lassorpfl:mabscases",
                  "feduinoneavcost", "lassorpfl:feduinoneavcost", 
                  "feduianyavcost", "lassorpfl:feduianyavcost",
                  "feduinoneunempl", "lassorpfl:feduinoneunempl",
                  "feduianyunempl",  "lassorpfl:feduianyunempl",
                  
                  "rule1", "lassorpfl:rule1",
                  "rule2" ,  "lassorpfl:rule2" , 
                  "rule3",  "lassorpfl:rule3",  
                  "rule4",  "lassorpfl:rule4",  
                  "rule5" , "lassorpfl:rule5" ,
                  "rule6" ,  "lassorpfl:rule6" ,
                  "rule7",   "lassorpfl:rule7",   
                  "rule8" ,  "lassorpfl:rule8" ,
                  "rule9" , "lassorpfl:rule9" ,
                  "rule10" ,  "lassorpfl:rule10" , 
                  "statquo", "lassorpfl:statquo")
  
  var.omit <- c("^lassorpfl$","^feduiany$","^factor\\(fedui\\)100$","^factor\\(fedui\\)200$","^factor\\(fedui\\)300$","^factor\\(fedui\\)400$")
  
}

# use robust SEs
{
 # vo2 <- data.frame(term=vars.order,y=1:length(vars.order))
  
  se1 <- summary(mainwith)$coefficients %>% as.data.frame() %>% rownames_to_column(var="term") %>% 
   # dplyr::filter(term!="lassorpfl") %>% 
    dplyr::select(`robust se`) %>% unlist() %>% as.numeric()
  
 # se1 <- left_join(se1,vo2)
#  se1 <- arrange(se1,y) %>% 
    
  
  se2 <- summary(mainwithout)$coefficients %>% as.data.frame() %>% rownames_to_column(var="term") %>% 
    dplyr::select(`robust se`) %>% unlist() %>% as.numeric()
  
  
  selist <- list(se2,se1)
  
}
# get N respondents and N choices
{
  p <- dat %>% dplyr::select(ResponseId,choiceofperson) %>% dplyr::filter(choiceofperson %in% 1:2) %>% unique()
  
  npeople <- length(unique(p$ResponseId))
  
  nchoices <- nrow(p)
  
  ns <- paste0("Respondents & ",npeople," & ",npeople,"\\\\\\\\ \n Choices & ",nchoices," & ",nchoices,"\\\\\\\\")
  
  
}

maintabs <- stargazer(mainwithout,mainwith,
                      type="latex",
                      title="Outcome model with selection corrections",
                      omit=var.omit,
                      order=paste0("^", vars.order , "$"),
                      se=selist,
                      keep.stat="n",
                      digits.extra = 10,
                      no.space = T)
maintable <- maintabs %>% str_replace_all("mabsdeaths:lassorpfl|lassorpfl:.{0,20}? ","$\\\\quad \\\\times$ Response propensity  ") %>%  
  fixnames2() %>% 
  str_replace("centering", "centering \n \\\\scriptsize") %>% 
  str_replace("Observations.{0,1000}\\\\\\\\",ns) 

cat(maintable)
write(maintable,file="~/covid-survey/tables/withandwithoutrp.bib")
