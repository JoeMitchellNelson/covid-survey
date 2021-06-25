require(pacman)

p_load(rpart,rpart.plot,readr,dplyr,RCurl,rjson,lubridate,
       rvest,stringr,Hmisc,rattle,RColorBrewer,ddpcr,tidytext,tidyr,
       ggrepel,ggplot2,png,ggpubr,tidycensus,sf,plm,lmtest,stargazer,MASS,
       xtable,knitr,magick,purrr,ggthemes,gifski,extrafont,latex2exp,
       cowplot,mapproj,patchwork,remotes,tictoc,Hmisc,english,readstata13,margins,lmtest,fastDummies)

#################### READ IN DATA ################
{
  
  newdemean <- read.csv("C:/Users/joe/Dropbox (University of Oregon)/VSL-COVID-shared/intermediate-files/demeanrp-lasso.csv")[,-1]
  cmatch <- read.csv("C:/Users/joe/Dropbox (University of Oregon)/VSL-COVID-shared/intermediate-files/countymatch.csv")[,-1]
  dat <- read.csv("C:/Users/joe/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_nointx.csv") %>% 
    # dplyr::filter(rejectonly==0) %>% 
    dplyr::filter(Durationinseconds > 360) %>% 
    dplyr::filter(choiceofperson %in% 1:2) %>% 
    # dplyr::filter(rejectonly==0) %>% 
    group_by(ResponseId) %>% 
    mutate(rejectever = sum(rejectonly)) %>% 
    ungroup() %>% 
    dplyr::filter(rejectever==0) 
  
  dat$owninc <- ifelse(dat$owninc==0,NA,dat$owninc)
  
  
  dat2 <- read.dta13("C:/Users/joe/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_nointx.dta")
  
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

  dat$income2 <- ifelse(dat$inccont<dat$zipmhhinc,"Poor","Rich") %>% as.factor()
  
  dat$wrong <- ifelse(dat$wrongsamecost == 1 | dat$wrongexactnum == 1,"Wrong","Right") %>% as.factor()
  
  dat <- dat %>% mutate(worstunempl = pmax(unempr20201,unempr20202,unempr20203,unempr20204,unempr20205,unempr20206,
                                           unempr20207,unempr20208,unempr20209,unempr202010,unempr202011,unempr202012))
  
  dat$relunemp <- ifelse(dat$worstunempl > median(dat$worstunempl),"WorseUnemp","BetterUnemp") %>% as.factor()
  
}


####### functions to format data #############
# 
df = dat[which(dat$ideol2 != "MISSING" & dat$choiceofperson %in% 1:2),]
fac="ideol2"

getsummary <- function (fac,df) {
  
  
  if (fac=="ideol2") {
    df <- df %>% dplyr::filter(ideol2!="MISSING")
  }
  
  df <- df %>% dplyr::filter(!is.na(eval(as.symbol(fac)))) %>% droplevels()
  
  
  levs <- df[,which(names(df)==fac)] %>%  unique()
  levs <- summary(levs[,1])[,1] %>% str_remove_all(":1") %>% trimws()
  levs <- levs %>% str_remove_all(" ")
  
  
  
  if (fac=="education") {
    levs <- c("Noncollege","College")
  }
  
  
  bv <- c("mabsdeaths","mabscases",
          "feduianyavcost","feduinoneavcost",
          "feduianyunempl","feduinoneunempl",
          "rule1.1",           "rule1.2",          
          "rule2.1",           "rule2.2",           "rule2.3",              "rule3.1",           "rule3.2",          
          "rule3.3",            "rule4.1",           "rule4.2",           "rule4.3",               "rule5.1",          
          "rule5.2",           "rule5.3",              "rule6.1",           "rule6.2",           "rule6.3",           
          "rule7.1" ,          "rule7.2",           "rule7.3",              "rule8.1",           "rule8.2" ,      "rule8.3",          
          "rule9.1",           "rule9.2",           "rule9.3",             "rule10.1" ,         "rule10.2",         
          "rule10.3",         
          "statquo")
  
  newdf <- df %>% dplyr::select("best","choice",all_of(bv),all_of(fac),"popwt","popwt2","lassorpfl")
  
  allv <- c()
  for (i in 1:length(levs)) {
    allv <- c(allv,paste0(bv,"_",levs[i]))
  }
  
  for (i in 1:length(allv)) {
    a <- allv[i] %>% str_split_fixed("_",2)
    newdf$newvar <- (newdf[,which(names(newdf)==a[,1])] * ifelse(newdf[,which(names(newdf)==fac)]==a[,2],1,0)) %>% unlist %>%  as.numeric()
    names(newdf)[which(names(newdf)=="newvar")] <- allv[i]
  }
  
  form1 <- paste0(allv,"*lassorpfl")
  form2 <- paste(form1,collapse=" + ")
  
  

  form3 <- paste0("best ~",form2,"+ strata(choice)") %>% as.formula
  
  summary(clogit(form3,data=newdf,weights=popwt,method="approximate"))
}


maketable <- function (fac,df,drop=c("MISSING")) {
  
  df <- df %>% dplyr::filter(!is.na(eval(as.symbol(fac)))) %>% droplevels()
  
  s <- getsummary(fac,df)
  
  levs <- df[,which(names(df)==fac)] %>%  unique()
  levs <- summary(levs[,1])[,1] %>% str_remove_all(":1") %>% trimws()
  levs <- levs %>% str_remove_all(" ")
  
  p <- df[,which(names(df) %in% c("ResponseId",fac))] %>% unique()
  
  npeople <- c()
  
  for (i in 1:length(levs)) {
    npeople <- c(npeople,sum(p[,2]==levs[i]))
  }
  
  p <- df[,which(names(df) %in% c("ResponseId",fac))]
  
  nchoices <- c()
  
  for (i in 1:length(levs)) {
    nchoices <- c(nchoices,sum(p[,2]==levs[i]))
  }
  
  nchoices <- nchoices/2
  
  if (length(levs)==3) {
    
    ns <- paste0("Respondents & ",npeople[1], " & ",npeople[2]," & ",npeople[3],"\\\\\\\\ \n Choices & ",nchoices[1], " & ",nchoices[2]," & ",nchoices[3],"\\\\\\\\")
    
  } else {
    ns <- paste0("Respondents & ",npeople[1], " & ",npeople[2],"\\\\\\\\ \n Choices & ",nchoices[1], " & ",nchoices[2],"\\\\\\\\")
    
  }
  
  t <- s$coefficients %>% as.data.frame() %>% rownames_to_column(var="term") %>% 
    dplyr::filter(! str_detect(term,"lassorpfl"))
  
  modelnames <- c()
  titles <- c()
  
  for (i in 1:length(levs)) {
    if (!levs[i] %in% drop) {
      tempcoef <- t %>% dplyr::filter(str_detect(term,levs[i]) )
      assign(paste0(levs[i],"coef"),tempcoef)
      modelnames <- c(modelnames,paste0(levs[i],"coef"))
      titles <- c(titles,levs[i])
    }
  }
  
  
  fake <- lm(best ~ mabsdeaths + mabscases + feduianyavcost + feduinoneavcost + feduianyunempl + feduinoneunempl +
                statquo + 0,data = df)
  
  vars.order <- c("mabsdeaths",  "mabscases", "feduinoneavcost",  "feduianyavcost",  
                  "feduinoneunempl",  "feduianyunempl",  
                  "statquo" )
  
  
  mods <- list()
  for (i in 1:length(modelnames)) {mods[[i]] <- fake}
  
  coeflist <- list()
  for (i in 1:length(modelnames)) {coeflist[[i]] <- (modelnames[i] %>% as.symbol() %>% eval())$coef}
  
  selist <- list()
  for (i in 1:length(modelnames)) {selist[[i]] <- (modelnames[i] %>% as.symbol() %>% eval())$`robust se`}
  
  
  b <- stargazer(mods,type="latex",title=str_to_title(fac),
                 # omit=":lassorpfl",
                 coef = coeflist,
                 # standard errors
                 se = selist,
                 order=paste0("^", vars.order , "$"),
                 column.labels = str_to_title(titles),
                 keep.stat="n",
                 # digits.extra = 5,
                 no.space = T,
                 t.auto = T) %>% 
    str_replace("Observations.{0,1000}\\\\\\\\",ns)
  
  paste(b,collapse="\n")
  
}


fixnames <- function (x) {
  
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
  
  x
  
}

ideoltable <- maketable(fac="ideol2", 
                        df=dat[which(dat$choiceofperson %in% 1:2 & dat$ideol2 != "MISSING"),], 
                        drop="MISSING") %>% 
  fixnames() %>% 
  str_replace_all("Ideol2","Ideology")
ideoltable %>% cat()

write(ideoltable,file="~/covid-survey/tables/ideologytable_newint.tex")

gendertable <- maketable(fac="gender", 
                         df=dat[which(dat$choiceofperson %in% 1:2 & !dat$gender %in% c("MISSING","NonBinary")),], 
                         drop=c("MISSING","NonBinary")) %>% 
  fixnames() 

cat(gendertable)

write(gendertable,file="~/covid-survey/tables/gendertable_newint.tex")


agetable <- maketable(fac="age", 
                      df=dat[which(dat$choiceofperson %in% 1:2),],
                      drop=NA) %>% 
  fixnames() %>% 
  str_replace_all("18to34","18 to 34") %>% 
  str_replace_all("35to64","35 to 64") %>% 
  str_replace_all("65andolder","65 +")

cat(agetable)

write(agetable,file="~/covid-survey/tables/agetable_newint.tex")


racetable <- maketable(fac="race", 
                       df=dat[which(dat$choiceofperson %in% 1:2),], 
                       drop=NA) %>% 
  fixnames() %>% 
  str_replace_all("Other","Non-white")
write(racetable,file="~/covid-survey/tables/racetable_newint.tex")

eductable <- maketable(fac="education", 
                       df=dat[which(dat$choiceofperson %in% 1:2 & dat$education != "MISSING"),], 
                       drop="MISSING") %>% 
  fixnames() 

write(eductable,file="~/covid-survey/tables/educationtable_newint.tex")


incometable <- maketable(fac="income", 
                         df=dat[which(dat$choiceofperson %in% 1:2),], 
                         drop=NA) %>% 
  fixnames()


cat(incometable)
write(incometable,file="~/covid-survey/tables/incometable_newint.tex")


income2table <- maketable(fac="income2", 
                         df=dat[which(dat$choiceofperson %in% 1:2),], 
                         drop=NA) %>% 
  fixnames()


cat(income2table)
write(income2table,file="~/covid-survey/tables/income2table_newint.tex")


wrongtable <- maketable(fac="wrong", 
                          df=dat[which(dat$choiceofperson %in% 1:2),], 
                          drop=NA) %>% 
  fixnames()


cat(wrongtable)
write(wrongtable,file="~/covid-survey/tables/wrongtable_newint.tex")

unemptable <- maketable(fac="relunemp", 
                        df=dat[which(dat$choiceofperson %in% 1:2),], 
                        drop=NA) %>% 
  fixnames()

cat(unemptable)

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
  main1 <- clogit(best ~
                    mabsdeaths*lassorpfl + mabscases*lassorpfl +
                    unempl*lassorpfl + avcost*lassorpfl +
                    
                    rule1*lassorpfl + rule2*lassorpfl +
                    rule3*lassorpfl + rule4*lassorpfl +
                    rule5*lassorpfl + rule6*lassorpfl +
                    rule7*lassorpfl + rule8*lassorpfl +
                    rule9*lassorpfl + rule10*lassorpfl +
                    
                    statquo*lassorpfl +
                    strata(choice),data=dat[which(dat$choiceofperson %in% 1:2),]
                  ,weights=popwt,method="approximate"
  )
  
  main3 <- clogit(best ~
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
  
  main2 <- clogit(best ~
                    mabsdeaths*lassorpfl + mabscases*lassorpfl +
                    feduinoneavcost*lassorpfl + fedui100avcost*lassorpfl + fedui200avcost*lassorpfl + fedui300avcost*lassorpfl + fedui400avcost*lassorpfl +
                    feduinoneunempl*lassorpfl + fedui100unempl*lassorpfl + fedui200unempl*lassorpfl + fedui300unempl*lassorpfl + fedui400unempl*lassorpfl +
                    
                    rule1*lassorpfl + rule2*lassorpfl +
                    rule3*lassorpfl + rule4*lassorpfl +
                    rule5*lassorpfl + rule6*lassorpfl +
                    rule7*lassorpfl + rule8*lassorpfl +
                    rule9*lassorpfl + rule10*lassorpfl +
                    
                    statquo*lassorpfl +
                    strata(choice),data=dat[which(dat$choiceofperson %in% 1:2),]
                  ,weights=popwt,method="approximate"
  )
  
  main1b <- clogit(best ~
                    mabsdeaths*lassorpfl + mabscases*lassorpfl +
                    unempl*lassorpfl + avcost*lassorpfl +
                     
                     factor(rule1)*lassorpfl + factor(rule2)*lassorpfl +
                     factor(rule3)*lassorpfl + factor(rule4)*lassorpfl +
                     factor(rule5)*lassorpfl + factor(rule6)*lassorpfl +
                     factor(rule7)*lassorpfl + factor(rule8)*lassorpfl +
                     factor(rule9)*lassorpfl + factor(rule10)*lassorpfl +
                    
                    statquo*lassorpfl +
                    strata(choice),data=dat[which(dat$choiceofperson %in% 1:2),]
                  ,weights=popwt,method="approximate"
  )
  
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
  
  main2b <- clogit(best ~
                    mabsdeaths*lassorpfl + mabscases*lassorpfl +
                    feduinoneavcost*lassorpfl + fedui100avcost*lassorpfl + fedui200avcost*lassorpfl + fedui300avcost*lassorpfl + fedui400avcost*lassorpfl +
                    feduinoneunempl*lassorpfl + fedui100unempl*lassorpfl + fedui200unempl*lassorpfl + fedui300unempl*lassorpfl + fedui400unempl*lassorpfl +
                    
                     factor(rule1)*lassorpfl + factor(rule2)*lassorpfl +
                     factor(rule3)*lassorpfl + factor(rule4)*lassorpfl +
                     factor(rule5)*lassorpfl + factor(rule6)*lassorpfl +
                     factor(rule7)*lassorpfl + factor(rule8)*lassorpfl +
                     factor(rule9)*lassorpfl + factor(rule10)*lassorpfl +
                    
                    statquo*lassorpfl +
                    strata(choice),data=dat[which(dat$choiceofperson %in% 1:2),]
                  ,weights=popwt,method="approximate"
  )
  
}



# reorder table
{
  vars.order <- c("mabsdeaths",  "mabscases", "avcost", 
                  "feduinoneavcost", "feduianyavcost",  "fedui100avcost", "fedui200avcost", "fedui300avcost","fedui400avcost",
                  "unempl",
                  "feduinoneunempl", "feduianyunempl", "fedui100unempl", "fedui200unempl", "fedui300unempl", "fedui400unempl",
                  
  
                  "statquo" )
  
  var.omit <- c("lassorpfl","^feduiany$","^factor\\(fedui\\)100$","^factor\\(fedui\\)200$",
                "^factor\\(fedui\\)300$","^factor\\(fedui\\)400$","rule")
  
}

# use robust SEs
{
  se1 <- summary(main1)$coefficients %>% as.data.frame() %>% rownames_to_column(var="term") %>% 
    dplyr::filter(! str_detect(term,":lassorpfl|lassorpfl:")) %>% dplyr::select(`robust se`) %>% unlist() %>% as.numeric()
  
  se2 <- summary(main2)$coefficients %>% as.data.frame() %>% rownames_to_column(var="term") %>% 
    dplyr::filter(! str_detect(term,":lassorpfl|lassorpfl:")) %>% dplyr::select(`robust se`) %>% unlist() %>% as.numeric()
  
  se3 <- summary(main3)$coefficients %>% as.data.frame() %>% rownames_to_column(var="term") %>% 
    dplyr::filter(! str_detect(term,":lassorpfl|lassorpfl:")) %>% dplyr::select(`robust se`) %>% unlist() %>% as.numeric()
  
  se1b <- summary(main1b)$coefficients %>% as.data.frame() %>% rownames_to_column(var="term") %>% 
    dplyr::filter(! str_detect(term,":lassorpfl|lassorpfl:")) %>% dplyr::select(`robust se`) %>% unlist() %>% as.numeric()
  
  se2b <- summary(main2b)$coefficients %>% as.data.frame() %>% rownames_to_column(var="term") %>% 
    dplyr::filter(! str_detect(term,":lassorpfl|lassorpfl:")) %>% dplyr::select(`robust se`) %>% unlist() %>% as.numeric()
  
  se3b <- summary(main3b)$coefficients %>% as.data.frame() %>% rownames_to_column(var="term") %>% 
    dplyr::filter(! str_detect(term,":lassorpfl|lassorpfl:")) %>% dplyr::select(`robust se`) %>% unlist() %>% as.numeric()
  
  selist <- list(se1,se2,se3,se1b,se2b,se3b)
  
}
# get N respondents and N choices
{
  p <- dat %>% dplyr::select(ResponseId,choiceofperson) %>% dplyr::filter(choiceofperson %in% 1:2) %>% unique()
  
  npeople <- length(unique(p$ResponseId))
  
  nchoices <- nrow(p)
  
  ns <- paste0("Respondents & ",npeople, " & ",npeople," & ",npeople," & ", npeople, " & ",npeople," & ",npeople,
               "\\\\\\\\ \n Choices & ", nchoices, " & ",nchoices," & ",nchoices, " & ", nchoices, " & ",nchoices," & ",nchoices,"\\\\\\\\")
  
  
}

# bottom text
{
  restr <- paste0("Restrictions (continuous) & $\\\\checkmark$ & $\\\\checkmark$ & $\\\\checkmark$ & & & \\\\\\\\ \n",
                  "Restrictions (indicators) & & & & $\\\\checkmark$ & $\\\\checkmark$ & $\\\\checkmark$ \\\\\\\\ \n",
                  "All response propensity interactions & $\\\\checkmark$ & $\\\\checkmark$ & $\\\\checkmark$ & $\\\\checkmark$ & $\\\\checkmark$ & $\\\\checkmark$ \\\\\\\\ \n",
                  "\\\\hline \\\\\\\\[-1.8ex]")


# log lik


  logliks <- paste0("\nLog likelihood & ",
                    round(main1$loglik[2],2)," & ",
                    round(main2$loglik[2],2)," & ",
                    round(main3$loglik[2],2)," & ",
                    round(main1b$loglik[2],2)," & ",
                    round(main2b$loglik[2],2)," & ",
                    round(main3b$loglik[2],2)," \\\\\\\\ ",
                    "\n")

# info criteria

AICs <- paste0("AIC & ",round(AIC(main1),2)," & ", round(AIC(main2),2)," & ", round(AIC(main3),2)," & ",
    round(AIC(main1b),2)," & ",round(AIC(main2b),2)," & ",round(AIC(main3b),2)," \\\\\\\\ \n")

BICs <- paste0("BIC & ",round(BIC(main1),2)," & ", round(BIC(main2),2)," & ", round(BIC(main3),2)," & ",
    round(BIC(main1b),2)," & ",round(BIC(main2b),2)," & ",round(BIC(main3b),2)," \\\\\\\\ \n")

}

maintabs <- stargazer(main1,main2,main3,main1b,main2b,main3b,
                      type="latex",
                      title="Effects of Federal UI",
                      omit=var.omit,
                      order=paste0("^", vars.order , "$"),
                      se=selist,
                      keep.stat="n",
                      digits.extra = 10,
                      no.space = T)
maintable <- fixnames2(maintabs) %>% str_replace("centering", "centering \n \\\\scriptsize") %>% 
  str_replace("Observations.{0,1000}\\\\\\\\",paste0(restr,ns,logliks,AICs,BICs))

cat(maintable)
write(maintable,file="~/covid-survey/tables/maintable_993_norestr.tex")


# use robust SEs BIG TABLE (APPENDIX)
{
  se1 <- summary(main1)$coefficients %>% as.data.frame() %>% rownames_to_column(var="term") %>% 
    dplyr::select(`robust se`) %>% unlist() %>% as.numeric()
  
  se2 <- summary(main2)$coefficients %>% as.data.frame() %>% rownames_to_column(var="term") %>% 
    dplyr::select(`robust se`) %>% unlist() %>% as.numeric()
  
  se3 <- summary(main3)$coefficients %>% as.data.frame() %>% rownames_to_column(var="term") %>% 
    dplyr::select(`robust se`) %>% unlist() %>% as.numeric()
  
  se1b <- summary(main1b)$coefficients %>% as.data.frame() %>% rownames_to_column(var="term") %>% 
    dplyr::select(`robust se`) %>% unlist() %>% as.numeric()
  
  se2b <- summary(main2b)$coefficients %>% as.data.frame() %>% rownames_to_column(var="term") %>% 
    dplyr::select(`robust se`) %>% unlist() %>% as.numeric()
  
  se3b <- summary(main3b)$coefficients %>% as.data.frame() %>% rownames_to_column(var="term") %>% 
     dplyr::select(`robust se`) %>% unlist() %>% as.numeric()
  
  selist <- list(se1,se2,se3,se1b,se2b,se3b)
  
}



maintabs2 <- stargazer(main1,main2,main3,main1b,main2b,main3b,
                      type="latex",
                      title="Effects of Federal UI",
                      omit="^lassorpfl$",
                     
                      order=paste0("^", vars.order , "$"),
                      se=selist,
                      keep.stat="n",
                      digits.extra = 10,
                      no.space = T)
maintable2 <- fixnames2(maintabs2) %>% str_replace("centering", "centering \n \\\\scriptsize") %>% 
  str_replace("Observations.{0,1000}\\\\\\\\",paste0(restr,ns,logliks)) %>% 
  str_replace_all("lassorpfl:","RP $\\\\times$ ") %>% str_replace_all("lassorpfl ","RP ")

cat(maintable2)

write(maintable2,file="~/covid-survey/tables/maintable_993_APPENDIX.tex")

###########################################################
############### with and without RP #######################
###########################################################

{

  mainwithoutrp <- clogit(best ~
                         mabsdeaths + mabscases +
                         feduinoneavcost + feduianyavcost +
                         feduinoneunempl + feduianyunempl +
                         
                         factor(rule1) + factor(rule2) +
                         factor(rule3) + factor(rule4) +
                         factor(rule5) + factor(rule6) +
                         factor(rule7) + factor(rule8) +
                         factor(rule9) + factor(rule10) +
                         
                         statquo +
                         strata(choice),data=dat[which(dat$choiceofperson %in% 1:2),]
                       ,weights=popwt,method="approximate"
  )
  
  mainwithrp <- clogit(best ~
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
  
}

vars.order <- c("mabsdeaths","mabsdeaths:lassorpfl",  "mabscases","lassorpfl:mabscases", 
                "feduinoneavcost","lassorpfl:feduinoneavcost", "feduianyavcost", "lassorpfl:feduianyavcost",
                "feduinoneunempl", "lassorpfl:feduinoneunempl", "feduianyunempl", "lassorpfl:feduianyunempl",
                "statquo", "lassorpfl:statquo")

rpcompare <- stargazer(mainwithoutrp,mainwithrp,
                       type="latex",
                       order=paste0("^", vars.order , "$"),
                       keep=paste0("^", vars.order , "$"),
                       keep.stat=c("n","ll"),
                       no.space=T)
rpcompare <- rpcompare %>% str_replace_all("lassorpfl:[:alpha:]{1,20} ","$\\\\quad \\\\times$ Response propensity ")  %>% fixnames2() 
cat(rpcompare)

write(rpcompare,"~/covid-survey/tables/rp_compare.tex")
############################################################
############################################################
######### MERGE HETEROGENEITY TABLES  ######################
############################################################
############################################################

df <- dat
fac <- c("age","race")
drop=NA



maketable3 <- function (fac,df,drop=c("MISSING")) {
  
  mods <- list()
  coeflist <- list()
  selist <- list()
  modelnames <- c()
  titles <- c()
  npeople <- c()
  nchoices <- c()
  
  
  
  for (j in 1:length(fac)) {
    
    df <- df %>% dplyr::filter(!is.na(eval(as.symbol(fac[j])))) %>% droplevels()
    
    s <- getsummary(fac[j],df)
    
    levs <- df[,which(names(df)==fac[j])] %>%  unique()
    levs <- summary(levs[,1])[,1] %>% str_remove_all(":1") %>% trimws()
    levs <- levs %>% str_remove_all(" ")
    
    p <- df[which(names(df) %in% c("ResponseId",fac[j]))] %>% unique()
    
    
    for (i in 1:length(levs)) {
      npeople <- c(npeople,sum(p[,2]==levs[i]))
    }
    
    p <- df[,which(names(df) %in% c("ResponseId",fac[j]))]
    
    
    for (i in 1:length(levs)) {
      nchoices <- c(nchoices,sum(p[,2]==levs[i]))
    }
    
    
    
    
    t <- s$coefficients %>% as.data.frame() %>% rownames_to_column(var="term") %>% 
      dplyr::filter(! str_detect(term,"lassorpfl"))
    
    
    
    for (i in 1:length(levs)) {
      if (!levs[i] %in% drop) {
        tempcoef <- t %>% dplyr::filter(str_detect(term,levs[i]) )
        assign(paste0(levs[i],"coef"),tempcoef)
        modelnames <- c(modelnames,paste0(levs[i],"coef"))
        titles <- c(titles,levs[i])
      }
    }
    
    
    fake <- lm(best ~ mabsdeaths + mabscases + feduianyavcost + feduinoneavcost + feduianyunempl + feduinoneunempl +
                 rule1a + rule2a + rule3a +rule4a +rule5a + 
                 rule6a + rule7a + rule8a + rule9a + rule10a +
                 
                 rule1b + rule2b + rule3b +rule4b +rule5b + 
                 rule6b + rule7b + rule8b + rule9b + rule10b +
                 
                 rule2c + rule3c +rule4c +rule5c + 
                 rule6c + rule7c + rule8c + rule9c + rule10c +
                  statquo + 0,data = df)
    
    vars.order <- c("mabsdeaths",  "mabscases", "feduinoneavcost",  "feduianyavcost",  
                    "feduinoneunempl",  "feduianyunempl", 
                    "statquo" )
    
    
    
    for (i in 1:length(modelnames)) {mods[[i]] <- fake}
    
    for (i in 1:length(modelnames)) {coeflist[[i]] <- (modelnames[i] %>% as.symbol() %>% eval())$coef}
    
    for (i in 1:length(modelnames)) {selist[[i]] <- (modelnames[i] %>% as.symbol() %>% eval())$`robust se`}
    
  }
  
  
  nchoices <- nchoices/2
  
  nchoices <- nchoices[which(npeople>200)]
  npeople <- npeople[which(npeople>200)]
  
  ns <- paste0("Respondents & ",npeople[1], " & ",npeople[2]," & ",npeople[3]," & ",npeople[4]," & ",npeople[5]," & ",npeople[6]," & ",npeople[7],
               "\\\\\\\\ \n Choices & ", nchoices[1], " & ",nchoices[2]," & ",nchoices[3]," & ",nchoices[4]," & ",nchoices[5]," & ",nchoices[6]," & ",nchoices[7],"\\\\\\\\")
  
  
  
  b <- stargazer(mods,type="latex",
                 omit="rule",
                 coef = coeflist,
                 # standard errors
                 se = selist,
                 order=paste0("^", vars.order , "$"),
                 column.labels = str_to_title(titles),
                 keep.stat="n",
                 # digits.extra = 5,
                 no.space = T,
                 t.auto = T) %>% 
    str_replace("Observations.{0,1000}\\\\\\\\",ns)
  
  paste(b,collapse="\n")
  
}

hettable1 <- maketable3(c("age","race","gender"),dat,drop=c("NonBinary","MISSING")) %>% 
  str_replace("centering", "centering \n \\\\scriptsize") %>% 
  fixnames() %>% 
  str_remove_all(" for county") %>% 
  str_replace_all("Other","Non-white") %>% 
  str_replace_all("18to34","18 to 34") %>% 
  str_replace_all("35to64","35 to 64") %>% 
  str_replace_all("65andolder","65 +") %>% 
  str_replace_all("& \\(1\\) & \\(2\\) & \\(3\\) & \\(4\\) & \\(5\\) & \\(6\\) & \\(7\\)",
                  "\\\\textit\\{Dep. var\\}: 1=Preferred policy & \\(1\\) & \\(2\\) & \\(3\\) & \\(4\\) & \\(5\\) & \\(6\\) & \\(7\\) \\\\\\\\ \n \\\\cline\\{1-1\\} \\\\cline\\{2-4\\} \\\\cline\\{5-6\\} \\\\cline\\{7-8\\}") %>% 
  str_remove(" & \\\\multicolumn\\{7\\}\\{c\\}\\{\\\\textit\\{Dependent variable:\\}\\} \\\\\\\\") %>% 
  str_remove("\\\\cline\\{2-8\\}") %>% 
  str_remove("\\\\\\\\\\[-1.8ex\\] & \\\\multicolumn\\{7\\}\\{c\\}\\{1=Preferred policy\\} \\\\\\\\") %>% 
  str_replace_all("\\\\hline \\\\\\\\\\[-1.8ex\\] \n Absolute deaths","\n Absolute deaths") 

write(hettable1,file="~/covid-survey/tables/hettable1_993_norestr.tex")


hettable2 <- maketable3(c("ideol2","education","income"),dat,drop=c("MISSING")) %>% 
  str_replace("centering", "centering \n \\\\scriptsize") %>% 
  fixnames() %>% 
  str_remove_all(" for county") %>% 
  str_replace_all("Other","Non-white") %>% 
  str_replace_all("18to34","18 to 34") %>% 
  str_replace_all("35to64","35 to 64") %>% 
  str_replace_all("65andolder","65 +") %>% 
  str_replace_all("Noncollege","Non-college") %>% 
  str_replace_all("Lessthan75","< \\\\$75k/yr") %>% 
  str_replace_all("Morethan75","> \\\\$75k/yr") %>%
  str_replace_all("& \\(1\\) & \\(2\\) & \\(3\\) & \\(4\\) & \\(5\\) & \\(6\\) & \\(7\\)",
                  "\\\\textit\\{Dep. var\\}: 1=Preferred policy & \\(1\\) & \\(2\\) & \\(3\\) & \\(4\\) & \\(5\\) & \\(6\\) & \\(7\\) \\\\\\\\ \n \\\\cline\\{1-1\\} \\\\cline\\{2-4\\} \\\\cline\\{5-6\\} \\\\cline\\{7-8\\}") %>% 
  str_remove(" & \\\\multicolumn\\{7\\}\\{c\\}\\{\\\\textit\\{Dependent variable:\\}\\} \\\\\\\\") %>% 
  str_remove("\\\\cline\\{2-8\\}") %>% 
  str_remove("\\\\\\\\\\[-1.8ex\\] & \\\\multicolumn\\{7\\}\\{c\\}\\{1=Preferred policy\\} \\\\\\\\") %>% 
  str_replace_all("\\\\hline \\\\\\\\\\[-1.8ex\\] \n Absolute deaths","\n Absolute deaths") 

cat(hettable2)

write(hettable2,file="~/covid-survey/tables/hettable2_993_norestr.tex")

##################################################
############### LR tests #########################
##################################################
{
  
  mainage <- summary(main3b)
  
  mainrace <- summary(main3b)
  
  maingender <- clogit(best ~
                         mabsdeaths*lassorpfl + mabscases*lassorpfl +
                         feduinoneavcost*lassorpfl + feduianyavcost*lassorpfl + 
                         feduinoneunempl*lassorpfl + feduianyunempl*lassorpfl + 
                         
                         factor(rule1)*lassorpfl + factor(rule2)*lassorpfl +
                         factor(rule3)*lassorpfl + factor(rule4)*lassorpfl +
                         factor(rule5)*lassorpfl + factor(rule6)*lassorpfl +
                         factor(rule7)*lassorpfl + factor(rule8)*lassorpfl +
                         factor(rule9)*lassorpfl + factor(rule10)*lassorpfl +
                         
                         statquo*lassorpfl +
                         strata(choice),data=dat[which(dat$gender %in% c("Women","Men")),]
                       ,weights=popwt,method="approximate"
  ) %>% summary()
  
  maineducation <- clogit(best ~
                     mabsdeaths*lassorpfl + mabscases*lassorpfl +
                     feduinoneavcost*lassorpfl + feduianyavcost*lassorpfl + 
                     feduinoneunempl*lassorpfl + feduianyunempl*lassorpfl + 
                     
                     factor(rule1)*lassorpfl + factor(rule2)*lassorpfl +
                     factor(rule3)*lassorpfl + factor(rule4)*lassorpfl +
                     factor(rule5)*lassorpfl + factor(rule6)*lassorpfl +
                     factor(rule7)*lassorpfl + factor(rule8)*lassorpfl +
                     factor(rule9)*lassorpfl + factor(rule10)*lassorpfl +
                     
                     statquo*lassorpfl +
                     strata(choice),data=dat[which(dat$education != "MISSING"),]
                   ,weights=popwt,method="approximate"
  ) %>% summary()
  
  
  mainincome <- clogit(best ~
                            mabsdeaths*lassorpfl + mabscases*lassorpfl +
                            feduinoneavcost*lassorpfl + feduianyavcost*lassorpfl + 
                            feduinoneunempl*lassorpfl + feduianyunempl*lassorpfl + 
                            
                            factor(rule1)*lassorpfl + factor(rule2)*lassorpfl +
                            factor(rule3)*lassorpfl + factor(rule4)*lassorpfl +
                            factor(rule5)*lassorpfl + factor(rule6)*lassorpfl +
                            factor(rule7)*lassorpfl + factor(rule8)*lassorpfl +
                            factor(rule9)*lassorpfl + factor(rule10)*lassorpfl +
                            
                            statquo*lassorpfl +
                            strata(choice),data=dat[which(dat$income != "MISSING"),]
                          ,weights=popwt,method="approximate"
  ) %>% summary()
  
  mainideol <- clogit(best ~
                         mabsdeaths*lassorpfl + mabscases*lassorpfl +
                         feduinoneavcost*lassorpfl + feduianyavcost*lassorpfl + 
                         feduinoneunempl*lassorpfl + feduianyunempl*lassorpfl + 
                         
                         factor(rule1)*lassorpfl + factor(rule2)*lassorpfl +
                         factor(rule3)*lassorpfl + factor(rule4)*lassorpfl +
                         factor(rule5)*lassorpfl + factor(rule6)*lassorpfl +
                         factor(rule7)*lassorpfl + factor(rule8)*lassorpfl +
                         factor(rule9)*lassorpfl + factor(rule10)*lassorpfl +
                         
                         statquo*lassorpfl +
                         strata(choice),data=dat[which(dat$ideol2 != "MISSING"),]
                       ,weights=popwt,method="approximate"
  ) %>% summary()
  
  
}

lrt <- function (x,y) {
  if (x$n == y$n) {
    lr = 2*(abs(x$loglik[2] - y$loglik[2]))
    rx = x$coefficients %>% as.data.frame() %>% dplyr::filter(!is.na(coef)) %>% nrow()
    ry = y$coefficients %>% as.data.frame() %>% dplyr::filter(!is.na(coef)) %>% nrow()
    df = abs(rx-ry)
    cat("lr=",lr,", df=",df,", crit value = ", 1-pchisq(lr,df)," n = ",x$n)
  } else {
    print("ns are not equal")
  }
}


agemod <- getsummary("age",dat)
racemod <- getsummary("race",dat)
gendermod <- getsummary("gender",dat[which(dat$gender %in% c("Women","Men")),])
ideolmod <- getsummary("ideol2",dat[which(dat$ideol2 != "MISSING"),])
incomemod <- getsummary("income",dat[which(dat$income != "MISSING"),])
educationmod <- getsummary("education",dat[which(dat$education != "MISSING"),])


lrt(agemod,mainage)
lrt(racemod,mainrace)
lrt(gendermod,maingender)
lrt(ideolmod,mainideol)
lrt(incomemod,mainincome)
lrt(educationmod,maineducation)

lrt(summary(main1),summary(main2b))
lrt(summary(main2),summary(main2b))
lrt(summary(main3),summary(main2b))
lrt(summary(main1b),summary(main2b))
lrt(summary(main3b),summary(main2b))

lrt(summary(main1),summary(main3b))
lrt(summary(main2),summary(main3b))
lrt(summary(main3),summary(main3b))
lrt(summary(main1b),summary(main3b))
lrt(summary(main2b),summary(main3b))

lrt(summary(main1),summary(main1b))
lrt(summary(main2),summary(main2b))
lrt(summary(main3),summary(main3b))


summary(clogit(best ~ mabsdeaths*lassorpfl + mabscases*lassorpfl +
                  feduiany*avcost*lassorpfl + 
                  feduiany*unempl*lassorpfl + 
                 
                 factor(rule1)*lassorpfl + factor(rule2)*lassorpfl +
                 factor(rule3)*lassorpfl + factor(rule4)*lassorpfl +
                 factor(rule5)*lassorpfl + factor(rule6)*lassorpfl +
                 factor(rule7)*lassorpfl + factor(rule8)*lassorpfl +
                 factor(rule9)*lassorpfl + factor(rule10)*lassorpfl +
                 
                 statquo*lassorpfl + strata(choice),data=dat[which(dat$ideol2=="liberal"),]))


############################################################
############################################################
######### HETEROGENEITY TABLES for APPENDIX  ###############
############################################################
############################################################

df <- dat
fac <- c("ideol2","education","income")
drop <- "MISSING"



maketable3 <- function (fac,df,drop=c("MISSING")) {
  
  mods <- list()
  coeflist <- list()
  selist <- list()
  modelnames <- c()
  titles <- c()
  npeople <- c()
  nchoices <- c()
  
  
  
  for (j in 1:length(fac)) {
    
    df <- df %>% dplyr::filter(!is.na(eval(as.symbol(fac[j])))) %>% droplevels()
    
    s <- getsummary(fac[j],df)
    
    levs <- df[,which(names(df)==fac[j])] %>%  unique()
    levs <- summary(levs[,1])[,1] %>% str_remove_all(":1") %>% trimws()
    levs <- levs %>% str_remove_all(" ")
    
    p <- df[which(names(df) %in% c("ResponseId",fac[j]))] %>% unique()
    
    
    for (i in 1:length(levs)) {
      npeople <- c(npeople,sum(p[,2]==levs[i]))
    }
    
    p <- df[,which(names(df) %in% c("ResponseId",fac[j]))]
    
    
    for (i in 1:length(levs)) {
      nchoices <- c(nchoices,sum(p[,2]==levs[i]))
    }
    
    
    
    
    t <- s$coefficients %>% as.data.frame() %>% rownames_to_column(var="term")  
     # dplyr::filter(! str_detect(term,"lassorpfl"))
    
    
    
    for (i in 1:length(levs)) {
      if (!levs[i] %in% drop) {
        tempcoef <- t %>% dplyr::filter(str_detect(term,levs[i]) )
        assign(paste0(levs[i],"coef"),tempcoef)
        modelnames <- c(modelnames,paste0(levs[i],"coef"))
        titles <- c(titles,levs[i])
      }
    }
    
    
    fake <- lm(best ~ mabsdeaths + mabscases + feduianyavcost + feduinoneavcost + feduianyunempl + feduinoneunempl +
                 rule1.1 + rule2.1 + rule3.1 +rule4.1 +rule5.1 + 
                 rule6.1 + rule7.1 + rule8.1 + rule9.1 + rule10.1 +
                 
                 rule1.2 + rule2.2 + rule3.2 +rule4.2 +rule5.2 + 
                 rule6.2 + rule7.2 + rule8.2 + rule9.2 + rule10.2 +
                 
                 rule2.3 + rule3.3 +rule4.3 +rule5.3 + 
                 rule6.3 + rule7.3 + rule8.3 + rule9.3 + rule10.3 +
                 statquo +
                 
                 mabsdeaths:lassorpfl + mabscases:lassorpfl + feduianyavcost:lassorpfl + 
                 feduinoneavcost:lassorpfl + feduianyunempl:lassorpfl + feduinoneunempl:lassorpfl +
                 rule1.1:lassorpfl + rule2.1:lassorpfl + rule3.1:lassorpfl +rule4.1:lassorpfl +rule5.1:lassorpfl + 
                 rule6.1:lassorpfl + rule7.1:lassorpfl + rule8.1:lassorpfl + rule9.1:lassorpfl + rule10.1:lassorpfl +
                 
                 rule1.2:lassorpfl + rule2.2:lassorpfl + rule3.2:lassorpfl +rule4.2:lassorpfl +rule5.2:lassorpfl + 
                 rule6.2:lassorpfl + rule7.2:lassorpfl + rule8.2:lassorpfl + rule9.2:lassorpfl + rule10.2:lassorpfl +
                 
                 rule2.3:lassorpfl + rule3.3:lassorpfl +rule4.3:lassorpfl +rule5.3:lassorpfl + 
                 rule6.3:lassorpfl + rule7.3:lassorpfl + rule8.3:lassorpfl + rule9.3:lassorpfl + rule10.3:lassorpfl +
                 statquo:lassorpfl 
               
               + 0,data = df)
    
    vars.order <- c("mabsdeaths",  "mabscases", "feduinoneavcost",  "feduianyavcost",  
                    "feduinoneunempl",  "feduianyunempl", 
                    "statquo" )
    
    
    
    for (i in 1:length(modelnames)) {mods[[i]] <- fake}
    
    for (i in 1:length(modelnames)) {coeflist[[i]] <- (modelnames[i] %>% as.symbol() %>% eval())$coef}
    
    for (i in 1:length(modelnames)) {selist[[i]] <- (modelnames[i] %>% as.symbol() %>% eval())$`robust se`}
    
  }
  
  
  nchoices <- nchoices/2
  
  nchoices <- nchoices[which(npeople>200)]
  npeople <- npeople[which(npeople>200)]
  
  ns <- paste0("Respondents & ",npeople[1], " & ",npeople[2]," & ",npeople[3]," & ",npeople[4]," & ",npeople[5]," & ",npeople[6]," & ",npeople[7],
               "\\\\\\\\ \n Choices & ", nchoices[1], " & ",nchoices[2]," & ",nchoices[3]," & ",nchoices[4]," & ",nchoices[5]," & ",nchoices[6]," & ",nchoices[7],"\\\\\\\\")
  
  
  
  b <- stargazer(mods,type="latex",
                # omit="rule",
                 coef = coeflist,
                 # standard errors
                 se = selist,
                 order=paste0("^", vars.order , "$"),
                 column.labels = str_to_title(titles),
                 keep.stat="n",
                 # digits.extra = 5,
                 no.space = T,
                 t.auto = T) %>% 
    str_replace("Observations.{0,1000}\\\\\\\\",ns)
  
  paste(b,collapse="\n")
  
}

hettable1 <- maketable3(c("age","race","gender"),dat,drop=c("NonBinary","MISSING")) %>% 
  str_replace("centering", "centering \n \\\\scriptsize") %>% 
  fixnames() %>% 
  str_remove_all(" for county") %>% 
  str_replace_all("Other","Non-white") %>% 
  str_replace_all("18to34","18 to 34") %>% 
  str_replace_all("35to64","35 to 64") %>% 
  str_replace_all("65andolder","65 +") %>% 
  str_replace_all("& \\(1\\) & \\(2\\) & \\(3\\) & \\(4\\) & \\(5\\) & \\(6\\) & \\(7\\)",
                  "\\\\textit\\{Dep. var\\}: 1=Preferred policy & \\(1\\) & \\(2\\) & \\(3\\) & \\(4\\) & \\(5\\) & \\(6\\) & \\(7\\) \\\\\\\\ \n \\\\cline\\{1-1\\} \\\\cline\\{2-4\\} \\\\cline\\{5-6\\} \\\\cline\\{7-8\\}") %>% 
  str_remove(" & \\\\multicolumn\\{7\\}\\{c\\}\\{\\\\textit\\{Dependent variable:\\}\\} \\\\\\\\") %>% 
  str_remove("\\\\cline\\{2-8\\}") %>% 
  str_remove("\\\\\\\\\\[-1.8ex\\] & \\\\multicolumn\\{7\\}\\{c\\}\\{1=Preferred policy\\} \\\\\\\\") %>% 
  str_replace_all("\\\\hline \\\\\\\\\\[-1.8ex\\] \n Absolute deaths","\n Absolute deaths") %>% 
  str_replace_all(":lassorpfl"," $\\\\times$ RP")

cat(hettable1)

write(hettable1,file="~/covid-survey/tables/hettable1_993_norestr_APPENDIX.tex")



hettable2 <- maketable3(c("ideol2","education","income"),dat,drop=c("MISSING")) %>% 
  str_replace("centering", "centering \n \\\\scriptsize") %>% 
  fixnames() %>% 
  str_remove_all(" for county") %>% 
  str_replace_all("Other","Non-white") %>% 
  str_replace_all("18to34","18 to 34") %>% 
  str_replace_all("35to64","35 to 64") %>% 
  str_replace_all("65andolder","65 +") %>% 
  str_replace_all("Noncollege","Non-college") %>% 
  str_replace_all("Lessthan75","< \\\\$75k/yr") %>% 
  str_replace_all("Morethan75","> \\\\$75k/yr") %>%
  str_replace_all("& \\(1\\) & \\(2\\) & \\(3\\) & \\(4\\) & \\(5\\) & \\(6\\) & \\(7\\)",
                  "\\\\textit\\{Dep. var\\}: 1=Preferred policy & \\(1\\) & \\(2\\) & \\(3\\) & \\(4\\) & \\(5\\) & \\(6\\) & \\(7\\) \\\\\\\\ \n \\\\cline\\{1-1\\} \\\\cline\\{2-4\\} \\\\cline\\{5-6\\} \\\\cline\\{7-8\\}") %>% 
  str_remove(" & \\\\multicolumn\\{7\\}\\{c\\}\\{\\\\textit\\{Dependent variable:\\}\\} \\\\\\\\") %>% 
  str_remove("\\\\cline\\{2-8\\}") %>% 
  str_remove("\\\\\\\\\\[-1.8ex\\] & \\\\multicolumn\\{7\\}\\{c\\}\\{1=Preferred policy\\} \\\\\\\\") %>% 
  str_replace_all("\\\\hline \\\\\\\\\\[-1.8ex\\] \n Absolute deaths","\n Absolute deaths") %>% 
  str_replace_all(":lassorpfl"," $\\\\times$ RP")

cat(hettable2)

write(hettable2,file="~/covid-survey/tables/hettable2_993_norestr_APPENDIX.tex")



##################### rules SD #################################


dat <- dat %>% group_by(choice,ResponseId,alt) %>% 
  mutate(rulesd = sd(c(rule1 , rule2 , rule3 , rule4,  rule5 , rule6 , rule7 , rule8 , rule9 , rule10))) %>% 
  ungroup() 

dat <- dat %>% group_by(choice,ResponseId) %>% mutate(rulesd=max(rulesd)) %>% ungroup()



main3b <- clogit(best ~
                   mabsdeaths*lassorpfl + mabscases*lassorpfl +
                   feduinoneavcost*lassorpfl + feduianyavcost*lassorpfl + 
                   feduinoneunempl*lassorpfl + feduianyunempl*lassorpfl + 
                   
                  
                   
                   factor(rule1)*lassorpfl + factor(rule2)*lassorpfl +
                   factor(rule3)*lassorpfl + factor(rule4)*lassorpfl +
                   factor(rule5)*lassorpfl + factor(rule6)*lassorpfl +
                   factor(rule7)*lassorpfl + factor(rule8)*lassorpfl +
                   factor(rule9)*lassorpfl + factor(rule10)*lassorpfl +
                   
                  rulesd*statquo*lassorpfl +
                   strata(choice),data=dat[which(dat$choiceofperson %in% 1:2),]
                 ,weights=popwt,method="approximate"
)
summary(main3b)



###################### Cluster by person ########################

main3cluster <- clogit(best ~
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
                ,weights=popwt,method="approximate",
                cluster=ResponseId
)


main6cluster <- clogit(best ~
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
                 ,weights=popwt,method="approximate",
                 cluster=ResponseId
)

varskeep <- c("^mabsdeaths$","^mabscases$","^feduinoneavcost$","^feduianyavcost$","^feduinoneunempl$","^feduianyunempl$","^statquo$")

t3 <- broom::tidy(main3cluster)
t6 <- broom::tidy(main6cluster)

model.se <- list(t3$robust.se,t6$robust.se)

a <- stargazer(main3cluster,main6cluster,
          keep=varskeep,
          se=model.se)
a %>% fixnames2 %>% cat()

##### base + shifters ####

main3cluster2 <- clogit(best ~
                         mabsdeaths*lassorpfl + mabscases*lassorpfl +
                          avcost*lassorpfl + avcost:feduiany*lassorpfl + 
                          unempl*lassorpfl + unempl:feduiany*lassorpfl + 
                         
                         rule1*lassorpfl + rule2*lassorpfl +
                         rule3*lassorpfl + rule4*lassorpfl +
                         rule5*lassorpfl + rule6*lassorpfl +
                         rule7*lassorpfl + rule8*lassorpfl +
                         rule9*lassorpfl + rule10*lassorpfl +
                         
                         statquo*lassorpfl +
                         strata(choice),data=dat[which(dat$choiceofperson %in% 1:2),]
                       ,weights=popwt,method="approximate",
                       cluster=ResponseId
)


main6cluster2 <- clogit(best ~
                         mabsdeaths*lassorpfl + mabscases*lassorpfl +
                          avcost*lassorpfl + avcost:feduiany*lassorpfl + 
                          unempl*lassorpfl + unempl:feduiany*lassorpfl + 
                         
                         factor(rule1)*lassorpfl + factor(rule2)*lassorpfl +
                         factor(rule3)*lassorpfl + factor(rule4)*lassorpfl +
                         factor(rule5)*lassorpfl + factor(rule6)*lassorpfl +
                         factor(rule7)*lassorpfl + factor(rule8)*lassorpfl +
                         factor(rule9)*lassorpfl + factor(rule10)*lassorpfl +
                         
                         statquo*lassorpfl +
                         strata(choice),data=dat[which(dat$choiceofperson %in% 1:2),]
                       ,weights=popwt,method="approximate",
                       cluster=ResponseId
)

varskeep2 <- c("^mabsdeaths$","^mabscases$","^avcost$","^avcost:feduiany$","^unempl$","^unempl:feduiany$","^statquo$")

t3b <- broom::tidy(main3cluster2)
t6b <- broom::tidy(main6cluster2)

model.se2 <- list(t3b$robust.se,t6b$robust.se)

a <- stargazer(main3cluster2,main6cluster2,
               keep=varskeep2,
               se=model.se2)
a %>% fixnames2 %>% cat()
