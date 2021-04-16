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


####### functions to format data #############
# 
 df = dat[which(dat$ideol2 != "MISSING" & dat$choiceofperson %in% 1:2),]
 fac="ideol2"
 
getsummary <- function (fac,df) {
  
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
          "rule1","rule2","rule3","rule4","rule5","rule6",
          "rule7","rule8","rule9","rule10","statquo")
  
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
  

  #form2 <- form2 %>% str_replace_all("unempl","feduiany*unempl") %>% str_replace_all("avcost","feduiany*avcost")
  
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
                 rule1 + rule2 + rule3 + rule4 + rule5 + rule6 +
                 rule7 + rule8 + rule9 + rule10 + statquo + 0,data = df)
    
    vars.order <- c("mabsdeaths",  "mabscases", "feduinoneavcost",  "feduianyavcost",  
                    "feduinoneunempl",  "feduianyunempl", "rule1", "rule2" , "rule3",  
                    "rule4",   "rule5" , "rule6" ,   "rule7",    "rule8"   ,   "rule9" , "rule10" ,   
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
  
}



# reorder table
{
  vars.order <- c("mabsdeaths",  "mabscases", "avcost", "unempl",
                  "feduinoneavcost", "feduianyavcost",  "fedui100avcost", "fedui200avcost", "fedui300avcost","fedui400avcost",
                  "feduinoneunempl", "feduianyunempl", "fedui100unempl", "fedui200unempl", "fedui300unempl", "fedui400unempl",
                  
                  "rule1", "rule2" , "rule3",  
                  "rule4",   "rule5" , "rule6" ,   "rule7",    "rule8"   ,   "rule9" , "rule10" ,   
                  "statquo" )
  
  var.omit <- c("lassorpfl","^feduiany$","^factor\\(fedui\\)100$","^factor\\(fedui\\)200$","^factor\\(fedui\\)300$","^factor\\(fedui\\)400$")
  
}

# use robust SEs
{
se1 <- summary(main1)$coefficients %>% as.data.frame() %>% rownames_to_column(var="term") %>% 
  dplyr::filter(! str_detect(term,":lassorpfl|lassorpfl:")) %>% dplyr::select(`robust se`) %>% unlist() %>% as.numeric()

se2 <- summary(main2)$coefficients %>% as.data.frame() %>% rownames_to_column(var="term") %>% 
  dplyr::filter(! str_detect(term,":lassorpfl|lassorpfl:")) %>% dplyr::select(`robust se`) %>% unlist() %>% as.numeric()

se3 <- summary(main3)$coefficients %>% as.data.frame() %>% rownames_to_column(var="term") %>% 
  dplyr::filter(! str_detect(term,":lassorpfl|lassorpfl:")) %>% dplyr::select(`robust se`) %>% unlist() %>% as.numeric()

selist <- list(se1,se2,se3)

}
# get N respondents and N choices
{
  p <- dat %>% dplyr::select(ResponseId,choiceofperson) %>% dplyr::filter(choiceofperson %in% 1:2) %>% unique()
  
  npeople <- length(unique(p$ResponseId))
  
  nchoices <- nrow(p)
  
  ns <- paste0("Respondents & ",npeople, " & ",npeople," & ",npeople,"\\\\\\\\ \n Choices & ",nchoices, " & ",nchoices," & ",nchoices,"\\\\\\\\")
  
  
}

maintabs <- stargazer(main1,main2,main3,
                      type="latex",
                      title="Effects of Federal UI",
                      omit=var.omit,
                      order=paste0("^", vars.order , "$"),
                      se=selist,
                      keep.stat="n",
                      digits.extra = 10,
                      no.space = T)
maintable <- fixnames2(maintabs) %>% str_replace("centering", "centering \n \\\\scriptsize") %>% 
  str_replace("Observations.{0,1000}\\\\\\\\",ns)

cat(maintable)
write(maintable,file="~/covid-survey/tables/maintable_993.tex")


############################################################
############################################################
######### MERGE HETEROGENEITY TABLES  ######################
############################################################
############################################################
df <- dat
fac <- c("age","race")
drop=NA

getsummary3 <- function (fac,df) {
  
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
          "rule1","rule2","rule3","rule4","rule5","rule6",
          "rule7","rule8","rule9","rule10","statquo")
  
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
  
  
  #form2 <- form2 %>% str_replace_all("unempl","feduiany*unempl") %>% str_replace_all("avcost","feduiany*avcost")
  
  form3 <- paste0("best ~",form2,"+ strata(choice)") %>% as.formula
  
  summary(clogit(form3,data=newdf,weights=popwt,method="approximate"))
}


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
               rule1 + rule2 + rule3 + rule4 + rule5 + rule6 +
               rule7 + rule8 + rule9 + rule10 + statquo + 0,data = df)
  
  vars.order <- c("mabsdeaths",  "mabscases", "feduinoneavcost",  "feduianyavcost",  
                  "feduinoneunempl",  "feduianyunempl", "rule1", "rule2" , "rule3",  
                  "rule4",   "rule5" , "rule6" ,   "rule7",    "rule8"   ,   "rule9" , "rule10" ,   
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

write(hettable1,file="~/covid-survey/tables/hettable1_993.tex")


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

write(hettable2,file="~/covid-survey/tables/hettable2_993.tex")


###################################################
########## descriptive stats ######################
###################################################


d1 <- c(mean(dat$months),sd(dat$months),min(dat$months),max(dat$months)) %>% round(2) %>% paste(collapse=" & ")
d2 <- c(mean(dat$fedui),sd(dat$fedui),min(dat$fedui),max(dat$fedui)) %>% round(2) %>% paste(collapse=" & ")

# without policy
d3 <- c(mean(dat$mabsdeaths[which(dat$statquo==1)]),sd(dat$mabsdeaths[which(dat$statquo==1)]),min(dat$mabsdeaths[which(dat$statquo==1)]),max(dat$mabsdeaths[which(dat$statquo==1)])) %>% round(2) %>% paste(collapse=" & ")
d4 <- c(mean(dat$mabscases[which(dat$statquo==1)]),sd(dat$mabscases[which(dat$statquo==1)]),min(dat$mabscases[which(dat$statquo==1)]),max(dat$mabscases[which(dat$statquo==1)])) %>% round(2) %>% paste(collapse=" & ")
d5 <- c(mean(dat$unempl[which(dat$statquo==1)]),sd(dat$unempl[which(dat$statquo==1)]),min(dat$unempl[which(dat$statquo==1)]),max(dat$unempl[which(dat$statquo==1)])) %>% round(2) %>% paste(collapse=" & ")
d6 <- c(mean(dat$avcost[which(dat$statquo==1)]),sd(dat$avcost[which(dat$statquo==1)]),min(dat$avcost[which(dat$statquo==1)]),max(dat$avcost[which(dat$statquo==1)])) %>% round(2) %>% paste(collapse=" & ")

# with policy
d7 <- c(mean(dat$mabsdeaths[which(dat$statquo==0)]),sd(dat$mabsdeaths[which(dat$statquo==0)]),min(dat$mabsdeaths[which(dat$statquo==0)]),max(dat$mabsdeaths[which(dat$statquo==0)])) %>% round(2) %>% paste(collapse=" & ")
d8 <- c(mean(dat$mabscases[which(dat$statquo==0)]),sd(dat$mabscases[which(dat$statquo==0)]),min(dat$mabscases[which(dat$statquo==0)]),max(dat$mabscases[which(dat$statquo==0)])) %>% round(2) %>% paste(collapse=" & ")
d9 <- c(mean(dat$unempl[which(dat$statquo==0)]),sd(dat$unempl[which(dat$statquo==0)]),min(dat$unempl[which(dat$statquo==0)]),max(dat$unempl[which(dat$statquo==0)])) %>% round(2) %>% paste(collapse=" & ")
d10 <- c(mean(dat$avcost[which(dat$statquo==0 & dat$feduinone==1)]),sd(dat$avcost[which(dat$statquo==0 & dat$feduinone==1)]),min(dat$avcost[which(dat$statquo==0 & dat$feduinone==1)]),max(dat$avcost[which(dat$statquo==0 & dat$feduinone==1)])) %>% round(2) %>% paste(collapse=" & ")
d11 <- c(mean(dat$avcost[which(dat$statquo==0 & dat$feduiany==1)]),sd(dat$avcost[which(dat$statquo==0 & dat$feduiany==1)]),min(dat$avcost[which(dat$statquo==0 & dat$feduiany==1)]),max(dat$avcost[which(dat$statquo==0 & dat$feduiany==1)])) %>% round(2) %>% paste(collapse=" & ")


# restrictions (with policy)
r1 <- c(mean(dat$rule1[which(dat$statquo==0)]),sd(dat$rule1[which(dat$statquo==0)]),min(dat$rule1[which(dat$statquo==0)]),max(dat$rule1[which(dat$statquo==0)])) %>% round(2) %>% paste(collapse=" & ")
r2 <- c(mean(dat$rule2[which(dat$statquo==0)]),sd(dat$rule2[which(dat$statquo==0)]),min(dat$rule2[which(dat$statquo==0)]),max(dat$rule2[which(dat$statquo==0)])) %>% round(2) %>% paste(collapse=" & ")
r3 <- c(mean(dat$rule3[which(dat$statquo==0)]),sd(dat$rule3[which(dat$statquo==0)]),min(dat$rule3[which(dat$statquo==0)]),max(dat$rule3[which(dat$statquo==0)])) %>% round(2) %>% paste(collapse=" & ")
r4 <- c(mean(dat$rule4[which(dat$statquo==0)]),sd(dat$rule4[which(dat$statquo==0)]),min(dat$rule4[which(dat$statquo==0)]),max(dat$rule4[which(dat$statquo==0)])) %>% round(2) %>% paste(collapse=" & ")
r5 <- c(mean(dat$rule5[which(dat$statquo==0)]),sd(dat$rule5[which(dat$statquo==0)]),min(dat$rule5[which(dat$statquo==0)]),max(dat$rule5[which(dat$statquo==0)])) %>% round(2) %>% paste(collapse=" & ")
r6 <- c(mean(dat$rule6[which(dat$statquo==0)]),sd(dat$rule6[which(dat$statquo==0)]),min(dat$rule6[which(dat$statquo==0)]),max(dat$rule6[which(dat$statquo==0)])) %>% round(2) %>% paste(collapse=" & ")
r7 <- c(mean(dat$rule7[which(dat$statquo==0)]),sd(dat$rule7[which(dat$statquo==0)]),min(dat$rule7[which(dat$statquo==0)]),max(dat$rule7[which(dat$statquo==0)])) %>% round(2) %>% paste(collapse=" & ")
r8 <- c(mean(dat$rule8[which(dat$statquo==0)]),sd(dat$rule8[which(dat$statquo==0)]),min(dat$rule8[which(dat$statquo==0)]),max(dat$rule8[which(dat$statquo==0)])) %>% round(2) %>% paste(collapse=" & ")
r9 <- c(mean(dat$rule9[which(dat$statquo==0)]),sd(dat$rule9[which(dat$statquo==0)]),min(dat$rule9[which(dat$statquo==0)]),max(dat$rule9[which(dat$statquo==0)])) %>% round(2) %>% paste(collapse=" & ")
r10 <- c(mean(dat$rule10[which(dat$statquo==0)]),sd(dat$rule10[which(dat$statquo==0)]),min(dat$rule10[which(dat$statquo==0)]),max(dat$rule10[which(dat$statquo==0)])) %>% round(2) %>% paste(collapse=" & ")

v1 <- c("Duration of policy (mo)",
"Federal UI per week",
"\\textbf{Without policy:}",
"Absolute deaths/mo/50k",
"Absolute cases/mo/50",
"Unempl rate for county",
"Avg. hhld cost for county",
"\\textbf{With policy}",
"Absolute deaths/mo/50k",
"Absolute cases/mo/50",
"Unempl rate for county",
"Avg. hhld cost (in \\$100s, UI = 0)",
"Avg. hhld cost (in \\$100s, UI > 0)",
"\\textbf{Restrictions:}",
'Grocery, essential retail',
"Non-essential retail",
"Schools, daycare ",
"Parks, outdoor sports",
"Gyms, indoor sports ",
"Theaters, concert halls",
"Restaurants, bars, clubs",
"Meetings, religious services",
"Assisted living facilities",
"Universities, colleges")

filler <- " & & & "
dtable <- data.frame(v1 = v1,v2=c(d1,d2,filler,d3,d4,d5,d6,filler,d7,d8,d9,d10,d11,filler,
                             r1,r2,r3,r4,r5,r6,r7,r8,r9,r10),v3=" \\\\ \n")


dtable <- dtable %>% mutate(v4 = paste(v1, " & ",v2,v3))
head(dtable)

dstats <- paste0(dtable$v4)

cat(dstats)
cat(round(mean(dat$feduinone[which(dat$statquo==0)]),2), " & ",round(sd(dat$feduinone[which(dat$statquo==0)]),2), " &  0 & 1 \\\\")
cat(round(mean(dat$fedui100[which(dat$statquo==0)]),2), " & ",round(sd(dat$fedui100[which(dat$statquo==0)]),2), " &  0 & 1 \\\\")
cat(round(mean(dat$fedui200[which(dat$statquo==0)]),2), " & ",round(sd(dat$fedui200[which(dat$statquo==0)]),2), " &  0 & 1 \\\\")
cat(round(mean(dat$fedui300[which(dat$statquo==0)]),2), " & ",round(sd(dat$fedui300[which(dat$statquo==0)]),2), " &  0 & 1 \\\\")
cat(round(mean(dat$fedui400[which(dat$statquo==0)]),2), " & ",round(sd(dat$fedui400[which(dat$statquo==0)]),2), " &  0 & 1 \\\\")


#################################################################
############# log prefs for money ? #############################
#################################################################

dat$owninc <- ifelse(dat$owninc==0,NA,dat$owninc)

dat$feduianyloginc <- dat$feduiany * log(dat$owninc/12 - dat$avcost/10)
dat$feduinoneloginc <- dat$feduinone * log(dat$owninc/12 - dat$avcost/10)


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
                    feduinoneloginc*lassorpfl + feduianyloginc*lassorpfl + 
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
  
}
summary(main3)


main2 <- clogit(best ~
                   mabsdeaths*lassorpfl + mabscases*lassorpfl +
                  feduinoneloginc*lassorpfl + feduianyloginc*lassorpfl + 
                  feduinoneunempl*lassorpfl + feduianyunempl*lassorpfl + 
                   
                   rule1*lassorpfl + rule2*lassorpfl +
                   rule3*lassorpfl + rule4*lassorpfl +
                   rule5*lassorpfl + rule6*lassorpfl +
                   rule7*lassorpfl + rule8*lassorpfl +
                   rule9*lassorpfl + rule10*lassorpfl +
                  
                  statquo*lassorpfl +
                  strata(choice),data=dat[which(!is.na(dat$owninc)),]
                ,weights=popwt,method="approximate"
)
summary(main2)

#######################################################
########### Descriptive stats sample ##################
#######################################################

people <- dat %>% dplyr::select(ResponseId,gender,income,race,education,age,ideol2) %>% unique()
summary(people)
