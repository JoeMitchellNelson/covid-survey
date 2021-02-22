require(pacman)

p_load(rpart,rpart.plot,readr,dplyr,RCurl,rjson,lubridate,
       rvest,stringr,Hmisc,rattle,RColorBrewer,ddpcr,tidytext,tidyr,
       ggrepel,ggplot2,png,ggpubr,tidycensus,sf,plm,lmtest,stargazer,MASS,
       xtable,knitr,magick,purrr,ggthemes,gifski,extrafont,latex2exp,
       cowplot,mapproj,patchwork,remotes,tictoc,Hmisc,english,readstata13)


p_load(glmnet,tibble,broom)

######### BIG MODEL ##################


dat <- read.dta13("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_nointx.dta")
pair <- read.dta13("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/forR_VSL-COVID_two-alt_long.dta")

#dat <- read.dta13("C:/Users/Joe/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_intx.dta")
#pair <- read.csv("C:/Users/Joe/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/forR_VSL-COVID_binary.csv")

varind <- c(4:18,26,27,30:33,35:36,39:40,42:43,45:74,89:90,92:96,107:110,112:121,122,173:182,198:207)

pdat <- pair %>% dplyr::filter(!is.na(y2020m6deaths))
pdat <- pdat[,c(3,varind)]

# col numbers of vars that VARY WITHIN choice set (e.g. deaths, rules)
vwic <- 2:16
# col numbers of vars that are CONSTANT in each choice set (e.g. gender, income)
nvwic <- 17:ncol(pdat)

for (i in vwic) {
  for (j in nvwic) {
    pdat$newvar <- pdat[,i]*pdat[,j]
    names(pdat)[which(names(pdat)=="newvar")] <- paste0(names(pdat)[i],":",names(pdat)[j])
  }
}

pair.int <- as.matrix(pdat)

pair.int <- pair.int[,setdiff(1:ncol(pair.int),nvwic)]


cv.lasso <- cv.glmnet(x = pair.int[,2:ncol(pair.int)], y = pair.int[,1], family = "binomial", alpha = 1, lambda = NULL)



model <- glmnet(x = pair.int[,2:ncol(pair.int)],y = pair.int[,1], alpha = 1, family = "binomial",
               # lambda = exp(-6.2))
                lambda = cv.lasso$lambda.min)
plot(cv.lasso)
coef(model)


keepers <- model$beta %>% as.matrix %>% as.data.frame() %>% rownames_to_column("vars") %>% dplyr::filter(s0!=0)
keepers <- keepers$vars %>% str_replace_all(":","*")
keepers <- keepers[which(!keepers %in% "statquo*reject")]

keepers <- c(names(pdat)[vwic],keepers) %>% unique()

formlasso <- paste0("best ~ ", paste(keepers,collapse=" + ")," + strata(choice)") %>% as.formula
summary(res <- clogit(formlasso,data=dat))

broom::tidy(res) %>% dplyr::filter(str_detect(term,"avcost"))

##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################


tempdat <- dat %>% 
  dplyr::select(which(str_detect(names(dat),"(choiceold|OR|WA|female|black|asian|multi|ageb|ideolconserv|ideolliberal|owninc|pzip|ageb[:digit:]|countypop|reject$|^com|ffcom|pvote|vuln|owninc|fedui|monthst)") & 
                                         !str_detect(names(dat),"_") )) %>% unique()

tempdat$countypop <- tempdat$countypop/100000

# remove reference groups because lasso gets confused by multicollinearity
tempdat <- tempdat %>% dplyr::select(-pvotedem,-pzipage45to54,-pziphealthinsyes,-pziprwhite,-ageb45to54,-agebcont,-pzipiedserv) %>% unique()

psmall1 <- pair %>% dplyr::select(choiceold,choice,best,mabsdeaths,mabsnfcases,avcost,unempl,
                                 rule1,rule2,rule3,rule4,rule5,rule6,rule7,rule8,rule9,rule10,statquo)

psmall1 <- psmall1 %>% group_by(choiceold) %>% mutate(n=n()) %>% mutate(wt=1/n) %>% 
  dplyr::select(choiceold,choice,best,wt,everything()) %>% dplyr::select(-n) %>% ungroup()
  
BASEVARS <- names(psmall1)[which(!names(psmall1) %in% c("best","wt","choice","choiceold","n"))]

psmall <- left_join(psmall1,tempdat,by="choiceold")
psmall <- psmall[,which(!str_detect(names(psmall),"\\.x"))]

names(psmall) <- str_remove_all(names(psmall),"\\.y")

pdat <- psmall %>% unique() %>% dplyr::select(-choiceold,-choice)


# col numbers of vars that VARY WITHIN choice set (e.g. deaths, rules)
vwic <- which(names(pdat) %in% BASEVARS)
# col numbers of vars that are CONSTANT in each choice set (e.g. gender, income)
nvwic <- which(!names(pdat) %in% c("best","wt",BASEVARS))

for (i in vwic) {
  for (j in nvwic) {
    pdat$newvar <- pdat[,i]*pdat[,j]
    names(pdat)[which(names(pdat)=="newvar")] <- paste0(names(pdat)[i],":",names(pdat)[j])
  }
}


pair.int <- as.matrix(pdat)

pair.int <- pair.int[,setdiff(1:ncol(pair.int),nvwic)]

########## FOR LASSO ############

cv.lasso <- cv.glmnet(x = pair.int[,3:ncol(pair.int)], y = pair.int[,1], family = "binomial", alpha = 1, lambda = NULL,weights=pair.int[,2])

model <- glmnet(x = pair.int[,3:ncol(pair.int)],y = pair.int[,1], alpha = 1, family = "binomial",
                lambda = exp(-6.2))
               # lambda = cv.lasso$lambda.min,weights=pair.int[,2])
plot(cv.lasso)
coef(model)

keepers <- model$beta %>% as.matrix %>% as.data.frame() %>% rownames_to_column("vars") %>% dplyr::filter(s0!=0)
keepers <- keepers$vars %>% str_replace_all(":","*")
keepers <- keepers[which(!keepers %in% "statquo*reject")]


DMRPBASEVARS <- paste(BASEVARS,"*demeanrp")
DMRPINTX <- paste(keepers,"*demeanrp")
DMRPVARS <- c(DMRPBASEVARS,DMRPINTX)

NORPVARS <- c(BASEVARS,keepers)


form1 <- paste0("best ~ ", paste(NORPVARS,collapse=" + ")," + strata(choice)") %>% as.formula
summary(res <- clogit(form1,data=dat))
#stargazer(res,type="text")

restidy <- broom::tidy(res) %>% dplyr::filter(!is.na(estimate))
restidy <- restidy[which(!str_detect(restidy$term,"demeanrp")),]

########## sort variables and make labels for lasso results ###########

restidy <- broom::tidy(res) %>% dplyr::filter(!is.na(estimate))
restidy[,(dim(restidy)[2]+1):(dim(restidy)[2]+2)] <- str_split_fixed(restidy$term,":",n=2)
names(restidy)[(dim(restidy)[2]-1):(dim(restidy)[2])] <- c("term1","term2")

medians <- summary(dat[which(dat$statquo==0),])[3,] %>% as.data.frame() %>% rownames_to_column("var")
names(medians)[2] <- "val1"
medians2 <- summary(dat[which(dat$statquo==1),])[3,] %>% as.data.frame() %>% rownames_to_column("var")
names(medians2)[2] <- "valstatquo"

medians$val1 <- str_remove(medians$val1,"Median :") %>% trimws() %>% as.character() %>% as.numeric()
medians$var <- trimws(medians$var)
medians2$valstatquo <- str_remove(medians2$valstatquo,"Median :") %>% trimws() %>% as.character() %>% as.numeric()
medians2$var <- trimws(medians2$var)


medians <- left_join(medians,medians2)
medians$valstatquo <- ifelse(!medians$var %in% BASEVARS,0,medians$valstatquo)
medians$val <- medians$val1 - medians$valstatquo
medians <- medians %>% dplyr::select(var,val)

getWTP <- left_join(restidy,medians,by=c("term1"="var"))
getWTP <- left_join(getWTP,medians,by=c("term2"="var"))
getWTP$val.y <- ifelse(getWTP$term2=="",1,getWTP$val.y)
getWTP$iscost <- ifelse(str_detect(getWTP$term,"avcost"),1,0)

getWTP$totals <- getWTP$estimate*getWTP$val.x*getWTP$val.y

denom <- sum(getWTP$totals * (getWTP$iscost))/medians$val[which(medians$var=="avcost")]

num <- sum(getWTP$totals * (1-getWTP$iscost))

totalWTP <- -num/denom

###

basevars <- names(pdat)[vwic]
names_key1 <- data.frame(var=basevars,code=(1:length(basevars))*10000)
intxvars <- names(pdat)[nvwic]
names_key2 <- data.frame(var=intxvars,code=1:length(intxvars))
varlist_key <- rbind(names_key1,names_key2)

varlist <- data.frame(var=restidy$term)
varlist[,2:3] <- str_split_fixed(varlist$var,":",n=2)

varlist <- left_join(varlist,varlist_key,by=c("V2"="var"))
varlist <- left_join(varlist,varlist_key,by=c("V3"="var"))
varlist$key <- rowSums(varlist[,4:5],na.rm=T)
varlist <- varlist[order(varlist$key),] %>% dplyr::select(var,key)
restidy <- left_join(restidy,varlist,by=c("term"="var"))
restidy <- restidy[order(restidy$key),]

varlabs <- data.frame(x=names(dat),y=attributes(dat)$var.labels)

restidy[,7:8] <- str_split_fixed(restidy$term,":",n=2)



restidy <- left_join(restidy,varlabs,by=c("V1"="x"))
restidy <- left_join(restidy,varlabs,by=c("V2"="x"))

for (i in 1:nrow(restidy)) {
  if (restidy$y.x[i]=="" | is.na(restidy$y.x[i])) {
    temp1 <- restidy$y.x[i]
    temp2 <- restidy$y.y[i]
    restidy$y.x[i] <- temp2
    restidy$y.y[i] <- temp1
  }
}

restidy$label <- ifelse(!is.na(restidy$y.y) & restidy$y.y!="", paste0(restidy$y.x," X ",restidy$y.y),
                        ifelse(is.na(restidy$y.y),paste0(restidy$y.x),paste0(restidy$y.x," X ",restidy$V2)))

restidy <- restidy %>% dplyr::select(term,label,estimate,std.error,p.value)
restidy$star <- ifelse(restidy$p.value<=0.05,"*","")
write.csv(restidy,"~/covid-survey/lasso_results_demeanrp.csv")

sigonly <- restidy %>% dplyr::filter(p.value<=0.05)

write.csv(sigonly,"~/covid-survey/lasso_results_signif_only.csv")

####           ###          ###           ### #               
#   #        #    #        #   #        #     #       
#    #    #       #     #      #     #        #  
#      ###          ###           ###         #


############################################
######### POP MEASURES ONLY ################
############################################

####           ###          ###           ### #               
#   #        #    #        #   #        #     #       
#    #    #       #     #      #     #        #  
#      ###          ###           ###         #


#dat <- read.dta13("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_intx.dta")
#pair <- read.csv("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/forR_VSL-COVID_binary.csv")

dat <- read.dta13("C:/Users/Joe/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_intx.dta")
pair <- read.csv("C:/Users/Joe/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/forR_VSL-COVID_binary.csv")

tempdat <- dat %>% 
  dplyr::select(which(str_detect(names(dat),"(choiceold|OR|WA|pzip|countypop|reject$|pvote|demeanrp|fedui|monthst)") & 
                        !str_detect(names(dat),"_") )) %>% unique()

tempdat$countypop <- tempdat$countypop/100000

# remove reference groups because lasso gets confused by multicollinearity
tempdat <- tempdat %>% dplyr::select(-pvotedem,-pzipage45to54,-pziphealthinsyes,-pziprwhite,-pzipiedserv) %>% unique()

psmall1 <- pair %>% dplyr::select(choiceold,choice,best,mabsdeaths,mabsnfcases,avcost,unempl,
                                  rule1,rule2,rule3,rule4,rule5,rule6,rule7,rule8,rule9,rule10,statquo)

psmall1 <- psmall1 %>% group_by(choiceold) %>% mutate(n=n()) %>% mutate(wt=1/n) %>% 
  dplyr::select(choiceold,choice,best,wt,everything()) %>% dplyr::select(-n) %>% ungroup()

BASEVARS <- names(psmall1)[which(!names(psmall1) %in% c("best","wt","choice","choiceold","n"))]

psmall <- left_join(psmall1,tempdat,by="choiceold")
psmall <- psmall[,which(!str_detect(names(psmall),"\\.x"))]

names(psmall) <- str_remove_all(names(psmall),"\\.y")

pdat <- psmall %>% unique() %>% dplyr::select(-choiceold,-choice)


# col numbers of vars that VARY WITHIN choice set (e.g. deaths, rules)
vwic <- which(names(pdat) %in% BASEVARS)
# col numbers of vars that are CONSTANT in each choice set (e.g. gender, income)
nvwic <- which(!names(pdat) %in% c("best","wt",BASEVARS))

for (i in vwic) {
  for (j in nvwic) {
    pdat$newvar <- pdat[,i]*pdat[,j]
    names(pdat)[which(names(pdat)=="newvar")] <- paste0(names(pdat)[i],":",names(pdat)[j])
  }
}

pdat <- pdat %>% dplyr::select(-`statquo:reject`)

pair.int <- as.matrix(pdat)

pair.int <- pair.int[,setdiff(1:ncol(pair.int),nvwic)]

########## FOR LASSO ############

cv.lasso <- cv.glmnet(x = pair.int[,3:ncol(pair.int)], y = pair.int[,1], family = "binomial", alpha = 1, lambda = NULL,weights=pair.int[,2])

model <- glmnet(x = pair.int[,3:ncol(pair.int)],y = pair.int[,1], alpha = 1, family = "binomial",
                  lambda = exp(-5.5))
              #  lambda = cv.lasso$lambda.min,weights=pair.int[,2])
plot(cv.lasso)
coef(model)

keepers <- model$beta %>% as.matrix %>% as.data.frame() %>% rownames_to_column("vars") %>% dplyr::filter(s0!=0)
keepers <- keepers$vars %>% str_replace_all(":","*")

form1 <- paste0("best ~ ", paste(BASEVARS,collapse=" + ")," + ",paste(keepers,collapse=" + ")," + strata(choice)") %>% str_remove("\\+ statquo\\*reject") %>% as.formula
summary(res <- clogit(form1,data=dat))
#stargazer(res,type="text")

restidy <- broom::tidy(res) %>% dplyr::filter(!is.na(estimate)) %>% 
  dplyr::select(-conf.low,-conf.high)

########## sort variables and make labels for lasso results ###########


basevars <- names(pdat)[vwic]
names_key1 <- data.frame(var=basevars,code=(1:length(basevars))*10000)
intxvars <- names(pdat)[nvwic]
names_key2 <- data.frame(var=intxvars,code=1:length(intxvars))
varlist_key <- rbind(names_key1,names_key2)

varlist <- data.frame(var=restidy$term)
varlist[,2:3] <- str_split_fixed(varlist$var,":",n=2)

varlist <- left_join(varlist,varlist_key,by=c("V2"="var"))
varlist <- left_join(varlist,varlist_key,by=c("V3"="var"))
varlist$key <- rowSums(varlist[,4:5],na.rm=T)
varlist <- varlist[order(varlist$key),] %>% dplyr::select(var,key)
restidy <- left_join(restidy,varlist,by=c("term"="var"))
restidy <- restidy[order(restidy$key),]

varlabs <- data.frame(x=names(dat),y=attributes(dat)$var.labels)

restidy[,7:8] <- str_split_fixed(restidy$term,":",n=2)



restidy <- left_join(restidy,varlabs,by=c("V7"="x"))
restidy <- left_join(restidy,varlabs,by=c("V8"="x"))

for (i in 1:nrow(restidy)) {
  if (restidy$y.x[i]=="" | is.na(restidy$y.x[i])) {
    temp1 <- restidy$y.x[i]
    temp2 <- restidy$y.y[i]
    restidy$y.x[i] <- temp2
    restidy$y.y[i] <- temp1
  }
}

restidy$label <- ifelse(!is.na(restidy$y.y) & restidy$y.y!="", paste0(restidy$y.x," X ",restidy$y.y),
                        ifelse(is.na(restidy$y.y),paste0(restidy$y.x),paste0(restidy$y.x," X ",restidy$V8)))

restidy <- restidy %>% dplyr::select(term,label,estimate,std.error,p.value)
restidy$star <- ifelse(restidy$p.value<=0.05,"*","")

####           ###          ###           ### #               
#   #        #    #        #   #        #     #       
#    #    #       #     #      #     #        #  
#      ###          ###           ###         #


############################################
########### TINY MODEL ONLY ################
############################################

####           ###          ###           ### #               
#   #        #    #        #   #        #     #       
#    #    #       #     #      #     #        #  
#      ###          ###           ###         #


dat <- read.dta13("C:/Users/Joe/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_intx.dta")

summary(res <- clogit(best ~ mabsdeaths*pvotedem + mabsnfcases*pvotedem + avcost*pvotedem + unempl*pvotedem + statquo*pvotedem + 
                        
                        rule1*pvotedem + rule2*pvotedem + rule3*pvotedem + rule4*pvotedem + rule5*pvotedem +
                        rule6*pvotedem + rule7*pvotedem + rule8*pvotedem + rule9*pvotedem + rule10*pvotedem +
                        
                        strata(choice),data=dat))

restidy <- broom::tidy(res) %>% dplyr::filter(!is.na(estimate))
restidy[,(dim(restidy)[2]+1):(dim(restidy)[2]+2)] <- str_split_fixed(restidy$term,":",n=2)
names(restidy)[(dim(restidy)[2]-1):(dim(restidy)[2])] <- c("term1","term2")

medians <- summary(dat[which(dat$statquo==0),])[3,] %>% as.data.frame() %>% rownames_to_column("var")
names(medians)[2] <- "val"
medians$val <- str_remove(medians$val,"Median :") %>% trimws() %>% as.character() %>% as.numeric()
medians$var <- trimws(medians$var)

getWTP <- left_join(restidy,medians,by=c("term1"="var"))
getWTP <- left_join(getWTP,medians,by=c("term2"="var"))
getWTP$val.y <- ifelse(getWTP$term2=="",1,getWTP$val.y)
getWTP$iscost <- ifelse(str_detect(getWTP$term,"avcost"),1,0)

getWTP$val.x <- ifelse(str_detect(getWTP$term,"deaths|cases"),-1*getWTP$val.x,getWTP$val.x)
getWTP$val.x <- ifelse(str_detect(getWTP$term1,"statquo"),-1,getWTP$val.x)
getWTP$val.y <- ifelse(str_detect(getWTP$term2,"statquo"),-1,getWTP$val.y)


denom <- sum(getWTP$estimate * (getWTP$iscost) * getWTP$val.x * getWTP$val.y)/270

num <- sum(getWTP$estimate * getWTP$val.x * getWTP$val.y)

totalWTP <- -num/denom



########## FOR RANDOM FOREST ############

p_load(randomForest,caTools)


rfdat <- pdat[,which(!str_detect(names(pdat),":"))]
rfdat$best <- as.factor(rfdat$best)

sample = sample.split(rfdat$best, SplitRatio = .75)
train = subset(rfdat, sample == TRUE)
test  = subset(rfdat, sample == FALSE)

rf_classifier = randomForest(best ~ ., data=train, ntree=500, mtry=7, importance=TRUE)

test$fitted <- predict(rf_classifier, newdata=test[-1])
pred <- predict(rf_classifier, newdata=test[-1])
rf_classifier$importance
sum(test$fitted == test$best)/nrow(test)
varImpPlot(rf_classifier)
