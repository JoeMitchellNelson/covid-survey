require(pacman)

p_load(rpart,rpart.plot,readr,dplyr,RCurl,rjson,lubridate,
       rvest,stringr,Hmisc,rattle,RColorBrewer,ddpcr,tidytext,tidyr,
       ggrepel,ggplot2,png,ggpubr,tidycensus,sf,plm,lmtest,stargazer,MASS,
       xtable,knitr,magick,purrr,ggthemes,gifski,extrafont,latex2exp,
       cowplot,mapproj,patchwork,remotes,tictoc,Hmisc,english,readstata13)


p_load(glmnet,tibble,broom)

######### BIG MODEL ##################


#dat <- read.dta13("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_intx.dta")
#pair <- read.csv("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/forR_VSL-COVID_binary.csv")

dat <- read.dta13("C:/Users/Joe/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_intx.dta")
pair <- read.csv("C:/Users/Joe/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/forR_VSL-COVID_binary.csv")

#### CHANGE DEFINITION OF DEATH ######
#dat$mabsdeaths <- (dat$mabsdeaths/dat$countypop)*100000
#dat$mabsnfcases <- (dat$mabsnfcases/dat$countypop)*100000


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
                lambda = exp(-5))
               # lambda = cv.lasso$lambda.min,weights=pair.int[,2])
plot(cv.lasso)
coef(model)

keepers <- model$beta %>% as.matrix %>% as.data.frame() %>% rownames_to_column("vars") %>% dplyr::filter(s0!=0)
keepers <- keepers$vars %>% str_replace_all(":","*")

DMRPBASEVARS <- paste(BASEVARS,"*demeanrp")
DMRPINTX <- paste(keepers,"*demeanrp")
DMRPVARS <- c(DMRPBASEVARS,DMRPINTX)

NORPVARS <- c(BASEVARS,keepers)

form1 <- paste0("best ~ ", paste(NORPVARS,collapse=" + ")," + strata(choice)") %>% as.formula
summary(res <- clogit(form1,data=dat))
#stargazer(res,type="text")

restidy <- broom::tidy(res) %>% dplyr::filter(!is.na(estimate)) %>% 
  dplyr::select(-conf.low,-conf.high)
restidy <- restidy[which(!str_detect(restidy$term,"demeanrp")),]

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
