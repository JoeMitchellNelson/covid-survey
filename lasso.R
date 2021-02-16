require(pacman)

p_load(rpart,rpart.plot,readr,dplyr,RCurl,rjson,lubridate,
       rvest,stringr,Hmisc,rattle,RColorBrewer,ddpcr,tidytext,tidyr,
       ggrepel,ggplot2,png,ggpubr,tidycensus,sf,plm,lmtest,stargazer,MASS,
       xtable,knitr,magick,purrr,ggthemes,gifski,extrafont,latex2exp,
       cowplot,mapproj,patchwork,remotes,tictoc,Hmisc,english,readstata13)



p_load(glmnet)

dat <- read.dta13("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_intx.dta")
pair <- read.csv("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/forR_VSL-COVID_binary.csv")

tempdat <- dat %>% dplyr::select(which(str_detect(names(dat),"(caseid|OR|WA|female|hhld|pzip|agecont|^com|ffcom|demeanrp|vuln|owninc)") & 
                                         !str_detect(names(dat),"_"))) %>% unique()

psmall1 <- pair %>% dplyr::select(caseid,best,mabsdeaths,mabsnfcases,avcost,unempl,
                                 rule1,rule2,rule3,rule4,rule5,rule6,rule7,rule8,rule9,rule10,statquo)

psmall <- left_join(psmall1,tempdat,by="caseid")
psmall <- psmall[,which(!str_detect(names(psmall),"\\.x"))]

names(psmall) <- str_remove_all(names(psmall),"\\.y")

pdat <- psmall %>% unique() %>% dplyr::select(!caseid)

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

########## FOR LASSO ############

cv.lasso <- cv.glmnet(x = pair.int[,2:ncol(pair.int)], y = pair.int[,1], family = "binomial", alpha = 1, lambda = NULL)

model <- glmnet(pair.int[,2:ncol(pair.int)],pair.int[,1], alpha = 1, family = "binomial",
              #  lambda = exp(-5))
                lambda = cv.lasso$lambda.min)
plot(cv.lasso)
coef(model)

keepers <- model$beta %>% as.matrix %>% as.data.frame() %>% dplyr::filter(s0!=0)
keepers <- row.names(keepers) %>% str_replace_all(":","*")
keepers

form1 <- paste0("best ~ ", paste(keepers,collapse=" + ")," + strata(choice)") %>% as.formula
summary(res <- clogit(form1,data=dat))
stargazer(res,type="text")

restidy <- tidy(res) %>% dplyr::filter(!is.na(estimate))

########## sort variables and make labels for lasso results ###########


basevars <- names(pdat)[vwic]
names_key1 <- data.frame(var=basevars,code=(1:length(basevars))*1000)
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
