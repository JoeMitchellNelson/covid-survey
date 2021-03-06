require(pacman)

p_load(rpart,rpart.plot,readr,dplyr,RCurl,rjson,lubridate,
       rvest,stringr,Hmisc,rattle,RColorBrewer,ddpcr,tidytext,tidyr,
       ggrepel,ggplot2,png,ggpubr,tidycensus,sf,plm,lmtest,stargazer,MASS,
       xtable,knitr,magick,purrr,ggthemes,gifski,extrafont,latex2exp,
       cowplot,mapproj,patchwork,remotes,tictoc,Hmisc,english,readstata13,lfe)

p_load(glmnet,tibble,broom,caret,h2o)



labsdat <- read.dta13("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/variables_for_selection_models.dta")

labs <- data.frame(variable = names(labsdat),label=attributes(labsdat)$var.labels)
labs$variable <- as.character(labs$variable)
labs$label <- as.character(labs$label)

completes <- read.csv("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_nointx.csv") %>%
  # dplyr::filter(rejectonly==0) %>%
  dplyr::filter(Durationinseconds > 360) %>%
  dplyr::filter(choiceofperson %in% 1:2) %>%
  group_by(ResponseId) %>%
  mutate(rejectever = sum(rejectonly)) %>%
  ungroup() %>%
  dplyr::filter(rejectever==0) %>%
  dplyr::select(ResponseId,rejectever) %>%
  unique()

 completes <- completes$ResponseId %>% as.character()

dat <- read.dta13("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/variables_for_selection_models.dta") %>% dplyr::select(-LocationLatitude,-LocationLongitude)

 dat$complete <- ifelse(dat$ResponseId %in% completes,1,0)
 
 
  dat$y2020m7deaths <- ifelse(dat$y2020m7deaths<0,0, dat$y2020m7deaths)
  dat$y2020m10deaths <- ifelse(dat$y2020m10deaths<0,0, dat$y2020m10deaths)
  dat$y2020m12deaths <- ifelse(dat$y2020m12deaths<0,0, dat$y2020m12deaths)
 

dat <- dat[,!str_detect(names(dat),"^On")]

dat <- dat %>% dplyr::select(ResponseId,eligible,complete,popwt,everything())
dat$countypop <- dat$countypop/1000

dat2 <- read.dta13("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_nointx.dta")
varlabs <- data.frame(variable = names(dat2),label= attributes(dat2)$var.labels)
varlabs <- varlabs %>% dplyr::filter(variable %in% names(dat))
varlabs$label <- as.character(varlabs$label)
varlabs$variable <- as.character(varlabs$variable)
varlabs$label <- ifelse(varlabs$label=="",varlabs$variable,varlabs$label)

exdat <- dat %>% dplyr::select(-ResponseId)

for (i in 5:ncol(dat)) {
  for (j in 5:ncol(dat)) {
    if (i <= j) {  
      exdat$newvar <- dat[,i] * dat[,j]
      names(exdat)[which(names(exdat)=="newvar")] <- paste0(names(dat)[i],":",names(dat)[j])
    }
  }
}


# remove duplicates, i.e. dummy and same dummies^2

exdat <- exdat[!duplicated(as.list(exdat))]

# remove columns with no variation

thenames <- names(exdat)

for (i in 2:length(thenames)) {
  if (length(unique(exdat[,which(names(exdat)==thenames[i])]))==1) {
    exdat <- exdat[,which(!names(exdat) == thenames[i])]
  }
}



probform <- names(exdat)[4:length(names(exdat))] %>% paste(collapse=" + ")
probform <- paste("complete ~", probform) %>% as.formula


exmat <- model.matrix(probform,exdat)[,-1]
bestweight <- exdat %>% dplyr::select(complete,popwt)


################### Ready for LASSO ###########################

set.seed(123)

cv.lasso <- cv.glmnet(x = exmat,y = bestweight[,1], alpha = 1, family="binomial",
                      weights=bestweight[,2],
                      lambda = NULL)

plot(cv.lasso)

model <- glmnet(x = exmat,y = bestweight[,1], alpha = 1, family = "binomial",weights=bestweight[,2],
                lambda = cv.lasso$lambda.min)
model2 <- glmnet(x = exmat,y = bestweight[,1], alpha = 1, family = "binomial",weights=bestweight[,2],
                lambda = cv.lasso$lambda.1se)

plot(cv.lasso)


keepers <- model$beta %>% as.matrix %>% as.data.frame() %>% rownames_to_column("vars") %>% dplyr::filter(s0!=0)
keepers <- keepers$vars 
keepers


keepers2 <- model2$beta %>% as.matrix %>% as.data.frame() %>% rownames_to_column("vars") %>% dplyr::filter(s0!=0)
keepers2 <- keepers2$vars 
keepers2

# probform2 doesn't force levels for all interactions
probform2 <- paste0("complete ~ ",paste(keepers,collapse=" + ")) %>% as.formula()

# probform3 does force levels for all interactions 
probform3 <- paste0("complete ~ ",paste(keepers,collapse=" + ")) %>% str_replace_all(":","*") %>% as.formula()

# probform2 doesn't force levels for all interactions
probform2a <- paste0("complete ~ ",paste(keepers2,collapse=" + ")) %>% as.formula()

# probform3 does force levels for all interactions 
probform3a <- paste0("complete ~ ",paste(keepers2,collapse=" + ")) %>% str_replace_all(":","*") %>% as.formula()

ran <- sample(1:1412,1412*.9,replace=F)

a <- glm(probform2, weights=popwt, family = "binomial", 
         data = dat[ran,])
b <- glm(probform3, weights=popwt, family = "binomial", 
              data = dat[ran,])

c <- glm(probform2a, weights=popwt, family = "binomial", 
         data = dat[ran,])
d <- glm(probform3a, weights=popwt, family = "binomial", 
         data = dat[ran,])

weighted.mean((predict(a,dat[-ran,])>0)==(dat$complete[-ran]==1),w=dat$popwt[-ran])
weighted.mean((predict(b,dat[-ran,])>0)==(dat$complete[-ran]==1),w=dat$popwt[-ran])
weighted.mean((predict(c,dat[-ran,])>0)==(dat$complete[-ran]==1),w=dat$popwt[-ran])
weighted.mean((predict(d,dat[-ran,])>0)==(dat$complete[-ran]==1),w=dat$popwt[-ran])

a <- glm(probform2, weights=popwt, family = "binomial", 
         data = dat)
b <- glm(probform3, weights=popwt, family = "binomial", 
         data = dat)

c <- glm(probform2a, weights=popwt, family = "binomial", 
         data = dat)
d <- glm(probform3a, weights=popwt, family = "binomial", 
         data = dat)



demeans <- data.frame(ResponseId = dat$ResponseId,
                      complete = dat$complete,
                      lassorp= (predict(a) - mean(predict(a)))/sd(predict(a)),
                      lassorpfl = (predict(b) - mean(predict(b)))/sd(predict(b)),
                      lassorp1se= (predict(c) - mean(predict(c)))/sd(predict(c)),
                      lassorpfl1se = (predict(d) - mean(predict(d)))/sd(predict(d)))

ggplot() +
  geom_vline(xintercept=0,linetype="dashed",color="grey50") +
  geom_density(data=demeans,aes(x=lassorpfl,group=complete,fill=factor(complete)),alpha=.5,color=NA,bw=.2) + 
  scale_fill_manual(values=c("red","forestgreen"),labels = c("Non-response", "Response")) +
  scale_color_manual(values="black") +
  geom_density(data=demeans,aes(x=lassorpfl),fill=NA,color="black",alpha=.3,bw=.2) +
  labs(x="Demeaned response propensity",y="Density",fill="") +
  lims(x=c(-4,4)) +
  theme_minimal()
  

write.csv(demeans,"~/covid-survey/demeanrp-lasso.csv")
write.csv(demeans,"C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID-shared/intermediate-files/demeanrp-lasso.csv")


######## make coef table ##########


fixnamesselect <- function (x) {
  x <- paste(x,collapse="\n")
  
  
  for (i in nrow(labs):1) {
    if(str_detect(x,paste0("",labs$variable[i]))) {
      x <- str_replace_all(x,labs$variable[i],labs$label[i])
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

var.order <-  scan("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/do-files/ESTUNIVERSESEL_list.txt",what=character())
var.order <- str_replace_all(var.order,"_",":")




lassocoef <- stargazer(b,type="latex",no.space=T,order=var.order,single.row=T) %>% str_replace_all(":"," $\\\\times$ ") %>% fixnamesselect()

lassocoef <- str_replace_all(lassocoef," \\((?=[:digit:])"," & (") %>% str_remove_all("race 1=Own ")
#%>% str_remove_all("\\(|\\)") 
cat(lassocoef)
write(lassocoef,"~/covid-survey/tables/lassorp.tex")

#######################################

# in-sample accuracy
weighted.mean(round(b$fitted.values)==bestweight[,1], w=bestweight[,2])

##########################################

######## descriptive stats ############

varlist <- data.frame(variable=names(b$coefficients),v2=NA)
varlist <- varlist[which(!str_detect(varlist$variable,":")),]
varlist$variable <- as.character(varlist$variable)
varlist <- left_join(varlist,labs)
varlist <- varlist %>% dplyr::filter(variable!="(Intercept)")

for (i in 1:nrow(varlist)) {
  if (length(which(var.order == varlist$variable[i]))>0) {
  varlist$v2[i] <- which(var.order == varlist$variable[i])
  }
}

varlist <- varlist[order(varlist$v2),]


# make descriptive statistics

v <- varlist$variable

t <- c()

for (i in 1:length(v)) {
  
  if (v[i] %in% names(exdat)) {
  
  r <- c(mean(exdat[which(exdat$complete==1),which(names(exdat)==v[i])]),
         sd(exdat[which(exdat$complete==1),which(names(exdat)==v[i])]),
         min(exdat[which(exdat$complete==1),which(names(exdat)==v[i])]),
         max(exdat[which(exdat$complete==1),which(names(exdat)==v[i])])) %>% round(3)
  
  } 
  
  if (!is.na(varlist$label[i])) {
  
  r <- c(varlist$label[i],r)
  
  } else {
    r <- c(varlist$variable[i],r)
  }
  r <- r %>% paste(collapse= " & ") %>% paste(" \\\\ \n ")
  
  t <- paste0(t,r)
  
}


cat(t)

# longhead <- read_file("~/covid-survey/tables/longtableheader.txt")
# longfoot <- read_file("~/covid-survey/tables/longtablefooter.txt")
# 
# s <- paste0(longhead,t,longfoot)
# cat(s)
write(t,"~/covid-survey/tables/selection-desc.tex")


# not using this
{

############## Elastic Net ##################

# cross-validation to find alpha

train_cont <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 5,
                           search = "random",
                           verboseIter = TRUE)

elastic_reg <- caret::train(y=bestweight[,1],
                     x = exmat,
                     weights=bestweight[,2],
                     method = "glmnet_h2o",
                     family=binomial(link="logit"),
                     preProcess = c("center", "scale"),
                     tuneLength = 10,
                     trControl = train_cont)

model <- glmnet(y = bestweight[,1],
                x=exmat, alpha = elastic_reg$bestTune[1], family=binomial(link="logit"),weights=bestweight[,2],
                lambda = elastic_reg$bestTune[2]/50)
coef(model)

keepers <- model$beta %>% as.matrix %>% as.data.frame() %>% rownames_to_column("vars") %>% dplyr::filter(s0!=0)
keepers <- keepers$vars 
keepers

# probform2 doesn't force levels for all interactions
probform2 <- paste0("complete ~ ",paste(keepers,collapse=" + ")) %>% as.formula()

# probform3 does force levels for all interactions 
probform3 <- paste0("complete ~ ",paste(keepers,collapse=" + ")) %>% str_replace_all(":","*") %>% as.formula()

ran <- sample(1:1412,1412*.9,replace=F)

a <- glm(probform2, weights=popwt, family = binomial(link = "logit"), 
         data = dat[ran,])
b <- glm(probform3, weights=popwt, family = binomial(link = "logit"), 
         data = dat)

sum((predict(a,dat[-ran,])>0)==(dat$complete[-ran]==1))/(1412-length(ran))
sum((predict(a,dat)>0)==(dat$complete==1))/1412


############################################
######## Testing on fake data ###############
#############################################

d1 <- data.frame(x1=rnorm(1000))

for (i in 2:100) {
  d1$newvar <- rnorm(1000)
  names(d1)[which(names(d1)=="newvar")] <- paste0("x",i)
}

d1 <- d1 %>% mutate(y = x1 + x11 + x13 + x17 + x31 + x53)
d1$y <- d1$y + rnorm(1000)
d1$y <- ifelse(d1$y>0,1,0)

d2 <- model.matrix(y ~ .,d1)[,-1]

train_cont <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 2,
                           search = "random",
                           verboseIter = TRUE)

elastic_reg <- caret::train(y = d1$y,
                            x=d2,
                            method = "glmnet",
                            family="binomial",
                            preProcess = c("center", "scale"),
                            tuneLength = 10,
                            trControl = train_cont)

model <- glmnet(y = d1$y,
                x=d2, alpha = elastic_reg$bestTune[1], family=binomial(link="logit"),
                lambda = elastic_reg$bestTune[2])
coef(model)

keepers <- model$beta %>% as.matrix %>% as.data.frame() %>% rownames_to_column("vars") %>% dplyr::filter(s0!=0)
keepers <- keepers$vars 
keepers

d1 <- data.frame(x1=rnorm(1000))

for (i in 2:100) {
  d1$newvar <- rnorm(1000)
  names(d1)[which(names(d1)=="newvar")] <- paste0("x",i)
}

d1 <- d1 %>% mutate(y = x1 + x11 + x13 + x17 + x31 + x53)
d1$y <- d1$y + rnorm(1000)
d1$y <- ifelse(d1$y>0,1,0)


a <- glm(
  (paste0("y ~ ",paste(keepers,collapse=" + ")) %>% as.formula()), family = binomial(link = "logit"), 
         data = d1)

d1$fitted <- fitted.values(a)

summary(a$fitted.values)
summary(round(d1$fitted) == d1$y)

}
