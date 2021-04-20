require(pacman)

p_load(dplyr,RCurl,lubridate,
       stringr,Hmisc,RColorBrewer,
       ggrepel,ggplot2,png,stargazer,
       ggthemes,extrafont,
       cowplot,readstata13,glmnet,tibble,broom,caret,h2o)


# change file path
dat <- read.dta13("C:/Users/joe/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/variables_for_selection_models.dta") %>% dplyr::select(-LocationLatitude,-LocationLongitude)

dat <- dat[,!str_detect(names(dat),"^On")]

dat <- dat %>% dplyr::select(ResponseId,eligible,complete,popwt,everything())

# integer overflow without this line
dat$countypop <- dat$countypop/1000


# create all possible interactions
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


# create formula that includes EVERY variable and interaction
probform <- names(exdat)[4:length(names(exdat))] %>% paste(collapse=" + ")
probform <- paste("complete ~", probform) %>% as.formula

# use formula and exdat to create massive "model.matrix" object for lasso
exmat <- model.matrix(probform,exdat)[,-1]

# outcomes and weights only
bestweight <- exdat %>% dplyr::select(complete,popwt)


################### Ready for LASSO ###########################

set.seed(123)

# This line is slow, but we're talking like 3-5 minutes, not 3-5 hours
cv.lasso <- cv.glmnet(x = exmat,y = bestweight[,1], alpha = 1, family= "binomial",
                      weights=bestweight[,2],
                      lambda = NULL)

plot(cv.lasso)

# with optimal lambda and lambda1se, run lasso for variable selection
model <- glmnet(x = exmat,y = bestweight[,1], alpha = 1, family = "binomial",weights=bestweight[,2],
                lambda = cv.lasso$lambda.min)
model2 <- glmnet(x = exmat,y = bestweight[,1], alpha = 1, family = "binomial",weights=bestweight[,2],
                lambda = cv.lasso$lambda.1se)

plot(cv.lasso)

# create vectors of "keepers" from each model
keepers <- model$beta %>% as.matrix %>% as.data.frame() %>% rownames_to_column("vars") %>% dplyr::filter(s0!=0)
keepers <- keepers$vars 
keepers

keepers2 <- model2$beta %>% as.matrix %>% as.data.frame() %>% rownames_to_column("vars") %>% dplyr::filter(s0!=0)
keepers2 <- keepers2$vars 
keepers2

# probform2 doesn't force levels for all interactions
probform2 <- paste0("complete ~ ",paste(keepers,collapse=" + ")) %>% as.formula()

# probform3 DOES force levels for all interactions, by switching ":" to "*" 
# : is like # and * is like ##, I think
probform3 <- paste0("complete ~ ",paste(keepers,collapse=" + ")) %>% str_replace_all(":","*") %>% as.formula()

# probform2a doesn't force levels for all interactions
probform2a <- paste0("complete ~ ",paste(keepers2,collapse=" + ")) %>% as.formula()

# probform3a DOES force levels for all interactions 
probform3a <- paste0("complete ~ ",paste(keepers2,collapse=" + ")) %>% str_replace_all(":","*") %>% as.formula()

##### OPTIONAL: split into train/test samples and test accuracy
### (highlight all lines and ctrl-shift-c to comment/uncomment)

# ran <- sample(1:1412,1412*.9,replace=F)
# 
# a <- glm(probform2, weights=popwt, family = "binomial", 
#          data = dat[ran,])
# b <- glm(probform3, weights=popwt, family = "binomial", 
#               data = dat[ran,])
# 
# c <- glm(probform2a, weights=popwt, family = "binomial", 
#          data = dat[ran,])
# d <- glm(probform3a, weights=popwt, family = "binomial", 
#          data = dat[ran,])
# 
# weighted.mean((predict(a,dat[-ran,])>0)==(dat$complete[-ran]==1),w=dat$popwt[-ran])
# weighted.mean((predict(b,dat[-ran,])>0)==(dat$complete[-ran]==1),w=dat$popwt[-ran])
# weighted.mean((predict(c,dat[-ran,])>0)==(dat$complete[-ran]==1),w=dat$popwt[-ran])
# weighted.mean((predict(d,dat[-ran,])>0)==(dat$complete[-ran]==1),w=dat$popwt[-ran])



a <- glm(probform2, weights=popwt, family = "binomial",   # uses exactly the variables selected by lasso at optimal lambda
         data = dat)
b <- glm(probform3, weights=popwt, family = "binomial",   # forces in levels from interactions in model "a" (this is the one i use)
         data = dat)

c <- glm(probform2a, weights=popwt, family = "binomial", # uses exactly the variables selected by lasso at 1se lambda
         data = dat)
d <- glm(probform3a, weights=popwt, family = "binomial", # forces in levels from interactions in model "c"
         data = dat)


demeans <- data.frame(ResponseId = dat$ResponseId,
                      complete = dat$complete,
                      lassorp= (predict(a) - mean(predict(a)))/sd(predict(a)),
                      lassorpfl = (predict(b) - mean(predict(b)))/sd(predict(b)),
                      lassorp1se= (predict(c) - mean(predict(c)))/sd(predict(c)),
                      lassorpfl1se = (predict(d) - mean(predict(d)))/sd(predict(d)))


# in-sample accuracy of lassorpfl
weighted.mean(round(b$fitted.values)==bestweight[,1], w=bestweight[,2])

# changing type to "text" might be convenient?
demeantable <- stargazer(a,b,type="latex")

write(demeantable,"~/covid-survey/responsenonresponse_model.bib")


demeans$complete <- ifelse(demeans$complete==1,"Responders","Non-responders")


ggplot(demeans) +
  geom_density(aes(x=lassorpfl,group=factor(complete),fill=factor(complete)),alpha=.5) +
  scale_fill_manual(values=c("blue","forestgreen")) +
  labs(x="Demeaned response propensity",y="Smoothed density",fill="") +
  theme_minimal() 


#write.csv(demeans,"~/covid-survey/demeanrp-lasso.csv")
#write.csv(demeans,"C:/Users/joe/Dropbox (University of Oregon)/VSL-COVID-shared/intermediate-files/demeanrp-lasso.csv")


