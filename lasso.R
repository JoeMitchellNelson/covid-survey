require(pacman)

p_load(rpart,rpart.plot,readr,dplyr,RCurl,rjson,lubridate,
       rvest,stringr,Hmisc,rattle,RColorBrewer,ddpcr,tidytext,tidyr,
       ggrepel,ggplot2,png,ggpubr,tidycensus,sf,plm,lmtest,stargazer,MASS,
       xtable,knitr,magick,purrr,ggthemes,gifski,extrafont,latex2exp,
       cowplot,mapproj,patchwork,remotes,tictoc,Hmisc,english,readstata13)

p_load(glmnet)

dat <- read.dta13("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_intx.dta")
pair <- read.csv("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/forR_VSL-COVID_binary.csv")

tempdat <- dat %>% dplyr::select(which(str_detect(names(dat),"(caseid|OR|WA|female|hhld|pzip|fedui|agecont|^com|ffcom|owninc|demeanrp|vuln|certain)") & 
                                         !str_detect(names(dat),"_"))) %>% unique()

psmall1 <- pair %>% dplyr::select(caseid,best,mabsdeaths,mabsnfcases,avcost,unempl,statquo,
                                 rule1,rule2,rule3,rule4,rule5,rule6,rule7,rule8,rule9,rule10)

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

cv.lasso <- cv.glmnet(x = pair.int[,2:ncol(pair.int)], y = pair.int[,1], family = "binomial", alpha = 1, lambda = NULL)

model <- glmnet(pair.int[,2:ncol(pair.int)],pair.int[,1], alpha = 1, family = "binomial",
                lambda = cv.lasso$lambda.min)
plot(cv.lasso)
coef(model)

keepers <- model$beta %>% as.matrix %>% as.data.frame() %>% dplyr::filter(s0!=0)
keepers <- row.names(keepers) %>% str_replace_all(":","*")
keepers

form1 <- paste0("best ~ ", paste(keepers,collapse=" + ")," + strata(choice)") %>% as.formula
summary(res <- clogit(form1,data=dat))
stargazer(res,type="text")
