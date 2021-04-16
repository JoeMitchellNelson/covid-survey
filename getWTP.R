require(pacman)

p_load(rpart,rpart.plot,readr,dplyr,RCurl,rjson,lubridate,
       rvest,stringr,Hmisc,rattle,RColorBrewer,ddpcr,tidytext,tidyr,
       ggrepel,ggplot2,png,ggpubr,tidycensus,sf,plm,lmtest,stargazer,MASS,
       xtable,knitr,magick,purrr,ggthemes,gifski,extrafont,latex2exp,
       cowplot,mapproj,patchwork,remotes,tictoc,Hmisc,english,readstata13)


p_load(glmnet,tibble,broom)

dat <- read.dta13("C:/Users/Joe/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_intx.dta")

getmodel <- function (fac,df) {
  
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
  
  form1 <- c(allv,paste0(allv,":lassorpfl"))
  form2 <- paste(form1,collapse=" + ")
  
  
  #form2 <- form2 %>% str_replace_all("unempl","feduiany*unempl") %>% str_replace_all("avcost","feduiany*avcost")
  
  form3 <- paste0("best ~",form2,"+ strata(choice)") %>% as.formula
  
  clogit(form3,data=newdf,weights=popwt,method="approximate")
}


# main regression
{
main3 <- clogit(best ~
                  
                  mabsdeaths + mabscases +
                  feduinoneavcost + feduianyavcost + 
                  feduinoneunempl + feduianyunempl + 
                  
                  rule1 + rule2 +
                  rule3 + rule4 +
                  rule5 + rule6 +
                  rule7 + rule8 +
                  rule9 + rule10 +
                  
                  statquo +
                  
                  mabsdeaths:lassorpfl + mabscases:lassorpfl +
                  feduinoneavcost:lassorpfl + feduianyavcost:lassorpfl +
                  feduinoneunempl:lassorpfl + feduianyunempl:lassorpfl +

                  rule1:lassorpfl + rule2:lassorpfl +
                  rule3:lassorpfl + rule4:lassorpfl +
                  rule5:lassorpfl + rule6:lassorpfl +
                  rule7:lassorpfl + rule8:lassorpfl +
                  rule9:lassorpfl + rule10:lassorpfl +

                  statquo:lassorpfl +
                  strata(choice),data=dat[which(dat$choiceofperson %in% 1:2),]
                ,weights=popwt,method="approximate"
)

}

wtpest <- mvrnorm(n=1000,mu=main3$coefficients,Sigma=main3$var) %>% as.data.frame()

wtpest <- wtpest %>% mutate(VSL = mabsdeaths/feduinoneavcost * 100 * 12500)

quantile(wtpest$VSL,probs=c(0.05,0.5,0.95))

###

gendermodel <- getmodel("gender",dat[which(dat$gender %in% c("Men","Women")),])

genderest <- mvrnorm(n=1000,mu=gendermodel$coefficients,Sigma=gendermodel$var) %>% as.data.frame()

genderest <- genderest %>% mutate(mrs = mabscases_Women/feduinoneavcost_Women * 100/1000)

quantile(genderest$mrs,probs=c(0.05,0.5,0.95))

###

ideol2model <- getmodel("ideol2",dat[which(dat$ideol2 != "MISSING"),])

ideol2est <- mvrnorm(n=1000,mu=ideol2model$coefficients,Sigma=ideol2model$var) %>% as.data.frame()

ideol2est <- ideol2est %>% mutate(mrs = mabscases_moderate/feduinoneavcost_moderate * 100/1000 * 12500)

quantile(ideol2est$mrs,probs=c(0.05,0.5,0.95))

ideol2est <- ideol2est %>% mutate(mrs = mabsdeaths_liberal/feduinoneavcost_liberal * 100 * 12500)

quantile(ideol2est$mrs,probs=c(0.05,0.5,0.95))

ideol2est <- ideol2est %>% mutate(mrs = mabsdeaths_conservative/feduinoneavcost_conservative * 100 * 12500)

quantile(ideol2est$mrs,probs=c(0.05,0.5,0.95))

###

educationmodel <- getmodel("education",dat[which(dat$education != "MISSING"),])

educationest <- mvrnorm(n=1000,mu=educationmodel$coefficients,Sigma=educationmodel$var) %>% as.data.frame()

educationest <- educationest %>% mutate(mrs = mabscases_Noncollege/feduinoneavcost_Noncollege * 100/1000 * 12500)

quantile(educationest$mrs,probs=c(0.05,0.5,0.95))

### EVERYONE #####

set.seed(665)

wtpest <- mvrnorm(n=1000,mu=main3$coefficients,Sigma=main3$var) %>% as.data.frame()

wtpest <- wtpest[,which(!str_detect(names(wtpest),"lassorpfl"))]

get95 <- function (x) {
  quantile(x,probs=c(0.025,0.5,0.975))
}

get90 <- function (x) {
  quantile(x,probs=c(0.05,0.5,0.95))
}

get99 <- function (x) {
  quantile(x,probs=c(0.005,0.5,0.995))
}

wtpest$mabscases <- wtpest$mabscases / 1000

wtp2 <- wtpest/wtpest$feduinoneavcost * 100

wtp3 <- sapply(wtp2,get95) %>% t() %>%  as.data.frame()
wtp90 <- sapply(wtp2,get90) %>% t() %>%  as.data.frame()
wtp99 <- sapply(wtp2,get99) %>% t() %>%  as.data.frame()

for (i in 1:nrow(wtp3)) {
  rownames(wtp3)[i] <- fixnames(rownames(wtp3)[i])
  rownames(wtp90)[i] <- fixnames(rownames(wtp90)[i])
  rownames(wtp99)[i] <- fixnames(rownames(wtp99)[i])
}

wtp3 <- round(wtp3,2)

wtp90$sig90 <- ifelse(sign(wtp90$`5%`)==sign(wtp90$`95%`),"*","")
wtp99$sig99 <- ifelse(sign(wtp99$`0.5%`)==sign(wtp99$`99.5%`),"*","")
wtp3$sig95 <- ifelse(sign(wtp3$`2.5%`)==sign(wtp3$`97.5%`),"*","")

wtp3$sig90 <- wtp90$sig90
wtp3$sig99 <- wtp99$sig99

wtp3$stars <- paste0(wtp3$sig90,wtp3$sig95,wtp3$sig99)

wtp3 <- wtp3[which(!str_detect(rownames(wtp3),"federal UI")),]

wtp3 <- wtp3 %>% dplyr::filter(stars!="") %>% dplyr::select(-sig95,-sig99,-sig90)
wtp3$stars <- paste0("$^{",wtp3$stars,"}$")

wtp3$`50%` <- paste0(wtp3$`50%`,wtp3$stars)
wtp3 <- wtp3 %>% dplyr::select(-stars)

i = 1

wtptable <- c()

for (i in 1:nrow(wtp3)) {
row <- paste0(rownames(wtp3)[i], " & ", wtp3$`50%`[i], " \\\\ \n",
              "               & (",wtp3$`2.5%`[i],", ",wtp3$`97.5%`[i],") \\\\ \n")
wtptable <- c(wtptable,row)
}

header <- "\\begin{table}[ht]
\\caption{Full sample}
\\centering
\\begin{tabular}{lc}
\\hline 
 & Everyone \\\\
  \\hline \n" 

footer <- "\\hline
\\end{tabular}
\\end{table}"

wtptable <- c(header,wtptable,footer)

cat(wtptable)
########## households per 50k residents ############

censdat <- get_acs(geography = "county",
                   state=c("OR","CA","WA"),
                   year=2018,
                   cache_table=T,
                   variables=c(hh='B00002_001',
                               pop='B00001_001'),

                   output="wide")
head(censdat)

sum(censdat$popE)/sum(censdat$hhE)

# average hh size is 2.616767
# so 19107.55 households per 50k population
