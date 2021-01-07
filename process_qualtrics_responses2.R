########################################################
####### this code reproduces the second part of Trudy's getdata_qualtrics_responses.do 
####### code functionality is almost one-to-one with Trudy's code
####### differences between R and Stata are noted (where they're not obvious)
########################################################

### regex for auto-fixing the label commands with search-replace
# first, search for "label variable ", replace with "label(dat$"
# second, turn on regex, search for (?<=label\(dat\$.{1,80})\s{1,3}(?="), replace with ") <- "

## regex for auto-fixing rename commands
# trailing quotation mark
# (?<=rename Q.{1,3} .{1,80})(?=$) -> '"'
# leading quotation mark
#(?<=rename Q.{1,3}) (?=.{1,80}$) -> ' "'
# "rename " -> names(dat)[which(names(dat)=="
# '(?<=\)==\"Q.{1,3}) ' -> "] <- 


# load in some useful R packages
require(pacman)

p_load(tidyverse,tidycensus,ggthemes,ggplot2,Hmisc,english)

# "clear all"
rm(list=ls())

dat <- read.csv("~/minimal_full_dataset.csv") %>% dplyr::select(-X)
PREP <- c("prepover","prepreas","preppoor")
RULES <- c("rulesbad", "rulesgood", "rulesinsuff")
WRONGS <- c("w_exactnum", "w_petstores", "w_daycare",  "w_nopopup", "w_uneven",  "w_samecost")

# starts on line 1105
dat$caseid = dat$ExternalReference

# variables constant within a choice set but differ across choice sets within each person (except for alt)
CHOALT <- c("best", "choice", "alt") #    

# individual-specific variables
INDIV <- c("caseid", "newcase", "lirule1", "lirule2", "lirule3", "lirule4", "lirule5", "lirule6", "lirule7", "lirule8", "lirule9", 
           "lirule10", "larule1", "larule2", "larule3", "larule4", "larule5", "larule6", "larule7", "larule8", "larule9", 
           "larule10", "hhld0to1", "hhld2to5", "hhld6to12", "hhld13to17", "hhld18to64", "hhld65up", "ffltc", "transityes", 
           "transitsome", "pastcounty0to2", "pastcounty3to5", "pastcounty6to10", "pastcounty10up", "futrcounty0to2", "futrcounty3to5", 
           "futrcounty6to10", "futrcounty10up", "futrcountyuncer", "comheart", "comdiab", "comresp", "comcanc", "comage", "compreg", 
           "comcovid", "comother", "ffcomheart", "ffcomdiab", "ffcomresp", "ffcomcanc", "ffcomage", "ffcompreg", "ffcomcovid", 
           "ffcomother", "rnought0to1", "rnought1to2", "rnought2to3", "rnought3to5", "rnought5to7", "rnought7to10", "rnought010up",
           "vulnchild", "vulnteens", "vulnseniors", "vulnwomen", "vulnmen", "vulnnonwhite", "vulnnoneng", "vulnlowinc", "vulnrural", 
           "vulnnosci", "vulnessent", "idrule1", "idrule2", "idrule3", "idrule4", "idrule5", "idrule6", "idrule7", "idrule8", 
           "idrule9", "idrule10", "stayhome", "hhldinc", "owninc", "pctincloss",PREP, RULES, WRONGS)     


#will be calculated during the stacking process?
#                    y         y       y     y      y      y      y      y      y        y        y     y    y     y      y
OTHERATTRIB <- c("threealts", "chset1", "chset2", "chset3", "chset4", "chset5", "chset6", "chset7", 
                 "chset8", "noPolicy", "t_ch", "c_ch", "cert", "cases", "deaths", "reject")  # fstch sloch

# cases deaths
DESIGN <- c("fedui", "months", "delcases", "deldeaths", "rule1", "rule2", "rule3", "rule4", "rule5", 
            "rule6", "rule7", "rule8", "rule9", "rule10", "unempl", "avcost")  # the generic version of the individual DESIGN_`1etter' cases

# cases_`letter' deaths_`letter'
for (letter in c("a","b","c","d","e","f","n")) {
  
  temp <- c("fedui_", "months_", "delcases_", "deldeaths_", "rule1_", "rule2_", "rule3_",
            "rule4_", "rule5_", "rule6_", "rule7_", "rule8_", "rule9_", "rule10_", "unempl_", "avcost_")
  
  temp <- paste0(temp,letter)
  
  assign(paste0("DESIGN_",letter),temp)
  
}



################ A/N  write first choice, first alternative  

# initialize these for the first choice, first alternative.  replace in all following cases
dat$choice=(dat$newcase-1)*8+1	
dat$alt=1
dat$threealts=0
dat$chset1=1
dat$chset2=0
dat$chset3=0
dat$chset4=0
dat$chset5=0
dat$chset6=0
dat$chset7=0
dat$chset8=0
dat$reject <- ifelse(dat$firstBadNo==1,1,0) %>% replace_na(0) # if bad reason for choosing neither 
dat$best <- ifelse(dat$firstSetChoiceIndicator == 1,1,0) # preserves NAs (aka . in stata)
dat$noPolicy <- dat$firstNoPolicy
dat$t_ch= dat$t_ch1
dat$c_ch= dat$c_ch1
dat$cert=dat$cert1
#dat$fstch=fstch1
#dat$sloch=sloch1 
dat$cases  = dat$cases_a
dat$deaths = dat$deaths_a

file1 <- dat %>% 
  dplyr::filter(!is.na(firstSetChoiceIndicator)) %>% 
  dplyr::select(CHOALT, INDIV, OTHERATTRIB, DESIGN_a)



################ A/N write first choice, second alternative  
dat$alt=3
dat$threealts=0
dat$chset1=1
dat$chset2=0
dat$chset3=0
dat$chset4=0
dat$chset5=0
dat$chset6=0
dat$chset7=0
dat$chset8=0
dat$reject= ifelse(dat$firstBadNo==1,1,0) %>% replace_na(0) # if bad reason for choosing neither 
dat$best <- ifelse(dat$firstSetChoiceIndicator==2,1,0)
dat$noPolicy=dat$firstNoPolicy
dat$t_ch= dat$t_ch1
dat$c_ch= dat$c_ch1
dat$cert= dat$cert1
#dat$fstch=fstch1
#dat$sloch=sloch1 
dat$cases  = dat$cases_a
dat$deaths = dat$deaths_a

file2 <- dat %>% 
  dplyr::filter(!is.na(firstSetChoiceIndicator)) %>% 
  dplyr::select(CHOALT, INDIV, OTHERATTRIB, DESIGN_n)


################ B/N  write second choice, first alternative
dat$choice=(dat$newcase-1)*8+2	
dat$alt=2
dat$threealts=0
dat$chset1=0
dat$chset2=1
dat$chset3=0
dat$chset4=0
dat$chset5=0
dat$chset6=0
dat$chset7=0
dat$chset8=0
dat$reject <- ifelse(dat$secondBadNo==1,1,0) %>% replace_na(0)
dat$best <- ifelse(dat$secondSetChoiceIndicator==1,1,0)
dat$noPolicy= dat$secondNoPolicy
dat$t_ch= dat$t_ch2
dat$c_ch= dat$c_ch2
dat$cert=dat$cert2
#dat$fstch=fstch2
#dat$sloch=sloch2 
dat$cases  = dat$cases_b
dat$deaths = dat$deaths_b

file3 <- dat %>% 
  dplyr::filter(!is.na(secondSetChoiceIndicator)) %>% 
  dplyr::select(CHOALT, INDIV, OTHERATTRIB, DESIGN_b)




################ B/N write second choice, second alternative
dat$alt=3
dat$threealts=0
dat$chset1=0
dat$chset2=1
dat$chset3=0
dat$chset4=0
dat$chset5=0
dat$chset6=0
dat$chset7=0
dat$chset8=0
dat$reject <- ifelse(dat$secondBadNo==1,1,0) %>% replace_na(0)
dat$best <- ifelse(dat$secondSetChoiceIndicator==2,1,0)
dat$noPolicy=dat$secondNoPolicy
dat$ t_ch= dat$t_ch2
dat$ c_ch= dat$c_ch2
dat$cert=dat$cert2
#dat$fstch=fstch2
#dat$sloch=sloch2 
dat$cases  = dat$cases_b
dat$deaths = dat$deaths_b


file4 <- dat %>% 
  dplyr::filter(!is.na(secondSetChoiceIndicator)) %>% 
  dplyr::select(CHOALT, INDIV, OTHERATTRIB, DESIGN_n)


################ C/D/N write third choice, first alternative
dat$choice=(dat$newcase-1)*8+3	
dat$alt=1
dat$threealts=1
dat$chset1=0
dat$chset2=0
dat$chset3=1
dat$chset4=0
dat$chset5=0
dat$chset6=0
dat$chset7=0
dat$chset8=0
dat$reject <- ifelse(dat$thirdBadNo==1,1,0) %>% replace_na(0)
dat$best <- ifelse(dat$thirdSetChoiceIndicator==1,1,0)
dat$noPolicy=dat$thirdNoPolicy
dat$t_ch= dat$t_ch3
dat$c_ch= dat$c_ch3
dat$cert=dat$cert3
#dat$fstch=fstch3
#dat$sloch=sloch3 
dat$cases  = dat$cases_c
dat$deaths = dat$deaths_c


file5 <- dat %>% 
  dplyr::filter(!is.na(thirdSetChoiceIndicator)) %>% 
  dplyr::select(CHOALT, INDIV, OTHERATTRIB, DESIGN_c)


################ C/D/N write third choice, second alternative
dat$alt=2
dat$threealts=1
dat$chset1=0
dat$chset2=0
dat$chset3=1
dat$chset4=0
dat$chset5=0
dat$chset6=0
dat$chset7=0
dat$chset8=0
dat$reject <- ifelse(dat$thirdBadNo==1,1,0) %>% replace_na(0)
dat$best <- ifelse(dat$thirdSetChoiceIndicator==2,1,0)
dat$noPolicy=dat$thirdNoPolicy
dat$t_ch= dat$t_ch3
dat$c_ch= dat$c_ch3
dat$cert=dat$cert3
#dat$fstch=fstch3
#dat$sloch=sloch3 
dat$cases  = dat$cases_c
dat$deaths = dat$deaths_c

file6 <- dat %>% 
  dplyr::filter(!is.na(thirdSetChoiceIndicator)) %>% 
  dplyr::select(CHOALT, INDIV, OTHERATTRIB, DESIGN_d)



################ C/D/N write third choice, third alternative
dat$alt=3
dat$threealts=1
dat$chset1=0
dat$chset2=0
dat$chset3=1
dat$chset4=0
dat$chset5=0
dat$chset6=0
dat$chset7=0
dat$chset8=0
dat$reject <- ifelse(dat$thirdBadNo==1,1,0) %>% replace_na(0)
dat$best <- ifelse(dat$thirdSetChoiceIndicator==3,1,0)
dat$noPolicy=dat$thirdNoPolicy
dat$ t_ch= dat$t_ch3
dat$ c_ch= dat$c_ch3
dat$cert=dat$cert3
#dat$fstch=fstch3
#dat$sloch=sloch3 
dat$cases  = dat$cases_c
dat$deaths = dat$deaths_c


file7 <- dat %>% 
  dplyr::filter(!is.na(thirdSetChoiceIndicator)) %>% 
  dplyr::select(CHOALT, INDIV, OTHERATTRIB, DESIGN_n)



################ C/N  write fourth choice, first alternative
dat$choice=(dat$newcase-1)*8+4	
dat$alt=1
dat$threealts=0
dat$chset1=0
dat$chset2=0
dat$chset3=0
dat$chset4=1
dat$chset5=0
dat$chset6=0
dat$chset7=0
dat$chset8=0
dat$reject <- ifelse(dat$fourthBadNo==1,1,0) %>% replace_na(0)
dat$best <- ifelse(dat$fourthSetChoiceIndicator==1,1,0)
dat$noPolicy=dat$fourthNoPolicy   # problem???
dat$t_ch= dat$t_ch4
dat$c_ch= dat$c_ch4
dat$cert=dat$cert4
#dat$fstch=fstch4
#dat$sloch=sloch4 
dat$cases  = dat$cases_c
dat$deaths = dat$deaths_c


file8 <- dat %>% 
  dplyr::filter(!is.na(fourthSetChoiceIndicator)) %>% 
  dplyr::select(CHOALT, INDIV, OTHERATTRIB, DESIGN_c)



################ C/N  write fourth choice, second alternative
dat$alt=3
dat$threealts=0
dat$chset1=0
dat$chset2=0
dat$chset3=0
dat$chset4=1
dat$chset5=0
dat$chset6=0
dat$chset7=0
dat$chset8=0
dat$reject <- ifelse(dat$fourthBadNo==1,1,0) %>% replace_na(0)
dat$best <- ifelse(dat$fourthSetChoiceIndicator==2,1,0)
dat$noPolicy=dat$fourthNoPolicy   # problem???
dat$t_ch= dat$t_ch4
dat$c_ch= dat$c_ch4
dat$cert=dat$cert4
#dat$fstch=fstch4
#dat$sloch=sloch4 
dat$cases  = dat$cases_c
dat$deaths = dat$deaths_c


file9 <- dat %>% 
  dplyr::filter(!is.na(fourthSetChoiceIndicator)) %>% 
  dplyr::select(CHOALT, INDIV, OTHERATTRIB, DESIGN_n)





################ D/N  write fifth choice, first alternative
dat$choice=(dat$newcase-1)*8+5	
dat$alt=2
dat$threealts=0
dat$chset1=0
dat$chset2=0
dat$chset3=0
dat$chset4=0
dat$chset5=1
dat$chset6=0
dat$chset7=0
dat$chset8=0
dat$reject <- ifelse(dat$fifthBadNo==1,1,0) %>% replace_na(0)
dat$best <- ifelse(dat$fifthSetChoiceIndicator==1,1,0)
dat$noPolicy=dat$fifthNoPolicy   # problem???
dat$ t_ch= dat$t_ch5
dat$ c_ch= dat$c_ch5
dat$cert=dat$cert5
#dat$fstch=fstch5
#dat$sloch=sloch5 
dat$cases  = dat$cases_c
dat$deaths = dat$deaths_c

file10 <- dat %>% 
  dplyr::filter(!is.na(fifthSetChoiceIndicator)) %>% 
  dplyr::select(CHOALT, INDIV, OTHERATTRIB, DESIGN_d)



################ D/N  write fifth choice, second alternative
dat$alt=3
dat$threealts=0
dat$chset1=0
dat$chset2=0
dat$chset3=0
dat$chset4=0
dat$chset5=1
dat$chset6=0
dat$chset7=0
dat$chset8=0
dat$reject <- ifelse(dat$fifthBadNo==1,1,0) %>% replace_na(0)
dat$best <- ifelse(dat$fifthSetChoiceIndicator==2,1,0)
dat$nopolicy=dat$fifthNoPolicy   # problem???
dat$t_ch= dat$t_ch5
dat$c_ch= dat$c_ch5
dat$cert=dat$cert5
#dat$fstch=fstch5
#dat$sloch=sloch5 
dat$cases  = dat$cases_c
dat$deaths = dat$deaths_c


file11 <- dat %>% 
  dplyr::filter(!is.na(fifthSetChoiceIndicator)) %>% 
  dplyr::select(CHOALT, INDIV, OTHERATTRIB, DESIGN_n)






################ E/F/N  write sixth choice, first alternative
dat$choice=(dat$newcase-1)*8+6	
dat$alt=1
dat$threealts=1
dat$chset1=0
dat$chset2=0
dat$chset3=0
dat$chset4=0
dat$chset5=0
dat$chset6=1
dat$chset7=0
dat$chset8=0
dat$reject <- ifelse(dat$sixthBadNo==1,1,0) %>% replace_na(0)
dat$best <- ifelse(dat$sixthSetChoiceIndicator==1,1,0)
dat$noPolicy=dat$sixthNoPolicy
dat$t_ch= dat$t_ch6
dat$c_ch= dat$c_ch6
dat$cert=dat$cert6
#dat$fstch=fstch6
#dat$sloch=sloch6 
dat$cases  = dat$cases_e
dat$deaths = dat$deaths_e

file12 <- dat %>% 
  dplyr::filter(!is.na(sixthSetChoiceIndicator)) %>% 
  dplyr::select(CHOALT, INDIV, OTHERATTRIB, DESIGN_e)


################ E/F/N  write sixth choice, second alternative
dat$alt=2
dat$threealts=1
dat$chset1=0
dat$chset2=0
dat$chset3=0
dat$chset4=0
dat$chset5=0
dat$chset6=1
dat$chset7=0
dat$chset8=0
dat$reject <- ifelse(dat$sixthBadNo==1,1,0) %>% replace_na(0)
dat$best <- ifelse(dat$sixthSetChoiceIndicator==2,1,0)
dat$noPolicy=dat$sixthNoPolicy
dat$ t_ch= dat$t_ch6
dat$ c_ch= dat$c_ch6
dat$cert=dat$cert6
#dat$fstch=fstch6
#dat$sloch=sloch6 
dat$cases  = dat$cases_e
dat$deaths = dat$deaths_e

file13 <- dat %>% 
  dplyr::filter(!is.na(sixthSetChoiceIndicator)) %>% 
  dplyr::select(CHOALT, INDIV, OTHERATTRIB, DESIGN_f)



################ E/F/N  write sixth choice, third alternative
dat$alt=3
dat$threealts=1
dat$chset1=0
dat$chset2=0
dat$chset3=0
dat$chset4=0
dat$chset5=0
dat$chset6=1
dat$chset7=0
dat$chset8=0
dat$reject <- ifelse(dat$sixthBadNo==1,1,0) %>% replace_na(0)
dat$best <- ifelse(dat$sixthSetChoiceIndicator==3,1,0)
dat$noPolicy=dat$sixthNoPolicy
dat$t_ch= dat$t_ch6
dat$c_ch= dat$c_ch6
dat$cert=dat$cert6
#dat$fstch=fstch6
#dat$sloch=sloch6 
dat$cases  = dat$cases_e
dat$deaths = dat$deaths_e

file14 <- dat %>% 
  dplyr::filter(!is.na(sixthSetChoiceIndicator)) %>% 
  dplyr::select(CHOALT, INDIV, OTHERATTRIB, DESIGN_n)





################ E/N  write seventh choice, first alternative
dat$choice=(dat$newcase-1)*8+7	
dat$alt=1
dat$threealts=0
dat$chset1=0
dat$chset2=0
dat$chset3=0
dat$chset4=0
dat$chset5=0
dat$chset6=0
dat$chset7=1
dat$chset8=0
dat$reject <- ifelse(dat$seventhBadNo==1,1,0) %>% replace_na(0)
dat$best <- ifelse(dat$seventhSetChoiceIndicator==1,1,0)
dat$noPolicy=dat$seventhNoPolicy   # problem???
dat$t_ch= dat$t_ch7
dat$c_ch= dat$c_ch7
dat$cert=dat$cert7
#dat$fstch=fstch7
#dat$sloch=sloch7 
dat$cases  = dat$cases_e
dat$deaths = dat$deaths_e


file15 <- dat %>% 
  dplyr::filter(!is.na(seventhSetChoiceIndicator)) %>% 
  dplyr::select(CHOALT, INDIV, OTHERATTRIB, DESIGN_e)



################ E/N  write seventh choice, second alternative
dat$alt=3
dat$threealts=0
dat$chset1=0
dat$chset2=0
dat$chset3=0
dat$chset4=0
dat$chset5=0
dat$chset6=0
dat$chset7=1
dat$chset8=0
dat$reject <- ifelse(dat$seventhBadNo==1,1,0) %>% replace_na(0)
dat$best <- ifelse(dat$seventhSetChoiceIndicator==2,1,0)
dat$noPolicy=dat$seventhNoPolicy   # problem???
dat$t_ch= dat$t_ch7
dat$c_ch= dat$c_ch7
dat$cert=dat$cert7
#dat$fstch=fstch7
#dat$sloch=sloch7 
dat$cases  = dat$cases_e
dat$deaths = dat$deaths_e

file16 <- dat %>% 
  dplyr::filter(!is.na(seventhSetChoiceIndicator)) %>% 
  dplyr::select(CHOALT, INDIV, OTHERATTRIB, DESIGN_n)





################ F/N  write eighth choice, first alternative
dat$choice=(dat$newcase-1)*8+8	
dat$alt=2
dat$threealts=0
dat$chset1=0
dat$chset2=0
dat$chset3=0
dat$chset4=0
dat$chset5=0
dat$chset6=0
dat$chset7=0
dat$chset8=1
dat$reject <- ifelse(dat$eighthBadNo==1,1,0) %>% replace_na(0)
dat$best <- ifelse(dat$eighthSetChoiceIndicator==1,1,0)
dat$noPolicy=dat$eighthNoPolicy   # problem???
dat$t_ch= dat$t_ch8
dat$c_ch= dat$c_ch8
dat$cert=dat$cert8
#dat$fstch=fstch8
#dat$sloch=sloch8 
dat$cases  = dat$cases_e
dat$deaths = dat$deaths_e

file17 <- dat %>% 
  dplyr::filter(!is.na(eighthSetChoiceIndicator)) %>% 
  dplyr::select(CHOALT, INDIV, OTHERATTRIB, DESIGN_f)



################ F/N  write eighth choice, second alternative
dat$alt=3
dat$threealts=0
dat$chset1=0
dat$chset2=0
dat$chset3=0
dat$chset4=0
dat$chset5=0
dat$chset6=0
dat$chset7=0
dat$chset8=1
dat$reject <- ifelse(dat$eighthBadNo==1,1,0) %>% replace_na(0)
dat$best <- ifelse(dat$eighthSetChoiceIndicator==2,1,0)
dat$noPolicy=dat$eighthNoPolicy   # problem???
dat$t_ch= dat$t_ch8
dat$c_ch= dat$c_ch8
dat$cert=dat$cert8
#dat$fstch=fstch8
#dat$sloch=sloch8 
dat$cases  = dat$cases_e
dat$deaths = dat$deaths_e


file18 <- dat %>% 
  dplyr::filter(!is.na(eighthSetChoiceIndicator)) %>% 
  dplyr::select(CHOALT, INDIV, OTHERATTRIB, DESIGN_n)





############################################
# Now stick these all back together in idcase idalt format for clogit  Same variables, but generic $DESIGN now

dat2 <- NULL

for (i in 1:18) {
  temp <- get(paste0("file",i))
  names(temp) <- names(temp) %>% str_remove_all("_[:lower:]$")
  
  dat2 <- rbind(dat2,temp)
}

dat <- dat2[order(dat2$choice,dat2$alt),]

#===========================================================;
# can now clean up intermediate temporary files;
#===========================================================;

for (i in 1:18) {
  temp <- paste0("file",i)
  remove(list=as.character(temp))
}

rm(dat2)

#==========================================================;



#==============================================================================
# additional choice-set-level attributes, calculated after stacking of the data
#============================================================================== 

# is there a problem here? ask Trudy
dat$choiceofperson <- 0
for (i in 1:8) {
  dat$choiceofperson[which(dat[[paste0("chset",i)]]==1)] <- i
}



# verify these...
label(dat$chset1) <- "Prog A vs No Prog (omit)"
label(dat$chset2) <- "Prog B vs No Prog"
label(dat$chset3) <- "Prog C, D, No Prog"
label(dat$chset4) <- "Prog C v No Prog"
label(dat$chset5) <- "Prog D v No Prog"
label(dat$chset6) <- "Prog E, F, No Prog"
label(dat$chset7) <- "Prog E v No Prog"
label(dat$chset8) <- "Prog F v No Prog"

#----------------------------------------------

dat <- dat[order(dat$caseid,dat$choiceofperson,dat$alt),]


dat <- dat %>% group_by(caseid) %>% mutate(choiceorder = data.table::frank(choiceofperson,ties.method = "dense"))

tabulate(dat$choiceorder)
dat$choiceorder2=dat$choiceorder^2   # allow for quadratic form in choiceorder 

#----------------------------------------------
# check with Trudy about task variable
# dat <- dat[order(dat$caseid,dat$choice,dat$alt),]
# 
# dat$task=0
# 
# dat$task=1 in 1
# 
# forvalues i=2/`=_N' {
#   if caseid[`i']~=caseid[`i'-1] {
# 	dat$task=1 in `i'
# }
# else {
#   if choice[`i']~=choice[`i'-1] {
# 		dat$task=task[`i'-1]+ 1 in `i'
# 		}
# 		else {
# 		dat$task=task[`i'-1] in `i'
# 		}
# 		# end if choice number is different (or same)
# 	}
# 	#end if caseid is different (or same)
# 	
# }
# #end forvalues
# 
# 	
# list caseid choice alt task
# 
# dat$task1=(task==1)
# dat$task2=(task==2)
# dat$task3=(task==3)
# dat$task4=(task==4)
# dat$task5=(task==5)
# dat$task6=(task==6)
# 
# summ task1-task6  
# 
# label(dat$task1) <- "1st choice task (omit)"
# label(dat$task2) <- "2nd choice task"	
# label(dat$task3) <- "3rd choice task"	
# label(dat$task4) <- "4th choice task"	
# label(dat$task5) <- "5th choice task"	
# label(dat$task6) <- "6th choice task"	
#----------------------------------------------



# take a look at all constructed variables


label(dat$fedui  ) <- "Federal UI"             
label(dat$months ) <- "Duration"               
label(dat$delcases) <- "Cases prevented"                
label(dat$deldeaths) <- "Deaths avoided"                 
label(dat$rule1  ) <- "Grocery"             
label(dat$rule2  ) <- "Nonessential"             
label(dat$rule3  ) <- "Schools"             
label(dat$rule4  ) <- "Colleges"             
label(dat$rule5  ) <- "Parks"             
label(dat$rule6  ) <- "Gyms"             
label(dat$rule7  ) <- "Theaters"             
label(dat$rule8  ) <- "Restaurants"             
label(dat$rule9  ) <- "Religion"             
label(dat$rule10 ) <- "Institutions"              
label(dat$unempl ) <- "Unemployment"              
label(dat$avcost ) <- "Avg. hhld cost"   

BASEVARS <- c("fedui", "months", "delcases", "deldeaths", "rule1", "rule2", "rule3", "rule4", "rule5", 
              "rule6", "rule7", "rule8", "rule9", "rule10", "unempl", "avcost", "noPolicy")

BASENONRULES <- c("fedui months", "delcases", "deldeaths", "unempl", "avcost", "noPolicy")
BASERULES <- c("rule1", "rule2", "rule3", "rule4", "rule5", "rule6", "rule7", "rule8", "rule9", "rule10")

label(dat$threealts) <- "1=Three-alt choice"                         
label(dat$noPolicy) <- "1=Status quo"                     
names(dat)[which(names(dat)=="t_ch")] <- "tch"
names(dat)[which(names(dat)=="c_ch")] <- "cch"
label(dat$tch) <- "Time on choice"                  
label(dat$cch) <- "Clicks on choice"                  
label(dat$cert) <- "Certainty"                  
label(dat$cases) <- "Cases w/o policy"                   
label(dat$deaths) <- "Deaths w/o policy"                    
label(dat$reject) <- "1=Scenario reject"

SETVARS <- c("threealts", "chset1", "chset2", "chset3", "chset4", "chset5", "chset6", 
             "chset7", "chset8", "noPolicy", "tch", "cch", "cert", "cases", "deaths", "reject")
SETVARSL <- c("threealts", "tch", "cch", "cert", "reject")

label(dat$caseid) <- ""   
label(dat$newcase) <- "" 

label(dat$lirule1) <- "1=Grocery income"             
label(dat$lirule2) <- "1=Nonessential income"             
label(dat$lirule3) <- "1=Schools income"             
label(dat$lirule4) <- "1=Colleges income"             
label(dat$lirule5) <- "1=Parks income"             
label(dat$lirule6) <- "1=Gyms income"             
label(dat$lirule7) <- "1=Theaters income"             
label(dat$lirule8) <- "1=Restaurants income"             
label(dat$lirule9) <- "1=Religion income"             
label(dat$lirule10) <- "1=Institutions income"
INCHARD <- c("lirule1", "lirule2", "lirule3", "lirule4", "lirule5", "lirule6", "lirule7", "lirule8", "lirule9", "lirule10")              

label(dat$larule1) <- "1=Grocery access"             
label(dat$larule2) <- "1=Nonessential access"             
label(dat$larule3) <- "1=Schools access"             
label(dat$larule4) <- "1=Colleges access"             
label(dat$larule5) <- "1=Parks access"             
label(dat$larule6) <- "1=Gyms access"             
label(dat$larule7) <- "1=Theaters access"             
label(dat$larule8) <- "1=Restaurants access"             
label(dat$larule9) <- "1=Religion access"             
label(dat$larule10) <- "1=Institutions access" 
ACCHARD <- c("larule1", "larule2", "larule3", "larule4", "larule5", "larule6", "larule7", "larule8", "larule9", "larule10")             

label(dat$hhld0to1) <- "1=Hhld members 0-1 yrs"              
label(dat$hhld2to5) <- "1=Hhld members 2-5 yrs"               
label(dat$hhld6to12) <- "1=Hhld members 6-12 yrs"                
label(dat$hhld13to17) <- "1=Hhld members 13-17 yrs"                 
label(dat$hhld18to64) <- "1=Hhld members 18-64 yrs"                 
label(dat$hhld65up) <- "1=Hhld members 65+ yrs"
HHLDAGES <- c("hhld0to1", "hhld2to5", "hhld6to12", "hhld13to17", "hhld18to64", "hhld65up")               

label(dat$ffltc) <- "1=Family in institution"           

label(dat$transityes) <- "1=Used transit for work"                 
label(dat$transitsome) <- "1=Sometimes used transit"                  

label(dat$pastcounty0to2 ) <- "1=Resident 0-2 years"               
label(dat$pastcounty3to5 ) <- "1=Resident 3-5 years"               
label(dat$pastcounty6to10) <- "1=Resident 6-10 years"                
label(dat$pastcounty10up ) <- "1=Resident 10+ years"               
label(dat$futrcounty0to2 ) <- "1=Will stay 0-2 years"               
label(dat$futrcounty3to5 ) <- "1=Will stay 3-5 years"               
label(dat$futrcounty6to10) <- "1=Will stay 6-10 years"                
label(dat$futrcounty10up ) <- "1=Will stay 10+ years"               
label(dat$futrcountyuncer) <- "1=Will stay uncertain" 
ATTACH <- c("pastcounty0to2", "pastcounty3to5", "pastcounty6to10", "pastcounty10up", "futrcounty0to2", 
            "futrcounty3to5", "futrcounty6to10", "futrcounty10up", "futrcountyuncer")                  

label(dat$comheart) <- "1=Own heart group"              
label(dat$comdiab) <- "1=Own diabetes group"             
label(dat$comresp) <- "1=Own respir. group"             
label(dat$comcanc) <- "1=Own cancer group"             
label(dat$comage) <- "1=Own aged group"            
label(dat$compreg) <- "1=Own pregnancy"             
label(dat$comcovid) <- "1=Own COVID"              
label(dat$comother) <- "1=Own other illness"               
OWNCOMORBID <- c("comheart", "comdiab", "comresp", "comcanc", "comage", "compreg", "comcovid", "comother")

label(dat$ffcomheart  ) <- "1=Family heart group"              
label(dat$ffcomdiab   ) <- "1=Family diabetes group"             
label(dat$ffcomresp   ) <- "1=Family respir. group"             
label(dat$ffcomcanc   ) <- "1=Family cancer group"             
label(dat$ffcomage    ) <- "1=Family aged group"            
label(dat$ffcompreg   ) <- "1=Family pregnancy"             
label(dat$ffcomcovid  ) <- "1=Family COVID"              
label(dat$ffcomother  ) <- "1=Family other illness" 
FFCOMORBID <- c("ffcomheart", "ffcomdiab", "ffcomresp", "ffcomcanc", "ffcomage", "ffcompreg", "ffcomcovid", "ffcomother")            

label(dat$rnought0to1) <- "1=R0 is 0 to 1"                
label(dat$rnought1to2) <- "1=R0 is 1 to 2"                
label(dat$rnought2to3) <- "1=R0 is 2 to 3"                
label(dat$rnought3to5) <- "1=R0 is 3 to 5"                
label(dat$rnought5to7) <- "1=R0 is 5 to 7"                
label(dat$rnought7to10) <- "1=R0 is 7 to 10"                 
label(dat$rnought010up) <- "1=R0 is 10 or up"
RNOUGHT <- c("rnought0to1", "rnought1to2", "rnought2to3", "rnought3to5", "rnought5to7", "rnought7to10", "rnought010up")                   

label(dat$vulnchild  ) <- "1=Children vulnerable"              
label(dat$vulnteens  ) <- "1=Teens vulnerable"              
label(dat$vulnseniors) <- "1=Seniors vulnerable"                
label(dat$vulnwomen  ) <- "1=Women vulnerable"              
label(dat$vulnmen    ) <- "1=Men vulnerable"            
label(dat$vulnnonwhite) <- "1=Nonwhites vulnerable"                 
label(dat$vulnnoneng ) <- "1=Non-Engl. vulnerable"               
label(dat$vulnlowinc ) <- "1=Low-income vulnerable"               
label(dat$vulnrural  ) <- "1=Rural vulnerable"              
label(dat$vulnnosci  ) <- "1=No science vulnerable"              
label(dat$vulnessent ) <- "1=Essentials vulnerable"                              
VULN <- c("vulnchild", "vulnteens", "vulnseniors", "vulnwomen", "vulnmen", "vulnnonwhite", 
          "vulnnoneng", "vulnlowinc", "vulnrural", "vulnnosci", "vulnessent")

# ideal policy rule levels
label(dat$idrule1  ) <- "Grocery ideal rule"             
label(dat$idrule2  ) <- "Nonessential ideal rule"             
label(dat$idrule3  ) <- "Schools ideal rule"             
label(dat$idrule4  ) <- "Colleges ideal rule"             
label(dat$idrule5  ) <- "Parks ideal rule"             
label(dat$idrule6  ) <- "Gyms ideal rule"             
label(dat$idrule7  ) <- "Theaters ideal rule"             
label(dat$idrule8  ) <- "Restaurants ideal rule"             
label(dat$idrule9  ) <- "Religion ideal rule"             
label(dat$idrule10 ) <- "Institutions ideal rule"
IDEALRULE <- c("idrule1", "idrule2", "idrule3", "idrule4", "idrule5", "idrule6", "idrule7", "idrule8", "idrule9", "idrule10")              

label(dat$stayhome) <- "1=Policy D is lockdown"             

label(dat$hhldinc) <- "Med hhld inc in 10K"             
label(dat$owninc) <- "Own hhld inc in 10K"           
label(dat$pctincloss ) <- "Percent income loss" 
INCVARS <-c("hhldinc", "owninc", "pctincloss")             


label(dat$best) <- "1=Preferred policy"
label(dat$choice) <- "Choice number"  # choices numbered consecutively 
label(dat$alt) <- "Alternative number"

dat$statusquo=(dat$alt==3)
dat$any = 1-dat$statusquo
label(dat$any) <- "any policy w/ any attrib."
label(dat$statusquo) <- "Status quo, no policy"


label(dat$prepover ) <- "1=Oregon over-prepared"              
label(dat$prepreas ) <- "1=Oregon reasonably prepared"            
label(dat$preppoor ) <- "1=Oregon poorly prepared"            

label(dat$rulesbad ) <- "1=Oregon rules burdensome"            
label(dat$rulesgood) <- "1=Oregon rules appropriate"             
label(dat$rulesinsuff) <- "1=Oregon rules insufficient"                

label(dat$w_exactnum) <- "1=Wrong on exact counts"              
label(dat$w_petstores) <- "1=Wrong on pet stores"               
label(dat$w_daycare) <- "1=Wrong on daycare"             
label(dat$w_nopopup) <- "1=Cannot use pop-ups"             
label(dat$w_uneven ) <- "1=Wrong on uneven rules"            
label(dat$w_samecost) <- "1=Wrong on all same cost"              

# build some key interactions between the rule counts

RULE_LI = "" 
RULE_LA = ""
RULE_ID = ""

foreach num of numlist 1/10 {
  dat$rule`num'_lirule`num' = rule`num' # lirule`num'	
  local templabel : variable label lirule`num'
		  display "`templabel'"
		  label(dat$rule`num'_lirule`num') <- "\cmmnt{rule`num'} \quad  ${INTERACT} `templabel'"
		  RULE_LI = "$RULE_LI " + "rule`num'_lirule`num' "
		  
		  dat$rule`num'_larule`num' = rule`num' # larule`num'	
		    local templabel : variable label larule`num'
		  display "`templabel'"
		  label(dat$rule`num'_larule`num') <- "\cmmnt{rule`num'} \quad  ${INTERACT} `templabel'"
		  RULE_LA = "$RULE_LA " + "rule`num'_larule`num' "

		  dat$rule`num'_idrule`num' = rule`num' # idrule`num'	
		    local templabel : variable label idrule`num'
		  display "`templabel'"
		  label(dat$rule`num'_idrule`num') <- "\cmmnt{rule`num'} \quad  ${INTERACT} `templabel'"
		  RULE_ID = "$RULE_ID " + "rule`num'_idrule`num' "
}	
# end foreach num

foreach var of varlist $BASEVARS {
	foreach intx of varlist   $HHLDAGES   ffltc transityes transitsome $ATTACH $OWNCOMORBID $FFCOMORBID $RNOUGHT $VULN $INCVARS $PREP $RULES $WRONGS {
		dat$`var'_`intx' = `var' # `intx'
		    local templabel : variable label `intx'
		label(dat$`var'_`intx') <- "\cmmnt{`var'} \quad  ${INTERACT} `templabel'"
	}
	# end foreach intx
}
# end foreach var	


#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# make some globals to hold subsets of interaction terms

INTX_HHLDAGES = ""
foreach var of varlist $BASEVARS {
	foreach intx of varlist $HHLDAGES {
		INTX_HHLDAGES = "$INTX_HHLDAGES " + "`var'_`intx' "
	}
}

INTX_ATTACH = ""
foreach var of varlist $BASEVARS {
	foreach intx of varlist $ATTACH {
		INTX_ATTACH = "$INTX_ATTACH " + "`var'_`intx' "
	}
}

INTX_OWNCOMORBID = ""
foreach var of varlist $BASEVARS {
	foreach intx of varlist $OWNCOMORBID {
		INTX_OWNCOMORBID = "$INTX_OWNCOMORBID " + "`var'_`intx' "
	}
}

INTX_FFCOMORBID = ""
foreach var of varlist $BASEVARS {
	foreach intx of varlist $FFCOMORBID {
		INTX_FFCOMORBID = "$INTX_FFCOMORBID " + "`var'_`intx' "
	}
}

INTX_RNOUGHT = ""
foreach var of varlist $BASEVARS {
	foreach intx of varlist $RNOUGHT {
		INTX_RNOUGHT = "$INTX_RNOUGHT " + "`var'_`intx' "
	}
}

INTX_VULN = ""
foreach var of varlist $BASEVARS {
	foreach intx of varlist $VULN {
		INTX_VULN = "$INTX_VULN " + "`var'_`intx' "
	}
}

INTX_INCVARS = ""
foreach var of varlist $BASEVARS {
	foreach intx of varlist $INCVARS {
		INTX_INCVARS = "$INTX_INCVARS " + "`var'_`intx' "
	}
}

INTX_INCVARS = ""
foreach var of varlist $BASEVARS {
	foreach intx of varlist $PREP $RULES $WRONGS {
		INTX_INCVARS = "$INTX_INCVARS " + "`var'_`intx' "
	}
}

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
	

save "${INT}\main_vars_intx.dta", replace
