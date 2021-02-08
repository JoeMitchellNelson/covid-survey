########################################################
####### this code reproduces the first part of Trudy's getdata_qualtrics_responses.do
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

#note that back-slashes in Stata need to be forward-slashses in R
DROPBOX <-  "C:/Users/Joe/Dropbox" # change this to your dropbox folder location

DIR <- paste0(DROPBOX,"/VSL-COVID/data_from_obsolete_version")

INT <- paste0(DROPBOX,"/VSL-COVID/intermediate-files")

INTERACT <- "$\times$"

# dat is the generic name I chose for the data table. data tables do not require names in Stata.
#dat <- read.csv(paste0(DIR,"/VSL-COVID-php-obsolete_January 6, 2021_06.00_numeric-cleaned.csv"))

dat <- read.csv("~/covid-survey/data/VSL-COVID-php-obsolete_January 6, 2021_06.00_numeric-cleaned.csv")
dat <- dat[3:nrow(dat),] # drop first 2 observations

# spaces in var names are converted to . in R and completely removed in Stata
# this command removes the .s in varnames
names(dat) <- str_replace_all(names(dat),"\\.","")

# All of these variable labels are temporary, to facilitate proof-reading of variable identities (replaced in stacked file, below)
label(dat$StartDate) <- ""
label(dat$EndDate) <- ""
label(dat$Status) <- ""
label(dat$IPAddress) <- ""
label(dat$Durationinseconds) <- ""
label(dat$Finished) <- ""
label(dat$RecordedDate) <- ""
label(dat$ResponseId) <- ""
label(dat$RecipientLastName) <- ""
label(dat$RecipientFirstName) <- ""
label(dat$RecipientEmail) <- ""
label(dat$ExternalReference) <- ""
label(dat$LocationLatitude) <- ""
label(dat$LocationLongitude) <- ""
label(dat$DistributionChannel) <- ""
label(dat$UserLanguage) <- ""


# CONSENT AND COUNTY IDENTIFICATION---------------------------------------------

label(dat$Q2) <- "Eligible to participate"

label(dat$Q3_Browser) <- "Browser"
label(dat$Q3_Version) <- ""
label(dat$Q3_OperatingSystem) <- "add underscore at end"
label(dat$Q3_Resolution) <-  ""

label(dat$Q4) <-	"Provide thoughtful answers"

label(dat$Q5)	<- "Did you mean that"

label(dat$Q7) <-	"Oregon county"

label(dat$Q8) <-	"Oregon count1 correct?"

label(dat$Q9) <-	"Oregon corrected county"


# TUTORIAL for CHOICE EXPERIMENTS-----------------------------------------------

label(dat$Q14) <-	"Oregon pandemic preparedness"
dat$prepover <- as.numeric(dat$Q14==9)
dat$prepreas <- as.numeric(dat$Q14==10) 
dat$preppoor <- as.numeric(dat$Q14==11)
PREP <- c("prepover","prepreas","preppoor")

dat$t_top300 <- dat$Q15_PageSubmit - dat$Q13_PageSubmit
dat$c_top300 <- dat$Q15_ClickCount

label(dat$Q16) <-	"Opinion about restrictions to date"
dat$rulesbad    = as.numeric(dat$Q16==4)
dat$rulesgood   = as.numeric(dat$Q16==6) 
dat$rulesinsuff = as.numeric(dat$Q16==5)
RULES <- c("rulesbad", "rulesgood", "rulesinsuff")

dat$t_burden <- dat$Q17_PageSubmit - dat$Q15_PageSubmit
dat$c_burden <- dat$Q17_ClickCount

dat$w_exactnum = as.numeric(dat$Q18 != 12)  # wrong on: we do not know the exact numbers

label(dat$Q25) <-	"Pet stores in grocery?"

dat$w_petstores = as.numeric(dat$Q25!=1)  # wrong on: Yes, pet stores are in Grocery, essential

# for loop to convert Q30-Q39 to string/character data type
for (num in 30:39) {
  dat[,which(names(dat) == paste0("Q",num))] <- as.character(dat[,which(names(dat) == paste0("Q",num))])
  print(paste0("Q",num, " is ", class(dat[,which(names(dat) == paste0("Q",num))])))
}

#str_detect returns T/F so the !=0 is redundant (but doesn't interfere either, since FALSE==0 and TRUE==1 return TRUE in R)
# note that this code preserves non-responses as NA rather than converting to FALSE, as the Stata code does
label(dat$Q30) <-	"Impact of Grocery restrictions"
dat$lirule1 = (str_detect(dat$Q30,"17"))
dat$larule1 = (str_detect(dat$Q30,"18"))

label(dat$Q31) <-	"Impact of Nonessential restrictions"
dat$lirule2 = (str_detect(dat$Q31,"17"))
dat$larule2 = (str_detect(dat$Q31,"18"))

label(dat$Q32) <-	"Impact of Schools restrictions"
dat$lirule3 = (str_detect(dat$Q32,"17"))
dat$larule3 = (str_detect(dat$Q32,"18"))

label(dat$Q33) <-	"Impact of Colleges restrictions"
dat$lirule4 = (str_detect(dat$Q33,"17"))
dat$larule4 = (str_detect(dat$Q33,"18"))

label(dat$Q34) <-	"Impact of Parks restrictions"
dat$lirule5 = (str_detect(dat$Q34,"17"))
dat$larule5 = (str_detect(dat$Q34,"18"))

label(dat$Q35) <-	"Impact of Gyms restrictions"
dat$lirule6 = (str_detect(dat$Q35,"17"))
dat$larule6 = (str_detect(dat$Q35,"18"))

label(dat$Q36) <-	"Impact of Theaters restrictions"
dat$lirule7 = (str_detect(dat$Q36,"17"))
dat$larule7 = (str_detect(dat$Q36,"18"))

label(dat$Q37) <-	"Impact of Restaurants restrictions"
dat$lirule8 = (str_detect(dat$Q37,"17"))
dat$larule8 = (str_detect(dat$Q37,"18"))

label(dat$Q38) <-	"Impact of Religious restrictions"
dat$lirule9 = (str_detect(dat$Q38,"17"))
dat$larule9 = (str_detect(dat$Q38,"18"))

label(dat$Q39) <-	"Impact of Assisted restrictions"
dat$lirule10 = (str_detect(dat$Q39,"17"))
dat$larule10 = (str_detect(dat$Q39,"18"))


label(dat$Q43) <-	"Do essential workers get daycare?"

dat$w_daycare = as.numeric(dat$Q43!=4)  # wrong on these kids still go to daycare

dat$w_nopopup = as.numeric(dat$Q43==6)  # can't get popups to work in browser

label(dat$Q48) <- "Match Oregon's Reopening Plan?"

dat$w_uneven = as.numeric(dat$Q48!=12)  # wrong on uneven rules

label(dat$Q55) <-	"Does every household pay same?"

dat$w_samecost = as.numeric(dat$Q55!=5)  # wrong on all households not paying the same cost

# This set of controls should be all zero; mistakes show up as 1s. Include as interaction terms in estimating specification
WRONGS <- c("w_exactnum", "w_petstores", "w_daycare",  "w_nopopup", "w_uneven",  "w_samecost")



# CHOICE EXPERIMENTS------------------------------------------------------------
dat <- rename(dat,firstSetChoiceIndicator="Q68")  # 1=A, 2=N
label(dat$firstSetChoiceIndicator)  <-	"Policy A"

dat <- rename(dat,cert1= "Q69")
label(dat$cert1) <- "Certainty A"

dat <- rename(dat,firstNoPolicy = "Q71")
label(dat$firstNoPolicy) <- "Reasons no A"    # will this be a comma-separated string?

label(dat$Q71_9_TEXT) <-	"Reasons no A, verbatim"

dat$t_ch1 = dat$Q165_PageSubmit - dat$Q67_PageSubmit
dat$c_ch1 = dat$Q165_ClickCount

#----------
dat <- dat %>% rename(secondSetChoiceIndicator = "Q73") # 1=B, 2=N
label(dat$secondSetChoiceIndicator) <- "Policy B"

dat <- dat %>% rename(cert2 = "Q74")
label(dat$cert2) <-	"Certainty B"

dat <- dat %>% rename(secondNoPolicy = "Q76")
label(dat$secondNoPolicy) <-	"Reasons no B"

label(dat$Q76_9_TEXT) <-	"Reasons no B, verbatim"

dat$t_ch2 = dat$Q75_PageSubmit - dat$Q72_PageSubmit
dat$c_ch2 = dat$Q75_ClickCount

#----------
dat <- dat %>% rename(thirdSetChoiceIndicator = "Q78") # 1 = C, 2 = D, 3 = N
label(dat$thirdSetChoiceIndicator) <-  	"Policies C or D"

dat <- dat %>% rename(cert3 = "Q79")
label(dat$cert3) <-	"Certainty C or D"

dat <- dat %>% rename(thirdNoPolicy = "Q81")
label(dat$thirdNoPolicy) <-	"Reasons neither C nor D"

label(dat$Q81_9_TEXT) <-	"Reasons neither C nor D, verbatim"

dat$t_ch3 = dat$Q80_PageSubmit - dat$Q77_PageSubmit
dat$c_ch3 = dat$Q80_ClickCount

#----------
dat <- dat %>% rename(fourthSetChoiceIndicator = "Q88") # if D unavailable:  1 = C, 2 = N
label(dat$fourthSetChoiceIndicator) <- 	"Policy C"   # brief name is wrong in qualtrics

dat <- dat %>% rename(cert4 = "Q89")
label(dat$cert4) <-	"Certainty C"

dat <- dat %>% rename(fourthNoPolicy = "Q91")
label(dat$fourthNoPolicy) <-	"Reasons no C"

label(dat$Q91_9_TEXT) <-	"Reasons no C, verbatim"

dat$t_ch4 = dat$Q90_PageSubmit - dat$Q87_PageSubmit
dat$c_ch4 = dat$Q90_ClickCount

#----------
dat <- dat %>% rename(fifthSetChoiceIndicator = "Q83") #  if C unavailable:  1 = D, 2 = N
label(dat$fifthSetChoiceIndicator) <- 	"Policy D"

dat <- dat %>% rename(cert5 = "Q84")
label(dat$cert5) <-	"Certainty D"

dat <- dat %>% rename(fifthNoPolicy = "Q86")
label(dat$fifthNoPolicy) <-	"Reasons no D"

label(dat$Q86_9_TEXT) <-	"Reasons no D, verbatim"

dat$t_ch5 = dat$Q85_PageSubmit - dat$Q82_PageSubmit
dat$c_ch5 = dat$Q85_ClickCount

#----------
dat <- dat %>% rename(sixthSetChoiceIndicator = "Q93")  # 1 = E, 2 = F, 3 = N
label(dat$sixthSetChoiceIndicator) <-  	"Policies E or F"

dat <- dat %>% rename(cert6 = "Q94")
label(dat$cert6) <-	"Certainty E or F"

dat <- dat %>% rename(sixthNoPolicy = "Q96")
label(dat$sixthNoPolicy) <-	"Reasons no E or F"

label(dat$Q96_9_TEXT) <-	"Reasons no E or F, verbatim"

dat$t_ch6 = dat$Q95_PageSubmit - dat$Q92_PageSubmit
dat$c_ch6 = dat$Q95_ClickCount

#----------
dat <- dat %>% rename(seventhSetChoiceIndicator = "Q103")  # if F unavailable:  1 = E, 2 = N
label(dat$seventhSetChoiceIndicator) <-  	"Policy E"

dat <- dat %>% rename(cert7 = "Q104")
label(dat$cert7) <-	"Certainty E"

dat <- dat %>% rename(seventhNoPolicy = "Q106")
label(dat$seventhNoPolicy) <-	"Reasons no E"

label(dat$Q106_9_TEXT) <-	"Reasons no E, verbatim"

dat$t_ch7 = dat$Q105_PageSubmit - dat$Q102_PageSubmit
dat$c_ch7 = dat$Q105_ClickCount

#----------
dat <- dat %>% rename(eighthSetChoiceIndicator = "Q98") # if E unavailable:  1 = F, 2 = N
label(dat$eighthSetChoiceIndicator) <-  	"Policy F"

dat <- dat %>% rename(cert8 = "Q99")
label(dat$cert8) <-	"Certainty F"

dat <- dat %>% rename(eighthNoPolicy = "Q101")
label(dat$eighthNoPolicy) <-	"Reasons no F"

label(dat$Q101_9_TEXT) <-	"Reasons no F, verbatim"

dat$t_ch8 = dat$Q100_PageSubmit - dat$Q97_PageSubmit
dat$c_ch8 = dat$Q100_ClickCount

#--------------------------------------------------------------------------------

for (nope in ordinal(1:8)) {  # ordinal() converts 1 to "first", etc.
  dat[,which(names(dat)==paste0(nope,"NoPolicy"))] <- as.character(dat[,which(names(dat)==paste0(nope,"NoPolicy"))])
  dat$new_var <- ifelse(str_detect(dat[,which(names(dat)==paste0(nope,"NoPolicy"))],"3"),1,0) #  did not believe ... would actually prevent that many illnesses or deaths.
  names(dat)[ncol(dat)] <- paste0(nope,"BadNo")
}







# FOLLOW-UP OPEN-ENDED PREFERENCES AND SOCIODEMOGRAPHICS------------------------

## regex for auto-fixing rename commands
# trailing quotation mark
# (?<=rename Q.{1,3} .{1,80})(?=$) -> '"'
# leading quotation mark
#(?<=rename Q.{1,3}) (?=.{1,80}$) -> ' "'
# "rename " -> names(dat)[which(names(dat)=="
# '(?<=\)==\"Q.{1,3}) ' -> ")] <- 

dat <- dat %>% rename(idrule1="Q108")
label(dat$idrule1) <-	"ideal grocery rules"
names(dat)[which(names(dat)=="Q109")] <- "idrule2"
label(dat$idrule2) <-	""
names(dat)[which(names(dat)=="Q110")] <-  "idrule3"
label(dat$idrule3) <-	""
names(dat)[which(names(dat)=="Q111")] <-  "idrule4"
label(dat$idrule4) <-	""
names(dat)[which(names(dat)=="Q112")] <-  "idrule5"
label(dat$idrule5) <-	""
names(dat)[which(names(dat)=="Q113")] <-  "idrule6"
label(dat$idrule6) <-	""
names(dat)[which(names(dat)=="Q114")] <-  "idrule7"
label(dat$idrule7) <-	""
names(dat)[which(names(dat)=="Q115")] <-  "idrule8"
label(dat$idrule8) <-	""
names(dat)[which(names(dat)=="Q116")] <-  "idrule9"
label(dat$idrule9) <-	""
names(dat)[which(names(dat)=="Q117")] <-  "idrule10"
label(dat$idrule10) <-	"ideal institution rules"

for (num in 1:10) {
  temp <- dat[which(names(dat)==paste0("idrule",num))]
  
  temp[which(temp==1),] <- 0
  temp[which(temp==12),] <- 1
  temp[which(temp==13),] <- 2
  temp[which(temp==14),] <- 3
  
  dat[which(names(dat)==paste0("idrule",num))] <- temp
  
}	


label(dat$Q119) <- "Ideal cases bin = cred..."

label(dat$Q121) <- "Ideal deaths bin = dred..."

label(dat$Q169) <- "Ideal unemployment = ..."

label(dat$Q126) <- "Age brackets hhld: comma-sep list"

# check with Trudy on this section
dat$hhld0to1 = (str_detect(dat$Q126,"1")!=0)
dat$hhld2to5 = (str_detect(dat$Q126,"2")!=0)
dat$hhld6to12 = (str_detect(dat$Q126,"3")!=0)
dat$hhld13to17 = (str_detect(dat$Q126,"4")!=0)
dat$hhld18to64 = (str_detect(dat$Q126,"5")!=0)
dat$hhld65up = (str_detect(dat$Q126,"6")!=0)



label(dat$Q128) <- "Family/friend in LTC?"
dat$ffltc = (dat$Q128==1)

label(dat$Q129) <- "Hhld depend on transit?"
dat$transityes = (dat$Q129==1)
dat$transitsome = (dat$Q129==2)

label(dat$Q130) <- "Past time in county?"
dat$pastcounty0to2 = (dat$Q130==1)
dat$pastcounty3to5 = (dat$Q130==2)
dat$pastcounty6to10 = (dat$Q130==3)
dat$pastcounty10up = (dat$Q130==4)

label(dat$Q131) <- "Expected future time in county?"
dat$futrcounty0to2 = (dat$Q131==1)
dat$futrcounty3to5 = (dat$Q131==2)
dat$futrcounty6to10 = (dat$Q131==3)
dat$futrcounty10up = (dat$Q131==4)
dat$futrcountyuncer = (dat$Q131==5)


# COMORBIDITIES ----------------------------------------------------------------

label(dat$Q132) <- "List of self comorbidities"

dat$comheart = (str_extract(dat$Q132,"^.")=="8")   # the eight will be in the first position
dat$comdiab  = (str_detect(dat$Q132,"23")!=0)
dat$comresp  = (str_detect(dat$Q132,"24")!=0)
dat$comcanc  = (str_detect(dat$Q132,"25")!=0)
dat$comage   = (str_detect(dat$Q132,"26")!=0)
dat$compreg  = (str_detect(dat$Q132,"27")!=0)
dat$comcovid = (str_detect(dat$Q132,"28")!=0)
dat$comother = (str_detect(dat$Q132,"32")!=0)

label(dat$Q132_32_TEXT) <- "Other self comorbidities"

label(dat$Q134) <- "List of others' comorbidities"
dat$ffcomheart = (str_extract(dat$Q134,"^.")=="8")
dat$ffcomdiab  = (str_detect(dat$Q134,"23")!=0)
dat$ffcomresp  = (str_detect(dat$Q134,"24")!=0)
dat$ffcomcanc  = (str_detect(dat$Q134,"25")!=0)
dat$ffcomage   = (str_detect(dat$Q134,"26")!=0)
dat$ffcompreg  = (str_detect(dat$Q134,"27")!=0)
dat$ffcomcovid = (str_detect(dat$Q134,"28")!=0)
dat$ffcomother = (str_detect(dat$Q134,"29")!=0)

label(dat$Q134_29_TEXT) <- "Other others' comorbidities"

label(dat$Q138) <- "R0 rate for COVID?"
dat$rnought0to1 = (str_detect(dat$Q138,"1")!=0)
dat$rnought1to2 = (str_detect(dat$Q138,"2")!=0)
dat$rnought2to3 = (str_detect(dat$Q138,"3")!=0)
dat$rnought3to5 = (str_detect(dat$Q138,"4")!=0)
dat$rnought5to7 = (str_detect(dat$Q138,"5")!=0)
dat$rnought7to10 = (str_detect(dat$Q138,"6")!=0)
dat$rnought010up = (str_detect(dat$Q138,"7")!=0)

# check with Trudy on this one 
label(dat$Q140) <- "COVID-susceptible morbidities"
dat$vulnchild = (str_extract(dat$Q140,"^.")=="1" & (str_extract(dat$Q140,"^..")!="10") & (str_extract(dat$Q140,"^..")!="11"))   # first element is 1 but this isn't 10 or 11
dat$vulnteens = (str_detect(dat$Q140,"10")!=0)
dat$vulnseniors = (str_detect(dat$Q140,"2")!=0)
dat$vulnwomen = (str_detect(dat$Q140,"3")!=0)
dat$vulnmen = (str_detect(dat$Q140,"4")!=0)
dat$vulnnonwhite = (str_detect(dat$Q140,"5")!=0)
dat$vulnnoneng = (str_detect(dat$Q140,"6")!=0)
dat$vulnlowinc = (str_detect(dat$Q140,"7")!=0)
dat$vulnrural = (str_detect(dat$Q140,"8")!=0)
dat$vulnnosci = (str_detect(dat$Q140,"9")!=0)
dat$vulnessent = (str_detect(dat$Q140,"11")!=0)

label(dat$Q140_12_TEXT) <- "other COVID-susc. morbidities"

label(dat$Q142) <- "Healthcare"
dat$medexcellent = (dat$Q142==37)
dat$medgood      = (dat$Q142==38)       
dat$medaverage   = (dat$Q142==39)        
dat$medpoor      = (dat$Q142==40)      
dat$medterrible  = (dat$Q142==41)         
dat$meddkns      = (dat$Q142==42)        

label(dat$Q144) <- "Expected pandemic duration"
dat$overnow       = (dat$Q144==1)              
dat$over1mo       = (dat$Q144==2)          
dat$over2to6mo    = (dat$Q144==3)           
dat$over6to12mo   = (dat$Q144==4)          
dat$over12to24mo  = (dat$Q144==5)           
dat$over24up      = (dat$Q144==6)        



# INCOME AND COVID-19 INCOME LOSSES --------------------------------------------

names(dat)[which(names(dat)=="Q146")] <- "ownincbracket"
label(dat$ownincbracket) <- "2019 hhld income bracket"

dat$owninc[which(dat$ownincbracket == 13)] <- 8
dat$owninc[which(dat$ownincbracket == 14)] <- 12.5
dat$owninc[which(dat$ownincbracket == 16)] = 20
dat$owninc[which(dat$ownincbracket == 17)] = 30
dat$owninc[which(dat$ownincbracket == 18)] = 42.5
dat$owninc[which(dat$ownincbracket == 19)] = 62.5
dat$owninc[which(dat$ownincbracket == 20)] = 87.5
dat$owninc[which(dat$ownincbracket == 21)] = 124
dat$owninc[which(dat$ownincbracket == 22)] = 175
dat$owninc[which(dat$ownincbracket == 23)] = 220

names(dat)[which(names(dat)=="Q148")] <- "anyincloss"
label(dat$anyincloss) <- "1 = Any Pandemic income loss?"

names(dat)[which(names(dat)=="Q150")] <- "inclosswk"
label(dat$inclosswk) <- "Amount lost per month = brack..."

####################### PICK IT UP HERE ##########################

dat$pctincloss <- 0
dat$pctincloss[which(dat$inclosswk == 5)] = 5
dat$pctincloss[which(dat$inclosswk == 6)] = 10
dat$pctincloss[which(dat$inclosswk == 7)] = 15
dat$pctincloss[which(dat$inclosswk == 8)] = 20
dat$pctincloss[which(dat$inclosswk == 9)] = 25
dat$pctincloss[which(dat$inclosswk == 10)] = 30
dat$pctincloss[which(dat$inclosswk == 11)] = 35
dat$pctincloss[which(dat$inclosswk == 12)] = 40
dat$pctincloss[which(dat$inclosswk == 13)] = 50
dat$pctincloss[which(dat$inclosswk == 14)] = 60
dat$pctincloss[which(dat$inclosswk == 15)] = 70
dat$pctincloss[which(dat$inclosswk == 16)] = NA
label(dat$pctincloss) <- "Percent loss in weekly income"

label(dat$Q152) <- "Any workers laid off?"
dat$anylayoff = (dat$Q152==1)

label(dat$Q154) <- "Number of workers laid off?"
dat$numlayoff1 = (dat$Q154==5)
dat$numlayoff2 = (dat$Q154==6)
dat$numlayoff3up = (dat$Q154==7)

label(dat$Q156) <- "Laid off workers expect return?"
dat$retlayoff0 = (dat$Q156==7)
dat$retlayoff1 = (dat$Q156==4)
dat$retlayoff2 = (dat$Q156==5)
dat$retlayoff3up = (dat$Q156==6)


label(dat$Q158) <- "Oregon zip"

label(dat$Q159) <- "Oregon zip correct?"

label(dat$Q160) <- "Corrected Oregon zip"


# WRAP-UP ----------------------------------------------------------------------

label(dat$Q161) <- "Federal response to COVID?"

label(dat$Q162) <- "Researcher bias-for first"
label(dat$Q163) <- "Researcher bias-con first"

# All the various time/clicks variables for non-choice pages




# IMPORTED EMBEDDED DATA FROM CSV UPLOADED TO QUALTRICS-------------------------

#-------------------------------------------------------------------------------
# numeric versions of displayed values for pandemic rules
label(dat$ch1_alt1_attr1) <- ""  # numeric versions of numbers of red bars
label(dat$ch1_alt1_attr2) <- ""
label(dat$ch1_alt1_attr3) <- ""
label(dat$ch1_alt1_attr4) <- ""
label(dat$ch1_alt1_attr5) <- ""
label(dat$ch1_alt1_attr6) <- ""
label(dat$ch1_alt1_attr7) <- ""
label(dat$ch1_alt1_attr8) <- ""
label(dat$ch1_alt1_attr9) <- ""
label(dat$ch1_alt1_attr10) <- ""

label(dat$ch1_alt2_attr1) <- ""
label(dat$ch1_alt2_attr2) <- ""
label(dat$ch1_alt2_attr3) <- ""
label(dat$ch1_alt2_attr4) <- ""
label(dat$ch1_alt2_attr5) <- ""
label(dat$ch1_alt2_attr6) <- ""
label(dat$ch1_alt2_attr7) <- ""
label(dat$ch1_alt2_attr8) <- ""
label(dat$ch1_alt2_attr9) <- ""
label(dat$ch1_alt2_attr10) <- ""

label(dat$ch2_alt1_attr1) <- ""
label(dat$ch2_alt1_attr2) <- ""
label(dat$ch2_alt1_attr3) <- ""
label(dat$ch2_alt1_attr4) <- ""
label(dat$ch2_alt1_attr5) <- ""
label(dat$ch2_alt1_attr6) <- ""
label(dat$ch2_alt1_attr7) <- ""
label(dat$ch2_alt1_attr8) <- ""
label(dat$ch2_alt1_attr9) <- ""
label(dat$ch2_alt1_attr10) <- ""

label(dat$ch2_alt2_attr1) <- ""
label(dat$ch2_alt2_attr2) <- ""
label(dat$ch2_alt2_attr3) <- ""
label(dat$ch2_alt2_attr4) <- ""
label(dat$ch2_alt2_attr5) <- ""
label(dat$ch2_alt2_attr6) <- ""
label(dat$ch2_alt2_attr7) <- ""
label(dat$ch2_alt2_attr8) <- ""
label(dat$ch2_alt2_attr9) <- ""
label(dat$ch2_alt2_attr10) <- ""

label(dat$ch3_alt1_attr1) <- ""
label(dat$ch3_alt1_attr2) <- ""
label(dat$ch3_alt1_attr3) <- ""
label(dat$ch3_alt1_attr4) <- ""
label(dat$ch3_alt1_attr5) <- ""
label(dat$ch3_alt1_attr6) <- ""
label(dat$ch3_alt1_attr7) <- ""
label(dat$ch3_alt1_attr8) <- ""
label(dat$ch3_alt1_attr9) <- ""
label(dat$ch3_alt1_attr10) <- ""

label(dat$ch3_alt2_attr1) <- ""
label(dat$ch3_alt2_attr2) <- ""
label(dat$ch3_alt2_attr3) <- ""
label(dat$ch3_alt2_attr4) <- ""
label(dat$ch3_alt2_attr5) <- ""
label(dat$ch3_alt2_attr6) <- ""
label(dat$ch3_alt2_attr7) <- ""
label(dat$ch3_alt2_attr8) <- ""
label(dat$ch3_alt2_attr9) <- ""
label(dat$ch3_alt2_attr10) <- ""

for (at in 1:10) {
  dat$tvar <- dat[,which(names(dat)==paste0("ch1_alt1_attr",at))]
  names(dat)[which(names(dat)=="tvar")] <- paste0("rule",at,"_a")
  
  dat$tvar <- dat[,which(names(dat)==paste0("ch1_alt2_attr",at))]
  names(dat)[which(names(dat)=="tvar")] <- paste0("rule",at,"_b")
  
  dat$tvar <- dat[,which(names(dat)==paste0("ch1_alt1_attr",at))]
  names(dat)[which(names(dat)=="tvar")] <- paste0("rule",at,"_c")
  
  dat$tvar <- dat[,which(names(dat)==paste0("ch1_alt2_attr",at))]
  names(dat)[which(names(dat)=="tvar")] <- paste0("rule",at,"_d")
  
  dat$tvar <- dat[,which(names(dat)==paste0("ch1_alt1_attr",at))]
  names(dat)[which(names(dat)=="tvar")] <- paste0("rule",at,"_e")
  
  dat$tvar <- dat[,which(names(dat)==paste0("ch1_alt2_attr",at))]
  names(dat)[which(names(dat)=="tvar")] <- paste0("rule",at,"_f")
  
}
#-------------------------------------------------------------------------------


# 
# Xch1_alt1_attr1 ""   # image URL for number of red bars
# Xch1_alt1_attr2  ""
# Xch1_alt1_attr3  ""
# Xch1_alt1_attr4  ""
# Xch1_alt1_attr5  ""
# Xch1_alt1_attr6  ""
# Xch1_alt1_attr7  ""
# Xch1_alt1_attr8  ""
# Xch1_alt1_attr9  ""
# Xch1_alt1_attr10  ""
# 
# Xch1_alt2_attr1  ""
# Xch1_alt2_attr2  ""
# Xch1_alt2_attr3  ""
# Xch1_alt2_attr4  ""
# Xch1_alt2_attr5  ""
# Xch1_alt2_attr6  ""
# Xch1_alt2_attr7  ""
# Xch1_alt2_attr8  ""
# Xch1_alt2_attr9  ""
# Xch1_alt2_attr10  ""
# 
# Xch2_alt1_attr1  ""
# Xch2_alt1_attr2  ""
# Xch2_alt1_attr3  ""
# Xch2_alt1_attr4  ""
# Xch2_alt1_attr5  ""
# Xch2_alt1_attr6  ""
# Xch2_alt1_attr7  ""
# Xch2_alt1_attr8  ""
# Xch2_alt1_attr9  ""
# Xch2_alt1_attr10  ""
# 
# Xch2_alt2_attr1  ""
# Xch2_alt2_attr2  ""
# Xch2_alt2_attr3  ""
# Xch2_alt2_attr4  ""
# Xch2_alt2_attr5  ""
# Xch2_alt2_attr6  ""
# Xch2_alt2_attr7  ""
# Xch2_alt2_attr8  ""
# Xch2_alt2_attr9  ""
# Xch2_alt2_attr10  ""
# 
# Xch3_alt1_attr1  ""
# Xch3_alt1_attr2  ""
# Xch3_alt1_attr3  ""
# Xch3_alt1_attr4  ""
# Xch3_alt1_attr5  ""
# Xch3_alt1_attr6  ""
# Xch3_alt1_attr7  ""
# Xch3_alt1_attr8  ""
# Xch3_alt1_attr9  ""
# Xch3_alt1_attr10  ""
# 
# Xch3_alt2_attr1  ""
# Xch3_alt2_attr2  ""
# Xch3_alt2_attr3  ""
# Xch3_alt2_attr4  ""
# Xch3_alt2_attr5  ""
# Xch3_alt2_attr6  ""
# Xch3_alt2_attr7  ""
# Xch3_alt2_attr8  ""
# Xch3_alt2_attr9  ""
# Xch3_alt2_attr10  ""



label(dat$time) <- "Time and date when dcreate ran"
label(dat$rownum) <- "Row number in .csv file uploaded as contacts"


label(dat$noise_lockdown) <- ""   

#-------------------------------------------------------------------------------
# numeric version of displayed attributes: duration of policy
label(dat$months1) <- ""
label(dat$months2) <- ""
label(dat$months3) <- ""

# for stacking
dat$months_a = dat$months1
dat$months_b = dat$months1
dat$months_c = dat$months2
dat$months_d = dat$months2
dat$months_e = dat$months3
dat$months_f = dat$months3
#-------------------------------------------------------------------------------


label(dat$months1_str) <- ""
label(dat$deathswo1) <- ""
label(dat$caseswo1) <- ""
label(dat$deldeaths1_1) <- ""
label(dat$deldeaths1_2) <- ""
label(dat$delcases1_1) <- ""
label(dat$delcases1_2) <- ""
label(dat$cost_ch1_alt1) <- ""
label(dat$cost_ch1_alt2) <- ""
label(dat$cost_ch1_diff) <- ""
label(dat$cost_ch1_tomindiff) <- ""
label(dat$deldeaths_ch1_diff) <- ""
label(dat$delcases_ch1_diff) <- ""

label(dat$months2_str) <- ""
label(dat$deathswo2) <- ""
label(dat$caseswo2) <- ""
label(dat$deldeaths2_1) <- ""
label(dat$deldeaths2_2) <- ""
label(dat$delcases2_1) <- ""
label(dat$delcases2_2) <- ""
label(dat$cost_ch2_alt1) <- ""
label(dat$cost_ch2_alt2) <- ""
label(dat$cost_ch2_diff) <- ""
label(dat$cost_ch2_tomindiff) <- ""
label(dat$deldeaths_ch2_diff) <- ""
label(dat$delcases_ch2_diff) <- ""

label(dat$months3_str) <- ""
label(dat$deathswo3) <- ""
label(dat$caseswo3) <- ""
label(dat$deldeaths3_1) <- ""
label(dat$deldeaths3_2) <- ""
label(dat$delcases3_1) <- ""
label(dat$delcases3_2) <- ""
label(dat$cost_ch3_alt1) <- ""
label(dat$cost_ch3_alt2) <- ""
label(dat$cost_ch3_diff) <- ""
label(dat$cost_ch3_tomindiff) <- ""
label(dat$deldeaths_ch3_diff) <- ""
label(dat$delcases_ch3_diff) <- ""

label(dat$highestcost) <- "Highest cost across all policies"
label(dat$coinflp1) <- ""
label(dat$coinflp2) <- ""

label(dat$buildtime) <- "when design_cases_deaths_cost.. ran"


# VARIABLES CREATED DYNAMICALLY IN JAVASCRIPT


label(dat$fourweekdate) <- "date of recent statistics"
label(dat$countycases_str) <- "counties with cases (comma)"
label(dat$countydeaths_str) <- "counties with deaths (comma)"
label(dat$cases90pct) <- "90th percentile of county cases on date"
label(dat$deaths90pct) <- "90th percentile of county deaths on date"
label(dat$countyname) <- "County name string"
label(dat$countypop_str) <- "County population (comma)"
label(dat$countypop) <- "County population (numeric)"
label(dat$cases90scaled_str) <- "Own county cases scaled to 90-ile rate"
label(dat$deaths90scaled_str) <- "Own county deaths scaled to 90-ile rate"

dat$hhldinc <- dat$hhldinc/1000 
label(dat$hhldinc) <- "Household income county (numeric)"

label(dat$hhldinc_str) <- "Household income county (commas)"
label(dat$hhldincmo_str) <- "Hhld income/month county (commas)"
label(dat$unemps_lose_str) <- "Unemployed lost inc/month (commas)"
label(dat$costunemp5_str) <- "Cost/mo like 5pct unemployment (obsolete)"
label(dat$costunemp10_str) <- "Cost/mo like 10pct unemployment (obsolete)"
label(dat$costunemp15_str) <- "Cost/mo like 15pct unemployment (obsolete)"
label(dat$costunemp20_str) <- "Cost/mo like 20pct unemployment (obsolete)"
label(dat$costunemp25_str) <- "Cost/mo like 25pct unemployment (obsolete)"
label(dat$avratio) <- "avratio--obsolete???"              # verify

label(dat$caseswo1_str) <- ""						
label(dat$caseswo2_str) <- ""
label(dat$caseswo3_num) <- ""   # 3rd set is also used for ideal policy: how many ideal policy will prevent
label(dat$caseswo3_str) <- ""

label(dat$deathswo1_str) <- ""
label(dat$deathswo2_str) <- ""
label(dat$deathswo3_num) <- ""  # 3rd set is also used for ideal policy:  how many ideal policy will prevent
label(dat$deathswo3_str) <- ""

# describe caseswo1_str caseswo2_str caseswo3_str deathswo1_str deathswo2_str deathswo3_str

#########################################################
#########################################################
#########################################################
set1 <- data.frame(x = 4:6, y = 6:4, z = c(1, 3, 5))

plot(1:10, type="n")
XX <- "set1"
with(eval(as.symbol(XX)), symbols(x, y, circles = z, add=TRUE))

#-------------------------------------------------------------------------------
# displayed attributes, cases and deaths with no policy
for (num in 1:3) {
  
  
  # describe caseswo`num'_str
  #	capture confirm numeric variable caseswo`num'_str 
  #  capture drop caseswo`num'_shown
  
  dat[[paste0("caseswo",num,"_shown")]] <- as.numeric(str_remove_all(dat[[paste0("caseswo",num,"_str")]],","))
  dat[[paste0("deathswo",num,"_shown")]] <- as.numeric(str_remove_all(dat[[paste0("deathswo",num,"_str")]],","))
  
  label(dat[[paste0("caseswo",num,"_shown")]]) <- ""
  label(dat[[paste0("deathswo",num,"_shown")]]) <- ""
}	

# for stacking  (sort of, by choice set)
dat$cases_a = dat$caseswo1_shown
dat$cases_b = dat$caseswo1_shown
dat$cases_c = dat$caseswo2_shown 
dat$cases_d = dat$caseswo2_shown 
dat$cases_e = dat$caseswo3_shown 
dat$cases_f = dat$caseswo3_shown 

# for stacking  (sort of, by choice set)
dat$deaths_a = dat$deathswo1_shown
dat$deaths_b = dat$deathswo1_shown
dat$deaths_c = dat$deathswo2_shown
dat$deaths_d = dat$deathswo2_shown
dat$deaths_e = dat$deathswo3_shown
dat$deaths_f = dat$deathswo3_shown

#-------------------------------------------------------------------------------


label(dat$cases_ch1_alt1_str) <- ""       # record of what was actually displayed, with commas if necessary
label(dat$cases_ch1_alt2_str) <- ""
label(dat$cases_ch2_alt1_str) <- ""
label(dat$cases_ch2_alt2_str) <- ""
label(dat$cases_ch3_alt1_str) <- ""
label(dat$cases_ch3_alt2_str) <- ""

label(dat$deaths_ch1_alt1_str) <- ""       # record of what was actually displayed, with commas if necessary
label(dat$deaths_ch1_alt2_str) <- ""
label(dat$deaths_ch2_alt1_str) <- ""
label(dat$deaths_ch2_alt2_str) <- ""
label(dat$deaths_ch3_alt1_str) <- ""
label(dat$deaths_ch3_alt2_str) <- ""

#describe cases_ch1_alt1_str cases_ch1_alt2_str cases_ch2_alt1_str cases_ch2_alt2_str cases_ch3_alt1_str cases_ch3_alt2_str
#describe deaths_ch1_alt1_str deaths_ch1_alt2_str deaths_ch2_alt1_str deaths_ch2_alt2_str deaths_ch3_alt1_str deaths_ch3_alt2_str

#-------------------------------------------------------------------------------
# displayed attributes, reductions in cases and reductions in deaths
for (ch in 1:3) {
  for (al in 1:2) {
    
    
    #	describe cases_ch`ch'_alt`al'_str
    #	capture confirm numeric variable cases_ch`ch'_alt`al'_str
    
    dat[[paste0("cases_ch",ch,"_alt",al)]] <- dat[[paste0("cases_ch",ch,"_alt",al,"_str")]] %>% str_remove_all(",") %>% as.numeric()
    dat[[paste0("deaths_ch",ch,"_alt",al)]] <- dat[[paste0("deaths_ch",ch,"_alt",al,"_str")]] %>% str_remove_all(",") %>% as.numeric()
    
    
    label(dat[[paste0("cases_ch",ch,"_alt",al)]]) <- ""
    label(dat[[paste0("deaths_ch",ch,"_alt",al)]]) <- ""
  }
}	

# for stacking
dat$delcases_a = dat$cases_ch1_alt1
dat$delcases_b = dat$cases_ch1_alt2 
dat$delcases_c = dat$cases_ch2_alt1 
dat$delcases_d = dat$cases_ch2_alt2 
dat$delcases_e = dat$cases_ch3_alt1 
dat$delcases_f = dat$cases_ch3_alt2 

# for stacking
dat$deldeaths_a = dat$deaths_ch1_alt1
dat$deldeaths_b = dat$deaths_ch1_alt2 
dat$deldeaths_c = dat$deaths_ch2_alt1 
dat$deldeaths_d = dat$deaths_ch2_alt2 
dat$deldeaths_e = dat$deaths_ch3_alt1 
dat$deldeaths_f = dat$deaths_ch3_alt2 
#-------------------------------------------------------------------------------




label(dat$cost1_1) <- ""                 # randomized costs scaled by county median hhldinc/55000
label(dat$cost1_2) <- ""
label(dat$cost2_1) <- ""
label(dat$cost2_2) <- ""
label(dat$cost3_1) <- ""
label(dat$cost3_2) <- ""


#-------------------------------------------------------------------------------
# numeric version of displayed attribute - average $/month
label(dat$cost1_1f) <- ""                 # numeric versions of displayed costs; displayed versions are cost1_1_str
label(dat$cost1_2f) <- ""
label(dat$cost2_1f) <- ""
label(dat$cost2_2f) <- ""
label(dat$cost3_1f) <- ""
label(dat$cost3_2f) <- ""

# for stacking
dat$avcost_a = dat$cost1_1f
dat$avcost_b = dat$cost1_2f
dat$avcost_c = dat$cost2_1f
dat$avcost_d = dat$cost2_2f
dat$avcost_e = dat$cost3_1f
dat$avcost_f = dat$cost3_2f
#-------------------------------------------------------------------------------



label(dat$cost1_1_str) <- ""			# string versions of displayed costs, commas added only
label(dat$cost1_2_str) <- ""
label(dat$cost2_1_str) <- ""
label(dat$cost2_2_str) <- ""
label(dat$cost3_1_str) <- ""
label(dat$cost3_2_str) <- ""

label(dat$costint0) <- " superceded by unemployment rates, fixed 0"
label(dat$costint1) <- " superceded by unemployment rates, fixed 1"
label(dat$costint2) <- " superceded by unemployment rates, fixed 2"
label(dat$costint3) <- " superceded by unemployment rates, fixed 3"
label(dat$costint4) <- " superceded by unemployment rates, fixed 4"
label(dat$costint5) <- " superceded by unemployment rates, fixed 5"
label(dat$costint6) <- " superceded by unemployment rates, fixed 6"
label(dat$costint7) <- " superceded by unemployment rates, fixed 7"
label(dat$costint8) <- " superceded by unemployment rates, fixed 8"
label(dat$costint9) <- " superceded by unemployment rates, fixed 9"
label(dat$costint10) <- " superceded by unemployment rates, fixed 10"
label(dat$costint11) <- " superceded by unemployment rates, fixed 11"

label(dat$for_first) <- "=1 if pro-policy bias offered first"

# string for lowest 5% interval, and subsequent 
# see javascript_for_reductions_cases_deaths_in_ideal_policy.txt
label(dat$cred0   ) <- "rounded deciles of baseline for choice 3"       # takes baseline cases for Choice3 takes 1/10th through 10/10ths
label(dat$cred10) <- ""	   	
label(dat$cred20) <- ""
label(dat$cred30) <- ""
label(dat$cred40) <- ""
label(dat$cred50) <- ""
label(dat$cred60) <- ""
label(dat$cred70) <- ""
label(dat$cred80) <- ""
label(dat$cred90) <- ""

label(dat$dred0) <- "rounded quintiles of baseline for choice 3"
label(dat$dred20) <- ""
label(dat$dred40) <- ""
label(dat$dred60) <- ""
label(dat$dred80) <- ""

# amount by which monthly income is/was lower in worst pandemic month so far
# see javascript_for_after_income_bracket_question.txt code
label(dat$brack0) <- ""
label(dat$brack5) <- ""
label(dat$brack10) <- ""
label(dat$brack15) <- ""
label(dat$brack20) <- ""
label(dat$brack25) <- ""
label(dat$brack30) <- ""
label(dat$brack35) <- ""
label(dat$brack40) <- ""
label(dat$brack50) <- ""
label(dat$brack60) <- ""
label(dat$brack70) <- ""

label(dat$cases_lastfourweeks ) <- "" 			#data processed for each county from COVID data daily records, written by stata and 
label(dat$deaths_lastfourweeks) <- ""			# copied into javascript_county_question_with_income_unemp.txt

label(dat$cases_lastfourweeks_str) <- ""
label(dat$deaths_lastfourweeks_str) <- ""

label(dat$stayhome) <- "=1 if Policy D described as Stay Home. Save Lives"
label(dat$stayhome1_str) <- ""
label(dat$stayhome2_str) <- ""



#-------------------------------------------------------------------------------
# numeric version of displayed attributes - unemployment
label(dat$unemp1_1_dec) <- "displayed unempl incl 3.3%, one decimal"
label(dat$unemp1_2_dec) <- ""

label(dat$unemp2_1_dec) <- ""
label(dat$unemp2_2_dec) <- ""

label(dat$unemp3_1_dec) <- ""
label(dat$unemp3_2_dec) <- ""

# for stacking
dat$unempl_a = dat$unemp1_1_dec
dat$unempl_b = dat$unemp1_2_dec
dat$unempl_c = dat$unemp2_1_dec
dat$unempl_d = dat$unemp2_2_dec
dat$unempl_e = dat$unemp3_1_dec
dat$unempl_f = dat$unemp3_2_dec
#-------------------------------------------------------------------------------

label(dat$state) <- "Name of state"
label(dat$stateresidents) <- "Name for state residents"

label(dat$remcases_ch1_alt1_str) <- "remaining cases, string format (obsolete)"
label(dat$remcases_ch1_alt2_str) <- ""
label(dat$remcases_ch2_alt1_str) <- ""
label(dat$remcases_ch2_alt2_str) <- ""
label(dat$remcases_ch3_alt1_str) <- ""
label(dat$remcases_ch3_alt2_str) <- ""

label(dat$remdeaths_ch1_alt1_str) <- "remaining deaths, string format (obsolete)"
label(dat$remdeaths_ch1_alt2_str) <- ""
label(dat$remdeaths_ch2_alt1_str) <- ""
label(dat$remdeaths_ch2_alt2_str) <- ""
label(dat$remdeaths_ch3_alt1_str) <- ""
label(dat$remdeaths_ch3_alt2_str) <- ""

# unemployment rates implied by hhld cost if no federal subsidy, before adding 3.3% baseline unemployment

label(dat$unemp1_1) <- " cost1_1/unemps_lose * 100"
label(dat$unemp1_2) <- ""
label(dat$unemp2_1) <- ""
label(dat$unemp2_2) <- ""
label(dat$unemp3_1) <- ""
label(dat$unemp3_2) <- ""


# verbiage to accommodate Umatilla county - columns need to be deleted before processing results file
# label(dat$top300_2) <- ""
# label(dat$top300_3) <- ""
# label(dat$top300_4) <- ""
# label(dat$top300_5) <- ""
# label(dat$top300_6) <- ""
# label(dat$top300_7) <- ""
# label(dat$top300_8) <- ""




# randomized in javascript


label(dat$fedui1) <- "Federal extra UI payment for AB choices"
label(dat$fedui2) <- "Federal extra UI payment for CD choices"
label(dat$fedui3) <- "Federal extra UI payment for EF choices"

label(dat$medloss1) <- "relevant value of unemps_loseX for this respondent"
label(dat$medloss2) <- ""
label(dat$medloss3) <- ""

label(dat$fedui1_trail) <- " extra verbiage if Federal UI payments >0"
label(dat$fedui2_trail) <- ""
label(dat$fedui3_trail) <- ""

# calculated for each county in Stata, and set in javascript
label(dat$unemps_lose  ) <- "Amount median hhld loses after Oregon UI only"
label(dat$unemps_lose100) <- "Med hhld loses after OR UI and $100 Fed"
label(dat$unemps_lose200) <- "Med hhld loses after OR UI and $200 Fed"
label(dat$unemps_lose300) <- "Med hhld loses after OR UI and $300 Fed"
label(dat$unemps_lose400) <- "Med hhld loses after OR UI and $400 Fed"
label(dat$unemps_lose500) <- "Med hhld loses after OR UI and $500 Fed"
label(dat$unemps_lose600) <- "Med hhld loses after OR UI and $600 Fed"

# for stacking
dat$fedui_a = dat$fedui1
dat$fedui_b = dat$fedui1
dat$fedui_c = dat$fedui2
dat$fedui_d = dat$fedui2
dat$fedui_e = dat$fedui3
dat$fedui_f = dat$fedui3




#===============================================================================




dat <- dat %>% mutate(newcase=row_number())    # consecutive number for observations in order of survey completion


# generate _n variables for Neither Policy cases


dat$fedui_n = 0
dat$months_n = 0
dat$delcases_n = 0
dat$deldeaths_n = 0
dat$unempl_n = 3.3
for (num in 1:10) {
  dat[[paste0("rule",num,"_n")]] = 0	
}

dat$avcost_n = 0

# change file path
write.csv(dat,"~/minimal_full_dataset.csv")

#save "${INT}\minimal_full_dataset.dta", replace