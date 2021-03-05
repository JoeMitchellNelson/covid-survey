require(pacman)

p_load(rpart,rpart.plot,readr,dplyr,RCurl,rjson,lubridate,
       rvest,stringr,Hmisc,rattle,RColorBrewer,ddpcr,tidytext,tidyr,
       ggrepel,ggplot2,png,ggpubr,tidycensus,sf,plm,lmtest,stargazer,MASS,
       xtable,knitr,magick,purrr,ggthemes,gifski,extrafont,latex2exp,
       cowplot,mapproj,patchwork,remotes,tictoc,Hmisc,english,readstata13)

raw <- read.csv("~/covid-survey/data/qualtricsraw.csv")
raw <- raw[14:nrow(raw),]
raw <- raw %>% dplyr::filter(gc==1)

dat <- read.csv("C:/Users/joem/Dropbox (University of Oregon)/VSL-COVID/intermediate-files/main_vars_nointx.csv")


summary((raw$caseswo1_str %>% str_remove_all(",") %>% as.numeric()) - (raw$deathswo1_str %>% str_remove_all(",") %>% as.numeric()))
summary((raw$caseswo2_str %>% str_remove_all(",") %>% as.numeric()) - (raw$deathswo2_str %>% str_remove_all(",") %>% as.numeric()))
summary((raw$caseswo3_str %>% str_remove_all(",") %>% as.numeric()) - (raw$deathswo3_str %>% str_remove_all(",") %>% as.numeric()))

summary((raw$cases_ch1_alt1_str %>% str_remove_all(",") %>% as.numeric()) - (raw$deaths_ch1_alt1_str %>% str_remove_all(",") %>% as.numeric()))
summary((raw$cases_ch2_alt1_str %>% str_remove_all(",") %>% as.numeric()) - (raw$deaths_ch2_alt1_str %>% str_remove_all(",") %>% as.numeric()))
summary((raw$cases_ch3_alt1_str %>% str_remove_all(",") %>% as.numeric()) - (raw$deaths_ch3_alt1_str %>% str_remove_all(",") %>% as.numeric()))

summary((raw$caseswo1_str %>% str_remove_all(",") %>% as.numeric()) + (raw$deathswo1_str %>% str_remove_all(",") %>% as.numeric()) -(raw$cases_ch1_alt2_str %>% str_remove_all(",") %>% as.numeric()) - (raw$deaths_ch1_alt2_str %>% str_remove_all(",") %>% as.numeric()))
summary((raw$caseswo2_str %>% str_remove_all(",") %>% as.numeric()) + (raw$deathswo2_str %>% str_remove_all(",") %>% as.numeric()) -(raw$cases_ch2_alt2_str %>% str_remove_all(",") %>% as.numeric()) - (raw$deaths_ch2_alt2_str %>% str_remove_all(",") %>% as.numeric()))
summary((raw$caseswo3_str %>% str_remove_all(",") %>% as.numeric()) + (raw$deathswo3_str %>% str_remove_all(",") %>% as.numeric()) -(raw$cases_ch3_alt2_str %>% str_remove_all(",") %>% as.numeric()) - (raw$deaths_ch3_alt2_str %>% str_remove_all(",") %>% as.numeric()))

summary((raw$caseswo1_str %>% str_remove_all(",") %>% as.numeric()) + (raw$deathswo1_str %>% str_remove_all(",") %>% as.numeric()) -(raw$cases_ch1_alt1_str %>% str_remove_all(",") %>% as.numeric()) - (raw$deaths_ch1_alt1_str %>% str_remove_all(",") %>% as.numeric()))
summary((raw$caseswo2_str %>% str_remove_all(",") %>% as.numeric()) + (raw$deathswo2_str %>% str_remove_all(",") %>% as.numeric()) -(raw$cases_ch2_alt1_str %>% str_remove_all(",") %>% as.numeric()) - (raw$deaths_ch2_alt1_str %>% str_remove_all(",") %>% as.numeric()))
summary((raw$caseswo3_str %>% str_remove_all(",") %>% as.numeric()) + (raw$deathswo3_str %>% str_remove_all(",") %>% as.numeric()) -(raw$cases_ch3_alt1_str %>% str_remove_all(",") %>% as.numeric()) - (raw$deaths_ch3_alt1_str %>% str_remove_all(",") %>% as.numeric()))

summary((raw$caseswo1_str %>% str_remove_all(",") %>% as.numeric()) - (raw$deaths_ch1_alt1_str %>% str_remove_all(",") %>% as.numeric()))
summary((raw$caseswo2_str %>% str_remove_all(",") %>% as.numeric()) - (raw$deaths_ch2_alt1_str %>% str_remove_all(",") %>% as.numeric()))
summary((raw$caseswo3_str %>% str_remove_all(",") %>% as.numeric()) - (raw$deaths_ch3_alt1_str %>% str_remove_all(",") %>% as.numeric()))


toaudit <- raw %>% dplyr::select(ResponseId,caseswo1_str,deathswo1_str,
                                 caseswo2_str,deathswo2_str,
                                 caseswo3_str,deathswo3_str,
                                 cases_ch1_alt1_str,cases_ch1_alt2_str,
                                 deaths_ch1_alt1_str,deaths_ch1_alt2_str,
                                 cases_ch2_alt1_str,cases_ch2_alt2_str,
                                 deaths_ch2_alt1_str,deaths_ch2_alt2_str,
                                 cases_ch3_alt1_str,cases_ch3_alt2_str,
                                 deaths_ch3_alt1_str,deaths_ch3_alt2_str)

toaudit$caseswo1_str <- toaudit$caseswo1_str %>% str_remove_all(",") %>% as.numeric()
toaudit$deathswo1_str <- toaudit$deathswo1_str %>% str_remove_all(",") %>% as.numeric()
toaudit$caseswo2_str <- toaudit$caseswo2_str %>% str_remove_all(",") %>% as.numeric()
toaudit$deathswo2_str <- toaudit$deathswo2_str %>% str_remove_all(",") %>% as.numeric()
toaudit$caseswo3_str <- toaudit$caseswo3_str %>% str_remove_all(",") %>% as.numeric()
toaudit$deathswo3_str <- toaudit$deathswo3_str %>% str_remove_all(",") %>% as.numeric()

toaudit$cases_ch1_alt1_str <- toaudit$cases_ch1_alt1_str %>% str_remove_all(",") %>% as.numeric()
toaudit$cases_ch1_alt2_str <- toaudit$cases_ch1_alt2_str %>% str_remove_all(",") %>% as.numeric()
toaudit$deaths_ch1_alt1_str <- toaudit$deaths_ch1_alt1_str %>% str_remove_all(",") %>% as.numeric()
toaudit$deaths_ch1_alt2_str <- toaudit$deaths_ch1_alt2_str %>% str_remove_all(",") %>% as.numeric()

toaudit$cases_ch2_alt1_str <- toaudit$cases_ch2_alt1_str %>% str_remove_all(",") %>% as.numeric()
toaudit$cases_ch2_alt2_str <- toaudit$cases_ch2_alt2_str %>% str_remove_all(",") %>% as.numeric()
toaudit$deaths_ch2_alt1_str <- toaudit$deaths_ch2_alt1_str %>% str_remove_all(",") %>% as.numeric()
toaudit$deaths_ch2_alt2_str <- toaudit$deaths_ch2_alt2_str %>% str_remove_all(",") %>% as.numeric()

toaudit$cases_ch3_alt1_str <- toaudit$cases_ch3_alt1_str %>% str_remove_all(",") %>% as.numeric()
toaudit$cases_ch3_alt2_str <- toaudit$cases_ch3_alt2_str %>% str_remove_all(",") %>% as.numeric()
toaudit$deaths_ch3_alt1_str <- toaudit$deaths_ch3_alt1_str %>% str_remove_all(",") %>% as.numeric()
toaudit$deaths_ch3_alt2_str <- toaudit$deaths_ch3_alt2_str %>% str_remove_all(",") %>% as.numeric()

#toaudit$ResponseId <- toaudit$ResponseId %>% as.character() %>% as.numeric()
toaudit2 <- dat %>% dplyr::select(ResponseId,best,alt,choiceofperson,countypop,caseswo,deathswo,delcases,deldeaths)

toaudit <- left_join(toaudit2,toaudit)
toaudit <- toaudit %>% mutate(caseswo_unt = caseswo * countypop/50000,
                              deathswo_unt = deathswo * countypop/50000,
                              delcases_unt = delcases * countypop/50000,
                              deldeaths_unt = deldeaths * countypop/50000)

problems_wo <- toaudit %>% dplyr::filter( ((caseswo_unt-caseswo1_str)^2 > 1 & choiceofperson %in% 1:2) |
                                         ((deathswo_unt-deathswo1_str)^2 > 1 & choiceofperson %in% 1:2) |
                                         ((caseswo_unt-caseswo2_str)^2 > 1 & choiceofperson %in% 3:5) |
                                         ((deathswo_unt-deathswo2_str)^2 > 1 & choiceofperson %in% 3:5) |
                                         ((caseswo_unt-caseswo3_str)^2 > 1 & choiceofperson %in% 6:8) |
                                         ((deathswo_unt-deathswo3_str)^2 > 1 & choiceofperson %in% 6:8))

problems_ch11 <- toaudit %>% 
  dplyr::filter((cases_ch1_alt1_str - delcases_unt)^2 > 1 & choiceofperson==1 & alt==1) %>% 
  dplyr::select(ResponseId,choiceofperson,alt,cases_ch1_alt1_str,delcases_unt) %>% mutate(factor=delcases_unt/cases_ch1_alt1_str)

problems_ch12 <- toaudit %>% 
  dplyr::filter((cases_ch1_alt2_str - delcases_unt)^2 > 1 & choiceofperson==2 & alt==2) %>% 
  dplyr::select(ResponseId,choiceofperson,alt,cases_ch1_alt2_str,delcases_unt) %>% mutate(factor=delcases_unt/cases_ch1_alt2_str)

problems_ch21 <- toaudit %>% 
  dplyr::filter((cases_ch2_alt1_str - delcases_unt)^2 > 1 & choiceofperson %in% 3:5 & alt==1) %>% 
  dplyr::select(ResponseId,choiceofperson,alt,cases_ch2_alt1_str,delcases_unt) %>% mutate(factor=delcases_unt/cases_ch2_alt1_str)

problems_ch22 <- toaudit %>% 
  dplyr::filter((cases_ch2_alt2_str - delcases_unt)^2 > 1 & choiceofperson %in% 3:5 & alt==2) %>% 
  dplyr::select(ResponseId,choiceofperson,alt,cases_ch2_alt2_str,delcases_unt) %>% mutate(factor=delcases_unt/cases_ch2_alt2_str)

###

problems_ch11d <- toaudit %>% 
  dplyr::filter((deaths_ch1_alt1_str - deldeaths_unt)^2 > 1 & choiceofperson==1 & alt==1) %>% 
  dplyr::select(ResponseId,choiceofperson,alt,deaths_ch1_alt1_str,deldeaths_unt) %>% mutate(factor=deldeaths_unt/deaths_ch1_alt1_str)

problems_ch12d <- toaudit %>% 
  dplyr::filter((deaths_ch1_alt2_str - deldeaths_unt)^2 > 1 & choiceofperson==2 & alt==2) %>% 
  dplyr::select(ResponseId,choiceofperson,alt,deaths_ch1_alt2_str,deldeaths_unt) %>% mutate(factor=deldeaths_unt/deaths_ch1_alt2_str)

problems_ch21d <- toaudit %>% 
  dplyr::filter((deaths_ch2_alt1_str - deldeaths_unt)^2 > 1 & choiceofperson %in% 3:5 & alt==1) %>% 
  dplyr::select(ResponseId,choiceofperson,alt,deaths_ch2_alt1_str,deldeaths_unt) %>% mutate(factor=deldeaths_unt/deaths_ch2_alt1_str)

problems_ch22d <- toaudit %>% 
  dplyr::filter((deaths_ch2_alt2_str - deldeaths_unt)^2 > 1 & choiceofperson %in% 3:5 & alt==2) %>% 
  dplyr::select(ResponseId,choiceofperson,alt,deaths_ch2_alt2_str,deldeaths_unt) %>% mutate(factor=deldeaths_unt/deaths_ch2_alt2_str)


write.csv(unique(problems_wo$caseid),"~/covid-survey/problem-caseids.csv")

hmm <- raw %>% group_by(ExternalReference) %>% summarise(n=n()) %>% dplyr::filter(n>14 | nchoice>6)

hmmm <- raw %>% dplyr::filter(ExternalReference %in% hmm$ExternalReference)

hm <- dat %>% group_by(caseid) %>% summarise(n=n(),nchoice=length(unique(choice))) %>% dplyr::filter(n>14 | nchoice>6)

write.csv(hm,"~/covid-survey/problem-caseids.csv")

#########################################
dupes <- raw %>% group_by(ExternalReference) %>% summarise(n=n()) %>% dplyr::filter(n>1)
negnf <- dat %>% dplyr::filter(mabsnfcases < 0)
negnf$caseid %in% dupes$ExternalReference

#######################################
# Q206 which state
# Q205 age group
# Q204 gender
# Q207 race
# Q146 income
# Q4 thoughtful and honest,
# Q68 Policy A vs no policy
# Q73 Policy B vs no policy
# Q78 Policy C, D, N
# Q83 Policy D or N
# Q88 Policy C or N
# Q93 Policy E, F or N
# Q98 Policy F or N
# Q103 Policy E or N

### BARS URLS
# ALL GREEN: https://oregon.qualtrics.com/CP/Graphic.php?IM=IM_8xhBXfU5LHYvWKN
# ONE RED: https://oregon.qualtrics.com/CP/Graphic.php?IM=IM_3fLQpVdXmWyOUzX
# TWO RED: https://oregon.qualtrics.com/CP/Graphic.php?IM=IM_eb2NHH5lwkXvJqJ
# THREE RED: https://oregon.qualtrics.com/CP/Graphic.php?IM=IM_6AqLaErdRD5TEbz

raw$Xch1_alt1_attr1_img <- ifelse(str_detect(raw$Xch1_alt1_attr1,"8xhBXfU5LHYvWKN"),0,
                                  ifelse(str_detect(raw$Xch1_alt1_attr1,"3fLQpVdXmWyOUzX"),1,
                                         ifelse(str_detect(raw$Xch1_alt1_attr1,"eb2NHH5lwkXvJqJ"),2,
                                                ifelse(str_detect(raw$Xch1_alt1_attr1,"6AqLaErdRD5TEbz"),3,NA))))
raw$Xch1_alt1_attr2_img <- ifelse(str_detect(raw$Xch1_alt1_attr2,"8xhBXfU5LHYvWKN"),0,
                                  ifelse(str_detect(raw$Xch1_alt1_attr2,"3fLQpVdXmWyOUzX"),1,
                                         ifelse(str_detect(raw$Xch1_alt1_attr2,"eb2NHH5lwkXvJqJ"),2,
                                                ifelse(str_detect(raw$Xch1_alt1_attr2,"6AqLaErdRD5TEbz"),3,NA))))
raw$Xch1_alt1_attr7_img <- ifelse(str_detect(raw$Xch1_alt1_attr7,"8xhBXfU5LHYvWKN"),0,
                                  ifelse(str_detect(raw$Xch1_alt1_attr7,"3fLQpVdXmWyOUzX"),1,
                                         ifelse(str_detect(raw$Xch1_alt1_attr7,"eb2NHH5lwkXvJqJ"),2,
                                                ifelse(str_detect(raw$Xch1_alt1_attr7,"6AqLaErdRD5TEbz"),3,NA))))

sum(raw$ch1_alt1_attr7!=raw$Xch1_alt1_attr7_img)


oneguy <- raw %>% dplyr::filter(ResponseId=="R_daKqb5ILM3r0XE5")
twoguy <- raw %>% dplyr::filter(ResponseId=="R_2rAzQxL0aTE0KkA")
write.csv(twoguy,"~/covid-survey/R_2rAzQxL0aTE0KkA_raw_from_qualtrics.csv")

twoguy$deaths_ch1_alt1_str
twoguy$deathswo1_str
twoguy$cases_ch1_alt1_str
twoguy$caseswo1_str

summary((raw$caseswo1_str %>% str_remove_all(",") %>% as.numeric()) - (raw$deathswo1_str %>% str_remove_all(",") %>% as.numeric()))
summary((raw$caseswo2_str %>% str_remove_all(",") %>% as.numeric()) - (raw$deathswo2_str %>% str_remove_all(",") %>% as.numeric()))
summary((raw$caseswo3_str %>% str_remove_all(",") %>% as.numeric()) - (raw$deathswo3_str %>% str_remove_all(",") %>% as.numeric()))

summary((raw$cases_ch1_alt2_str %>% str_remove_all(",") %>% as.numeric()) - (raw$deaths_ch1_alt2_str %>% str_remove_all(",") %>% as.numeric()))
summary((raw$cases_ch2_alt2_str %>% str_remove_all(",") %>% as.numeric()) - (raw$deaths_ch2_alt2_str %>% str_remove_all(",") %>% as.numeric()))
summary((raw$cases_ch3_alt2_str %>% str_remove_all(",") %>% as.numeric()) - (raw$deaths_ch3_alt2_str %>% str_remove_all(",") %>% as.numeric()))

toaudit <- raw %>% dplyr::select(ExternalReference,caseswo1_str,deathswo1_str)
toaudit <- toaudit[3:nrow(toaudit),]
toaudit$cases <- toaudit$caseswo1_str %>% str_remove_all(",") %>% as.numeric()
toaudit$deaths <- toaudit$deathswo1_str %>% str_remove_all(",") %>% as.numeric()
toaudit$caseid <- toaudit$ExternalReference %>% as.character() %>% as.numeric()
toaudit2 <- dat %>% dplyr::select(caseid,countypop,caseswo,deathswo,alt,choiceofperson) %>% dplyr::filter(alt==1,choiceofperson==1)

toaudit <- left_join(toaudit2,toaudit)
toaudit <- toaudit %>% mutate(casesraw = caseswo * countypop/50000) %>% dplyr::filter(!is.na(cases))
toaudit <- toaudit %>% mutate(deathsraw = deathswo * countypop/50000) %>% dplyr::filter(!is.na(cases))

huh <- toaudit %>% dplyr::filter((casesraw - cases)^2>1)
huh2 <- toaudit %>% dplyr::filter((deathsraw-deaths)^2>1)

huh2 <- huh2 %>% mutate(caseratio = casesraw/cases,deathsratio=deathsraw/deaths) 
# for twoguy, policy A
# IN CSV: 2	3	2	0	1	3	1	2	2	2
# IN PDF: 2 3 2 2 0 1 3 1 2 2

# for twoguy, policy B
# IN CSV: 1	2	1	3	2	1	0	1	1	0
# IN PDF: 1 2 1 0 3 2 1 0 1 1

written <- raw %>% dplyr::select(which(str_detect(names(raw),"TEXT")))
 
whynot <- written$Q71_9_TEXT[which(written$Q71_9_TEXT!="")] %>% paste(collapse = "\n\n") 
whynot2 <- written$Q76_9_TEXT[which(written$Q76_9_TEXT!="")] %>% paste(collapse = "\n\n") 
whynot3 <- written$Q81_9_TEXT[which(written$Q81_9_TEXT!="")] %>% paste(collapse = "\n\n") 
whynot4 <- written$Q86_9_TEXT[which(written$Q86_9_TEXT!="")] %>% paste(collapse = "\n\n") 
whynot5 <- written$Q96_9_TEXT[which(written$Q96_9_TEXT!="")] %>% paste(collapse = "\n\n") 

cat(whynot5)

raw <- raw %>% dplyr::select(ResponseId,Q206,Q205)