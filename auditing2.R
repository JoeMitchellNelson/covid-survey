require(pacman)

p_load(rpart,rpart.plot,readr,dplyr,RCurl,rjson,lubridate,
       rvest,stringr,Hmisc,rattle,RColorBrewer,ddpcr,tidytext,tidyr,
       ggrepel,ggplot2,png,ggpubr,tidycensus,sf,plm,lmtest,stargazer,MASS,
       xtable,knitr,magick,purrr,ggthemes,gifski,extrafont,latex2exp,
       cowplot,mapproj,patchwork,remotes,tictoc,Hmisc,english,readstata13)

raw <- read.csv("~/covid-survey/data/qualtricsraw.csv")
raw <- raw[3:nrow(raw),]
raw <- raw %>% dplyr::filter(gc==1)

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