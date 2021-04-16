import delimited C:\Users\joem\Documents\covid-survey\data\for_stata_lclogit.csv, clear

gen over75k = 0

replace over75k = 1 if income=="Morethan75"

gen collegegrad = 0

replace collegegrad = 1 if education=="College"

gen younger = 0
gen older = 0

replace younger=1 if age=="18to34"
replace older=1 if age=="65andolder"

gen nonwhite = 0
replace nonwhite=1 if race=="Other"

gen mabsdeathsrp = mabsdeaths*lassorpfl
gen mabscasesrp = mabscases*lassorpfl
gen feduinoneavcostrp = feduinoneavcost*lassorpfl
gen feduianyavcostrp = feduianyavcost*lassorpfl
gen feduinoneunemplrp = feduinoneunempl*lassorpfl
gen feduianyunemplrp = feduianyunempl*lassorpfl
gen rule1rp = rule1*lassorpfl
gen rule2rp = rule2*lassorpfl
gen rule3rp = rule3*lassorpfl
gen rule4rp = rule4*lassorpfl
gen rule5rp = rule5*lassorpfl
gen rule6rp = rule6*lassorpfl
gen rule7rp = rule7*lassorpfl
gen rule8rp = rule8*lassorpfl
gen rule9rp = rule9*lassorpfl
gen rule10rp = rule10*lassorpfl
gen statquorp = statquo*lassorpfl

lclogit best mabsdeaths mabscases feduinoneavcost feduianyavcost feduinoneunempl feduianyunempl ///
  rule1 rule2 rule3 rule4 rule5 rule6 rule7 rule8 rule9 rule10 statquo ///
  mabsdeathsrp mabscasesrp feduinoneavcostrp feduianyavcostrp feduinoneunemplrp feduianyunemplrp ///
  rule1rp rule2rp rule3rp rule4rp rule5rp rule6rp rule7rp rule8rp rule9rp rule10rp statquorp, ///
  group(choice) id(caseid) nclasses(3) membership(ideolcon ideollib female over75k collegegrad younger older nonwhite)
