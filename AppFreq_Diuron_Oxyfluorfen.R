#####SET-UP
if(Sys.info()[4]=="DZ2626UTPURUCKE"){
  #  UserPwd<-path.expand("CHANGE TO WHERE YOU HAVE YOUR DROPBOX FOLDER")
}
#tom mac air
if(Sys.info()[4]=="stp-air"){
  #  UserPwd<-path.expand("CHANGE TO WHERE YOU HAVE YOUR DROPBOX FOLDER")#
}
#carmen personal laptop
if(Sys.info()[4]=="Ashleys-MBP"||Sys.info()[4]=="Ashleys-MacBook-Pro-2.local"||
   Sys.info()[4]=="Ashleys-MBP-2"||Sys.info()[4]=="Ashleys-MacBook-Pro.local") {
  UserPwd<-path.expand("~/")
}
#carmen epa desktop 2
if(Sys.info()[4]=="DZ2626UCKUAN"){
  UserPwd<-path.expand("C:/Users/CKuan/")
}

setwd(dir = paste(UserPwd, 'Dropbox/orise_carmen/urban pesticides/data', sep = ''))
getwd()
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
#####BEGIN
#read in CA PUR text files of Rights-of-Way pesticide applications
for (i in 2006:2015){
  assign(paste('RoW_', i, sep=''), read.table(file = paste(i, '_RightsofWay.txt', sep=''), sep = "\t", header = T, fill = T, strip.white = T))
}
# IndivLicenses <- read.csv(file = "IndivDPRLicenses.csv", header = T, fill = T)
# BusiLicenses <- read.csv(file = "BusiDPRLicenses.csv", header = T, fill = T)

# #filter Rights-of-Way data to Fresno and Madera counties
# Fresno_Madera_2006 <- filter(RoW_2006, COUNTY_NAME=="FRESNO" | COUNTY_NAME=="MADERA")
# Fresno_Madera_2007 <- filter(RoW_2007, COUNTY_NAME=="FRESNO" | COUNTY_NAME=="MADERA")
# Fresno_Madera_2008 <- filter(RoW_2008, COUNTY_NAME=="FRESNO" | COUNTY_NAME=="MADERA")
# Fresno_Madera_2009 <- filter(RoW_2009, COUNTY_NAME=="FRESNO" | COUNTY_NAME=="MADERA")
# Fresno_Madera_2010 <- filter(RoW_2010, COUNTY_NAME=="FRESNO" | COUNTY_NAME=="MADERA")
# Fresno_Madera_2011 <- filter(RoW_2011, COUNTY_NAME=="FRESNO" | COUNTY_NAME=="MADERA")
# Fresno_Madera_2012 <- filter(RoW_2012, COUNTY_NAME=="FRESNO" | COUNTY_NAME=="MADERA")
# Fresno_Madera_2013 <- filter(RoW_2013, COUNTY_NAME=="FRESNO" | COUNTY_NAME=="MADERA")
# Fresno_Madera_2014 <- filter(RoW_2014, COUNTY_NAME=="FRESNO" | COUNTY_NAME=="MADERA")
# Fresno_Madera_2015 <- filter(RoW_2015, COUNTY_NAME=="FRESNO" | COUNTY_NAME=="MADERA")

# IndLic_FresnoMadera <- filter(IndivLicenses, County == "FRESNO" | County == "MADERA")
# BusLic_FresnoMadera <- filter(BusiLicenses, County == "FRESNO" | County == "MADERA")
# Licenses_FresnoMadera <- IndLic_FresnoMadera %>%
#   union_all(BusLic_FresnoMadera, by = "License.No") %>%
#   rename(LICENSE_NUMBER = License.No, COUNTY_NAME=County)


#calculate pesticide usage
pest_calc_2006<- RoW_2006 %>% 
  #select relevant columns
  select(YEAR, APPLICATION_MONTH, COUNTY_NAME, PRODUCT_NAME, CHEMICAL_NAME, PRODUCT_CHEMICAL_PERCENT, AG_NONAG, POUNDS_PRODUCT_APPLIED,POUNDS_CHEMICAL_APPLIED) %>%
  #calculate applied amounts of pesticide (lbs) per entry
  mutate(chem_calc = as.numeric(POUNDS_PRODUCT_APPLIED) * (as.numeric(levels(PRODUCT_CHEMICAL_PERCENT))[PRODUCT_CHEMICAL_PERCENT])/100) %>%
  #filter NAs
  filter(!is.na(chem_calc)) 
pest_calc_2007<- RoW_2007 %>% 
  select(YEAR, APPLICATION_MONTH, COUNTY_NAME, PRODUCT_NAME, CHEMICAL_NAME, PRODUCT_CHEMICAL_PERCENT, AG_NONAG, POUNDS_PRODUCT_APPLIED,POUNDS_CHEMICAL_APPLIED) %>%
  mutate(chem_calc = as.numeric(POUNDS_PRODUCT_APPLIED) * (as.numeric(levels(PRODUCT_CHEMICAL_PERCENT))[PRODUCT_CHEMICAL_PERCENT])/100) %>%
  filter(!is.na(chem_calc)) 
pest_calc_2008<- RoW_2008 %>% 
  select(YEAR, APPLICATION_MONTH, COUNTY_NAME, PRODUCT_NAME, CHEMICAL_NAME, PRODUCT_CHEMICAL_PERCENT, AG_NONAG, POUNDS_PRODUCT_APPLIED,POUNDS_CHEMICAL_APPLIED) %>%
  mutate(chem_calc = as.numeric(POUNDS_PRODUCT_APPLIED) * (as.numeric(levels(PRODUCT_CHEMICAL_PERCENT))[PRODUCT_CHEMICAL_PERCENT])/100) %>%
  filter(!is.na(chem_calc))
pest_calc_2009<- RoW_2009 %>% 
  select(YEAR, APPLICATION_MONTH, COUNTY_NAME, PRODUCT_NAME, CHEMICAL_NAME, PRODUCT_CHEMICAL_PERCENT, AG_NONAG, POUNDS_PRODUCT_APPLIED,POUNDS_CHEMICAL_APPLIED) %>%
  mutate(chem_calc = as.numeric(POUNDS_PRODUCT_APPLIED) * (as.numeric(levels(PRODUCT_CHEMICAL_PERCENT))[PRODUCT_CHEMICAL_PERCENT])/100) %>%
  filter(!is.na(chem_calc)) 
pest_calc_2010<- RoW_2010 %>% 
  select(YEAR, APPLICATION_MONTH, COUNTY_NAME, PRODUCT_NAME, CHEMICAL_NAME, PRODUCT_CHEMICAL_PERCENT, AG_NONAG, POUNDS_PRODUCT_APPLIED,POUNDS_CHEMICAL_APPLIED) %>%
  mutate(chem_calc = as.numeric(POUNDS_PRODUCT_APPLIED) * (as.numeric(levels(PRODUCT_CHEMICAL_PERCENT))[PRODUCT_CHEMICAL_PERCENT])/100) %>%
  filter(!is.na(chem_calc))
pest_calc_2011<- RoW_2011 %>% 
  select(YEAR, APPLICATION_MONTH, COUNTY_NAME, PRODUCT_NAME, CHEMICAL_NAME, PRODUCT_CHEMICAL_PERCENT, AG_NONAG, POUNDS_PRODUCT_APPLIED,POUNDS_CHEMICAL_APPLIED) %>%
  mutate(chem_calc = as.numeric(POUNDS_PRODUCT_APPLIED) * (as.numeric(levels(PRODUCT_CHEMICAL_PERCENT))[PRODUCT_CHEMICAL_PERCENT])/100) %>%
  filter(!is.na(chem_calc)) 
pest_calc_2012<- RoW_2012 %>% 
  select(YEAR, APPLICATION_MONTH, COUNTY_NAME, PRODUCT_NAME, CHEMICAL_NAME, PRODUCT_CHEMICAL_PERCENT, AG_NONAG, POUNDS_PRODUCT_APPLIED,POUNDS_CHEMICAL_APPLIED) %>%
  mutate(chem_calc = as.numeric(POUNDS_PRODUCT_APPLIED) * (as.numeric(levels(PRODUCT_CHEMICAL_PERCENT))[PRODUCT_CHEMICAL_PERCENT])/100) %>%
  filter(!is.na(chem_calc)) 
pest_calc_2013<- RoW_2013 %>% 
  select(YEAR, APPLICATION_MONTH, COUNTY_NAME, PRODUCT_NAME, CHEMICAL_NAME, PRODUCT_CHEMICAL_PERCENT, AG_NONAG, POUNDS_PRODUCT_APPLIED,POUNDS_CHEMICAL_APPLIED) %>%
  mutate(chem_calc = as.numeric(POUNDS_PRODUCT_APPLIED) * (as.numeric(levels(PRODUCT_CHEMICAL_PERCENT))[PRODUCT_CHEMICAL_PERCENT])/100) %>%
  filter(!is.na(chem_calc)) 
pest_calc_2014<- RoW_2014 %>% 
  select(YEAR, APPLICATION_MONTH, COUNTY_NAME, PRODUCT_NAME, CHEMICAL_NAME, PRODUCT_CHEMICAL_PERCENT, AG_NONAG, POUNDS_PRODUCT_APPLIED,POUNDS_CHEMICAL_APPLIED) %>%
  mutate(chem_calc = as.numeric(POUNDS_PRODUCT_APPLIED) * (as.numeric(levels(PRODUCT_CHEMICAL_PERCENT))[PRODUCT_CHEMICAL_PERCENT])/100) %>%
  filter(!is.na(chem_calc))
pest_calc_2015<- RoW_2015 %>% 
  select(YEAR, APPLICATION_MONTH, COUNTY_NAME, PRODUCT_NAME, CHEMICAL_NAME, PRODUCT_CHEMICAL_PERCENT, AG_NONAG, POUNDS_PRODUCT_APPLIED,POUNDS_CHEMICAL_APPLIED) %>%
  mutate(chem_calc = as.numeric(POUNDS_PRODUCT_APPLIED) * (as.numeric(levels(PRODUCT_CHEMICAL_PERCENT))[PRODUCT_CHEMICAL_PERCENT])/100) %>%
  filter(!is.na(chem_calc)) 

# combine all Fresno_Madera data
PestCalc <- rbind(pest_calc_2006, pest_calc_2007, pest_calc_2008, pest_calc_2009, 
                  pest_calc_2010, pest_calc_2011, pest_calc_2012, pest_calc_2013, 
                  pest_calc_2014, pest_calc_2015)
# CompDF <- FresnoMadera_PestCalc %>%
#   mutate_each(funs(as.integer), LICENSE_NUMBER) %>%
#   inner_join(Licenses_FresnoMadera, by = c("LICENSE_NUMBER","COUNTY_NAME"))


# filter data by unique chemicals, sum total use per chemical, and subset top 20 chemicals by year and county
TotalAppliedPerChemical <- PestCalc %>%
  select(YEAR, COUNTY_NAME, PRODUCT_NAME, CHEMICAL_NAME, chem_calc) %>%
  group_by(YEAR, CHEMICAL_NAME) %>%
  mutate(total_lbs_per_mile = if(YEAR=="2006") (cumsum(chem_calc)/170296.67) 
         else if(YEAR=="2007") (cumsum(chem_calc)/171154.13) 
         else if(YEAR=="2008") (cumsum(chem_calc)/172511.33) 
         else if(YEAR=="2009") (cumsum(chem_calc)/171873.92)
         else if(YEAR=="2010") (cumsum(chem_calc)/172138.66)
         else if(YEAR=="2011") (cumsum(chem_calc)/172201.63)
         else if(YEAR=="2012") (cumsum(chem_calc)/175543.79)
         else if(YEAR=="2013") (cumsum(chem_calc)/174991.13)
         else if(YEAR=="2014") (cumsum(chem_calc)/174802.85)
         else if(YEAR=="2015") (cumsum(chem_calc)/174802.85)) %>%
  group_by(YEAR, CHEMICAL_NAME) %>%
  summarise(lbs_per_mile_per_year = sum(total_lbs_per_mile))
