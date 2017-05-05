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
library(reshape2)
#####BEGIN
#read in CA PUR text files of Rights-of-Way pesticide applications
for (i in 2006:2015){
  assign(paste('RoW_', i, sep=''), read.table(file = paste(i, '_RightsofWay.txt', sep=''), sep = "\t", header = T, fill = T, strip.white = T))
}

#filter Rights-of-Way data to Fresno and Madera counties
Fresno_Madera_2006 <- filter(RoW_2006, COUNTY_NAME=="FRESNO" | COUNTY_NAME=="MADERA")
Fresno_Madera_2007 <- filter(RoW_2007, COUNTY_NAME=="FRESNO" | COUNTY_NAME=="MADERA")
Fresno_Madera_2008 <- filter(RoW_2008, COUNTY_NAME=="FRESNO" | COUNTY_NAME=="MADERA")
Fresno_Madera_2009 <- filter(RoW_2009, COUNTY_NAME=="FRESNO" | COUNTY_NAME=="MADERA")
Fresno_Madera_2010 <- filter(RoW_2010, COUNTY_NAME=="FRESNO" | COUNTY_NAME=="MADERA")
Fresno_Madera_2011 <- filter(RoW_2011, COUNTY_NAME=="FRESNO" | COUNTY_NAME=="MADERA")
Fresno_Madera_2012 <- filter(RoW_2012, COUNTY_NAME=="FRESNO" | COUNTY_NAME=="MADERA")
Fresno_Madera_2013 <- filter(RoW_2013, COUNTY_NAME=="FRESNO" | COUNTY_NAME=="MADERA")
Fresno_Madera_2014 <- filter(RoW_2014, COUNTY_NAME=="FRESNO" | COUNTY_NAME=="MADERA")
Fresno_Madera_2015 <- filter(RoW_2015, COUNTY_NAME=="FRESNO" | COUNTY_NAME=="MADERA")


#calculate pesticide usage
pest_calc_2006<- Fresno_Madera_2006 %>% 
  #select relevant columns
  select(YEAR, APPLICATION_MONTH, COUNTY_NAME, PRODUCT_NAME, CHEMICAL_NAME, PRODUCT_CHEMICAL_PERCENT, AG_NONAG, POUNDS_PRODUCT_APPLIED,POUNDS_CHEMICAL_APPLIED) %>%
  #calculate applied amounts of pesticide (lbs) per entry
  mutate(chem_calc = as.numeric(POUNDS_PRODUCT_APPLIED) * (as.numeric(levels(PRODUCT_CHEMICAL_PERCENT))[PRODUCT_CHEMICAL_PERCENT])/100) %>%
  #filter NAs
  filter(!is.na(chem_calc)) 
pest_calc_2007<- Fresno_Madera_2007 %>% 
  select(YEAR, APPLICATION_MONTH, COUNTY_NAME, PRODUCT_NAME, CHEMICAL_NAME, PRODUCT_CHEMICAL_PERCENT, AG_NONAG, POUNDS_PRODUCT_APPLIED,POUNDS_CHEMICAL_APPLIED) %>%
  mutate(chem_calc = as.numeric(POUNDS_PRODUCT_APPLIED) * (as.numeric(levels(PRODUCT_CHEMICAL_PERCENT))[PRODUCT_CHEMICAL_PERCENT])/100) %>%
  filter(!is.na(chem_calc)) 
pest_calc_2008<- Fresno_Madera_2008 %>% 
  select(YEAR, APPLICATION_MONTH, COUNTY_NAME, PRODUCT_NAME, CHEMICAL_NAME, PRODUCT_CHEMICAL_PERCENT, AG_NONAG, POUNDS_PRODUCT_APPLIED,POUNDS_CHEMICAL_APPLIED) %>%
  mutate(chem_calc = as.numeric(POUNDS_PRODUCT_APPLIED) * (as.numeric(levels(PRODUCT_CHEMICAL_PERCENT))[PRODUCT_CHEMICAL_PERCENT])/100) %>%
  filter(!is.na(chem_calc))
pest_calc_2009<- Fresno_Madera_2009 %>% 
  select(YEAR, APPLICATION_MONTH, COUNTY_NAME, PRODUCT_NAME, CHEMICAL_NAME, PRODUCT_CHEMICAL_PERCENT, AG_NONAG, POUNDS_PRODUCT_APPLIED,POUNDS_CHEMICAL_APPLIED) %>%
  mutate(chem_calc = as.numeric(POUNDS_PRODUCT_APPLIED) * (as.numeric(levels(PRODUCT_CHEMICAL_PERCENT))[PRODUCT_CHEMICAL_PERCENT])/100) %>%
  filter(!is.na(chem_calc)) 
pest_calc_2010<- Fresno_Madera_2010 %>% 
  select(YEAR, APPLICATION_MONTH, COUNTY_NAME, PRODUCT_NAME, CHEMICAL_NAME, PRODUCT_CHEMICAL_PERCENT, AG_NONAG, POUNDS_PRODUCT_APPLIED,POUNDS_CHEMICAL_APPLIED) %>%
  mutate(chem_calc = as.numeric(POUNDS_PRODUCT_APPLIED) * (as.numeric(levels(PRODUCT_CHEMICAL_PERCENT))[PRODUCT_CHEMICAL_PERCENT])/100) %>%
  filter(!is.na(chem_calc))
pest_calc_2011<- Fresno_Madera_2011 %>% 
  select(YEAR, APPLICATION_MONTH, COUNTY_NAME, PRODUCT_NAME, CHEMICAL_NAME, PRODUCT_CHEMICAL_PERCENT, AG_NONAG, POUNDS_PRODUCT_APPLIED,POUNDS_CHEMICAL_APPLIED) %>%
  mutate(chem_calc = as.numeric(POUNDS_PRODUCT_APPLIED) * (as.numeric(levels(PRODUCT_CHEMICAL_PERCENT))[PRODUCT_CHEMICAL_PERCENT])/100) %>%
  filter(!is.na(chem_calc)) 
pest_calc_2012<- Fresno_Madera_2012 %>% 
  select(YEAR, APPLICATION_MONTH, COUNTY_NAME, PRODUCT_NAME, CHEMICAL_NAME, PRODUCT_CHEMICAL_PERCENT, AG_NONAG, POUNDS_PRODUCT_APPLIED,POUNDS_CHEMICAL_APPLIED) %>%
  mutate(chem_calc = as.numeric(POUNDS_PRODUCT_APPLIED) * (as.numeric(levels(PRODUCT_CHEMICAL_PERCENT))[PRODUCT_CHEMICAL_PERCENT])/100) %>%
  filter(!is.na(chem_calc)) 
pest_calc_2013<- Fresno_Madera_2013 %>% 
  select(YEAR, APPLICATION_MONTH, COUNTY_NAME, PRODUCT_NAME, CHEMICAL_NAME, PRODUCT_CHEMICAL_PERCENT, AG_NONAG, POUNDS_PRODUCT_APPLIED,POUNDS_CHEMICAL_APPLIED) %>%
  mutate(chem_calc = as.numeric(POUNDS_PRODUCT_APPLIED) * (as.numeric(levels(PRODUCT_CHEMICAL_PERCENT))[PRODUCT_CHEMICAL_PERCENT])/100) %>%
  filter(!is.na(chem_calc)) 
pest_calc_2014<- Fresno_Madera_2014 %>% 
  select(YEAR, APPLICATION_MONTH, COUNTY_NAME, PRODUCT_NAME, CHEMICAL_NAME, PRODUCT_CHEMICAL_PERCENT, AG_NONAG, POUNDS_PRODUCT_APPLIED,POUNDS_CHEMICAL_APPLIED) %>%
  mutate(chem_calc = as.numeric(POUNDS_PRODUCT_APPLIED) * (as.numeric(levels(PRODUCT_CHEMICAL_PERCENT))[PRODUCT_CHEMICAL_PERCENT])/100) %>%
  filter(!is.na(chem_calc))
pest_calc_2015<- Fresno_Madera_2015 %>% 
  select(YEAR, APPLICATION_MONTH, COUNTY_NAME, PRODUCT_NAME, CHEMICAL_NAME, PRODUCT_CHEMICAL_PERCENT, AG_NONAG, POUNDS_PRODUCT_APPLIED,POUNDS_CHEMICAL_APPLIED) %>%
  mutate(chem_calc = as.numeric(POUNDS_PRODUCT_APPLIED) * (as.numeric(levels(PRODUCT_CHEMICAL_PERCENT))[PRODUCT_CHEMICAL_PERCENT])/100) %>%
  filter(!is.na(chem_calc)) 

# combine all Fresno_Madera data
FresnoMadera_PestCalc <- rbind(pest_calc_2006, pest_calc_2007, pest_calc_2008, pest_calc_2009, 
                               pest_calc_2010, pest_calc_2011, pest_calc_2012, pest_calc_2013, 
                               pest_calc_2014, pest_calc_2015)

# filter data by unique chemicals, sum total use per chemical, and subset top 20 chemicals by year and county
TotalAppliedPerChemical <- FresnoMadera_PestCalc %>%
  select(YEAR, COUNTY_NAME,CHEMICAL_NAME, chem_calc) %>%
  group_by(CHEMICAL_NAME, YEAR, COUNTY_NAME) %>%
  mutate(total_lbs_per_mile = if(COUNTY_NAME=="FRESNO") (cumsum(chem_calc)/9190.918987) else (cumsum(chem_calc)/7282.631774))

# subset top 10 chemicals used per county per year
Top10Chemicals <- TotalAppliedPerChemical %>%
  select(YEAR, COUNTY_NAME, CHEMICAL_NAME, total_lbs_per_mile) %>%
  group_by(YEAR, COUNTY_NAME, CHEMICAL_NAME) %>%
  arrange(CHEMICAL_NAME, desc(total_lbs_per_mile)) %>%
  top_n(10,total_lbs_per_mile)
  
# plot
ggplot(Top10Chemicals, aes(x=CHEMICAL_NAME, y = total_lbs_per_mile)) +
  geom_bar(aes(fill= COUNTY_NAME), position = "dodge", stat = "identity",na.rm = TRUE) +
  coord_flip() +
  facet_wrap(~ YEAR, nrow=1)

