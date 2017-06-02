#####SET-UP ####
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
library(abind)
##### READ IN DATA #####
#read in CA PUR text files of Rights-of-Way pesticide applications
for (i in 2006:2015){
  assign(paste('RoW_', i, sep=''), read.table(file = paste(i, '_RightsofWay.txt', sep=''), sep = "\t", header = T, fill = T, strip.white = T, na.strings = "N/A", stringsAsFactors = T))
}
# CA public roads by county
for (i in 2005:2014){
  assign(paste('caRoads_', i, sep=''), read.csv(file = paste(i, '_caPublicRoads.csv', sep=''),strip.white = T,fill = T,stringsAsFactors = T))
}
# CA population 2004-2014
caPop <- read.csv(file = "California_Population_1990_2014.csv", strip.white = T, fill = T)
# CA Weather data
caMet <- read.csv(file = "caCounties_metStations.csv", strip.white = T, fill = T)
wDF <- array(data = NA, dim = c(19723,8,length(caMet$stationID)))
for(i in 1:length(caMet$stationID)){
  ifelse (
    caMet$stationID[i] <= 20000
    , temp <- read.table(file = paste('J:/new_weather_files_20000/', caMet$stationID[i], '_grid.wea', sep=''), sep = ",", header = F, fill = T, strip.white = T, na.strings = "N/A", stringsAsFactors = T)
    , temp <- read.table(file = paste('J:/new_weather_files_30000/', caMet$stationID[i], '_grid.wea', sep=''), sep = ",", header = F, fill = T, strip.white = T, na.strings = "N/A", stringsAsFactors = T)
  )
  wDF[,,i] <- abind(temp[,], along=3)
  print(i)
}
save(wDF, file = 'caMetWeather.RData')
rm(wDF)
load("caMetWeather.RData")

##### COMPILE DATA ####
df <- rbind(RoW_2006,RoW_2007,RoW_2008,RoW_2009,RoW_2010,RoW_2011,RoW_2012,RoW_2013,RoW_2014,RoW_2015)
rdf <- rbind(caRoads_2005,caRoads_2006,caRoads_2007,caRoads_2008,caRoads_2009,caRoads_2010,caRoads_2011,caRoads_2012,caRoads_2013,caRoads_2014)

df_MissingChemNames<- df %>%
  filter(is.na(CHEMICAL_NAME)) %>%
  distinct(REGISTRATION_NUMBER, PRODUCT_NAME)

Prod_Chem <- df %>%
  filter(!is.na(CHEMICAL_NAME)) %>%
  distinct(REGISTRATION_NUMBER, PRODUCT_NAME, CHEMICAL_NAME, PRODUCT_CHEMICAL_PERCENT)

chemfill <- df_MissingChemNames %>%
  left_join(Prod_Chem, by = c("REGISTRATION_NUMBER", "PRODUCT_NAME")) %>%
  distinct(REGISTRATION_NUMBER, PRODUCT_NAME, CHEMICAL_NAME, PRODUCT_CHEMICAL_PERCENT) %>%
  filter(!is.na(CHEMICAL_NAME))

df_match <- df %>% 
  left_join(chemfill, by = c("REGISTRATION_NUMBER", "PRODUCT_NAME", "CHEMICAL_NAME", "PRODUCT_CHEMICAL_PERCENT"))


#calculate pesticide usage
df_calc<- df_match %>%
  #select relevant columns
  select(YEAR, APPLICATION_MONTH, COUNTY_NAME, PRODUCT_NAME, CHEMICAL_NAME, PRODUCT_CHEMICAL_PERCENT, AG_NONAG, POUNDS_PRODUCT_APPLIED,POUNDS_CHEMICAL_APPLIED) %>%
  #calculate applied amounts of pesticide (lbs) per entry
  mutate(chem_calc = as.numeric(POUNDS_PRODUCT_APPLIED) * (as.numeric(PRODUCT_CHEMICAL_PERCENT)/100))

df_na <- df_calc %>%
  #filter NAs
  filter(is.na(chem_calc))

df_filled <- df_calc %>%
  filter(!is.na(chem_calc))

rdf_total <- rdf %>%
  select(YEAR, 'COUNTY_NAME'=COUNTY, 'COUNTY_TOTAL'=TOTAL)
rdf_total$YEAR <- as.factor(rdf$YEAR)

##### ANALYZE DATA #####
# filter data by unique chemicals, sum total use per chemical, and subset top 20 chemicals by year and county
TotalAppliedPerChemical <- df_filled %>%
  select(YEAR, COUNTY_NAME, PRODUCT_NAME, CHEMICAL_NAME, chem_calc) %>%
  group_by(YEAR, COUNTY_NAME, CHEMICAL_NAME) %>%
  summarise(lbs_per_year = sum(chem_calc))

chemByCounty <- TotalAppliedPerChemical %>%
  full_join(rdf_total, by = c("YEAR","COUNTY_NAME")) %>%
  group_by(YEAR, COUNTY_NAME, CHEMICAL_NAME, COUNTY_TOTAL) %>%
  mutate(lbs_per_mile = lbs_per_year/(as.double(factor(levels(COUNTY_TOTAL))[COUNTY_TOTAL])))

# subset top 10 chemicals used per county per year
Top5Chemicals <- chemByCounty %>%
  filter(!"GLYPHOSATE, ISOPROPYLAMINE SALT" %in% CHEMICAL_NAME) %>%
  group_by(YEAR, COUNTY_NAME) %>%
  top_n(5, lbs_per_mile)

# bc_TotalAppliedPerChemical <- df_filled %>%
#   select(YEAR, COUNTY_NAME, PRODUCT_NAME, CHEMICAL_NAME, chem_calc) %>%
#   group_by(YEAR, CHEMICAL_NAME, COUNTY_NAME) %>%
#   summarise(lbs_per_year = sum(chem_calc)) %>%
#   group_by(YEAR, lbs_per_year, COUNTY_NAME) %>%
#   mutate(total_lbs_per_mile = if(YEAR=="2006") (sum(lbs_per_year)/170296.67)
#          else if(YEAR=="2007") (sum(lbs_per_year)/171154.13)
#          else if(YEAR=="2008") (sum(lbs_per_year)/172511.33)
#          else if(YEAR=="2009") (sum(lbs_per_year)/171873.92)
#          else if(YEAR=="2010") (sum(lbs_per_year)/172138.66)
#          else if(YEAR=="2011") (sum(lbs_per_year)/172201.63)
#          else if(YEAR=="2012") (sum(lbs_per_year)/175543.79)
#          else if(YEAR=="2013") (sum(lbs_per_year)/174991.13)
#          else if(YEAR=="2014") (sum(lbs_per_year)/174802.85)
#          else if(YEAR=="2015") (sum(lbs_per_year)/174802.85))



####### plotting #####
cbPalette_top5 <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", 
                    "#CC79A7","#CC6666", "#9999CC", "#66CC99", "purple")
cbPalette_top10 <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", 
                     "#CC79A7","#CC6666", "#9999CC", "#66CC99", "purple", "orange", "green",
                     "dark yellow", "dark blue", "violet", "dark green", "dark gray", "light orange",
                     "light purple", "black", "dark orange", "light red", "dark red", "red", "brown",
                     "light brown", "light gray", "indigo")

ggplot(Top5Chemicals, aes(x=YEAR, y = lbs_per_mile, fill=CHEMICAL_NAME)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  #coord_flip() +
  #geom_line(aes(group=factor(CHEMICAL_NAME)), size=1) +
  scale_y_log10() +
  #scale_color_discrete() +
  scale_color_manual(values = cbPalette_top10) +
  facet_wrap(~COUNTY_NAME)+
  ylab("Pounds of A.I. (per mile)") +
  theme(legend.position = "none")


