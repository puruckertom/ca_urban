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
  assign(paste('RoW_', i, sep=''), read.table(file = paste(i, '_RightsofWay.txt', sep=''), sep = "\t", header = T, fill = T, strip.white = T, na.strings = "N/A"))
}

df <- rbind(RoW_2006,RoW_2007,RoW_2008,RoW_2009,RoW_2010,RoW_2011,RoW_2012,RoW_2013,RoW_2014,RoW_2015)

df_MissingChemNames<- df %>%
  filter(is.na(CHEMICAL_NAME)) %>%
  distinct(REGISTRATION_NUMBER, PRODUCT_NAME)

Prod_Chem <- df %>%
  filter(!is.na(CHEMICAL_NAME)) %>%
  distinct(REGISTRATION_NUMBER, PRODUCT_NAME, CHEMICAL_NAME)

chemfill <- df_MissingChemNames %>%
  left_join(Prod_Chem, by = "REGISTRATION_NUMBER") %>%
  distinct(REGISTRATION_NUMBER, CHEMICAL_NAME) %>%
  filter(!is.na(CHEMICAL_NAME))

df_filled <- df %>% 
  left_join(chemfill, by = "REGISTRATION_NUMBER")
