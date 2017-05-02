#####
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
#####

#read in Water Quality CSV files of pesticide sampling
FresnoWQ_2005_2015 <- read.csv(file = "FresnoWQ_2005_2015.csv", header = T)
MaderaWQ_2005_2015 <- read.csv(file = "MaderaWQ_2005_2015.csv", header = T)

FresnoWQ_2005_2015 %>% select(OrganizationFormalName, ActivityIdentifier, ActivityMediaName,
                              ActivityStartDate, ActivityEndDate, )