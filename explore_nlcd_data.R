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
nlcd_caCounties_csv <- read.csv('NLCD_caCounties.csv',header = T, stringsAsFactors = F)
nlcd_caCounties <- nlcd_caCounties_csv %>%
  select(COUNTY.num,County.Name,histogram) 

nlcd_caCounties$histogram <-  substring(nlcd_caCounties$histogram, 2, nchar(nlcd_caCounties$histogram)-1)

for (i in 1:dim(nlcd_caCounties)[[1]]){
 # assign(paste('nlcdFreq_',nlcd_caCounties$County.Name[i],sep=''), unlist(strsplit(nlcd_caCounties$histogram[i], c('=|, '))))
  nlcd_histograms[i] <- unlist(strsplit(nlcd_caCounties$histogram[i], c('=|, ')))
  }

nlcd_v<- as.character(c(11, 12, 21, 22, 23, 24, 31, 41, 42, 43, 51, 52, 71, 72, 73, 74, 81, 82, 90, 95))
nlcd_k<- c('Open Water', 'Perennial Ice/Snow', 'Developed, Open Space', 'Developed, Low Intensity', 
  'Developed, Medium Intensity', 'Developed, High Intensity', 'Barren Land (Rock/Sand/Clay)',
  'Deciduous Forest', 'Evergreen Forest', 'Mixed Forest', 'Dwarf Scrub', 'Shrub/Scrub',
  'Grassland/Herbaceous', 'Sedge/Herbaceous', 'Lichens', 'Moss', 'Pasture/Hay', 'Cultivated Crops',
  'Woody Wetlands', 'Emergent Herbaceous Wetlands')
names(nlcd_v) <- nlcd_k

nlcd_histograms <- array(data = NA, dim = c(58, 32))
rownames(nlcd_histograms) <- nlcd_caCounties$County.Name
for (i in 1:58){
  nlcd_histograms[i,] <- unlist(strsplit(nlcd_caCounties$histogram[i], c('=|, ')))
}
