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
  assign(paste('nlcdFreq_',nlcd_caCounties$County.Name[i],sep=''), unlist(strsplit(nlcd_caCounties$histogram[i], c('=|, '))))
#  nlcd_histograms[i] <- unlist(strsplit(nlcd_caCounties$histogram[i], c('=|, ')))
  }

nlcd_v<- as.character(c(11, 12, 21, 22, 23, 24, 31, 41, 42, 43, 51, 52, 71, 72, 73, 74, 81, 82, 90, 95))
nlcd_k<- c('Open Water', 'Perennial Ice/Snow', 'Developed, Open Space', 'Developed, Low Intensity', 
  'Developed, Medium Intensity', 'Developed, High Intensity', 'Barren Land (Rock/Sand/Clay)',
  'Deciduous Forest', 'Evergreen Forest', 'Mixed Forest', 'Dwarf Scrub', 'Shrub/Scrub',
  'Grassland/Herbaceous', 'Sedge/Herbaceous', 'Lichens', 'Moss', 'Pasture/Hay', 'Cultivated Crops',
  'Woody Wetlands', 'Emergent Herbaceous Wetlands')
names(nlcd_v) <- nlcd_k

for (i in 1:dim(nlcd_caCounties)[[1]]){
  county<- paste('nlcdFreq_', nlcd_caCounties$County.Name[i], sep = '')
  data <- get(paste('nlcdFreq_', nlcd_caCounties$County.Name[i], sep = ''))
  label<- na.omit(match(data, nlcd_v))
  element<- c(which(data%in%nlcd_v))
  data[element]<- names(nlcd_v[label])
  assign(county, data)
}

nlcd_histograms <- rbind(nlcdFreq_Alameda,nlcdFreq_Alpine,nlcdFreq_Amador,nlcdFreq_Butte,
                         nlcdFreq_Calaveras,nlcdFreq_Colusa,`nlcdFreq_Contra Costa`,
                         `nlcdFreq_Del Norte`, `nlcdFreq_El Dorado`,nlcdFreq_Fresno, nlcdFreq_Glenn,
                         nlcdFreq_Humboldt,nlcdFreq_Imperial,nlcdFreq_Inyo,nlcdFreq_Kern,
                         nlcdFreq_Kings, nlcdFreq_Lake,nlcdFreq_Lassen,`nlcdFreq_Los Angeles`,
                         nlcdFreq_Madera,nlcdFreq_Marin,nlcdFreq_Mariposa,nlcdFreq_Mendocino,
                         nlcdFreq_Merced,nlcdFreq_Modoc,nlcdFreq_Mono,nlcdFreq_Monterey,nlcdFreq_Napa,
                         nlcdFreq_Nevada,nlcdFreq_Orange,nlcdFreq_Placer,nlcdFreq_Plumas,nlcdFreq_Riverside,
                         nlcdFreq_Sacramento,`nlcdFreq_San Benito`,`nlcdFreq_San Bernardino`,
                         `nlcdFreq_San Diego`,`nlcdFreq_San Francisco`,`nlcdFreq_San Joaquin`,`nlcdFreq_San Luis Obispo`,
                         `nlcdFreq_San Mateo`,`nlcdFreq_Santa Barbara`,`nlcdFreq_Santa Clara`,`nlcdFreq_Santa Cruz`,
                         nlcdFreq_Shasta,nlcdFreq_Sierra,nlcdFreq_Siskiyou,nlcdFreq_Solano,nlcdFreq_Sonoma,
                         nlcdFreq_Stanislaus,nlcdFreq_Sutter,nlcdFreq_Tehama,nlcdFreq_Trinity,nlcdFreq_Tulare,
                         nlcdFreq_Tuolumne,nlcdFreq_Ventura,nlcdFreq_Yolo, nlcdFreq_Yuba)
rownames(nlcd_histograms) <- sort(nlcd_caCounties$County.Name)

