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
library(dataRetrieval)
library(rgdal)
library(geojsonio)
library(leaflet)
#####

#read in Water Quality CSV files of pesticide sampling
FresnoWQ_2005_2015 <- read.csv(file = "FresnoWQ_2005_2015.csv", header = T)
MaderaWQ_2005_2015 <- read.csv(file = "MaderaWQ_2005_2015.csv", header = T)

FresnoWQData<- FresnoWQ_2005_2015 %>% 
  select(OrganizationFormalName, ActivityStartDate, MonitoringLocationIdentifier, 
         CharacteristicName, ResultMeasureValue, ResultMeasure.MeasureUnitCode, 
         ResultStatusIdentifier, ResultValueTypeName, USGSPCode, AnalysisStartDate, 
         ProviderName) %>%
  filter(!is.na(ResultMeasureValue)) %>%
  arrange(ActivityStartDate)

MaderaWQData<- MaderaWQ_2005_2015 %>% 
  select(OrganizationFormalName, ActivityStartDate, MonitoringLocationIdentifier, 
         CharacteristicName, ResultMeasureValue, ResultMeasure.MeasureUnitCode, 
         ResultStatusIdentifier, ResultValueTypeName, USGSPCode, AnalysisStartDate, 
         ProviderName) %>%
  filter(!is.na(ResultMeasureValue)) %>%
  arrange(ActivityStartDate)

FresnoSites <- as.character(unique(FresnoWQData$MonitoringLocationIdentifier))
MaderaSites <- as.character(unique(MaderaWQData$MonitoringLocationIdentifier))
FresnoCharacteristicName <- as.character(unique(FresnoWQData$CharacteristicName))
MaderaCharacteristicName <- as.character(unique(MaderaWQData$CharacteristicName))

# import data with site information from WQP using dataRetrieval tool
dataImport <- readWQPdata(siteNumbers= c(FresnoSites,MaderaSites), 
                          parameterCd= c(FresnoCharacteristicName,MaderaCharacteristicName),
                          startDate="2005-01-01", 
                          endDate="2015-12-31", 
                          tz="UTC",
                          querySummary = F)
siteInfo <- attr(dataImport, "siteInfo")
caDF<- FresnoWQData %>%
  bind_rows(MaderaWQData) %>%
  group_by(MonitoringLocationIdentifier) %>%
  left_join(siteInfo, by="MonitoringLocationIdentifier") %>%
  filter(ResultMeasure.MeasureUnitCode != "% recovery") %>%
  group_by(ResultMeasure.MeasureUnitCode) %>%
  mutate(ResultMeasureUgL = 
           if (ResultMeasure.MeasureUnitCode == "ng/l") 
              (ResultMeasureValue/1000) else ResultMeasureValue) %>%
  select(-contains("Drainage"))

# plot sampling data onto map using leaflet
col_types <- c("darkblue","dodgerblue","green4","gold1","orange","brown","red")
leg_vals <- unique(as.numeric(quantile(caDF$ResultMeasureUgL, 
                                       probs=c(0,0.01,0.1,0.25,0.5,0.75,0.9,.99,1), na.rm=TRUE)))
pal = colorBin(col_types, caDF$ResultMeasureUgL, bins = leg_vals)
rad <-3*seq(1,4,length.out = 16)
caDF$sizes <- rad[as.numeric(cut(caDF$ResultMeasureUgL, breaks=16))]

leaflet(data = caDF) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(~dec_lon_va, ~dec_lat_va,
                   fillColor = ~pal(ResultMeasureUgL),
                   radius = ~as.numeric(sizes),
                   fillOpacity = 0.8, opacity = 0.8, stroke=F,
                   popup = ~CharacteristicName) %>%
  addLegend(position = "bottomleft",
            pal = pal,
            values = ~ResultMeasureUgL,
            opacity = 0.8,
            labFormat = labelFormat(digits = 3),
            title = 'Sample Measures (ug/L)')


