setwd(dir = 'C:/Users/Ckuan/Dropbox/orise_carmen/urban pesticides/data')
library(ggplot2)
library(dplyr)
library(reshape2)

for (i in 2010:2014){
  assign(paste('Fresno_', i, sep=''), read.table(file = paste(i, '_FresnoRightsofWay_PUR.txt', sep=''), sep = "\t", header = T))
}

chem_applied_2010 <- Fresno_2010 %>% 
  select(YEAR, APPLICATION_MONTH, PRODUCT_NAME, CHEMICAL_NAME, PRODUCT_CHEMICAL_PERCENT, AMOUNT_PRODUCT_APPLIED, UNITS_PRODUCT_APPLIED) %>%
  filter(!is.na(PRODUCT_CHEMICAL_PERCENT)) %>%
  mutate(ACTIVE_INGR_APPLIED = as.numeric(AMOUNT_PRODUCT_APPLIED) * (as.numeric(levels(PRODUCT_CHEMICAL_PERCENT))[PRODUCT_CHEMICAL_PERCENT])/100) %>%
  filter(!is.na(ACTIVE_INGR_APPLIED))
chem_applied_2011 <- Fresno_2011 %>% 
  select(YEAR, APPLICATION_MONTH, PRODUCT_NAME, CHEMICAL_NAME, PRODUCT_CHEMICAL_PERCENT, AMOUNT_PRODUCT_APPLIED, UNITS_PRODUCT_APPLIED) %>%
  filter(!is.na(PRODUCT_CHEMICAL_PERCENT)) %>%
  mutate(ACTIVE_INGR_APPLIED = as.numeric(AMOUNT_PRODUCT_APPLIED) * (as.numeric(levels(PRODUCT_CHEMICAL_PERCENT))[PRODUCT_CHEMICAL_PERCENT])/100) %>%
  filter(!is.na(ACTIVE_INGR_APPLIED)) 
chem_applied_2012 <- Fresno_2012 %>% 
  select(YEAR, APPLICATION_MONTH, PRODUCT_NAME, CHEMICAL_NAME, PRODUCT_CHEMICAL_PERCENT, AMOUNT_PRODUCT_APPLIED, UNITS_PRODUCT_APPLIED) %>%
  filter(!is.na(PRODUCT_CHEMICAL_PERCENT)) %>%
  mutate(ACTIVE_INGR_APPLIED = as.numeric(AMOUNT_PRODUCT_APPLIED) * (as.numeric(levels(PRODUCT_CHEMICAL_PERCENT))[PRODUCT_CHEMICAL_PERCENT])/100) %>%
  filter(!is.na(ACTIVE_INGR_APPLIED)) 
chem_applied_2013 <- Fresno_2013 %>% 
  select(YEAR, APPLICATION_MONTH, PRODUCT_NAME, CHEMICAL_NAME, PRODUCT_CHEMICAL_PERCENT, AMOUNT_PRODUCT_APPLIED, UNITS_PRODUCT_APPLIED) %>%
  filter(!is.na(PRODUCT_CHEMICAL_PERCENT)) %>%
  mutate(ACTIVE_INGR_APPLIED = as.numeric(AMOUNT_PRODUCT_APPLIED) * (as.numeric(levels(PRODUCT_CHEMICAL_PERCENT))[PRODUCT_CHEMICAL_PERCENT])/100) %>%
  filter(!is.na(ACTIVE_INGR_APPLIED)) 
chem_applied_2014 <- Fresno_2014 %>% 
  select(YEAR, APPLICATION_MONTH, PRODUCT_NAME, CHEMICAL_NAME, PRODUCT_CHEMICAL_PERCENT, AMOUNT_PRODUCT_APPLIED, UNITS_PRODUCT_APPLIED) %>%
  filter(!is.na(PRODUCT_CHEMICAL_PERCENT)) %>%
  mutate(ACTIVE_INGR_APPLIED = as.numeric(AMOUNT_PRODUCT_APPLIED) * (as.numeric(levels(PRODUCT_CHEMICAL_PERCENT))[PRODUCT_CHEMICAL_PERCENT])/100) %>%
  filter(!is.na(ACTIVE_INGR_APPLIED)) 

ggplot(chem_applied_2014, aes(x=CHEMICAL_NAME, y = ACTIVE_INGR_APPLIED)) +
  geom_bar(stat='identity') +
  coord_flip()
