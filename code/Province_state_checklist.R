
#import ebutterfly data

eB_data<-read.csv("/Volumes/Jayme_SSD1/Distribution_Data/Raw_Data/eButterfly/eButterfly_all_records_2021_05_07_02_03_29_engagement_project.csv")
eB_data$newSpecies<-paste(eB_data$Genus,eB_data$Species,sep="_")
length(unique(eB_data$newSpecies))
#1147
table(eB_data$Province.State)
#rename some provinces as any accented letters were not imported properly
eB_data$Province.State[eB_data$Province.State=="Ciego de \xc1vila"] <- "Ciego de Avila"
eB_data$Province.State[eB_data$Province.State=="Col\xf3n"] <- "Colon"
eB_data$Province.State[eB_data$Province.State=="Dari\xe9n"] <- "Darien"
eB_data$Province.State[eB_data$Province.State=="Guant\xe1namo"] <- "Guantanamo"
eB_data$Province.State[eB_data$Province.State=="M\xe9xico"] <- "Mexico"
eB_data$Province.State[eB_data$Province.State=="Nuevo Le\xf3n"] <- "Nuevo Leon"
eB_data$Province.State[eB_data$Province.State=="Panam\xe1"] <- "Panama"
eB_data$Province.State[eB_data$Province.State=="San Jos\xe9"] <- "San Jose"
eB_data$Province.State[eB_data$Province.State=="Soufri\xe8re"] <- "Soufriere"
eB_data$Province.State[eB_data$Province.State=="Yucat\xe1n"] <- "Yucatan"

library(dplyr)
province_state_checklist <- eB_data %>%
  group_by(newSpecies) %>%
  summarise(Province.State=unique(Province.State))


#create a separate dataframe for each species and combine into a list
list_species_provinces  <- lapply( 
  split(province_state_checklist, province_state_checklist$newSpecies), 
  function(x){
    data.frame(Province.State   = x$Province.State)
  })

list_species_provinces$Abaeis_nicippe
