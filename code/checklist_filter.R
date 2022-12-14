library(dplyr)
library(sf)
library(ggplot2)
library(data.table)
library(tidyr)
library(cowplot)
library(stringr)
library(purrr)

setwd("/Volumes/Jayme_SSD1/Outlier_identification")


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

#rename states/provinces so that they match the countries shapefile
eB_data$Province.State[eB_data$Province.State=="Alberta"] <- "CA-AB"
eB_data$Province.State[eB_data$Province.State=="British Columbia"] <- "CA-BC"
eB_data$Province.State[eB_data$Province.State=="Manitoba"] <- "CA-MB"
eB_data$Province.State[eB_data$Province.State=="New Brunswick"] <- "CA-NB"
eB_data$Province.State[eB_data$Province.State=="Newfoundland and Labrador"] <- "CA-NL"
eB_data$Province.State[eB_data$Province.State=="Nova Scotia"] <- "CA-NS"
eB_data$Province.State[eB_data$Province.State=="Northwest Territories"] <- "CA-NT"
eB_data$Province.State[eB_data$Province.State=="Nunavut"] <- "CA-NU"
eB_data$Province.State[eB_data$Province.State=="Ontario"] <- "CA-ON"
eB_data$Province.State[eB_data$Province.State=="Prince Edward Island"] <- "CA-PE"
eB_data$Province.State[eB_data$Province.State=="Quebec"] <- "CA-QC"
eB_data$Province.State[eB_data$Province.State=="Saskatchewan"] <- "CA-SK"
eB_data$Province.State[eB_data$Province.State=="Yukon"] <- "CA-YT"

eB_data$Province.State[eB_data$Province.State=="Alaska"] <- "US-AK"
eB_data$Province.State[eB_data$Province.State=="Alabama"] <- "US-AL"
eB_data$Province.State[eB_data$Province.State=="Arkansas"] <- "US-AR"
eB_data$Province.State[eB_data$Province.State=="Arizona"] <- "US-AZ"
eB_data$Province.State[eB_data$Province.State=="California"] <- "US-CA"
eB_data$Province.State[eB_data$Province.State=="Colorado"] <- "US-CO"
eB_data$Province.State[eB_data$Province.State=="Connecticut"] <- "US-CT"
eB_data$Province.State[eB_data$Province.State=="District of Columbia"] <- "US-DC"
eB_data$Province.State[eB_data$Province.State=="Delaware"] <- "US-DE"
eB_data$Province.State[eB_data$Province.State=="Florida"] <- "US-FL"
eB_data$Province.State[eB_data$Province.State=="Georgia"] <- "US-GA"
eB_data$Province.State[eB_data$Province.State=="Hawaii"] <- "US-HI"
eB_data$Province.State[eB_data$Province.State=="Iowa"] <- "US-IA"
eB_data$Province.State[eB_data$Province.State=="Idaho"] <- "US-ID"
eB_data$Province.State[eB_data$Province.State=="Illinois"] <- "US-IL"
eB_data$Province.State[eB_data$Province.State=="Indiana"] <- "US-IN"
eB_data$Province.State[eB_data$Province.State=="Kansas"] <- "US-KS"
eB_data$Province.State[eB_data$Province.State=="Kentucky"] <- "US-KY"
eB_data$Province.State[eB_data$Province.State=="Louisiana"] <- "US-LA"
eB_data$Province.State[eB_data$Province.State=="Massachusetts"] <- "US-MA"
eB_data$Province.State[eB_data$Province.State=="Maryland"] <- "US-MD"
eB_data$Province.State[eB_data$Province.State=="Maine"] <- "US-ME"
eB_data$Province.State[eB_data$Province.State=="Michigan"] <- "US-MI"
eB_data$Province.State[eB_data$Province.State=="Minnesota"] <- "US-MN"
eB_data$Province.State[eB_data$Province.State=="Missouri"] <- "US-MO"
eB_data$Province.State[eB_data$Province.State=="Mississippi"] <- "US-MS"
eB_data$Province.State[eB_data$Province.State=="Montana"] <- "US-MT"
eB_data$Province.State[eB_data$Province.State=="North Carolina"] <- "US-NC"
eB_data$Province.State[eB_data$Province.State=="North Dakota"] <- "US-ND"
eB_data$Province.State[eB_data$Province.State=="Nebraska"] <- "US-NE"
eB_data$Province.State[eB_data$Province.State=="New Hampshire"] <- "US-NH"
eB_data$Province.State[eB_data$Province.State=="New Jersey"] <- "US-NJ"
eB_data$Province.State[eB_data$Province.State=="New Mexico"] <- "US-NM"
eB_data$Province.State[eB_data$Province.State=="Nevada"] <- "US-NV"
eB_data$Province.State[eB_data$Province.State=="New York"] <- "US-NY"
eB_data$Province.State[eB_data$Province.State=="Ohio"] <- "US-OH"
eB_data$Province.State[eB_data$Province.State=="Oklahoma"] <- "US-OK"
eB_data$Province.State[eB_data$Province.State=="Oregon"] <- "US-OR"
eB_data$Province.State[eB_data$Province.State=="Pennsylvania"] <- "US-PA"
eB_data$Province.State[eB_data$Province.State=="Rhode Island"] <- "US-RI"
eB_data$Province.State[eB_data$Province.State=="South Carolina"] <- "US-SC"
eB_data$Province.State[eB_data$Province.State=="South Dakota"] <- "US-SD"
eB_data$Province.State[eB_data$Province.State=="Tennessee"] <- "US-TN"
eB_data$Province.State[eB_data$Province.State=="Texas"] <- "US-TX"
eB_data$Province.State[eB_data$Province.State=="Utah"] <- "US-UT"
eB_data$Province.State[eB_data$Province.State=="Virginia"] <- "US-VA"
eB_data$Province.State[eB_data$Province.State=="Vermont"] <- "US-VT"
eB_data$Province.State[eB_data$Province.State=="Washington"] <- "US-WA"
eB_data$Province.State[eB_data$Province.State=="Wisconsin"] <- "US-WI"
eB_data$Province.State[eB_data$Province.State=="West Virginia"] <- "US-WV"
eB_data$Province.State[eB_data$Province.State=="Wyoming"] <- "CA-WY"


calculate_accuracy_checklist <- function(species_checklist){
  
  ## calculate the percentage of right outliers
  col_use <- paste0("iso_3166_2")
  
  province_state_checklist <- eB_data %>%
    filter(newSpecies==species_name) %>%
    summarise(Province.State=unique(Province.State))
  
  species_checklist_outlier <- with_outliers_state_prov %>% 
    st_drop_geometry() %>% 
    dplyr::select(old_name:outliers, all_of(col_use)) %>% 
    rename(state_name = all_of(col_use)) %>% 
    mutate(checklist_outlier = ifelse(state_name %in% province_state_checklist$Province.State, 0, 1)) 
  
  ## percentage of true outliers identified as outliers 
  
  table_outliers_prop <- table(species_checklist_outlier$outlier, species_checklist_outlier$checklist_outlier)
  
  data.frame(sensitivity_prop = table_outliers_prop[2,2]/sum(table_outliers_prop[2,2], table_outliers_prop[2,1]),
             specificity_prop = table_outliers_prop[1,1]/sum(table_outliers_prop[1,1], table_outliers_prop[1,2]),
             accuracy_prop = sum(table_outliers_prop[2,2], table_outliers_prop[1,1])/sum(table_outliers_prop),
  total_number_outliers = sum(species_checklist_outlier$outlier), total_number_points = length(species_checklist_outlier$outlier) ) %>% 
    data.table()
  
}

#read in eB data
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


eB_data$Province.State[eB_data$Province.State=="British Columbia"] <- "CA-BC"
eB_data$Province.State[eB_data$Province.State=="Quebec"] <- "CA-QC"
eB_data$Province.State[eB_data$Province.State=="Manitoba"] <- "CA-MB"
eB_data$Province.State[eB_data$Province.State=="Alberta"] <- "CA-AB"
eB_data$Province.State[eB_data$Province.State=="Yukon"] <- "CA-YT"
eB_data$Province.State[eB_data$Province.State=="New Mexico"] <- "US-NM"
eB_data$Province.State[eB_data$Province.State=="Colorado"] <- "US-CO"
eB_data$Province.State[eB_data$Province.State=="Saskatchewan"] <- "CA-SK"
eB_data$Province.State[eB_data$Province.State=="Northwest Territories"] <- "CA-NT"
eB_data$Province.State[eB_data$Province.State=="Washington"] <- "US-WA"
eB_data$Province.State[eB_data$Province.State=="Montana"] <- "US-MT"
eB_data$Province.State[eB_data$Province.State=="Nunavut"] <- "CA-NU"
eB_data$Province.State[eB_data$Province.State=="Utah"] <- "US-UT"
eB_data$Province.State[eB_data$Province.State=="California"] <- "US-CA"
eB_data$Province.State[eB_data$Province.State=="Wyoming"] <- "CA-WY"
eB_data$Province.State[eB_data$Province.State=="Newfoundland and Labrador"] <- "CA-NL"

#read in state/provinces file
countries <- read_sf(dsn='/Volumes/Jayme_SSD1/Butterfly_Postdoc/R/sites/shapefiles/ne_50m_admin_1_states_provinces/ne_50m_admin_1_states_provinces.shp') 

## read the species files ##

species_files <- list.files('data/outliers_labelled/', full.names = TRUE)

species_outputs <- list()

## loop over all species files

for(file_s in species_files){
  
  # extract species names from the file
  
  species_name <- str_extract(file_s, "[A-Z][a-z]+\\_[a-z]+")
  
  # filter any latitude and logitude and read the file
  
  species_data <- read.csv(file_s) %>% 
    filter(!is.na(longitude) & !is.na(latitude))
  
  # transform coordinates into sf object and change crs to match ecoregion crs
  
  species_lat <- st_as_sf(species_data, coords = c("longitude", "latitude"), 
                          crs = 4326)
  
  ## intersect the points with the ecoregions ##
  
  sf_use_s2(FALSE)
  with_outliers_state_prov <- st_join(species_lat, countries, join = st_within)
  

  ## run the accuracy function
  checklist_outlier_test<-calculate_accuracy_checklist(species_checklist = with_outliers_state_prov)
  
  checklist_outlier_test_long <- gather(checklist_outlier_test,factor_key=TRUE)

  ## output in list 
  species_outputs[[species_name]] <- rbind(checklist_outlier_test)
  
}


#for one species
#species_name="Erebia_mancinus"
#species_data<-read.csv("/Users/jaymelewthwaite/Downloads/Agriades_glandon_df_filt_crop_merge_outlierslabeled.csv") %>% filter(!is.na(longitude) & !is.na(latitude))






## put all of the list files into a single dataframe

species_outputs_all <- species_outputs %>% 
  map_df(~as.data.frame(.x), .id = 'finalname')

## split the data into the proportion and cumulative proportion results

species_output_prop <- species_outputs_all %>% 
  select(finalname,total_number_outliers, total_number_points,  sensitivity_prop:accuracy_prop) %>% 
  pivot_longer(names_to = 'test', values_to = "values", sensitivity_prop:accuracy_prop)
  #%>% mutate(method = 'ecoregion_prop') %>% 
  #rename(proportion = proportion_data)


## plot the results for each species across proportions


## calculate tss

species_outputs_combined <- species_output_prop %>% 
  mutate(test = str_remove(test, "\\_[a-z]+")) %>% 
  pivot_wider(names_from = test, values_from = values) %>%
  mutate(tss = sensitivity + specificity - 1)

## maximize tss

species_outputs_combined %>% 
  group_by(finalname) %>% 
  filter(tss  == max(tss)) %>% 
  mutate(ratio_outliers = total_number_outliers/total_number_points)