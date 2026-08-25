library(dplyr)
library(stringr)
library(geodata)
library(terra)
library(sf)
library(biomod2)
library(randomForest)
library(rgeos)
library(stars)
library(parallel)
library(lubridate)

##### get climate stack for all variables #####

climate_files <- list.files('rasters/', full.names = TRUE)

climate_stack <- rast(climate_files)

########## get climate data for each point for each species ######

## read the species files ##

species_files <- list.files('data/labeled_outlier_files/', full.names = TRUE)

#species_outputs <- list()

## loop over all species files

extract_climate <- function(file_s){ 
  #file_s <- species_files[1]
  # extract species names from the file
  
  species_name <- str_extract(file_s, "[A-Z][a-z]+\\_[a-z]+")
  
  # filter any latitude and logitude and read the file
  
  species_data <- read.csv(file_s) %>% 
    filter(!is.na(longitude) & !is.na(latitude))
  
  species_data_d <- species_data %>% 
    mutate(date = ymd(paste0(year,"-", month, "-", day)))  %>% 
    mutate(Julian_date = yday(date))
  
  lat_lon <- species_data_d %>% 
    st_as_sf(
      coords = c("longitude", "latitude"),
      agr = "constant",
      crs = 4326,  ##WGS84   
      #crs = 4269,
      stringsAsFactors = FALSE,
      remove = FALSE) 
  
  
  ## extract the climate variables manually for each point
  
  n_bios <- 19
  
  biovals <- matrix(NA, nrow = nrow(lat_lon), ncol = n_bios)
  # 
  for(i in 1:n_bios){
    #extract values for site
    r <- rast(climate_files[i])
    x1 <- extract(r, lat_lon)
    biovals[,i] <- x1[,2]
  }
  
  fil_name <- str_extract(climate_files[1:n_bios], 'bio_\\d+')
  
  biovals_df <- data.frame(biovals)
  
  colnames(biovals_df) <-   fil_name
  
  occurrence_bio <- bind_cols(species_data_d, biovals_df)
  
  write.csv(occurrence_bio, paste0('data/climate_outlier_files/', species_name, "_climate.csv"))  
}

mclapply(species_files, extract_climate, mc.cores = 2)




