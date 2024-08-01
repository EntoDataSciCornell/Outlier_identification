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
library(ggplot2)

climate_files <- list.files('rasters/', full.names = TRUE)

climate_stack <- rast(climate_files)

species_climate_files <- list.files('data/climate_outlier_files/', full.names = TRUE)
all_sp <- str_extract(species_climate_files, "[A-Z][a-z]*\\_[a-z]*")

sdms_done <- list.files('data/pseudosdm/')

sdms_done_sp <- str_extract(sdms_done, "[A-Z][a-z]*\\_[a-z]*")

species_climate_files <- species_climate_files[!all_sp %in% sdms_done_sp]


species_climate_files <- species_climate_files[1:length(species_climate_files)]


for(file_s in species_climate_files){
  
  tryCatch({
    
    
    
    ### some observations fall in the same climate box, get unique locations##
    
    occurrence_bio <- read.csv(file_s)
    
    species_name <- str_extract(file_s, "[A-Z][a-z]+\\_[a-z]+")
    
    #### make the pseudo absences and the random forest only on the points that are not outliers 
    
    n_unique_obs <- occurrence_bio %>% 
      filter(outliers == 0) %>% 
      select(starts_with("bio")) %>% 
      unique() %>% nrow()
    
    # unique_obs_outliers <- occurrence_bio %>% 
    #   select(outliers, starts_with("bio")) %>% nrow()
    #   unique() %>%
    #   mutate(presence = 1, climate_box = 1:n())
    
    unique_obs <- occurrence_bio %>% 
      select(starts_with("bio")) %>% 
      unique() %>%
      mutate(presence = 1, climate_box = 1:n())
    
    lat_long_climate <- unique_obs %>% 
      select(starts_with("bio"), climate_box) %>% 
      right_join(occurrence_bio) %>% 
      group_by(climate_box) %>% 
      slice(1) %>% 
      select(longitude, latitude) 
    
    ### get pseudo absences and extract climate variables together ##
    
    
    myBiomodData.r <- BIOMOD_FormatingData(resp.var = unique_obs$presence,
                                           expl.var = climate_stack,
                                           resp.xy = lat_long_climate[,c('longitude', 'latitude')],
                                           resp.name = species_name,
                                           PA.nb.rep = 4,
                                           PA.nb.absences = round(n_unique_obs/2),
                                           PA.strategy = 'random',
                                           filter.raster = TRUE)
    
    presence_psudoabs <- bind_cols(presence = myBiomodData.r@data.species, myBiomodData.r@data.env.var) %>% 
      mutate(presence = ifelse(is.na(presence), 0, presence))
    
    write.csv(presence_psudoabs, paste0('data/pseudoabsences/', species_name, "_pseudoabs.csv"))
    
    ## run random forest 
    pseudoRF <- randomForest(presence ~., data = presence_psudoabs, importance=TRUE,
                             proximity=TRUE)
    
    
    actual_data <-  occurrence_bio %>% 
      select(starts_with("bio")) 
    
    actual_data <- actual_data[,1:19]
    
    colnames(actual_data) <- colnames(presence_psudoabs[-1])
    
    predicted_presence <- predict(pseudoRF, actual_data, type="response")
    
    outlier_scores <- data.frame(outlier_score = predicted_presence -1)
    
    outlier_scores_true <- bind_cols(occurrence_bio, outlier_scores) %>% 
      select(outliers, outlier_score) 
    
    write.csv(outlier_scores_true, paste0('data/pseudosdm/', species_name, "_pseudosdm.csv"))
    
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
}


### compile results ###

pseudo_sdm_files <- list.files("data/pseudosdm", full.names = TRUE)

pseudo_sdm_files <- pseudo_sdm_files[-c(7, 54)]

pseudo_sdm_list <- list()

for(f in pseudo_sdm_files){
  
  species <- str_extract(f, "[A-Z][a-z]*\\_[a-z]*")
  
  pseudo_sdm_res <- read.csv(f)
  
  p10 <- round(nrow(pseudo_sdm_res)*0.1)
  p1 <- round(nrow(pseudo_sdm_res)*0.01)
  
  #assiging as outliers the 10% of records with the highest outlier scores
  
  predicting_outlier <- pseudo_sdm_res %>% 
    arrange(outlier_score) %>% 
    mutate(predicted_outlier_10 = c(rep(1, p10), rep(0, nrow(pseudo_sdm_res)-p10))) %>% 
    mutate(predicted_outlier_1 = c(rep(1, p1), rep(0, nrow(pseudo_sdm_res)-p1)))
  
  table_outliers_10 <- table(predicting_outlier$outliers, predicting_outlier$predicted_outlier_10)
  
  table_outliers_1 <- table(predicting_outlier$outliers, predicting_outlier$predicted_outlier_1)
  
  if(all(predicting_outlier$predicted_outlier_10 == 0)){
    table_outliers_10 <- cbind(table(predicting_outlier$outliers, predicting_outlier$predicted_outlier_10), c(0,0))
  }
  
  if(all(predicting_outlier$predicted_outlier_1 == 0)){
    table_outliers_1 <- cbind(table(predicting_outlier$outliers, predicting_outlier$predicted_outlier_1), c(0,0))
  }

  
  ## return all values 
  
  pseudo_sdm_list[[species]] <- data.frame(species = species, sensitivity_10 = table_outliers_10[2,2]/sum(table_outliers_10[2,2], table_outliers_10[2,1]),
             specificity_10 = table_outliers_10[1,1]/sum(table_outliers_10[1,1], table_outliers_10[1,2]),
             accuracy_10 = sum(table_outliers_10[2,2], table_outliers_10[1,1])/sum(table_outliers_10),
             sensitivity_1 = table_outliers_1[2,2]/sum(table_outliers_1[2,2], table_outliers_1[2,1]),
             specificity_1 = table_outliers_1[1,1]/sum(table_outliers_1[1,1], table_outliers_1[1,2]),
             accuracy_1 = sum(table_outliers_1[2,2], table_outliers_1[1,1])/sum(table_outliers_1), 
             total_number_outliers = sum(pseudo_sdm_res$outliers), total_number_points = length(pseudo_sdm_res$outliers) ) %>% 
    data.table()
}


pseudo_sdm_results <- pseudo_sdm_list %>%
  rbindlist() %>% 
  mutate(tss_10 = sensitivity_10 + specificity_10 - 1,
         tss_1 = sensitivity_1 + specificity_1 - 1)


write.csv(pseudo_sdm_results, file = "outputs/pseudo_sdm_results.csv", row.names = FALSE)
