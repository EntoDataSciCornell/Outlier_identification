library(dplyr)
library(sf)
library(ggplot2)
library(data.table)
library(tidyr)
library(cowplot)
library(stringr)
library(purrr)

## function to calculate accuracy 

calculate_accuracy_biome <- function(species_ecoregion, level, proportion_data, proportion_cumulative){
  
  ## calculate the percentage of observations in each ecoregion type
  
  col_use <- paste0("NA_L",level,"NAME")
  
  proportion_ecoregions <- table(species_ecoregion[[col_use]])/nrow(species_ecoregion)
  
  ecoregions_more_prop <- proportion_ecoregions[proportion_ecoregions > proportion_data]
  
  ## ecoregions that contain 90% of data
  
  sorted_ecoregions <- sort(proportion_ecoregions, decreasing = TRUE)
  
  ecoregions_90_prop <- sorted_ecoregions[cumsum(sorted_ecoregions) < proportion_cumulative]
  
  ## calculate the percentage of right outliers
  
  species_ecoregion_outlier <- species_ecoregion %>% 
    st_drop_geometry() %>% 
    select(final_name:outliers, all_of(col_use)) %>% 
    rename(ecoregion_name = all_of(col_use)) %>% 
    mutate(ecoregion_outlier = ifelse(ecoregion_name %in% names(ecoregions_more_prop), 0, 1)) %>% 
    mutate(ecoregion_outlier_cum = ifelse(ecoregion_name %in% names(ecoregions_90_prop), 0, 1))
  
  ## percentage of true outliers identified as outliers 
  
  table_outliers_prop <- table(species_ecoregion_outlier$outliers, species_ecoregion_outlier$ecoregion_outlier)
  
  table_outliers_cum <- table(species_ecoregion_outlier$outliers, species_ecoregion_outlier$ecoregion_outlier)
  
  ## return all values 
  
  data.frame(level = level, proportion_data = proportion_data, proportion_cumulative = proportion_cumulative,
             sensitivity_prop = table_outliers_prop[2,2]/sum(table_outliers_prop[2,2], table_outliers_prop[2,1]),
             specificity_prop = table_outliers_prop[1,1]/sum(table_outliers_prop[1,1], table_outliers_prop[1,2]),
             accuracy_prop = sum(table_outliers_prop[2,2], table_outliers_prop[1,1])/sum(table_outliers_prop),
             sensitivity_cum = table_outliers_cum[2,2]/sum(table_outliers_cum[2,2], table_outliers_cum[2,1]),
             specificity_cum = table_outliers_cum[1,1]/sum(table_outliers_cum[1,1], table_outliers_cum[1,2]),
             accuracy_cum = sum(table_outliers_cum[2,2], table_outliers_cum[1,1])/sum(table_outliers_cum), 
             total_number_outliers = sum(species_ecoregion_outlier$outlier), total_number_points = length(species_ecoregion_outlier$outlier) ) %>% 
    data.table()
  
}


## read ecoregion file ##
# define level ecoregion 

#ecoregion files can be obtained from https://www.epa.gov/eco-research/ecoregions-north-america; they are too large to upload to GitHub. 
ecoregion <- read_sf('biome_raw_files/NA_CEC_Eco_Level3/NA_CEC_Eco_Level3.shp')

ecoregion_crs <- ecoregion %>% st_crs()

## read the species files ##

species_files <- list.files('data/labeled_outlier_files/', full.names = TRUE)

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
                          crs = 4326) %>% 
    st_transform(ecoregion_crs)
  
  ## intersect the points with the ecoregions ##
  
  species_ecoregion <- st_join(species_lat, ecoregion, join = st_within)
  
  ## parameters that we want to test for ecoregion level and proportions used
  
  level_test <- cbind(expand.grid(level = c(1:3), proportion_data = c(0.001, 0.01, 0.1)), expand.grid(level = c(1:3), proportion_cumulative = c(0.9, 0.95, 0.99)))
  
  ## run the accuracy function for all combinations of the level-test
  
  biome_outlier_test <- lapply(1:nrow(level_test), FUN = function(x) calculate_accuracy_biome(species_ecoregion = species_ecoregion, 
                                                                                              level = level_test$level[x], 
                                                                                              proportion_data = level_test$proportion_data[x], 
                                                                                              proportion_cumulative = level_test$proportion_cumulative[x]))
  
  
  ## output in list 
  species_outputs[[species_name]] <- rbindlist(biome_outlier_test)
  
}


## put all of the list files into a single dataframe

species_outputs_all <- species_outputs %>% 
  map_df(~as.data.frame(.x), .id = 'finalname')

## split the data into the proportion and cumulative proportion results

species_output_prop <- species_outputs_all %>% 
  select(level, proportion_data, finalname,total_number_outliers, total_number_points,  sensitivity_prop:accuracy_prop) %>% 
  pivot_longer(names_to = 'test', values_to = "values", sensitivity_prop:accuracy_prop) %>% 
  mutate(method = 'ecoregion_prop') %>% 
  rename(proportion = proportion_data)


species_output_cum <- species_outputs_all %>% 
  select(level, proportion_cumulative,finalname, total_number_outliers, total_number_points, sensitivity_cum:accuracy_cum) %>% 
  pivot_longer(names_to = 'test', values_to = "values", sensitivity_cum:accuracy_cum) %>% 
  mutate(method = 'ecoregion_cum') %>% 
  rename(proportion = proportion_cumulative)


## plot the results for each species across proportions


species_output_prop %>% 
  ggplot(aes(x = proportion, y = values, colour = factor(level), shape = finalname)) +
  facet_wrap(~test, scales = 'free_y') +
  geom_point() +
  scale_x_log10() +
  theme_cowplot()


species_output_cum %>% 
  ggplot(aes(x = proportion, y = values, colour = factor(level), shape = finalname)) +
  facet_wrap(~test, scales = 'free_y') +
  geom_point() +
  scale_x_log10() +
  theme_cowplot()


## join the proportion and cumulative proportion data
## calculate tss

species_outputs_combined <- bind_rows(species_output_prop, species_output_cum) %>% 
  mutate(test = str_remove(test, "\\_[a-z]+")) %>% 
  pivot_wider(names_from = test, values_from = values) %>%
  mutate(tss = sensitivity + specificity - 1)

## maximize tss

species_outputs_combined %>% 
  group_by(finalname) %>% 
  filter(tss  == max(tss)) %>% 
  mutate(ratio_outliers = total_number_outliers/total_number_points)

write.csv(species_outputs_combined, "ecoregion_filter_results.csv")
