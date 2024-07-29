library(dplyr)
library(sf)
library(stringr)
library(purrr)
library(units)

## load previous results to get species names ##

ecoregion_results <- readRDS('outputs/ecoregion_combined.rds')

species_evaluated <- sort(unique(ecoregion_results$finalname))

## load range maps ##

maps <- read_sf("range_shapefiles/Kaufman/") %>% 
  filter(!st_is_empty(geometry))

for(d in list.dirs("range_shapefiles")[2:6]){
  map_other_one <- read_sf(d)
  maps <- bind_rows(maps, map_other_one)
}

maps <- maps %>% 
  mutate(sciname = str_replace(sciname, " ", "_")) 

sf_use_s2(FALSE)

species_with_maps <- maps$sciname %>% unique()

species_evaluated[!species_evaluated %in% species_with_maps]

species_names <- data.frame(outlier_name = species_evaluated) %>% 
  mutate(name_in_map = ifelse(species_evaluated %in% species_with_maps, species_evaluated, NA)) %>% 
  mutate(name_in_map = case_when(outlier_name == 'Lethe_anthedon' ~ 'Enodia_anthedon',
                                 outlier_name == 'Lethe_eurydice' ~ 'Satyrodes_eurydice',
                                 outlier_name == 'Lethe_appalachia' ~ 'Satyrodes_appalachia',
                                 TRUE ~ name_in_map))

species_names

## Get the area for the species that have maps

area <- list()

species_from_map <- species_names %>% 
  filter(!is.na(name_in_map))

for(sp in as.character(species_from_map$name_in_map)){
  
  area[[sp]] <- maps %>% 
    filter(sciname == sp) %>% unique() %>% 
    st_area() %>% sum()
}

area_df_from_map <- area %>% 
  map_df(~as.data.frame(.x), .id = 'name_in_map') %>% 
  rename(area_m2 = .x) %>% 
  mutate(area_km = set_units(area_m2, km^2)) %>% 
  mutate(from_range_maps = TRUE) %>% 
  left_join(species_names)

### get area for species without range map 

species_from_convex <- species_names %>% 
  left_join(area_df_from_map) %>% 
  filter(is.na(area_km)|drop_units(area_km) == 0)

area_convex <- list()

for(sp in species_from_convex$outlier_name){
  
  sp_data <- read.csv(paste0("data/labeled_outlier_files/",
                             sp, "_fixed.csv"))
  
  lat_lon <- sp_data %>% 
    st_as_sf(
      coords = c("longitude", "latitude"),
      agr = "constant",
      #crs = 4326,  ##WGS84   
      crs = 4269,
      stringsAsFactors = FALSE,
      remove = FALSE) 
  
  # ggplot() + geom_sf(data = lat_lon)
  
  # convex_hull <- lat_lon %>% 
  #   st_union() %>% 
  #   st_convex_hull() 
  # 
  # ggplot() + geom_sf(data = convex_hull) + geom_sf(data = lat_lon)
  # 
  concave_hull <- lat_lon %>% 
    st_union() %>% 
    st_concave_hull(ratio = 0.3)
  
  # ggplot() + geom_sf(data = concave_hull) + geom_sf(data = lat_lon)
  
  area_convex[[sp]] <- st_area(concave_hull)
  
}



area_convex_df <- area_convex %>% 
  map_df(~as.data.frame(.x), .id = 'outlier_name') %>% 
  rename(area_m2 = .x) %>% 
  mutate(area_km = set_units(area_m2, km^2)) %>% 
  mutate(from_range_maps = FALSE)

area_all <- bind_rows(area_df_from_map, area_convex_df) %>% 
  select(outlier_name, name_in_map, area_km, from_range_maps) %>% 
  arrange(outlier_name) %>% 
  filter(drop_units(area_km) != 0)

saveRDS(area_all, "outputs/area.rds")

write.csv(area_all, "outputs/area.csv", row.names = FALSE)

