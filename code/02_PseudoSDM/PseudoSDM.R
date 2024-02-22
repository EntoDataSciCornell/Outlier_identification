## Liu et al 2018

#RF and SVM used in methods 9 and 10 are commonly used in SDM where the response is a binary variable. 
#If true absences are not available, pseudo-absences (typically random points) are used. 
#In applying methods 9 and 10, we used the Nrd occurrence records 
#(including the outliers and database records representing sampling bias) 
#and an additional 2 × Nrd random points as pseudo-absences, 
#as suggested by some authors (Barbet-Massin et al., 2012; Liu, White, Newell & Griffioen, 2013), 
#to build models using RF and SVM. Fitted models were then applied to the same set of training occurrence records 
# (including outliers) to obtain predicted values. 
#Finally, subtracting these predictions from 1 gave the deviation or “outlierness” or “outlier scores” of the records.
#We anticipated that the degree of “outlierness” would discriminate outliers from most true presences.


library(dplyr)
library(stringr)
library(ntbox)

## read the species files ##

species_files <- list.files('data/labeled_outlier_files/', full.names = TRUE)

#species_outputs <- list()

## loop over all species files

#for(file_s in species_files[53:length(species_files)]){
file_s <- species_files[1]
  
  # extract species names from the file
  
  species_name <- str_extract(file_s, "[A-Z][a-z]+\\_[a-z]+")
  
  # filter any latitude and logitude and read the file
  
  species_data <- read.csv(file_s) %>% 
    filter(!is.na(longitude) & !is.na(latitude))

