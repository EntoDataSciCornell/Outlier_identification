library(dplyr)
library(tidyr)
library(ggplot2)

########### classifiers!! ######### 

# load data
results_classifiers <- list.files('Results/', full.names = TRUE)

## accuracy 
res1 <- read.csv(results_classifiers[1])

res1 %>% 
  ggplot(aes(x = num_points)) + geom_histogram() +
  scale_x_log10()

res1 %>% filter(num_points < 1000) %>% nrow()


res1 %>% 
  mutate(abundant = ifelse(num_points > 1000, 'abundant', 'rare')) %>% 
  group_by(abundant) %>% 
  summarise(mean_naive = mean(naive_bayes), sd_naive = sd(naive_bayes), 
            mean_random_forest = mean(random_forests), sd_random_forest = sd(random_forests),
            mean_kNN = mean(kNN), sd_kNN = sd(kNN))


## sensitivity 
res2 <- read.csv(results_classifiers[2])

res2 %>% 
  mutate(abundant = ifelse(num_points > 1000, 'abundant', 'rare')) %>% 
  group_by(abundant) %>% 
  summarise(mean_naive = mean(naive_bayes), sd_naive = sd(naive_bayes), 
            mean_random_forest = mean(random_forests), sd_random_forest = sd(random_forests),
            mean_kNN = mean(kNN), sd_kNN = sd(kNN))


## specificity  
res3 <- read.csv(results_classifiers[3])

res3 %>% 
  mutate(abundant = ifelse(num_points > 1000, 'abundant', 'rare')) %>% 
  group_by(abundant) %>% 
  summarise(mean_naive = mean(naive_bayes), sd_naive = sd(naive_bayes), 
            mean_random_forest = mean(random_forests), sd_random_forest = sd(random_forests),
            mean_kNN = mean(kNN), sd_kNN = sd(kNN))

## tss

full_join(res2, res3, by = c('species')) %>% 
  mutate(tss_naive = naive_bayes.x + naive_bayes.y -1, 
         tss_random_forest = random_forests.x + random_forests.y -1, 
         tss_kNN = kNN.x + kNN.y -1)  %>% 
  mutate(abundant = ifelse(num_points.x > 1000, 'abundant', 'rare')) %>% 
  group_by(abundant) %>% 
  summarise(mean_naive = mean(tss_naive), sd_naive = sd(tss_naive), 
            mean_random_forest = mean(tss_random_forest), sd_random_forest = sd(tss_random_forest),
            mean_kNN = mean(tss_kNN), sd_kNN = sd(tss_kNN))


##### Classifiers with ecoregion ######

# load data
results_classifiers <- list.files('Results/', full.names = TRUE)

## accuracy 
res1 <- read.csv(results_classifiers[4])

res1 %>% 
  ggplot(aes(x = num_points)) + geom_histogram() +
  scale_x_log10()

res1 %>% filter(num_points < 1000) %>% nrow()

res1 %>% 
  filter(!species %in% c("Ancyloxypha numitor", "Coenonympha nipisiquit")) %>% 
  mutate(abundant = ifelse(num_points > 1000, 'abundant', 'rare')) %>% 
  group_by(abundant) %>% 
  summarise(mean_naive = mean(naive_bayes), sd_naive = sd(naive_bayes), 
            mean_random_forest = mean(random_forests), sd_random_forest = sd(random_forests),
            mean_kNN = mean(kNN), sd_kNN = sd(kNN))


## sensitivity 
res2 <- read.csv(results_classifiers[5])

res2 %>% 
  filter(!species %in% c("Ancyloxypha numitor", "Coenonympha nipisiquit")) %>% 
  mutate(abundant = ifelse(num_points > 1000, 'abundant', 'rare')) %>% 
  group_by(abundant) %>% 
  summarise(mean_naive = mean(naive_bayes), sd_naive = sd(naive_bayes), 
            mean_random_forest = mean(random_forests), sd_random_forest = sd(random_forests),
            mean_kNN = mean(kNN), sd_kNN = sd(kNN))


## specificity  
res3 <- read.csv(results_classifiers[6])

res3 %>% 
  filter(!species %in% c("Ancyloxypha numitor", "Coenonympha nipisiquit")) %>% 
  mutate(abundant = ifelse(num_points > 1000, 'abundant', 'rare')) %>% 
  group_by(abundant) %>% 
  summarise(mean_naive = mean(naive_bayes), sd_naive = sd(naive_bayes), 
            mean_random_forest = mean(random_forests), sd_random_forest = sd(random_forests),
            mean_kNN = mean(kNN), sd_kNN = sd(kNN))

## tss

full_join(res2, res3, by = c('species')) %>% 
  filter(!species %in% c("Ancyloxypha numitor", "Coenonympha nipisiquit")) %>% 
  mutate(tss_naive = naive_bayes.x + naive_bayes.y -1, 
         tss_random_forest = random_forests.x + random_forests.y -1, 
         tss_kNN = kNN.x + kNN.y -1)  %>% 
  mutate(abundant = ifelse(num_points.x > 1000, 'abundant', 'rare')) %>% 
  group_by(abundant) %>% 
  summarise(mean_naive = mean(tss_naive), sd_naive = sd(tss_naive), 
            mean_random_forest = mean(tss_random_forest), sd_random_forest = sd(tss_random_forest),
            mean_kNN = mean(tss_kNN), sd_kNN = sd(tss_kNN))


###### Neural networks #######

NN <- read.csv(results_classifiers[4])

NN %>% View()

########### ecoregion filter ########

ecoregion_results <- readRDS('outputs/ecoregion_combined.rds')

ecoregion_results %>% 
  filter(!finalname %in% c("Ancyloxypha_numitor", "Coenonympha_nipisiquit")) %>% 
  mutate(abundant = ifelse(total_number_points > 1000, 'abundant', 'rare')) %>% 
  group_by(abundant, level, proportion) %>% 
  summarise(mean_accuracy = round(mean(accuracy),3), sd_accuracy = round(sd(accuracy),3), 
            mean_sensitivity = round(mean(sensitivity),3), sd_sensitivity = round(sd(sensitivity),3),
            mean_specificity = round(mean(specificity),3), sd_specificity = round(sd(specificity),3),
            mean_tss = round(mean(tss),3), sd_tss = round(sd(tss),3)) %>% 
  mutate(accuracy = paste0(mean_accuracy, " (", sd_accuracy, ")")) %>%
  mutate(sensitivity = paste0(mean_sensitivity, " (", sd_sensitivity, ")")) %>%
  mutate(specificity = paste0(mean_specificity, " (", sd_specificity, ")")) %>%
  mutate(tss = paste0(mean_tss, " (", sd_tss, ")")) %>% 
  select(abundant, level, proportion, accuracy, sensitivity, specificity, tss) %>% View()

#### pseudo sdm #####

pseudo_sdm_results <- read.csv('outputs/pseudo_sdm_results.csv')

pseudo_sdm_results %>% 
  filter(!species %in% c("Ancyloxypha_numitor", "Coenonympha_nipisiquit")) %>% 
  mutate(abundant = ifelse(total_number_points > 1000, 'abundant', 'rare')) %>% 
  group_by(abundant) %>% 
  summarise(mean_accuracy_10 = round(mean(accuracy_10),3), sd_accuracy_10 = round(sd(accuracy_10),3), 
            mean_sensitivity_10 = round(mean(sensitivity_10),3), sd_sensitivity_10 = round(sd(sensitivity_10),3),
            mean_specificity_10 = round(mean(specificity_10),3), sd_specificity_10 = round(sd(specificity_10),3),
            mean_tss_10 = round(mean(tss_10),3), sd_tss_10 = round(sd(tss_10),3)) %>% 
  mutate(accuracy_10 = paste0(mean_accuracy_10, " (", sd_accuracy_10, ")")) %>%
  mutate(sensitivity_10 = paste0(mean_sensitivity_10, " (", sd_sensitivity_10, ")")) %>%
  mutate(specificity_10 = paste0(mean_specificity_10, " (", sd_specificity_10, ")")) %>%
  mutate(tss_10 = paste0(mean_tss_10, " (", sd_tss_10, ")")) %>% 
  select(abundant, accuracy_10, sensitivity_10, specificity_10, tss_10) %>% View()

  
  pseudo_sdm_results %>% 
    filter(!species %in% c("Ancyloxypha_numitor", "Coenonympha_nipisiquit")) %>% 
    mutate(abundant = ifelse(total_number_points > 1000, 'abundant', 'rare')) %>% 
    group_by(abundant) %>% 
    summarise(mean_accuracy_1 = round(mean(accuracy_1),3), sd_accuracy_1 = round(sd(accuracy_1),3), 
              mean_sensitivity_1 = round(mean(sensitivity_1),3), sd_sensitivity_1 = round(sd(sensitivity_1),3),
              mean_specificity_1 = round(mean(specificity_1),3), sd_specificity_1 = round(sd(specificity_1),3),
              mean_tss_1 = round(mean(tss_1),3), sd_tss_1 = round(sd(tss_1),3)) %>% 
    mutate(accuracy_1 = paste0(mean_accuracy_1, " (", sd_accuracy_1, ")")) %>%
    mutate(sensitivity_1 = paste0(mean_sensitivity_1, " (", sd_sensitivity_1, ")")) %>%
    mutate(specificity_1 = paste0(mean_specificity_1, " (", sd_specificity_1, ")")) %>%
    mutate(tss_1 = paste0(mean_tss_1, " (", sd_tss_1, ")")) %>% 
    select(abundant, accuracy_1, sensitivity_1, specificity_1, tss_1) %>% View()


