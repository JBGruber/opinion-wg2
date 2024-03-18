### Script for abstract coding - reliability, sample for coding, results of coding 1 (N = 1,002) ###

#all relevant data for reproducibility in this Drive folder: https://drive.google.com/drive/u/1/folders/1KSkZW_IoIAjcSiaTGmjHdhZetNV41_l6 
#(except for sample of abstracts, see specific link below)

library(here)
library(tidyverse)
library(tidycomm)
library(gt)
library(readxl)

#### Step 1. Read in data (abstracts, annotations) ####

# Read in Excel-File containing deduplicated abstracts 2018-2023

#or from connected Drive
#drive_deauth()
#data_file <- "../data/abstracts-2018-2023-cleaned.xlsx"
#if (!file.exists(data_file)) {
#  drive_download(file = "https://docs.google.com/spreadsheets/d/1CIF_45xFEALSMeF_1rzzVR_YHXPaTxDq/edit?usp=drive_link&ouid=107651210144024418798&rtpof=true&sd=true",
#                 path = data_file)
#}

#abstracts <- read_excel(datafile)

# Read in CSV-File containing annotated data for intercoder tests 1 and 2 (6 coders, N = 100 units)

#locally from folder
annotations <- read.csv("annotations_ICR-round-4.csv")  %>%
  rbind(read.csv("annotations_ICR-round-5.csv")) %>%
  
  #remove empty "starty" coding by daniel 
  filter(coder_id != 196 & coder_id != 205) %>%
  
  #harmonize names of coders across coding jobs
  mutate(coder_name = NA,
         coder_name = case_when(coder_id == 96 ~ "Daniel",
                                coder_id %in% c(193, 203) ~ "Valerie",
                                coder_id %in% c(194, 206) ~ "Sjoerd",
                                coder_id %in% c(195, 210) ~ "Samia",
                                coder_id %in% c(197, 207) ~ "Roksana",
                                coder_id %in% c(198, 208) ~ "Sasha"))

#### Step 2. Establish intercoder reliability for abstract coding ####

# Create "overall" coding yes/no for reliability: based on relevant-1 (= yes) and relevant-2 (= yes)
reli_individual <- annotations %>%
  
  #reduce to necessary variables
  filter(variable != "welcome") %>%
  select(coder_name,  unit_id, variable, value) %>%
  
  #to long format
  pivot_wider(names_from = c("variable"), values_from = "value") %>%
  
  #create single measure of inclusion/exclusion
  mutate(inclusion = 0,
         inclusion = replace(inclusion,
                             `relevant-1` == "Yes" & `relevant-2` == "Yes",
                             1))

##### 2.1 Reliability test: comparing individual coders against each other ##### 
reli_individual %>%
  test_icr(unit_var = unit_id,
           coder_var = coder_name, lotus = TRUE,
           inclusion)

##### 2.2 Reliability test: Comparing reliability within teams of 3 ##### 

#create vector with unique coders
coders <- annotations %>%
  pull(coder_name) %>%
  unique()

#create vector with combinations of coders (only keep combination of 3 people)
combinations_within <- map(2:length(coders), \(i) combn(coders, i, simplify = FALSE)) %>%
  unlist(recursive = FALSE) %>%
  purrr::keep(., ~ length(.x) == 3)

#create potential values within teams of coders & calculate reliability in teams
reli_within <- map(combinations_within, \(c) {
  data_coded_temp <- reli_individual %>%
    filter(coder_name %in% c) %>%
    group_by(unit_id) %>%
    mutate(n_coded = length(coder_name)) %>%
    ungroup() %>%
    filter(n_coded == length(c)) # only take complete cases
  
  data_coded_temp %>%
    test_icr(unit_var = unit_id, 
             coder_var = coder_name, 
             inclusion,
             cohens_kappa = FALSE,
             lotus = TRUE) %>%
    mutate(coders = toString(c), .before = 1)
}) %>%
  bind_rows()

##### 2.3 Reliability test: Comparing reliability across teams of 3 with majority voting ##### 

#create variation of all potential teams (N = 3 vs. N = 3)

# Initialize an empty list to store combinations
combinations_across <- list()

# Iterate over combinations_within and bind them together
for (i in 1:(length(combinations_within) / 2)) {
  team1_index <- i
  team2_index <- length(combinations_within) - i + 1
  
  combination <- data.frame(
    team1 = combinations_within[[team1_index]],
    team2 = combinations_within[[team2_index]])
  
  combinations_across[[i]] <- combination
}

rm(combination, team1_index, team2_index, coders)

#create potential values across teams of coders & calculate reliability across teams  

# Define a function to process each combination
process_combination <- function(combinations_across) {
  
  #create coding for team 1
  data_coded_temp <- reli_individual %>%
    filter(coder_name %in% (combinations_across %>% 
                              getElement(i) %>%
                              select(team1) %>%
                            flatten_chr())) %>%
    group_by(unit_id) %>%
    mutate(inclusion_team1 = ifelse(sum(inclusion) <= 1, 0, 1)) %>%
    ungroup() %>%
    
    #create coding for team 2
    mutate(inclusion_team2 = (reli_individual %>%
                                filter(coder_name %in% (combinations_across %>% 
                                                          getElement(i) %>%
                                                          select(team2) %>%
                                                          flatten_chr())) %>%
                                group_by(unit_id) %>%
                                mutate(inclusion_team2 = ifelse(sum(inclusion) <= 1, 0, 1)) %>%
                                ungroup() %>%
                                pull(inclusion_team2))) %>%
    
    #reduce to necessary variables
    distinct(., unit_id, .keep_all = TRUE) %>%
    select(unit_id, inclusion_team1, inclusion_team2) %>%
    
    #to long format for reliability test
    pivot_longer(cols = inclusion_team1:inclusion_team2) %>%
    rename(coder_name = "name")
}

# Initialize an empty list to store combinations
reli_across <- list()

# Apply the function to each combination and bind the results
for (i in 1:(length(combinations_across))) {
  reli_across[[i]] <- process_combination(combinations_across) 
}

#Calculate reliability across teams
process_data_frame <- function(reli_across) {
  reli_across %>%
    test_icr(unit_var = unit_id, 
             coder_var = coder_name, 
             cohens_kappa = FALSE,
             lotus = TRUE)
}

# Apply the function to each data frame in reli_across and bind the results
reli_across <- map(reli_across, process_data_frame) %>%
  bind_rows()

#get average Krippendorf's alpha across teams
reli_across %>%
  summarize(mean_agreement = mean(Agreement),
            mean_holsti = mean(Holstis_CR),
            mean_krippendorff = mean(Krippendorffs_Alpha),
            mean_lotus = mean(Lotus))

#clean house
rm(i)

#### Step 3. Draw stratified sample & assign coders ####

##### 3.1 Stratified sample by year #####
set.seed(31673)
abstracts_sample <- abstracts %>%
  group_by(year) %>%
  slice_sample(n = 167) %>%
  ungroup() %>%
  
  #randomly shuffle rows
  slice_sample(n = nrow(.))

##### 3.2 Assign to coders in random teams to create coding jobs #####

#create list of coders in different comvinations
coders <- bind_rows(combinations_across, .id = "combination") %>%
  
  #create id for coder 1, 2 or 3
  mutate(coder = rep(1:3, nrow(.)/3)) %>%
  
  #make wide for abstracts_sample format
  pivot_wider(names_from = c("combination"), values_from = c("team1", "team2")) %>%
  t() %>%
  as_tibble() %>%
  
  #exclude unnecessary first row
  slice(-1) %>%
  
  #rename for understandability
  rename(coder1 = V1,
         coder2 = V2,
         coder3 = V3)

#replicate based on number of abstracts in sample
coders <- map_dfr(1:50, ~ coders) %>%
  bind_rows(coders %>%
              slice(1:2))

# Assign coders in teams
abstracts_sample <- abstracts_sample %>%
  cbind(coders)

##### 3.3 Write out individual data for all six coders #####

#create data sets
abstracts_sample_daniel <- abstracts_sample %>%
  filter(coder1 == "Daniel" | coder2 == "Daniel" | coder3 == "Daniel")

abstracts_sample_valerie <- abstracts_sample %>%
  filter(coder1 == "Valerie" | coder2 == "Valerie" | coder3 == "Valerie")

abstracts_sample_roksana <- abstracts_sample %>%
  filter(coder1 == "Roksana" | coder2 == "Roksana" | coder3 == "Roksana")

abstracts_sample_samia <- abstracts_sample %>%
  filter(coder1 == "Samia" | coder2 == "Samia" | coder3 == "Samia")

abstracts_sample_sjoerd <- abstracts_sample %>%
  filter(coder1 == "Sjoerd" | coder2 == "Sjoerd" | coder3 == "Sjoerd")

abstracts_sample_sasha <- abstracts_sample %>%
  filter(coder1 == "Sasha" | coder2 == "Sasha" | coder3 == "Sasha")

#write out: saved via Drive, see here: https://drive.google.com/drive/folders/1KSkZW_IoIAjcSiaTGmjHdhZetNV41_l6?usp=drive_link
#write.csv2(abstracts_sample_daniel, "coding_1_sheet_daniel.csv", row.names = F)
#write.csv2(abstracts_sample_valerie, "coding_1_sheet_valerie.csv", row.names = F)
#write.csv2(abstracts_sample_roksana, "coding_1_sheet_roksana.csv", row.names = F)
#write.csv2(abstracts_sample_samia, "coding_1_sheet_samia.csv", row.names = F)
#write.csv2(abstracts_sample_sjoerd, "coding_1_sheet_sjoerd.csv", row.names = F)
#write.csv2(abstracts_sample_sasha, "coding_1_sheet_sasha.csv", row.names = F)

#### Step 4. Coding 1 (N = 1,002) ####

# N = 1,002 abstracts (N = 167 per year, 2018-2023, everyone coded N = 501 in shuffled teams, majority vote)

##### 4.1 Read in coding jobs and clean #####

abstracts_coding_1 <- read.csv("coding_1_result_sasha.csv")  %>%
  filter(coder_id == "229") %>%
  
  #add codings Valerie
  rbind(read.csv("coding_1_result_valerie.csv")) %>%
  
  #add codings Daniel
  rbind(read.csv("coding_1_result_daniel.csv") %>%
          filter(coder_id == "96")) %>%
  
  #add codings Roskana
  rbind(read.csv("coding_1_result_roksana.csv") %>%
          filter(coder_id == "212")) %>%
  
  #add codings Samia
  rbind(read.csv("coding_1_result_samia.csv") %>%
          filter(coder_id == "230")) %>%
  
  #add codings Sjoerd
  rbind(read.csv("coding_1_result_sjoerd.csv") %>%
          filter(coder_id == "224")) %>%
  
  #remove id variable
  filter(unit_id != "id") %>%
  
  #harmonize names of coders across coding jobs
  mutate(coder_name = NA,
         coder_name = case_when(coder_id == 96 ~ "Daniel",
                                coder_id %in% c(217) ~ "Valerie",
                                coder_id %in% c(224) ~ "Sjoerd",
                                coder_id %in% c(230) ~ "Samia",
                                coder_id %in% c(212) ~ "Roksana",
                                coder_id %in% c(229) ~ "Sasha"))

##### 4.2 Harmonize team codes: Majority voting #####

# Create "overall" coding yes/no: based on relevant-1 (= yes) and relevant-2 (= yes)
abstracts_coding_1 <- abstracts_coding_1 %>%
  
  #reduce to necessary variables
  select(coder_name,  unit_id, variable, value) %>%
  
  #to long format
  pivot_wider(names_from = c("variable"), values_from = "value") %>%
  
  #create single measure of inclusion/exclusion
  mutate(inclusion = 0,
         inclusion = replace(inclusion,
                             `relevant-1` == "Yes" & `relevant-2` == "Yes",
                             1)) %>%
  
  #create majority vote
  group_by(unit_id) %>%
  mutate(inclusion_team = sum(inclusion),
         inclusion_team = replace(inclusion_team,
                                  inclusion_team <=1,
                                  0),
         inclusion_team = replace(inclusion_team,
                                  inclusion_team >=2,
                                  1)) %>%
  
  #keep single observation per unit
  ungroup() %>%
  distinct(., unit_id, .keep_all = TRUE) %>%
  
  #reduce to necessary variables & bind to abstract data
  filter(inclusion_team == 1) %>%
  select(unit_id, inclusion_team) %>%
  left_join(abstracts %>%
              rename(unit_id = id)) %>%
  select(-inclusion_team)

#write.csv2(abstracts_coding_1, "abstracts_coding_1.csv", row.names = F)

#save working space
#save.image("abstract_coding-1.RDATA")