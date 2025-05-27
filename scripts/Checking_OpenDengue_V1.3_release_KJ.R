#' ---
#' title: "Checking OpenDengue V1.3 release"
#' author: "K Joshi"
#' 
#' ---
#' 
#' Overview: 
#' =========
#' 
#' Check V1.3 for errors/ unwanted differences from V1.2. 
#' 
#' Timeline:
#' =========
#' 16-05-2025: Code to check for:
#'                Double counts by location and time period
#'                Sub zero cases
#'                Comparing annual totals between V1.2 and V1.3 for national extract
#'                Comparing number of observations by country and year 
#'                Subsetting data in V1.2 and V1.3 into overlap and reverse overlap
#'                Checking whether annual cases by country in V1.3 is bigger than V1.2 
#'                Checking epiweek definition applied correctly
#'                Checking for errors/ duplicate counts relating to leap years.


library(readr)
library(dplyr)
library(tidyverse)

National_extract_V1.3 <- read_csv("data/releases/V1.3/National_extract_V1_3.csv")
Spatial_extract_V1.3 <- read_csv("data/releases/V1.3/Spatial_extract_V1_3.csv")
Temporal_extract_V1.3 <- read_csv("data/releases/V1.3/Temporal_extract_V1_3.csv")

National_extract_V1.2 <- read_csv("data/releases/V1.2.2/National_extract_V1_2_2.csv")
Spatial_extract_V1.2 <- read_csv("data/releases/V1.2.2/Spatial_extract_V1_2_2.csv")
Temporal_extract_V1.2 <- read_csv("data/releases/V1.2.2/Temporal_extract_V1_2_2.csv")
 
#--------------- Double counts checking 

#----- National double counts
National_extract_V1.3_double_counts <- National_extract_V1.3 %>% 
  ungroup() %>% 
  group_by(adm_0_name, calendar_start_date, calendar_end_date) %>% 
  mutate(Count = n()) %>% 
  filter(Count > 1)

National_extract_V1.3_double_counts_PER <- National_extract_V1.3_double_counts %>% 
  filter(ISO_A0 == "PER")
National_extract_V1.3_double_counts_PER %>% select(calendar_start_date, calendar_end_date, dengue_total) %>% distinct() %>% dim()
#' 104 duplicate observations. 2 for Laos, 2013, 102 for Peru, 2008. Two data sources used for Peru 2008: MOH-PER-2008-W01-52, MOH-PER-20002023-Y02-00. 
#' Laos dengue counts different. Both sources from grey literature. Choose higher count or apply double counts protocol. 
#' Dengue counts for Peru duplicated within each source exactly. Choose either.

#----- High S double counts

Spatial_extract_V1.3_double_counts <- Spatial_extract_V1.3 %>% 
  ungroup() %>% 
  group_by(full_name, calendar_start_date, calendar_end_date) %>% 
  mutate(Count = n()) %>% 
  filter(Count > 1)
dim(Spatial_extract_V1.3_double_counts)

# No double counts in high S extract. 

#----- High T double counts

Temporal_extract_V1.3_double_counts <- Temporal_extract_V1.3 %>% 
  ungroup() %>% 
  group_by(full_name, calendar_start_date, calendar_end_date) %>% 
  mutate(Count = n()) %>% 
  filter(Count > 1)
dim(Temporal_extract_V1.3_double_counts)

# No double counts in high T extract. 

#--------------- Sub zero counts checking 

#----- National extract 
National_extract_V1.3_subzero <- National_extract_V1.3 %>% 
  filter(dengue_total < 0)
dim(National_extract_V1.3_subzero)

#----- High S extract 
Spatial_extract_V1.3_subzero <- Spatial_extract_V1.3 %>% 
  filter(dengue_total < 0)
dim(Spatial_extract_V1.3_subzero)

#----- High T extract 
Temporal_extract_V1.3_subzero <- Temporal_extract_V1.3 %>% 
  filter(dengue_total < 0)
dim(Temporal_extract_V1.3_subzero)

#' None of the extracts have below zero counts. 

#--------------- Compare annual totals between V1.2.2 and V1.3 - NATIONAL ---------------------------- NEEDS ASSESSING

compare_annual_totals <- function(old_extract, new_extract){
  
  old_extract_totals_by_country <- old_extract %>% 
    ungroup() %>%
    group_by(full_name, Year) %>% 
    summarise(old_extract_annual_total = sum(dengue_total))
  
  new_extract_totals_by_country <- new_extract %>% 
    ungroup() %>%
    group_by(full_name, Year) %>% 
    summarise(new_extract_annual_total = sum(dengue_total))
  
  old_new_comparison <- full_join(old_extract_totals_by_country, 
                                  new_extract_totals_by_country, by = c("full_name", "Year")) %>%
    mutate(old_higher = case_when(new_extract_annual_total > old_extract_annual_total ~ "Higher",
                                  new_extract_annual_total == old_extract_annual_total ~ "Equal",
                                  new_extract_annual_total < old_extract_annual_total ~ "Lower"))
    
  
  return(old_new_comparison)
  
}

National_extract_V1.2_V1.3_annual_tot_comparison <- compare_annual_totals(National_extract_V1.2, National_extract_V1.3)
National_extract_V1.2_V1.3_annual_tot_comparison_new_lower <- National_extract_V1.2_V1.3_annual_tot_comparison %>% 
  filter(old_higher == "Lower") %>% 
  mutate(unique_id = paste(full_name, "_", Year))

# Check data in old and new datasets with lower counts in new dataset. 
National_extract_V1.2_higher_than_new <- National_extract_V1.2 %>% 
  mutate(unique_id = paste(full_name, "_", Year)) %>% 
  filter(unique_id %in% National_extract_V1.2_V1.3_annual_tot_comparison_new_lower$unique_id)

National_extract_V1.3_lower_than_old <- National_extract_V1.3 %>% 
  mutate(unique_id = paste(full_name, "_", Year)) %>% 
  filter(unique_id %in% National_extract_V1.2_V1.3_annual_tot_comparison_new_lower$unique_id)

# Compare characteristics 
compare_lower_annual_counts_in_new_dataset_NATIONAL <- function(old_extract_lower, new_extract_higher){
  
  old_extract_lower_clean <- old_extract_lower %>% 
    group_by(adm_0_name, Year) %>% 
    mutate(Annual_obs = n()) %>%
    select(adm_0_name, Year, case_definition_standardised, T_res, unique_id, Annual_obs) %>% 
    distinct()
  
  new_extract_higher_clean <- new_extract_higher %>% 
    group_by(adm_0_name, Year) %>% 
    mutate(Annual_obs = n()) %>%
    select(adm_0_name, Year, case_definition_standardised, T_res, unique_id, Annual_obs) %>% 
    distinct()
  
  comparing_old_and_new <- full_join(old_extract_lower_clean, new_extract_higher_clean, by = "unique_id", suffix = c(".old", ".new")) %>% 
    select(adm_0_name.old, Year.old, 
           case_definition_standardised.old, T_res.old, Annual_obs.old, 
           case_definition_standardised.new, T_res.new, Annual_obs.new)

  return(comparing_old_and_new)
}
lower_annual_counts_in_new_dataset_chars <- compare_lower_annual_counts_in_new_dataset_NATIONAL(National_extract_V1.2_higher_than_new, 
                                                                                                National_extract_V1.3_lower_than_old)

#' Assess algorithm as to why the years with fewer total cases were selected. 
#' Code above extracts data from extract where extract with fewer annual cases was selected.
#' Characteristics (T_res, case definition, and annual obs) extracted.

#--------------- Differences between V1.2.2 and V1.3 

#---------- National extract 
# Fewer rows in National extract V1.3 than V1.2. Relates to changes in algorithm to generate data extract.
compare_number_of_rows <- function(old_extract, new_extract){
  
  old_extract_datapoints_by_country <- old_extract %>% 
    ungroup() %>%
    group_by(full_name) %>% 
    summarise(new_extract_obs = n())
  
  new_extract_datapoints_by_country <- new_extract %>% 
    ungroup() %>%
    group_by(full_name) %>% 
    summarise(old_extract_obs = n())
  
  comparison <- full_join(old_extract_datapoints_by_country, 
                          new_extract_datapoints_by_country, by = "full_name") %>% 
    select(full_name, old_extract_obs, new_extract_obs) %>% 
    mutate(Data_lost = case_when(new_extract_obs < old_extract_obs ~ "Lost",
                                 new_extract_obs >= old_extract_obs ~ "Not lost"))
  
}
National_extract_V1.2_V1.3_nrow_comparison <- compare_number_of_rows(National_extract_V1.2, National_extract_V1.3)

Locations_with_fewer_rows_in_national_V1.3 <- National_extract_V1.2_V1.3_nrow_comparison %>% 
  filter(Data_lost == "Lost")

# Where are the differences? 
where_are_the_differences <- function(old_extract, new_extract){
  
  new_extract_clean <- new_extract %>% 
    mutate(calendar_start_date = as.Date(calendar_start_date),
           calendar_end_date = as.Date(calendar_end_date),
           unique_id = paste(full_name, format(calendar_start_date, "%Y%m%d"), 
                             format(calendar_end_date, "%Y%m%d"), 
                             sep = "_"))
  
  old_extract_clean <- old_extract %>% 
    mutate(calendar_start_date = as.Date(calendar_start_date),
           calendar_end_date = as.Date(calendar_end_date),
           unique_id = paste(full_name, format(calendar_start_date, "%Y%m%d"), 
                             format(calendar_end_date, "%Y%m%d"), 
                             sep = "_"))
  
  missing_in_new_extract_IDs <- setdiff(old_extract_clean$unique_id, new_extract_clean$unique_id)
  missing_in_new_extract_v3_df <- old_extract_clean %>% 
    filter(unique_id %in% missing_in_new_extract_IDs)
  
  return(missing_in_new_extract_v3_df)
}
missing_in_national_v3_df  <- where_are_the_differences(National_extract_V1.2, National_extract_V1.3)

#---------- Spatial extract 
# Make sure there is no data lost by country.

Spatial_extract_V1.2_V1.3_nrow_comparison <- compare_number_of_rows(Spatial_extract_V1.2, Spatial_extract_V1.3)

Locations_with_less_data_in_spatial_V1.3 <- Spatial_extract_V1.2_V1.3_comparison %>% 
  filter(Data_lost == "Lost")

# Where are the differences? 
missing_in_spatial_v3_df  <- where_are_the_differences(Spatial_extract_V1.2, Spatial_extract_V1.3)

#---------- Temporal extract 
# Make sure there is no data lost by country.
Temporal_extract_V1.2_V1.3_nrow_comparison <- compare_number_of_rows(Temporal_extract_V1.2, Temporal_extract_V1.3)
Locations_with_less_data_in_temporal_V1.3 <- Temporal_extract_V1.2_V1.3_comparison %>% 
  filter(Data_lost == "Lost")

# Where are the differences? 
missing_in_temporal_v3_df  <- where_are_the_differences(Temporal_extract_V1.2, Temporal_extract_V1.3)

#--------------- Overlap and reverse overlap between V1.2.2 and V1.3 

data_only_in_old_extract_fun <- function(old_extract, new_extract){
  
    new_extract_clean <- new_extract %>% 
      mutate(calendar_start_date = as.Date(calendar_start_date),
             calendar_end_date = as.Date(calendar_end_date),
             unique_id = paste(full_name, dengue_total, 
                               format(calendar_start_date, "%Y%m%d"), 
                               format(calendar_end_date, "%Y%m%d"), 
                               sep = "_"))
    
    old_extract_clean <- old_extract %>% 
      mutate(calendar_start_date = as.Date(calendar_start_date),
             calendar_end_date = as.Date(calendar_end_date),
             unique_id = paste(full_name, dengue_total, 
                               format(calendar_start_date, "%Y%m%d"), 
                               format(calendar_end_date, "%Y%m%d"), 
                               sep = "_"))
    
    only_present_in_old_extract_IDs <- setdiff(old_extract_clean$unique_id, new_extract_clean$unique_id)
    only_in_old_extract_data <- old_extract_clean %>% 
      filter(unique_id %in% only_present_in_old_extract_IDs)
    
    return(only_in_old_extract_data)
}
data_only_in_new_extract_fun <- function(old_extract, new_extract){
  
  new_extract_clean <- new_extract %>% 
    mutate(calendar_start_date = as.Date(calendar_start_date),
           calendar_end_date = as.Date(calendar_end_date),
           unique_id = paste(full_name, dengue_total, 
                             format(calendar_start_date, "%Y%m%d"), 
                             format(calendar_end_date, "%Y%m%d"), 
                             sep = "_"))
  
  old_extract_clean <- old_extract %>% 
    mutate(calendar_start_date = as.Date(calendar_start_date),
           calendar_end_date = as.Date(calendar_end_date),
           unique_id = paste(full_name, dengue_total, 
                             format(calendar_start_date, "%Y%m%d"), 
                             format(calendar_end_date, "%Y%m%d"), 
                             sep = "_"))
  
  only_present_in_new_extract_IDs <- setdiff(new_extract_clean$unique_id, old_extract_clean$unique_id)
  only_present_in_new_extract_data <- new_extract_clean %>% 
    filter(unique_id %in% only_present_in_new_extract_IDs)
  
  return(only_present_in_new_extract_data)
}
data_in_both_extracts_fun <- function(old_extract, new_extract){
  
  new_extract_clean <- new_extract %>% 
    mutate(calendar_start_date = as.Date(calendar_start_date),
           calendar_end_date = as.Date(calendar_end_date),
           unique_id = paste(full_name, dengue_total, 
                             format(calendar_start_date, "%Y%m%d"), 
                             format(calendar_end_date, "%Y%m%d"), 
                             sep = "_"))
  
  old_extract_clean <- old_extract %>% 
    mutate(calendar_start_date = as.Date(calendar_start_date),
           calendar_end_date = as.Date(calendar_end_date),
           unique_id = paste(full_name, dengue_total, 
                             format(calendar_start_date, "%Y%m%d"), 
                             format(calendar_end_date, "%Y%m%d"), 
                             sep = "_"))
  
  present_in_both_extracts_IDs <- intersect(new_extract_clean$unique_id, old_extract_clean$unique_id)
  present_in_both_extracts_data <- new_extract_clean %>% 
    filter(unique_id %in% present_in_both_extracts_IDs)
  
  return(present_in_both_extracts_data)
}

#---------- National
Data_only_in_National_extract_V1.2 <- data_only_in_old_extract_fun(National_extract_V1.2, National_extract_V1.3)
Data_only_in_National_extract_V1.3 <- data_only_in_new_extract_fun(National_extract_V1.2, National_extract_V1.3)
Data_in_both_National_extracts <- data_in_both_extracts_fun(National_extract_V1.2, National_extract_V1.3)

#--- Check subsetting
nrow(Data_only_in_National_extract_V1.2) + nrow(Data_only_in_National_extract_V1.3) + 2*nrow(Data_in_both_National_extracts) == nrow(National_extract_V1.2) + nrow(National_extract_V1.3)

#--- Are the V1.3 counts larger than V1.2 
comparing_updated_annual_tot_vs_old_annual_tot_fun <- function(old_extract_unique, new_extract_unique){
  
  new_extract_clean <- new_extract_unique %>% 
    mutate(calendar_start_date = as.Date(calendar_start_date),
           calendar_end_date = as.Date(calendar_end_date),
           unique_id = paste(full_name, Year, sep = "-")) %>% 
    group_by(unique_id) %>% 
    summarise(Annual_total = sum(dengue_total))
   
  old_extract_clean <- old_extract_unique %>% 
    mutate(calendar_start_date = as.Date(calendar_start_date),
           calendar_end_date = as.Date(calendar_end_date),
           unique_id = paste(full_name, Year, sep = "-")) %>% 
    group_by(unique_id) %>% 
    summarise(Annual_total = sum(dengue_total))
  
  comparing_new_and_old <- full_join(old_extract_clean, new_extract_clean, by = "unique_id", suffix = c(".old", ".new")) %>%
    mutate(New_higher_annual_count = case_when(Annual_total.new > Annual_total.old ~ "TRUE",
                                               Annual_total.new == Annual_total.old ~ "EQUAL",
                                               Annual_total.new < Annual_total.old ~ "FALSE"),
           Present_new_but_not_old = is.na(Annual_total.old),
           Present_old_but_not_new = is.na(Annual_total.new)) 

  return(comparing_new_and_old)
}
Comparing_national_unique_V1.2_V1.3_new_data <- comparing_updated_annual_tot_vs_old_annual_tot_fun(Data_only_in_National_extract_V1.2, 
                                                                                                   Data_only_in_National_extract_V1.3)
#' NAs in Data_only_in_National_extract_V1.3 but not in Data_only_in_National_extract_V1.2 are due to presence of multiple reporting modes in the latter.

#---------- Spatial
Data_only_in_Spatial_extract_V1.2 <- data_only_in_old_extract_fun(Spatial_extract_V1.2, Spatial_extract_V1.3)
Data_only_in_Spatial_extract_V1.3 <- data_only_in_new_extract_fun(Spatial_extract_V1.2, Spatial_extract_V1.3)
Data_in_both_Spatial_extracts <- data_in_both_extracts_fun(Spatial_extract_V1.2, Spatial_extract_V1.3)

#--- Check subsetting
nrow(Data_only_in_Spatial_extract_V1.2) + nrow(Data_only_in_Spatial_extract_V1.3) + 2*nrow(Data_in_both_Spatial_extracts) == nrow(Spatial_extract_V1.2) + nrow(Spatial_extract_V1.3)

#--- Are the V1.3 counts larger than V1.2 
Comparing_spatial_unique_V1.2_V1.3_new_data <- comparing_updated_annual_tot_vs_old_annual_tot_fun(Data_only_in_Spatial_extract_V1.2, 
                                                                                                  Data_only_in_Spatial_extract_V1.3)

#---------- Temporal
Data_only_in_Temporal_extract_V1.2 <- data_only_in_old_extract_fun(Temporal_extract_V1.2, Temporal_extract_V1.3)
Data_only_in_Temporal_extract_V1.3 <- data_only_in_new_extract_fun(Temporal_extract_V1.2, Temporal_extract_V1.3)
Data_in_both_Temporal_extracts <- data_in_both_extracts_fun(Temporal_extract_V1.2, Temporal_extract_V1.3)

#--- Check subsetting
nrow(Data_only_in_Temporal_extract_V1.2) + nrow(Data_only_in_Temporal_extract_V1.3) + 2*nrow(Data_in_both_Temporal_extracts) == nrow(Temporal_extract_V1.2) + nrow(Temporal_extract_V1.3)

#--- Are the V1.3 counts larger than V1.2 
Comparing_temporal_unique_V1.2_V1.3_new_data <- comparing_updated_annual_tot_vs_old_annual_tot_fun(Data_only_in_Temporal_extract_V1.2, 
                                                                                                   Data_only_in_Temporal_extract_V1.3)

#--------------- Checking errors relating to epiweek definition 

epiweek_definition_checking <- function(dengue_data_extract){
  
  epiweek_checks <- dengue_data_extract %>% 
    select(calendar_start_date, calendar_end_date, T_res, Year) %>% 
    distinct() %>% 
    filter(T_res == "Week") %>% 
    group_by(Year) %>% 
    mutate(No_of_weeks = n())
  
  return(epiweek_checks)
} 

National_extract_V1.3_epiweek_checks <- epiweek_definition_checking(National_extract_V1.3)
Spatial_extract_V1.3_epiweek_checks <- epiweek_definition_checking(Spatial_extract_V1.3)
Temporal_extract_V1.3_epiweek_checks <- epiweek_definition_checking(Temporal_extract_V1.3)

summary(National_extract_V1.3_epiweek_checks$No_of_weeks) # as expected 
summary(Spatial_extract_V1.3_epiweek_checks$No_of_weeks) # as expected 
summary(Temporal_extract_V1.3_epiweek_checks$No_of_weeks) # as expected 

#--------------- Check for errors relating to dates, e.g. leap years 
leap_year_data_filtering_fun <- function(dengue_data){
  dengue_data_leap_year_filtered <- dengue_data %>% 
    mutate(calendar_start_date_mod = format(calendar_start_date, "%m-%d"),
           calendar_end_date_mod = format(calendar_start_date, "%m-%d")) %>% 
    filter(calendar_start_date_mod %in% c("02-28", "02-29") |
             calendar_end_date_mod %in% c("02-28", "02-29")) %>% 
    select(!calendar_start_date_mod & !calendar_end_date_mod) %>% 
    ungroup() %>% 
    group_by(full_name, Year) %>% 
    summarise(Number_of_obs = n()) %>%
    filter(Number_of_obs > 1)
  
  return(dengue_data_leap_year_filtered)
}
National_extract_V1.3_leap_year <- leap_year_data_filtering_fun(National_extract_V1.3)
Spatial_extract_V1.3_leap_year <- leap_year_data_filtering_fun(Spatial_extract_V1.3)
Temporal_extract_V1.3_leap_year <- leap_year_data_filtering_fun(Temporal_extract_V1.3)

# No duplicates for leap years.

