library(dplyr)
library(lubridate)
library(ggplot2)
library(stringi)
week <- read.csv("data/processed_data/PAHO_processed_A.csv")
#priority <- read.csv("processed_data/Priority_country.csv")
annual <- read.csv("data/ref_data/1980_2023_pahocountries_paho_adm0_annual_source.csv")

plyr::count(week$adm_0_name)
week %>% 
  group_by(adm_0_name)%>% tally()
#priority_pic <- priority %>%
#  filter(p_group %in% c("Group 3", "Group 4"))

annual <- annual %>%
  select(-X, -Tipo)%>%
  rename(adm_0_name = Country, 
         year = Year,
         annual_count = Total)%>%
  filter(year > 2013 & year <2023)

annual$adm_0_name<- stri_trans_general(annual$adm_0_name,"Latin-ASCII")

plyr::count(annual$adm_0_name) #52 countries

# extract the latest cumulative value for each year and country
week_to_year <- week %>%
  filter(!is.na(dengue_total_cumulative))%>%
  add_count (adm_0_name, year) %>%
  arrange(desc(epidemiological_week))%>%
  group_by(adm_0_name, year)%>% slice_head(n=1)%>%
  mutate(key = paste0(adm_0_name, "_", year))%>%
  rename(num_rep_week = n, 
          latest_week = epidemiological_week, 
         latest_count = dengue_total_cumulative)%>%
  select(adm_0_name, year, key, latest_week, num_rep_week, latest_count)%>%
  merge(., annual, by=c("adm_0_name", "year"), all.x=T)
  
# always assuming that leveraging annual data only when latest_count == annual_count
# 1) country-years that can be changed to 0 (no case at all for that year)
v1 <- week_to_year %>%
  mutate(total_week = ifelse(year %in% c(2015:2019, 2021:2022), 52, 53))%>% # total number of weeks for each year  
  #data coverage (number of reported weeks = total number of weeks of the year)?
  #if yes, full data exist already so no need to consider here
  filter(!(num_rep_week == total_week))%>% 
  filter(latest_count==0)%>%
  filter(latest_count == annual_count)



# 2) is the latest data point = last year of the year?
# filling in 0s for the weeks after the latest cumulative count when compare==0 

v2 <- week_to_year %>%
  mutate(total_week = ifelse(year %in% c(2015:2019, 2021:2022), 52, 53))%>% # total number of weeks for each year  
  filter(!(num_rep_week == total_week))%>% 
  filter(!latest_count==0)%>%
  filter(!(latest_week == total_week)) %>%
  filter(latest_count == annual_count)

plyr::count(week_to_year$adm_0_name) #52 countries
plyr::count(week_to_year$compare) # 0= 432; 1= 18





