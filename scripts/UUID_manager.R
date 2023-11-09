rm(list= ls())
library(readxl)
library(countrycode)
library(dplyr)
library(data.table)
library(tidyr)
setwd("C:/Users/AhyoungLim/Dropbox/WORK/Dengue Project 2022/11_DENData")

f <- read_excel("./01_Dengue_data/OD_master/OD_V1.0/filingDB_V1.0.xlsx", sheet=1)
f1 <- read_excel("./01_Dengue_data/OD_master/OD_V1.1/filingDB_V1.1.xlsx", sheet=1) %>% mutate(UUID_old = NA, original_filename = NA, NOTES = NA)

names(f)
names(f1)

f$version = "V1.0"
f1$version = "V1.1"

m <- rbind(f[, c("version", "original_filename", "standard_filename", "source_cat", "country", "year", "serial_cat", "serial_cat_num", "serial_id", "NOTES", "UUID_old", "metadata_description", "metadata_url", "metadata_steps")], 
           f1[, c("version", "original_filename","standard_filename", "source_cat", "country", "year", "serial_cat", "serial_cat_num", "serial_id", "NOTES", "UUID_old", "metadata_description", "metadata_url", "metadata_steps") ])

# !!!! check if any duplicated file records
m %>% 
  group_by(standard_filename, metadata_description, metadata_url)%>%
  tally()%>%
  arrange(desc(n))

m <- m %>%
  filter(!(grepl("20190103|20201217", standard_filename ) & is.na(UUID_old)))



#####################################################################

# V1.0 ==============================================================

#####################################################################
db1 <- read.csv("01_Dengue_data/open_dengue_1.0/MOH_PAHO_SUB_transformed.csv")
db3 <- read.csv("01_Dengue_data/open_dengue_1.0/TYCHO_transformed.csv")


db1 <- db1 %>% 
  select(adm_0_name:dengue_total, source_cat, UUID, source_id) %>%
  mutate(source_cat = ifelse(source_cat %in% c("paho"), "PAHO_PLISA", "PAHO_MOH"))%>% 
  filter(!UUID %in% c("9e92834d-c739-11ed-aa9a-1e00d12e8869")) # remove duplicates (PANAMA 2018-2022 EW09 )

db3 <- db3 %>%
  select(adm_0_name:dengue_total, source_cat, UUID)%>% mutate(source_id = "tycho_dengue_allcountries.csv")

names(db1)
names(db3)

db1 <- rbind(db1, db3)

names(db1)[11] <- "UUID_old"
names(db1)[12] <- "standard_filename"

db1 <- merge(db1, m[, c("UUID_old", "standard_filename")], by=c("UUID_old", "standard_filename"), all.x=T)%>%
  select(standard_filename, adm_0_name:source_cat, UUID_old)%>%
  mutate(original_filename= NA)


# adding new UUIDs (no/wrong UUID for release V1.0 ) ============================================

# adding old UUIDs for PAHO PLISA data 
#!!!! need to work on PAHO-cum-inc project again
db2 <- read.csv("01_Dengue_data/open_dengue_1.0/paho_for_opendengue.csv")


source_lookup <- db2 %>%
    filter(!(is.na(source_id) | !grepl("all_PAHO|PAHO_2022", source_id)))%>%
    group_by(calendar_start_date,calendar_end_date, source_id)%>%
    tally()

db2 <- merge(db2, source_lookup, by=c("calendar_start_date", "calendar_end_date"), all.x=T)%>%
  select( -n)%>%
  rename(source_id = source_id.y)%>%
  mutate(source_id = paste0(source_id, ".csv"), 
         source_cat = ifelse(grepl("all_PAHO|PAHO_2022", source_id.x), "PAHO", paste0(source_id.x)))%>%
  select(adm_0_name:adm_2_code, calendar_start_date, calendar_end_date, dengue_total, source_id, source_cat)

db2 <- merge(db2, m[, c("standard_filename", "UUID_old")], by.x="source_id", by.y="standard_filename")%>%
  # mutate(UUID = ifelse(source_cat == "paho_imputed", paste0(UUID, " (imputed)"), UUID))%>%
  rename(standard_filename = source_id)%>%
  mutate(original_filename= NA)
unique(db2$standard_filename)
# db2 <- db2 %>%
#   mutate(source_cat = ifelse(source_cat %in% c("paho_adm0"), "PAHO_PLISA", "PAHO_IMPUTED"))
#   select(-source_id, -source_cat)
#   # mutate(source_cat = ifelse(source_cat %in% c("paho_adm0"), "PAHO_PLISA", "PAHO_IMPUTED"))



db4 <- read.csv("01_Dengue_data/open_dengue_1.0_asia/open_dengue_asia.csv") %>% select(-UUID) %>% mutate(source_cat = NA)# delete wrong UUID

db4 <- merge(db4, m[, c("original_filename", "UUID_old")], by.x="source_file", by.y="original_filename")%>%
  rename(original_filename = source_file )%>% 
  mutate(standard_filename = NA)%>%
  select(standard_filename, adm_0_name:UUID_old,original_filename)

# db4 <- db4 %>%
  # mutate(source_cat = ifelse(grepl("tycho", source_file), "ASIA_TYCHO",
  #                            ifelse(grepl("Dengue-", source_file), "ASIA_WHO", "ASIA_MOH")))%>%
  # select(-source_file)

names(db1)
names(db2)
names(db4)

data <- rbindlist(list(db1, db2, db4)) %>% 
  rename(source_cat_old = source_cat) 


# Error correction ================================

# to input 
# 1) Ecuador 2018-2022
ecu_2018_22 <- data %>%
  filter(adm_0_name == "Ecuador" & year(calendar_start_date)>2016)%>%
  group_by(adm_0_name, adm_1_name, adm_2_name,
           calendar_start_date, calendar_end_date, UUID_old, standard_filename)%>%
  # tally()%>% filter(n>1)
  summarise(dengue_total = sum(dengue_total, na.rm=T))%>% 

  select(standard_filename, adm_0_name:calendar_end_date, dengue_total, UUID_old)%>%
  mutate(standard_filename = paste0(standard_filename, ".csv"))

sum(ecu_2018_22$n) # 1793 rows

# to be removed
data <- data %>%
  filter(!(UUID_old == "9e927df4-c739-11ed-aa9a-1e00d12e8869" & calendar_start_date == "2013-03-24"))%>% # Ecuador (2013)
  filter(!(adm_0_name == "Ecuador" & year(calendar_start_date)>2016))%>% # Ecuador (2018-2022)
  filter(!UUID_old %in% c("9e92855f-c739-11ed-aa9a-1e00d12e8869","9e928901-c739-11ed-aa9a-1e00d12e8869" )) # Peru 2008, 2013

# to input
# 2) Ecuador 2013
ecu_2013 <-  read.csv("01_Dengue_data/open_dengue_1.0/error_correction/Ecuador_correction.csv") %>%
  select(standard_filename, adm_0_name:calendar_end_date, dengue_total, UUID_old)

# 3) Peru 2008 & 2013
peru_2008_13 <-  read.csv("01_Dengue_data/open_dengue_1.0/error_correction/Peru_correction.csv") %>%
  select(standard_filename, adm_0_name:calendar_end_date, dengue_total, UUID_old)

data_input <- rbindlist(list(ecu_2013, ecu_2018_22, peru_2008_13))%>% 
  mutate(source_cat_old = NA, 
         original_filename = NA, 
         adm_0_code = NA, 
         adm_1_code = NA, 
         adm_2_code = NA)%>%
  select(standard_filename, adm_0_name, adm_0_code, adm_1_name, adm_1_code, adm_2_name, adm_2_code, 
         calendar_start_date:dengue_total, source_cat_old, UUID_old, original_filename)

data <- rbind(data, data_input)




#####################################################################

# V1.1 ==============================================================

#####################################################################

dt <- read.csv("./01_Dengue_data/open_dengue_1.1/open_dengue_update_v1.1.csv")

dt <- merge(dt, m[, c("standard_filename")], by.x="source_file", by.y="standard_filename", all.x=T)

dt <- dt %>%
  mutate(source_file = ifelse(source_file == "SaudiArabia_2012_annual_statistics.xlsx", 
                              "SaudiArabia_2012_annual_statistics.pdf",
                             ifelse(source_file == "SaudiArabia_2019_annual_statistics.pdf", 
                                    "SaudiArabia_2019_annual_statistics.xlsx", 
                                    ifelse(source_file == "SaudiArabia_2020_annual_statistics.pdf", 
                                           "SaudiArabia_2019_annual_statistics.xlsx", 
                                           ifelse(source_file == "SaudiArabia_2021_annual_statistics.pdf",
                                                  "SaudiArabia_2021_annual_statistics.xlsx", source_file)))))%>%
  mutate(UUID_old = NA, original_filename = NA, source_cat_old = NA, 
         adm_0_code = NA, adm_1_code = NA, adm_2_code = NA)%>%
  rename(standard_filename = source_file)%>%
  select(standard_filename, adm_0_name, adm_0_code, adm_1_name, adm_1_code, adm_2_name, adm_2_code, 
         calendar_start_date:dengue_total, source_cat_old, UUID_old, original_filename)


dt <- merge(dt, m[, c("standard_filename")], by="standard_filename", all.x=T)

names(data)
names(dt)

data <- rbind(data, dt)
plyr::count(data$adm_0_name)

# remove countries with imported cases only
data <- data %>%
  filter(!adm_0_name %in% c("Republic Of Korea", "Canada", "New Zealand"))%>%
  filter(!(adm_0_name == "Colombia" & adm_2_name == "Unknown"))

# Meta data =====================================================================================
# back to metadata, filter out source files that are not included in the release 

m <- m %>%
  mutate(released = ifelse(standard_filename %in% data$standard_filename | UUID_old %in% data$UUID_old , "Y", "N"))%>%
  mutate(released = ifelse(country == "Brazil", "Y", released))

m$metadata_steps <- gsub("rleevant", "relevant", m$metadata_steps)


# get country name, source_cat
meta_r <- m %>%
  filter(released == "Y")%>%
  mutate(source_cat = gsub("_", "", source_cat),
         year = gsub("_|-", "-", year))%>%
  separate(year, into = c("year", "year2"), sep = "-")%>%
  mutate(year2 = ifelse(is.na(year2), paste0(year), year2))%>%
  mutate(iso3c = countrycode(country, origin = "country.name", destination = "iso3c"))%>%
  mutate(iso3c = ifelse(is.na(iso3c), paste0(country), iso3c))

# serial_cat_num
meta_r <- meta_r %>%
  group_by(source_cat, iso3c)%>%
  mutate(num_types = n_distinct(metadata_description), 
        serial_cat_num = match(metadata_description, unique(metadata_description)) 
        # serial_cat_num = ifelse(source_cat == "LITERATURE", 0, serial_cat_num)
  )%>% ungroup() 

# serial_id
meta_r <- meta_r %>% 
  group_by(source_cat, iso3c, serial_cat_num)%>%
  arrange(year)%>%
  mutate(serial_id = ifelse(serial_cat == "Y", 0:n(), serial_id),
         # num_types = n_distinct(metadata_description), 
         # serial_cat_num = match(metadata_description, unique(metadata_description)), 
         # serial_id = ifelse(serial_cat == "Y" & grepl("/", year), 0, serial_id), 
         period = ifelse(min(as.numeric(year), na.rm=T) < max(as.numeric(year2), na.rm=T), 
         paste0(min(as.numeric(year), na.rm=T), max(as.numeric(year2), na.rm=T)), paste0(year) ))%>% ungroup()

meta_r <- meta_r %>%
  mutate(sourceID = paste0(source_cat, "-", iso3c, "-", period, "-", serial_cat, 
                           sprintf("%02d", as.numeric(serial_cat_num))))%>%
  mutate(UUID = ifelse(!year == year2, paste0(sourceID, "-",  sprintf("%02d", as.numeric(serial_id))), 
                       paste0(source_cat, "-", iso3c, "-", year, "-", serial_cat, 
                              sprintf("%02d", as.numeric(serial_cat_num)), "-", 
                              sprintf("%02d", as.numeric(serial_id)))))

  # filter(!UUID_old == "9e92834d-c739-11ed-aa9a-1e00d12e8869")

# !!!! check if any duplicated UUID 
meta_r %>% 
  group_by(UUID)%>%
  tally()%>%
  arrange(desc(n))




###### CASE DEFINITION =========================================

cdf <- read_excel("01_Dengue_data/open_dengue_1.2/Case_definition/case_def.xlsx", sheet=1)%>% 
  filter(released == "Y") %>% 
  select(UUID, case_definition_original, case_definition_standardised) 

cdf_nr <- read_excel("01_Dengue_data/open_dengue_1.2/Case_definition/case_def.xlsx", sheet=1)%>% 
  filter(released == "N" & !is.na(case_definition_original)) %>% 
  select(case_definition_original, case_definition_standardised, standard_filename, original_filename, UUID_old) 

meta_r <- merge(meta_r, cdf, by=c("UUID"), all.x=T)

meta_r %>% select(UUID, case_definition_original, case_definition_standardised)%>%
  filter(is.na(case_definition_standardised))


# Publish metadata (UUID and sourceIDs are only assigned for the released data)
meta_r <- meta_r %>%
  select(version, UUID, released, source_cat, country, period, 
         case_definition_original, case_definition_standardised, NOTES, original_filename, standard_filename, UUID_old, metadata_description:metadata_steps)

meta_nr <- m %>%
  filter(released == "N")%>%
  mutate(source_cat = gsub("_", "", source_cat))%>%
  mutate(period = gsub("_|-", "", year))%>%

  merge(., cdf_nr, by=c("original_filename", "standard_filename", "UUID_old"), all.x=T)%>%
  mutate(UUID = NA)%>%
  select(version, UUID, released, source_cat, country, period, 
         case_definition_original, case_definition_standardised, NOTES, original_filename, standard_filename, UUID_old, metadata_description:metadata_steps)

names(meta_r)
names(meta_nr)

meta_all <- rbind(meta_r, meta_nr)%>% arrange( UUID, country, period)

# meta_public <- m3 %>%
#   select("UUID", "source_cat","country","period", "case_definition_original","metadata_description", "metadata_url", "metadata_steps")%>%
#   # group_by(sourceID, source_cat, country, period)%>%
#   # summarize(metadata_description = paste(unique(metadata_description), collapse = "; "), 
#   #           metadata_url = paste(unique(metadata_url), collapse = "; "), 
#   #           metadata_steps = paste(unique(metadata_steps), collapse = "; ")) %>%
#   # ungroup()%>%
#   arrange(desc(source_cat), country, period)

writexl::write_xlsx(meta_all, "01_Dengue_data/OD_master/filingDB_allV.xlsx")

###!!! check filing DB first 
write.csv(meta_public, "./01_Dengue_data/OD_master/OD_V1.2/sourcedata_V1.2.csv")


# attach sourceID and UUID to data

data1 <- merge(data[!is.na(data$UUID_old), ], meta_all[, c("UUID_old", "case_definition_standardised", "UUID")], by=c("UUID_old"), all.x=T) %>%
  
  mutate(UUID = ifelse(grepl("zero",source_cat_old), paste0(UUID, " (Zero filling)"), 
                      ifelse(grepl("imputed", source_cat_old), paste0(UUID, " (Imputed)"), paste0(UUID))))%>%
  select(-UUID_old, -standard_filename, -original_filename, -source_cat_old)

data2 <- merge(data[is.na(data$UUID_old), ], meta_all[, c("standard_filename",  "case_definition_standardised", "UUID")], by=c("standard_filename"), all.x=T)%>% select(-UUID_old, -standard_filename, -original_filename, -source_cat_old)

names(data1)
names(data2)
summary(is.na(data1$case_definition_standardised))
summary(is.na(data2$case_definition_standardised))
summary(is.na(data1$UUID))
summary(is.na(data2$UUID))

master <- rbind(data1, data2)

summary(master$dengue_total)
summary(is.na(master))

# missing calendar end date
master$calendar_end_date <- ifelse(is.na(master$calendar_end_date), format(as.Date(master$calendar_start_date)+6,  "%Y-%m-%d"),  format(as.Date(master$calendar_end_date), "%Y-%m-%d"))

master <- master[!is.na(master$dengue_total)]
master <- master %>% mutate(dengue_total = ifelse(dengue_total < 0, 0, as.numeric(dengue_total)))
summary(master$dengue_total) #!!! Check any NAs / abnormal values

# check any duplicated rows
dup <- master %>%
  group_by(adm_0_name, adm_1_name, adm_2_name, calendar_start_date, calendar_end_date, dengue_total, UUID) %>%
  mutate(dup1 = ifelse(n()>1, TRUE, FALSE)) %>%
  filter(dup1 == TRUE )%>%
  tally()# 55 obs 

master2 <- master %>%
  group_by(adm_0_name, adm_0_code, adm_1_name, adm_1_code, adm_2_name, adm_2_code, 
           calendar_start_date, calendar_end_date, dengue_total, UUID) %>%
  distinct() %>% ungroup()


write.csv(master2, "01_Dengue_data/OD_master/OD_V1.2/masterDB_V1.2.csv", row.names=F)


# f1 <- read_excel("./01_Dengue_data/open_dengue_update_1.1/new_data_sources.xlsx", sheet=1) 
# names(f1)
# dt <- read.csv("./01_Dengue_data/open_dengue_update_1.1/open_dengue_update_v1.1.csv")
# 
# f1 <- dt %>%
#   group_by(source_file)%>%
#   tally()%>%
#   merge(., f1, by.x="source_file", by.y="source_file_name", all.x=T, all.y=T)%>%
#   select(source_file, source_name, Country,  Time_span, link, step )%>%
#   rename(standard_filename = source_file, 
#          source_cat = source_name, 
#          country= Country, 
#          year = Time_span , 
#          metadata_url = link, 
#          metadata_steps = step
#          
#          )%>%
#   mutate(serial_cat = "", 
#          serial_id = "", 
#          metadata_description = ""
#          )%>%
#   select(standard_filename:year, serial_cat, serial_id, metadata_description, metadata_url, metadata_steps)
#   
# 
# writexl::write_xlsx(f1, "./01_Dengue_data/open_dengue_update_1.1/filingDB_V1.1.xlsx") # export and manually fill it






# dt2 <- read_excel(dat, sheet=2)
# 
# m <- read.csv("C:/Users/AhyoungLim/Dropbox/WORK/Dengue Project 2022/Dengue DB/master-repo/data/raw_data/master_data.csv")
# 
# asia <- read.csv("C:/Users/AhyoungLim/Dropbox/WORK/Dengue Project 2022/11_DENData/01_Dengue_data/open_dengue_asia_1.0/metadata_asia.csv")
# 
# dt$UUID_in_v1 <- ifelse(dt$UUID %in% m$UUID, "1", "0")
# dt2$UUID_in_v1 <- ifelse(dt2$standard_filename %in% asia$standard_filename , "1", "0")
# write.csv(dt2, "C:/Users/AhyoungLim/Desktop/metadata_V1.0_2.csv",row.names=F)
# names(dt)
# dt <- dt %>%
#   mutate(source_cat = gsub("_", "", source_cat), 
#          year = gsub("_", "", year))%>%
#   # mutate(iso3c = countrycode(country, origin = "country.name", destination = "iso3c"))%>%
#   mutate(iso3c = ifelse(country == "ALL", "ALL", iso3c))%>%
#  mutate(SourceID = paste0(source_cat, "-", iso3c, "-", year, "-", serial_cat, sprintf("%02d", as.numeric(serial_id))))
# 
# write.csv(dt, "C:/Users/AhyoungLim/Desktop/metadata_V1.0.csv",row.names=F)


