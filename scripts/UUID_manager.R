rm(list = ls())
library(readxl)
library(countrycode)
library(dplyr)
library(data.table)
library(tidyr)
library(lubridate)

dev_path <- "C:/Users/AhyoungLim/Dropbox/WORK/OpenDengue/OpenDengue-Dev/"
git_path <- "C:/Users/AhyoungLim/Dropbox/WORK/OpenDengue/master-repo-alim/master-repo/"
today <- gsub("-", "_", Sys.Date())
release <- TRUE

# load in previous version datasets
source(paste0(dev_path, "scripts/common/OD_data_merge_V1.1.R"))
source(paste0(dev_path, "scripts/common/OD_data_merge_V1.3.R"))

# merge filing DBs ----------
m <- rbind(m, f3)

# update serial id for Mexico
m$serial_id[grepl("Mexico_2007", m$standard_filename)] <- as.numeric(sub(
  ".*EW(\\d+).*",
  "\\1",
  m$standard_filename[grepl("Mexico_2007", m$standard_filename)]
))

# !!!! check if any duplicated file records
m %>%
  group_by(standard_filename, metadata_description, metadata_url) %>%
  tally() %>%
  filter(n > 1)

m %>%
  group_by(standard_filename) %>%
  tally() %>%
  filter(n > 1)

# check filename extensions
summary(grepl("\\.[^.]+$", m$standard_filename)) # should be all TRUE
m$standard_filename[grepl("\\.[^.]+$", m$standard_filename) == FALSE]

rm(f3)

# merge datasets ----------

data <- rbind(data, data_v1_3)

rm(data_v1_3)

# list of unique std filenames/UUIDs that are included in the dataset:
# both standard filename and UUID available
s_n_all <- data %>% # 507
  filter(!is.na(standard_filename) & !is.na(UUID_old)) %>%
  select(standard_filename, UUID_old) %>%
  distinct()
nrow(s_n_all)

# only std filename available
s_sf_all <- data %>% # 609
  filter(is.na(UUID_old) & !is.na(standard_filename)) %>%
  select(standard_filename) %>%
  distinct()
nrow(s_sf_all)

# only uuid available
s_uuid_all <- data %>% # 90
  filter(is.na(standard_filename) & !is.na(UUID_old)) %>%
  select(UUID_old) %>%
  distinct()
nrow(s_uuid_all)

s_sf_all <- s_sf_all %>% # 607
  filter(!standard_filename %in% s_n_all$standard_filename)
nrow(s_sf_all)

s_uuid_all <- s_uuid_all %>% # 89
  filter(!UUID_old %in% s_n_all$UUID_old)
nrow(s_uuid_all)

s_uuid_all <- rbind(s_uuid_all, s_n_all[, c("UUID_old")])

nrow(s_sf_all) + nrow(s_uuid_all) # 1203 <- this should be equal to released == "Y" in source data


# source data ==============================================
# back to metadata, filter out source files that are not included in the release
m <- m %>%
  mutate(
    sf_released = ifelse(
      standard_filename %in% s_sf_all$standard_filename,
      "Y",
      "N"
    ),
    uuid_released = ifelse(UUID_old %in% s_uuid_all$UUID_old, "Y", "N")
  ) %>%
  mutate(
    released = ifelse(sf_released == "N" & uuid_released == "N", "N", "Y")
  ) %>%
  mutate(released = ifelse(country == "Brazil", "Y", released)) %>% # brazil adm2 data (V1.2 and V1.3) hasnt been incorporated yet
  mutate(released = ifelse(standard_filename == "dengue_explorer_all_countries.csv", "Y", released))

nrow(m[m$released == "Y", ]) -
  nrow(m[m$released == "Y" & m$country == "Brazil", ]) -
  nrow(m[m$released == "Y" & m$standard_filename == "dengue_explorer_all_countries.csv", ]) # 1203

rm(s_n_all, s_sf_all, s_uuid_all)

# get country name, source_cat
meta_r <- m %>%
  filter(released == "Y") %>%
  # trimming source_cat and year values
  mutate(
    source_cat = gsub("_", "", source_cat),
    year = gsub("_|-", "-", year)
  ) %>%
  # if the 'year' spans multiple years, separate it into two columns
  separate(year, into = c("year", "year2"), sep = "-") %>%
  mutate(year2 = ifelse(is.na(year2), paste0(year), year2)) %>%
  # get iso country code
  mutate(
    iso3c = countrycode(country, origin = "country.name", destination = "iso3c")
  ) %>%
  mutate(iso3c = ifelse(is.na(iso3c), paste0(country), iso3c))

# serial_cat (W/M/Y): categorize publications based on their frequency of publication
# serial_cat_num: uniquely identify different types of publications within the same frequency category
## for example, weekly publications in Pakistan: `01` for IDSR, `02` for FELTP.
# serial_id: assign a unique identifier to each individual publication

meta_r <- meta_r %>%
  group_by(source_cat, iso3c) %>%
  mutate(
    num_types = n_distinct(metadata_description),
    serial_cat_num = match(metadata_description, unique(metadata_description))
  ) %>%
  ungroup()

# serial_id
meta_r <- meta_r %>%
  group_by(source_cat, iso3c, serial_cat_num) %>%
  arrange(year) %>%
  mutate(
    serial_id = ifelse(serial_cat == "Y", 0:n(), serial_id),
    # period = indicating the years of the report (for the same report type)
    period = ifelse(
      min(as.numeric(year), na.rm = T) < max(as.numeric(year2), na.rm = T),
      paste0(
        min(as.numeric(year), na.rm = T),
        max(as.numeric(year2), na.rm = T)
      ),
      paste0(year)
    )
  ) %>%
  ungroup()

meta_r <- meta_r %>%
  mutate(
    sourceID = paste0(
      source_cat, "-", iso3c, "-", period, "-", serial_cat,
      sprintf("%02d", as.numeric(serial_cat_num))
    )
  ) %>%
  mutate(
    UUID = ifelse(
      !year == year2,
      paste0(sourceID, "-", sprintf("%02d", as.numeric(serial_id))),
      paste0(
        source_cat, "-", iso3c, "-", year, "-", serial_cat,
        sprintf("%02d", as.numeric(serial_cat_num)), "-",
        sprintf("%02d", as.numeric(serial_id))
      )
    )
  )


# checking if any UUIDs have been changed since the last release
f_old <- read_xlsx(paste0(dev_path, "archive/filingDB_allV_V1.2.2.xlsx")) %>%
  filter(released == "Y") %>%
  rename(UUID_old_v = UUID)

f_all <- merge(
  f_old[, c("UUID_old_v", "standard_filename")],
  meta_r,
  by = c("standard_filename"),
  all = T
)

f_all %>%
  filter(UUID_old_v != UUID) %>%
  select(UUID_old_v, UUID)

filename_to_change <- f_all %>%
  filter(UUID_old_v != UUID) %>%
  filter(grepl("COL|MEX", UUID)) %>%
  select(UUID_old_v, UUID)



# updated UUID -- data scraped again from some existing sources
# so likely include some duplicated records -- double check
f_all %>%
  filter(UUID_old_v != UUID) %>%
  filter(!grepl("COL|MEX", UUID)) %>%
  pull(UUID)

# [1] "MOH-AUS-19912025-Y01-00" "MOH-HKG-19942025-Y01-00" "MOH-IND-20152024-Y01-00" "MOH-SGP-20122024-Y02-00" "MOH-TWN-19982024-Y01-00"
# will be revisited later

# !!!! check if any duplicated UUID
meta_r %>%
  group_by(UUID) %>%
  tally() %>%
  filter(n > 1)

plyr::count(is.na(meta_r$standard_filename))


###### CASE DEFINITION =========================================

cdf <- rbind(cdf, f3_cdf)

plyr::count(meta_r$standard_filename %in% cdf$standard_filename) # should be all TRUE
meta_r$standard_filename[!meta_r$standard_filename %in% cdf$standard_filename]

meta_r <- merge(meta_r, cdf, by = c("standard_filename"), all.x = T)

# check any NAs in case definition
summary(is.na(meta_r$case_definition_original))
unique(meta_r$sourceID[is.na(meta_r$case_definition_original)])

meta_r <- meta_r %>%
  select(
    version,
    UUID,
    released,
    source_cat,
    country,
    period,
    case_definition_original,
    case_definition_standardised,
    NOTES,
    original_filename,
    standard_filename,
    UUID_old,
    metadata_description:metadata_steps
  )


# data not included in the release
cdf_nr <- cdf %>%
  filter(!standard_filename %in% meta_r$standard_filename)

meta_nr <- m %>%
  filter(released == "N") %>%
  mutate(source_cat = gsub("_", "", source_cat)) %>%
  mutate(period = gsub("_|-", "", year)) %>%
  merge(., cdf_nr, by = c("standard_filename"), all.x = T) %>%
  mutate(UUID = NA) %>%
  select(
    version,
    UUID,
    released,
    source_cat,
    country,
    period,
    case_definition_original,
    case_definition_standardised,
    NOTES,
    original_filename,
    standard_filename,
    UUID_old,
    metadata_description:metadata_steps
  )

names(meta_r)
names(meta_nr)

meta_all <- rbind(meta_r, meta_nr) %>% arrange(UUID, country, period)


# attach UUID to data
data1 <- merge(
  data[!is.na(data$UUID_old), ],
  meta_all[, c("UUID_old", "case_definition_standardised", "UUID")],
  by = c("UUID_old"),
  all.x = T
) %>%
  mutate(
    UUID = ifelse(
      grepl("zero", source_cat_old),
      paste0(UUID, " (Zero filling)"),
      ifelse(
        grepl("imputed", source_cat_old),
        paste0(UUID, " (Imputed)"),
        paste0(UUID)
      )
    )
  ) %>%
  select(-UUID_old, -standard_filename, -original_filename, -source_cat_old)

data2 <- merge(
  data[is.na(data$UUID_old), ],
  meta_all[, c("standard_filename", "case_definition_standardised", "UUID")],
  by = c("standard_filename"),
  all.x = T
) %>%
  mutate(
    UUID = ifelse(
      grepl("zero", source_cat_old),
      paste0(UUID, " (Zero filling)"),
      ifelse(
        grepl("imputed", source_cat_old),
        paste0(UUID, " (Imputed)"),
        paste0(UUID)
      )
    )
  ) %>%
  select(-UUID_old, -standard_filename, -original_filename, -source_cat_old)


data2[is.na(data2$case_definition_standardised)]


names(data1)
names(data2)
summary(is.na(data1$case_definition_standardised))
summary(is.na(data2$case_definition_standardised))
summary(is.na(data1$UUID))
summary(is.na(data2$UUID))

master <- rbind(data1, data2)

summary(master$dengue_total)
summary(is.na(master))

unique(master$adm_0_name[is.na(master$dengue_total)])
unique(master$UUID[is.na(master$dengue_total)])

# missing dengue_total - manual edits
master$dengue_total[
  master$adm_0_name == "China" & is.na(master$dengue_total)
] <- 62

# remove NAs from Philippines, India, Haiti
master <- master[!is.na(master$dengue_total)]

master[master$dengue_total < 0, ]
summary(master$dengue_total) # !!! Check any NAs / abnormal values

# check any duplicated rows
dup <- master %>%
  group_by(
    adm_0_name,
    adm_1_name,
    adm_2_name,
    calendar_start_date,
    calendar_end_date,
    dengue_total,
    UUID
  ) %>%
  filter(n() > 1)
nrow(dup) # 114 (PICs)

unique(dup$UUID)

# original PSSS weekly reports date error (w5: 1/29-2/4 and w6: 1/30-2/5)
# dates fix resulted in duplicated counts
# this is going to be addressed below

# handling potentiall duplicates from the existing data sources
# Some data sources include the full historical range (e.g. 1991–2025) even when updating only recent years
# This can result in duplicates for overlapping periods (e.g. 2023-2025)
# To avoid this, keep only the row with the highest UUID suffix (e.g. "-01", "-02") per group
check_uuid_list <- c(
  "MOH-AUS-19912025-Y01-00", "MOH-HKG-19942025-Y01-00",
  "MOH-IND-20152024-Y01-00", "MOH-SGP-20122024-Y02-00",
  "MOH-TWN-19982024-Y01-00"
)

# Extract base prefixes (excluding the last "-00")
uuid_prefixes <- sub("-\\d{2}$", "", check_uuid_list)

# Extract rows matching those prefixes
target_rows <- master %>%
  filter(grepl(paste(uuid_prefixes, collapse = "|"), UUID))

# Add a column with the numeric suffix extracted from UUID
target_rows <- target_rows %>%
  mutate(uuid_suffix = as.integer(stringr::str_extract(UUID, "(?<=-)\\d{2}$")))

# For each group, keep the row with the highest UUID suffix (latest version data)
deduped_rows <- target_rows %>%
  group_by(adm_0_name, adm_1_name, adm_2_name, calendar_start_date, calendar_end_date) %>%
  slice_max(order_by = uuid_suffix, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-uuid_suffix)

# Remove all the original duplicates from master
master <- master %>%
  anti_join(target_rows, by = colnames(master)) %>%
  bind_rows(deduped_rows)

# check again
lapply(uuid_prefixes, function(prefix) {
  master %>%
    filter(grepl(prefix, UUID)) %>%
    group_by(adm_0_name, adm_1_name, adm_2_name, calendar_start_date, calendar_end_date) %>%
    filter(n() > 1)
})


master2 <- master %>%
  group_by(
    adm_0_name,
    adm_0_code,
    adm_1_name,
    adm_1_code,
    adm_2_name,
    adm_2_code,
    calendar_start_date,
    calendar_end_date,
    dengue_total,
    UUID
  ) %>%
  distinct() %>%
  ungroup()

# standardise country names
master2$adm_0_name <- toupper(master2$adm_0_name)
unique(master2$adm_0_name)

master2 <- master2 %>%
  filter(!adm_0_name == "OTHER CARIBBEAN ISLANDS")

master2 <- master2 %>%
  mutate(adm_0_name = recode(adm_0_name,
    "VIETNAM" = "VIET NAM",
    "WALLIS & FUTUNA" = "WALLIS AND FUTUNA",
    "PITCAIRN ISLANDS" = "PITCAIRN",
    "PNG" = "PAPUA NEW GUINEA",
    "PAPUA NE GUINEA" = "PAPUA NEW GUINEA",
    "ST. LUCIA" = "SAINT LUCIA",
    "ST. MARTIN" = "SAINT MARTIN",
    "BONAIRE" = "BONAIRE, SAINT EUSTATIUS AND SABA",
    "LAO PEOPLE'S DEM. REP." = "LAO PEOPLE'S DEMOCRATIC REPUBLIC",
    "LAOS" = "LAO PEOPLE'S DEMOCRATIC REPUBLIC",
    "FSM" = "MICRONESIA (FEDERATED STATES OF)",
    "MICRONESIA (FED. STATES OF)" = "MICRONESIA (FEDERATED STATES OF)",
    "N MARIANA IS" = "NORTHERN MARIANA ISLANDS",
    "NORTHERN MARIANA ISLANDS (COMMONWEALTH OF THE)" = "NORTHERN MARIANA ISLANDS",
    "ECUARDOR" = "ECUADOR",
    "VIRGIN ISLANDS (USA)" = "VIRGIN ISLANDS (US)",
    "ST. KITTS AND NEVIS" = "SAINT KITTS AND NEVIS",
    "ST. VINCENT" = "SAINT VINCENT AND THE GRENADINES",
    "ANTIGUA" = "ANTIGUA AND BARBUDA",
    "BRITISH VIRGIN ISLANDS" = "VIRGIN ISLANDS (UK)",
    "MARSHALL ISLANDS (THE)" = "MARSHALL ISLANDS",
    "NORTHERN MARIANA ISLANDS (THE)" = "NORTHERN MARIANA ISLANDS"
  ))


# checking country-years with no data
no_data <- master2 %>%
  mutate(
    Year = year(calendar_start_date)
  ) %>%
  group_by(adm_0_name, Year) %>%
  tally() %>%
  ungroup() %>%
  complete(adm_0_name, Year, fill = list(n = 0)) %>%
  filter(Year > 1989 & n == 0)

library(countrycode)
no_data$ISO_A0 <- countrycode::countrycode(
  sourcevar = no_data$adm_0_name,
  origin = "country.name",
  destination = "iso3c"
)

unique(no_data$adm_0_name[is.na(no_data$ISO_A0)])
no_data$ISO_A0[no_data$adm_0_name == "SAINT MARTIN"] <- "MAF"

# merge dengue explorer data
# https://ntdhq.shinyapps.io/dengue5/
de <- read.csv(paste0(dev_path, "open_dengue_1.3/source_files/original_name/dengue_explorer_all_countries.csv"))

de_uuid <- meta_all$UUID[meta_all$standard_filename == "dengue_explorer_all_countries.csv"]

de <- de %>%
  transmute(
    adm_0_name = Country,
    Year,
    dengue_total = Cases
  ) %>%
  filter(!is.na(dengue_total))

# Netherlands Antilles: Curacao, Sint Maarten, Bonaire Saba Sint Eustatius
na_rows <- de %>%
  filter(adm_0_name == "Netherlands Antilles" & dengue_total == 0)

territories <- c("Curacao", "Sint Maarten", "Bonaire, Saint Eustatius and Saba")

expanded_rows <- na_rows %>%
  slice(rep(1:n(), each = length(territories))) %>%
  mutate(
    adm_0_name = rep(territories, times = nrow(na_rows)),
    dengue_total = 0
  )

de <- de %>%
  filter(!(adm_0_name == "Netherlands Antilles")) %>%
  bind_rows(expanded_rows)

de$ISO_A0 <- countrycode::countrycode(
  sourcevar = de$adm_0_name,
  origin = "country.name",
  destination = "iso3c"
)

# filter only the rows from de that correspond to missing country-years in no_data
de_useful <- de %>%
  semi_join(no_data, by = c("ISO_A0", "Year"))

unique(de_useful$adm_0_name)

# remove non-endemic countries
de_useful <- de_useful %>%
  filter(!adm_0_name %in% c("Japan", "New Zealand", "Mongolia"))

de_useful <- de_useful %>%
  merge(., no_data %>% select(ISO_A0, adm_0_name),
    by = c("ISO_A0"),
    suffix = c("", "_corrected"), all.x = T
  ) %>%
  mutate(adm_0_name = coalesce(adm_0_name_corrected, adm_0_name)) %>%
  select(-adm_0_name_corrected) %>%
  distinct()

de_useful_clean <- de_useful %>%
  transmute(
    adm_0_name,
    adm_0_code = NA,
    adm_1_name = NA,
    adm_1_code = NA,
    adm_2_name = NA,
    adm_2_code = NA,
    calendar_start_date = paste0(Year, "-01-01"),
    calendar_end_date = paste0(Year, "-12-31"),
    dengue_total,
    raw_data = "V1_3_dengue_explorer_all_countries.csv",
    case_definition_standardised = "Suspected and confirmed",
    UUID = de_uuid
  )

master3 <- rbind(master2, de_useful_clean)
summary(is.na(master3))
master3[master3$dengue_total < 0, ]
summary(master3$dengue_total) # !!! Check any NAs / abnormal values

# check any duplicated rows
master3 %>%
  group_by(
    adm_0_name,
    adm_1_name,
    adm_2_name,
    calendar_start_date,
    calendar_end_date,
    dengue_total,
    UUID
  ) %>%
  filter(n() > 1)


# save master DB
# !!! if this is the final release, update the file name with Version
filename_today <- paste0(git_path, "data/raw_data/masterDB_", today, ".csv")

if (release) {
  filename_md <- gsub(today, "V1.3", filename_today)
} else {
  filename_md <- filename_today
}

write.csv(master3, filename_md, row.names = F)



# save filing DB
# !!! if this is the final release, update the file name with the version name
filename_today <- paste0(dev_path, "archive/filingDB_allV_", today, ".xlsx")

if (release) {
  filename_fd <- gsub(today, "V1.3", filename_today)
} else {
  filename_fd <- filename_today
}

writexl::write_xlsx(meta_all, filename_fd)


# Publish source data (UUIDs are only assigned for the released data)
meta_public <- meta_r %>%
  select(
    "UUID",
    "source_cat",
    "country",
    "period",
    "case_definition_original",
    "metadata_description",
    "metadata_url",
    "metadata_steps"
  ) %>%
  arrange(desc(source_cat), country, period)

summary(is.na(meta_public))

# save source data
### !!! check filing DB first
# !!! if this is the final release, update the file name with Version
filename_today <- paste0(git_path, "data/raw_data/sourcedata_", today, ".csv")

if (release) {
  filename_sd <- gsub(today, "V1.3", filename_today)
} else {
  filename_sd <- filename_today
}

write.csv(meta_public, filename_sd, row.names = F)



# rename source files --------------------
library(tools)

rename_f_list <- meta_all[meta_all$version == "V1.3" & meta_all$released == "Y", ]
rename_f_list <- rename_f_list %>% select(UUID, standard_filename)
rename_f_list$current_name <- paste0(dev_path, "open_dengue_1.3/source_files/", rename_f_list$standard_filename)
rename_f_list$old_name <- paste0(dev_path, "open_dengue_1.3/source_files/original_name/", rename_f_list$standard_filename)
rename_f_list$UUID_with_ext <- paste0(dev_path, "open_dengue_1.3/source_files/UUID/", rename_f_list$UUID, ".", file_ext(rename_f_list$old_name))

summary(is.na(rename_f_list))

# Initialise status column
rename_f_list$status <- NA


for (i in 1:nrow(rename_f_list)) {
  if (file.exists(rename_f_list$current_name[i])) {
    original_path <- rename_f_list$old_name[i]
    UUID_path <- rename_f_list$UUID_with_ext[i]

    # Copy files only if either of the destinations doesn't already exist
    if (!file.exists(UUID_path) || !file.exists(original_path)) {
      if (!file.exists(original_path)) {
        file.copy(rename_f_list$current_name[i], original_path)
      }

      if (!file.exists(UUID_path)) {
        file.copy(rename_f_list$current_name[i], UUID_path)
      }

      rename_f_list$status[i] <- "Copied"
    } else {
      rename_f_list$status[i] <- "Already exists"
    }
  } else {
    print(paste("File Not found:", basename(rename_f_list$standard_filename[i])))
    rename_f_list$status[i] <- "Not found"
  }
}




# copy source files from other git repos
rename_f_list_git <- rename_f_list %>%
  filter(status == "Not found")

path <- "C:/Users/AhyoungLim/Dropbox/WORK/OpenDengue/"

# WHO global dashboard
rename_f_list_git <- rename_f_list_git %>%
  mutate(current_name2 = ifelse(grepl("global-data", standard_filename),
    paste0(path, "DengueCrawler/Downloads/", standard_filename), NA
  ))

# WHO SEARO
rename_f_list_git <- rename_f_list_git %>%
  mutate(current_name2 = ifelse(grepl("SEARO|Indonesia", standard_filename),
    paste0(path, "SEARO-crawler/output/", standard_filename), current_name2
  ))

# PAHO crawler
rename_f_list_git <- rename_f_list_git %>%
  mutate(current_name2 = ifelse(grepl("paho_for_opendengue", standard_filename),
    paste0(path, "PAHO-cum-inc/transformed_data/", standard_filename), current_name2
  ))

# PAHO crawler
rename_f_list_git <- rename_f_list_git %>%
  mutate(current_name2 = ifelse(grepl("PAHO_2014_2024", standard_filename),
    paste0(path, "PAHO-crawler/data/OD_DL_20250513/", standard_filename), current_name2
  ))


unique(rename_f_list_git$current_name2)



for (i in 1:nrow(rename_f_list_git)) {
  if (file.exists(rename_f_list_git$current_name2[i])) {
    original_path <- rename_f_list_git$old_name[i]
    UUID_path <- rename_f_list_git$UUID_with_ext[i]

    # Copy files only if either of the destinations doesn't already exist
    if (!file.exists(UUID_path) || !file.exists(original_path)) {
      if (!file.exists(original_path)) {
        file.copy(rename_f_list_git$current_name2[i], original_path)
      }

      if (!file.exists(UUID_path)) {
        file.copy(rename_f_list_git$current_name2[i], UUID_path)
      }

      rename_f_list_git$status[i] <- "Copied"
    } else {
      rename_f_list_git$status[i] <- "Already exists"
    }
  } else {
    print(paste("File Not found:", basename(rename_f_list_git$standard_filename[i])))
    rename_f_list_git$status[i] <- "Not found"
  }
}

# for some mexico source files...
rename_f_list <- meta_all[meta_all$country == "Mexico" & meta_all$released == "Y", ]
rename_f_list <- rename_f_list %>% select(UUID, standard_filename)
rename_f_list$current_name <- paste0(dev_path, "open_dengue_1.3/source_files/", rename_f_list$standard_filename)
rename_f_list$old_name <- paste0(dev_path, "open_dengue_1.3/source_files/original_name/", rename_f_list$standard_filename)
rename_f_list$UUID_with_ext <- paste0(dev_path, "open_dengue_1.3/source_files/UUID/", rename_f_list$UUID, ".", file_ext(rename_f_list$old_name))

rename_f_list$status <- NA


for (i in 1:nrow(rename_f_list)) {
  if (file.exists(rename_f_list$current_name[i])) {
    original_path <- rename_f_list$old_name[i]
    UUID_path <- rename_f_list$UUID_with_ext[i]

    # Copy files only if either of the destinations doesn't already exist
    if (!file.exists(UUID_path) || !file.exists(original_path)) {
      if (!file.exists(original_path)) {
        file.copy(rename_f_list$current_name[i], original_path)
      }

      if (!file.exists(UUID_path)) {
        file.copy(rename_f_list$current_name[i], UUID_path)
      }

      rename_f_list$status[i] <- "Copied"
    } else {
      rename_f_list$status[i] <- "Already exists"
    }
  } else {
    print(paste("File Not found:", basename(rename_f_list$standard_filename[i])))
    rename_f_list$status[i] <- "Not found"
  }
}



s <- list.files(path = paste0(dev_path, "open_dengue_1.3/source_files/"))
o <- list.files(path = paste0(dev_path, "open_dengue_1.3/source_files/original_name"))
uuid <- list.files(path = paste0(dev_path, "open_dengue_1.3/source_files/UUID"))

length(o) == length(uuid)

file_status <- data.frame(
  filename = unique(c(s, o, uuid)),
  in_source = unique(c(s, o, uuid)) %in% s,
  in_original = unique(c(s, o, uuid)) %in% o
)

file_status %>% arrange(filename)
file_status %>% filter(in_source == TRUE & in_original == FALSE)
file_status %>% filter(in_source == FALSE & in_original == TRUE)


# source files in the Dropbox:
dropbox_path <- "C:/Users/AhyoungLim/Dropbox/WORK/OpenDengue/11_DENData/01_Dengue_data/source_files/UUID/"

# Get a list of all files in the directory
all_files <- list.files(path = dropbox_path, full.names = FALSE)

# Rename files based on matched base names
for (i in seq_len(nrow(filename_to_change))) {
  matches <- all_files[tools::file_path_sans_ext(all_files) == filename_to_change$UUID_old_v[i]]

  if (length(matches) == 1) {
    extension <- tools::file_ext(matches)
    new_file <- paste0(dropbox_path, filename_to_change$UUID[i], ".", extension)
    file.rename(from = paste0(dropbox_path, matches), to = new_file)
  } else if (length(matches) > 1) {
    warning(paste("Multiple matches found for:", filename_to_change$UUID_old_v[i]))
  } else {
    warning(paste("File not found for base name:", filename_to_change$UUID_old_v[i]))
  }
}
