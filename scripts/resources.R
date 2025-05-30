dt_heatmap <- read.csv("heatmap.csv")

region_lookup <- dt_heatmap %>%
  select(region, adm_0_name) %>%
  distinct() %>%
  mutate(adm_0_name = toupper(adm_0_name))


National <- read.csv("./data/releases/V1.3/National_extract_V1_3.csv") %>%
  merge(., region_lookup, by = c("adm_0_name"), all.x = T)

Temporal <- read.csv("./data/releases/V1.3/Temporal_extract_V1_3.csv") %>%
  merge(., region_lookup, by = c("adm_0_name"), all.x = T) %>%
  filter(!(adm_0_name == "BRAZIL" & S_res == "Admin2"))

Spatial <- Spatial <- read.csv("./data/releases/V1.3/Spatial_extract_V1_3.csv") %>%
  merge(., region_lookup, by = c("adm_0_name"), all.x = T) %>%
  filter(!(adm_0_name == "BRAZIL" & S_res == "Admin2"))


save_by_region <- function(dataType) {
  data <- get(dataType)
  data <- data %>% filter(!adm_0_name %in% c("CANADA", "MONGOLIA", "NEW ZEALAND"))
  print(any(is.na(data$region)))

  data$region <- as.character(data$region)

  # Get unique region names in the order they appear
  region_names <- unique(data$region)

  # Loop through each region and save the corresponding subset
  for (region in region_names) {
    subset <- data %>% filter(region == !!region)

    # Replace spaces with underscores in file names
    file_name <- paste0("assets/", dataType, "_extract_", region, "_V1_3.csv")

    write.csv(subset, file_name, row.names = FALSE)
  }
}

save_by_region("National")
save_by_region("Spatial")
save_by_region("Temporal")
