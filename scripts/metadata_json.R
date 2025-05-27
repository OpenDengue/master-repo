library(dplyr)
library(readr)
library(jsonlite)

dt_heatmap <- read.csv("src/heatmap.csv")

region_lookup <- dt_heatmap %>%
  select(region, adm_0_name) %>%
  distinct() %>%
  mutate(adm_0_name = toupper(adm_0_name))

# List all CSV files
csv_files <- list.files("data/releases/V1.3/", pattern = "_extract_V1_3\\.csv$", full.names = TRUE)

metadata_list <- lapply(csv_files, function(file_path) {
  file_name <- basename(file_path)
  match <- regmatches(file_name, regexec("^(.*?)_extract_V1_3\\.csv$", file_name))[[1]]

  if (length(match) != 2) {
    cat("Skipping (no match):", file_name, "\n")
    return(NULL)
  }

  data_type <- match[2]

  df <- tryCatch(read_csv(file_path, show_col_types = FALSE), error = function(e) {
    cat("Error reading:", file_name, "-", e$message, "\n")
    return(NULL)
  })

  df <- df %>%
    left_join(region_lookup, by = "adm_0_name") %>%
    select(adm_0_name, calendar_start_date, calendar_end_date, region)

  if (is.null(df)) {
    return(NULL)
  }
  if (!all(c("adm_0_name", "calendar_start_date", "calendar_end_date") %in% names(df))) {
    cat("Missing required columns in", file_name, "\n")
    return(NULL)
  }

  df <- df %>%
    mutate(
      start_date = as.Date(calendar_start_date),
      end_date = as.Date(calendar_end_date)
    ) %>%
    group_by(adm_0_name, region) %>%
    summarise(
      dataType = data_type,
      start_date = format(min(start_date), "%Y-%m-%d"),
      end_date = format(max(end_date), "%Y-%m-%d"),
      .groups = "drop"
    )

  return(df)
})

# Combine and write JSON
metadata_df <- bind_rows(metadata_list)
write_json(metadata_df, "src/assets/metadata.json", pretty = TRUE, auto_unbox = TRUE)
