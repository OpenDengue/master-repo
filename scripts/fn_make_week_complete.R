library(lubridate)

# Function to calculate the total number of epiweeks in a year
EpiweekCounter <- function(year) {
  start_date <- as.Date(paste(year, "01-01", sep = "-"))
  end_date <- as.Date(paste(year + 1, "01-01", sep = "-"))

  # Create a sequence of dates for the entire year
  dates <- seq(start_date, end_date, by = "day")

  # Calculate epiweeks for each date
  epiweeks <- sapply(dates, epiweek)

  # Delete week 53 of the previous years
  idx <- which(epiweeks == 1)[[1]] # find the index of the first week 1
  # Generate a sequence of indices for the vector
  all_idx <- seq_len(length(epiweeks))

  # Remove items with indices lower than index_of_first_one
  epiweeks <- epiweeks[all_idx[idx:length(all_idx)]]

  # Count unique epiweeks
  unique_epiweeks <- length(unique(epiweeks))


  return(unique_epiweeks)
}

len <- (max_year + 1) - 1923
nweeks <- vector(mode = "numeric", length = len)
for (i in 1:len) {
  nweeks[i] <- EpiweekCounter(i + 1923)
}

epiw <- cbind(data.frame(Year = 1924:(max_year + 1)), nweeks)



WeeklyDateSequence <- function(start_year, end_year) {
  nweeks <- epiw$nweeks[epiw$Year == end_year]

  start_date <- EpiWeek::epiweekToDate(start_year, 1)$d0
  end_date <- EpiWeek::epiweekToDate(end_year, nweeks)$d1

  all_dates <- data.frame(calendar_start_date = as.character(seq(as.Date(start_date),
    as.Date(end_date),
    by = 7
  )))
  all_dates$time_seq <- 1:nrow(all_dates)
  all_dates$week <- epiweek(as.Date(all_dates$calendar_start_date))

  return(all_dates)
}


# make time series sequence for weekly data
make_week_complete <- function(data, keep_vars = FALSE) {
  dt_subset <- data %>%
    group_by(adm_0_name, Year) %>%
    group_split()

  # loop through each country-year and save into a list
  dt_subset_new <- list()

  for (i in 1:length(dt_subset)) { # loop through each year
    dat <- dt_subset[[i]]
    current_year <- unique(dat$Year)

    # make a template weekly data series
    nweeks <- epiw$nweeks[epiw$Year == current_year] # total number of weeks per year

    start_date <- EpiWeek::epiweekToDate(current_year, 1)$d0
    end_date <- EpiWeek::epiweekToDate(current_year, nweeks)$d1

    all_dates <- data.frame(calendar_start_date = as.character(seq(as.Date(start_date),
      as.Date(end_date),
      by = 7
    )))

    # all_dates$time_seq <- 1:nrow(all_dates)
    all_dates$week <- epiweek(as.Date(all_dates$calendar_start_date))

    # merge with original data and save it into a list
    dat$week <- epiweek(as.Date(dat$calendar_start_date))
    dat$calendar_start_date <- as.character(dat$calendar_start_date)
    dat <- merge(dat, all_dates, by = c("calendar_start_date", "week"), all.y = T)

    # Handle missing values
    cols_to_fill <- c("adm_0_name", "Year")
    for (col in cols_to_fill) {
      dat[[col]][is.na(dat[[col]])] <- first(na.omit(dat[[col]]))
    }

    cat(paste("\r inner loop completed", i))


    dt_subset_new[[i]] <- as.data.frame(dat)
  }

  # merge all country-years
  data_new <- rbindlist(dt_subset_new)

  return(data_new)
}
