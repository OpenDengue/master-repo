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

# number of total weeks for each year
len = end_year - start_year + 1
nweeks <- vector(mode = "numeric", length = len)
for (i in 1:len) {
  nweeks[i] = EpiweekCounter(i + start_year - 1)
}

epiw <- cbind(data.frame(Year = start_year:end_year), nweeks)
# make a template weekly data series

get_std_wdates <- function(start_year, end_year) {
  start_date = EpiWeek::epiweekToDate(start_year, 1)$d0
  end_year_nweeks = epiw$nweeks[epiw$Year == end_year] # total number of weeks per year
  end_date = EpiWeek::epiweekToDate(end_year, end_year_nweeks)$d1

  all_dates = data.frame(
    std_start_date = as.character(seq(
      as.Date(start_date),
      as.Date(end_date),
      by = 7
    ))
  )
  all_dates$std_end_date = as.character(as.Date(all_dates$std_start_date) + 6)
  all_dates$year <- year(as.Date(all_dates$std_start_date))
  all_dates$month <- month(as.Date(all_dates$std_start_date))
  all_dates$week <- epiweek(as.Date(all_dates$std_start_date))
  all_dates$std_date = paste0(
    all_dates$std_start_date,
    "_",
    all_dates$std_end_date
  )

  all_dates$year <- ifelse(
    all_dates$month == 12 & all_dates$week == 1,
    all_dates$year + 1,
    all_dates$year
  )
  return(all_dates)
}
