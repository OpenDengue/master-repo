# Define the function
Year_checker <- function(df) {
  
  # Select relevant columns and get distinct rows
  calendar_dates <- df %>%
    select(calendar_start_date, calendar_end_date, Year, T_res) %>%
    distinct()
  
  # Split data into Yearly, Monthly, and Weekly
  Y_dates <- calendar_dates %>% filter(T_res == "Year")
  M_dates <- calendar_dates %>% filter(T_res == "Month")
  W_dates <- calendar_dates %>% filter(T_res == "Week")
  
  # 1) Yearly matching
  day_check_yearly <- Y_dates %>%
    filter(!(month(calendar_start_date) == 1 & day(calendar_start_date) == 1) |
           !(month(calendar_end_date) == 12 & day(calendar_end_date) == 31))
  
  if (any(!year(Y_dates$calendar_start_date) == Y_dates$Year)) {
    print("Year mismatch found in Yearly data")
    print(day_check_yearly)

  } else {
    print("No year mismatch found in Yearly data")
  }
  
  # 2) Monthly matching
  day_check_monthly <- M_dates %>%
    filter(!(day(calendar_start_date) == 1) |
           !(day(calendar_end_date) %in% c(30, 31))) %>%
    filter(!(month(calendar_end_date) == 2 & day(calendar_end_date) > 27))
  

  if (any(!year(M_dates$calendar_start_date) == M_dates$Year)) {
    print("Year mismatch found in Monthly data")
    print(day_check_monthly)

  } else {
    print("No year mismatch found in Monthly data")
  }
  
  # 3) Weekly matching
  W_dates <- W_dates %>%
    mutate(week = epiweek(calendar_start_date))
  
  W_dates <- W_dates %>%
    filter(week == 1 | week >= 52)
  
  W_dates <- W_dates %>%
    mutate(Year_adj = ifelse(month(calendar_start_date) == 1 & year(calendar_start_date) == Year, FALSE,
                        ifelse(month(calendar_end_date) == 1 & year(calendar_end_date) == Year, FALSE, 
                              ifelse(month(calendar_start_date) == 12 & week >= 52, FALSE, TRUE))))
  
  df$Year <- ifelse(with(df, paste(calendar_start_date, calendar_end_date, Year)) %in%
                    with(W_dates[W_dates$Year_adj == TRUE,], paste(calendar_start_date, calendar_end_date, Year)),
                    df$Year + 1,
                    df$Year)
  # df_new <- subset(df, with(df, paste(calendar_start_date, calendar_end_date, Year)) %in%
  #                with(W_dates[W_dates$Year_adj == TRUE,], paste(calendar_start_date, calendar_end_date, Year)))
  
  if (nrow(W_dates[W_dates$Year_adj == TRUE,])>0) {
    print("Year mismatch found in Weekly data")
    print(W_dates[W_dates$Year_adj == TRUE,])
    # print(unique(df_new[, c("adm_0_name", "calendar_start_date", "calendar_end_date", "Year", "UUID")]))

  } else {
    print("No year mismatch found in Weekly data")
  }
  
  # Return the modified data frame
  return(df)
}

