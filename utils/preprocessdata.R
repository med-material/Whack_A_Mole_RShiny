PreprocessGlobalData <- function(df) {
  df_new <- df %>% 
    dplyr::mutate(Timestamp = as.POSIXct(Timestamp.Event, format = "%Y-%m-%d %H:%M:%OS")) %>% 
    arrange(Timestamp) %>%
    dplyr::mutate(row_duration = as.numeric(difftime(Timestamp, lag(Timestamp)),units="secs"))
  return(df_new)
}
