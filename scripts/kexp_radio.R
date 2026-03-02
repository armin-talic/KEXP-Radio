library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)

cutoff_date <- Sys.time() - days(365)
# play_type=trackplay added to URL to filter out breaks at the server level
url <- "https://api.kexp.org/v2/plays/?format=json&limit=100&play_type=trackplay" 
output_file <- "kexp_plays_past_year.csv"

buffer_df <- data.frame()
keep_fetching <- TRUE
total_saved <- 0

while(keep_fetching && !is.null(url)) {
  Sys.sleep(0.5)
  
  response <- GET(url)
  
  if (status_code(response) == 200) {
    data <- fromJSON(content(response, "text", encoding = "UTF-8"), flatten = TRUE)
    
    if (length(data$results) == 0) break
    
    # Select columns and apply a secondary local filter just in case
    plays_df <- data$results %>%
      select(any_of(c(
        "airdate", "artist", "song", "album", "play_type",
        "comment", "is_live", "release_date", "show.program"
      ))) %>%
      filter(play_type == "trackplay") 
    
    plays_df <- plays_df %>%
      mutate(airdate_parsed = as.POSIXct(airdate, format="%Y-%m-%dT%H:%M:%S", tz="UTC")) %>%
      filter(!is.na(airdate_parsed)) %>% 
      filter(airdate_parsed >= cutoff_date) %>%
      select(-airdate_parsed) 
    
    buffer_df <- bind_rows(buffer_df, plays_df)
    
    if (nrow(buffer_df) >= 1000) {
      write.table(buffer_df, output_file, sep = ",", row.names = FALSE, 
                  col.names = !file.exists(output_file), append = TRUE)
      total_saved <- total_saved + nrow(buffer_df)
      buffer_df <- data.frame() 
    }
    
    valid_dates <- as.POSIXct(data$results$airdate, format="%Y-%m-%dT%H:%M:%S", tz="UTC")
    min_date_in_batch <- min(valid_dates, na.rm = TRUE)
    
    if (!is.infinite(min_date_in_batch) && min_date_in_batch < cutoff_date) {
      keep_fetching <- FALSE
    } else {
      url <- data$`next` 
    }
    
  } else {
    break
  }
}

if (nrow(buffer_df) > 0) {
  write.table(buffer_df, output_file, sep = ",", row.names = FALSE, 
              col.names = !file.exists(output_file), append = TRUE)
  total_saved <- total_saved + nrow(buffer_df)
}