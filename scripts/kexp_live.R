#install.packages("tuber")
library(tuber)
library(dplyr)
library(purrr)


# 1. Authenticate with your Google API Credentials
yt_oauth("id", "secret code")



# Check the exact column names available
print(colnames(kexp_videos))




# 1. Pull playlist history silently
kexp_videos <- suppressMessages(suppressWarnings(get_playlist_items(
  filter = c(playlist_id = "UU3I2GFN_F8WudD_2jUZbojA"), 
  part = "snippet", 
  max_results = 40000 
)))

# 2. Filter for specified columns and "Full Performance"
live_performances <- kexp_videos %>%
  select(
    published_at = snippet.publishedAt,
    title = snippet.title,
    description = snippet.description,
    channel_title = snippet.channelTitle,
    kind = snippet.resourceId.kind,
    video_owner = snippet.videoOwnerChannelTitle,
    video_id = snippet.resourceId.videoId
  ) %>%
  filter(grepl("Full Performance", title, ignore.case = TRUE))

# 3. Initialize views column and set output filename
live_performances$views <- NA
output_file <- "kexp_live_performances.csv"

# 4. Loop through rows to get views safely, delaying and saving in batches
for (i in 1:nrow(live_performances)) {
  # Sleep for 0.2 seconds (~5 requests per second)
  Sys.sleep(0.2)
  
  vid_id <- live_performances$video_id[i]
  
  # tryCatch prevents the loop from crashing if a single video fetch fails
  stats <- tryCatch(get_stats(video_id = vid_id), error = function(e) NULL)
  
  if (!is.null(stats)) {
    live_performances$views[i] <- as.numeric(stats$viewCount)
  }
  
  # Save to CSV every 100 rows
  if (i %% 100 == 0) {
    write.csv(live_performances, output_file, row.names = FALSE)
  }
}

# 5. Final save to capture the remaining rows
write.csv(live_performances, output_file, row.names = FALSE)
