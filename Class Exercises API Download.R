library(httr)
library(stringr)
library(jsonlite)

#Store API key password
guardian_api_key <- rstudioapi::askForPassword()

# Create empty vectors to store publication dates and headlines
headline_vector <- vector()
date_vector <- vector()

for (j in 1:500){# loop through 500 pages of resulst to get all relevant headlines
  if (j%%12==0){Sys.sleep(1)}# Slow down request rate i line with API limit
  
  # Download all headlines containing search term "Syria" from 2011-present
  guardian_url_string <- str_c("https://content.guardianapis.com/search?page-size=50&page=", 
  j,
  "&from-date=2011-01-01&q=%22Syria%22&api-key=00198df8-cef5-4a18-b105-c5884070ff53")
  response <- httr::GET(guardian_url_string, query=list(q= "Syria", apiKey=guardian_api_key))
  content <- httr::content(response, as="parse")
  
    # Loop through all 50 entries per page extracting the publication date and headline
    for (i in 1:50){
      current_headline <- content$response$results[[i]]$webTitle[[1]]
      current_date <- content$response$results[[i]]$webPublicationDate
      headline_vector[50*(j-1)+i] <- current_headline
      date_vector[50*(j-1)+i] <- current_date
    }
}

# Export resulting vectors as R objects
saveRDS(date_vector, file = "syria_dates.rds")
saveRDS(headline_vector, file = "syria_headlines.rds")
