library(dplyr)
library(quanteda) # for text analysis
library(SnowballC) # for removing stopwords
library(lubridate)
library(rio)

# Import Syrian headlines and convert to corpus
syria_headlines_dataframe <- readRDS(file = "Data files/syrian_headlines_dataframe.rds")

#Create new column containing only year and month for grouping later
headlines_month <- syria_headlines_dataframe %>% 
  mutate(Month_Yr = format_ISO8601(datetime_vector, precision = "ym"))

#Identify important events with more than 200 headlines a month
top_headlines <- headlines_month %>% 
  group_by(Month_Yr) %>% 
  summarise(n=n()) %>% 
  filter(n>200) %>% 
  arrange(desc(n))

#Create vector containing months with more than 200 headlines
top_headline_dates <- top_headlines$Month_Yr
keywords_vector <- vector()

#Loop identifying the top 5 words for every month with more than 200 headlines
for (i in 1:length(top_headline_dates)){

current_date <- headlines_month %>% 
  filter(Month_Yr==top_headline_dates[i])

current_corpus <- quanteda::corpus(current_date, text_field="headline_vector")

# Add more words to stop word dictionary
custom_dictionary <- stopwords("english")
custom_dictionary <- append(custom_dictionary,c("review", "video", "syria", "editorial"))

tokens_cleaned  <- tokens(current_corpus, remove_punct = TRUE, remove_numbers=TRUE, remove_symbols = TRUE, split_hyphens = TRUE, 
                                remove_separators = TRUE, remove_url=TRUE)

no_stopwords <- tokens_remove(tokens_cleaned, custom_dictionary)

cleaned_dfm <- dfm(no_stopwords)
top_features <- topfeatures(cleaned_dfm , 5) 
list_features <- list(names(top_features)) # so results display correctly
keywords_vector[i] <- list_features

}

#Display keywords in dataframe
keywords_by_date <- data.frame(top_headline_dates)
keywords_by_date$keywords  <- keywords_vector

#Export keywords by date as csv
rio::export(keywords_by_date, "Data files/SyrianKeyWords.csv")

#Export keywords as R object
saveRDS(keywords_by_date, file = "Data files/keywords_by_date_dataframe.rds")

###--- Create worldcloud with key words from the corpus

current_corpus <- quanteda::corpus(current_date, text_field="headline_vector")

# Add more words to stop word dictionary
custom_dictionary <- stopwords("english")
custom_dictionary <- append(custom_dictionary,c("review", "video", "syria", "editorial"))

tokens_cleaned  <- tokens(current_corpus, remove_punct = TRUE, remove_numbers=TRUE, remove_symbols = TRUE, split_hyphens = TRUE, 
                          remove_separators = TRUE, remove_url=TRUE)

no_stopwords <- tokens_remove(tokens_cleaned, custom_dictionary)

cleaned_dfm <- dfm(no_stopwords)
top_features <- topfeatures(cleaned_dfm , 5) 


                             