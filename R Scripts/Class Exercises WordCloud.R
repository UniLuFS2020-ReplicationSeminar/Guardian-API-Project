library(dplyr)
library(quanteda) # for text analysis
library(SnowballC) # for removing stopwords
library(lubridate) # for manipulating dates
library(rio)
library(quanteda.textplots) # for plotting wordcloud

# Import Syrian headlines and convert to corpus
syria_headlines_dataframe <- readRDS(file = "Data files/syrian_headlines_dataframe.rds")

###--- Create word cloud with key words from the corpus
current_corpus <- quanteda::corpus(syria_headlines_dataframe, text_field="headline_vector")

# Add more words to stop word dictionary
custom_dictionary <- stopwords("english")
custom_dictionary <- append(custom_dictionary,c("review", "video", "syria", "editorial"))

tokens_cleaned  <- tokens(current_corpus, remove_punct = TRUE, remove_numbers=TRUE, remove_symbols = TRUE, split_hyphens = TRUE, 
                          remove_separators = TRUE, remove_url=TRUE)

# Remove stopwords
no_stopwords <- tokens_remove(tokens_cleaned, custom_dictionary)

# Identify 400 most common bigrams and combine then e.g. David Cameron, Al-Assad etc.
toks_ngram <- tokens_ngrams(no_stopwords, n = 2:2, concatenator = " ")
bigrams_dfm <- dfm(toks_ngram)
list <- as.list(topfeatures(bigrams_dfm , 400))
names <- names(list)

combined_tokens_list <- phrase(names)
tokens_combined <- tokens_compound(no_stopwords,combined_tokens_list, case_insensitive=TRUE)

# Create document ferquency matrix and trim so that each term must be mentioned at least 3 times in total
cleaned_dfm <- dfm(tokens_combined) %>% 
  dfm_trim(min_termfreq = 3)

# Plot wordcloud
set.seed(123) # ensure get same wordcloud every time
syria_wordcloud <- textplot_wordcloud(cleaned_dfm , max_words=100, rotation= 0.25, color = RColorBrewer::brewer.pal(8,"Dark2"))


