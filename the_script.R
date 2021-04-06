# Libraries ---------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(stringr)
library(tidytext)
library(textdata)
library(textstem)

# Import data -------------------------------------------------------------

# Still didnt fix our character 
raw_data <- fread(here::here("data", "Tweets-1.csv"), integer64 = "character")

# Clean the data -----------------------------------------------------------

tidy_tweets <- 
  raw_data %>%
  select(-airline_sentiment_confidence, -airline_sentiment_gold, -negativereason_confidence, -negativereason_gold) %>%
  mutate(tweet_id = row_number(), 
         tweet_created = as.Date(tweet_created, "%m/%d/%Y %H:%M"),
         text = iconv(text, from = "UTF-8", to = "ASCII//TRANSLIT"),
         text = gsub(text, pattern = "[@]\\S\\w*", replacement = "")) %>%
  separate(tweet_coord, into = c("latitude", "longitude"), sep = ",") %>%
  mutate(latitude = gsub(latitude, pattern = "\\[", replacement = ""),
         longitude = gsub(longitude, pattern = "\\]", replacement = "")) %>%
  mutate(across(.cols = c("latitude", "longitude"), .fns = as.numeric))

str(tidy_tweets)


# Analysis ----------------------------------------------------------------


# Create custom stopwords
custom_stopwords <- tibble::tribble(~word,~lexicon,
                  "t.co", "custom",
                  "http", "custom")
# Get all stop words 
stop_words <- 
  stop_words %>%
  bind_rows(custom_stopwords)

# Unnest tokens 
# Remove stopwords
# Tally the words
tidy_tweets_tokens <- 
  tidy_tweets %>%
  select(text, tweet_id, airline) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  group_by(word) %>%
  tally() %>%
  ungroup

# Stemmed version of words
tidy_tweets_stem <- 
  tidy_tweets %>%
  select(text, tweet_id, airline, tweet_created) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  mutate(word = SnowballC::wordStem(word))

# Get the number of words in each tweet 
text_stats <- 
  tidy_tweets_stem %>%
  group_by(tweet_id) %>%
  tally()

# Tweet Stats
summary(text_stats$n)





# Lemmatized version of words 
tidy_tweets_lemma <-
  tidy_tweets %>%
  select(text, tweet_id, airline, tweet_created) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  mutate(word = lemmatize_words(word)) 

# All versions of words 
all_tokens <-
  tidy_tweets_tokens %>%
  left_join(tidy_tweets_stem, by = "word") 

# Quick sentiment test
tidy_tweets_sentiment <-
  tidy_tweets %>%
  select(text, tweet_id, airline) %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("afinn")) %>% 
  group_by(tweet_id) %>%
  summarise(tweet_sent = mean(value)) %>%
  ungroup() %>%
  left_join(tidy_tweets)

# How many of each airline we have
airline_check <- 
  tidy_tweets %>% 
  mutate(airline = tolower(airline)) %>%
  group_by(airline) %>%
  tally()