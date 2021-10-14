library(tidyverse)
library(lubridate)
library(scales)

#install.packages("rtweet")
library(rtweet)

#doc
#https://cran.r-project.org/web/packages/rtweet/vignettes/auth.html
#https://cran.r-project.org/web/packages/rtweet/rtweet.pdf
#https://cran.r-project.org/web/packages/rtweet/vignettes/intro.html

api_key <- "xxx"
api_secret_key <- "xx"

token <- create_token(
  app = "laurencefirstapp",
  consumer_key = api_key,
  consumer_secret = api_secret_key)

token

access_token <- "xxx"
access_token_secret <- "xxx"


token <- create_token(
  app = "laurencefirstapp",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

library(rtweet)
get_token()

rt <- search_tweets(
  "#pfizer", n = 500, include_rts = FALSE
)

pfizer <- as.data.frame(rt)
str(pfizer)

#install.packages('tidytext')
library(tidyverse)
library(tidytext)

pfizertxt <- pfizer$text

pfizertxt <- as.data.frame(pfizertxt)

View(pfizertxt)

#supprimer lignes vides

clean1 <- pfizertxt %>%
  select(pfizertxt)  %>%
  filter(!is.na(pfizertxt))

prepa <- clean1 %>%
  unnest_tokens(word, pfizertxt)

stopd <- data.frame(word = stop_words$word)

clean2 <- prepa %>%
  anti_join (stopd, by ="word")

countw <- clean2 %>%
  count(word, sort = TRUE) 

countw2 <- clean2 %>%
  count(word, sort = TRUE) %>%
  filter(n > 200) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
countw2

#install.packages('Rcpp')
library(Rcpp)

bigrams <- clean2 %>% 
  unnest_tokens(bigram, word, token = "ngrams", n = 2)
bigrams

library(textdata)

senti1 <- get_sentiments("bing")

senti2 <- get_sentiments("afinn")

senti3 <- get_sentiments("loughran")

senti4 <- get_sentiments("nrc")

sentiments1 <- clean2 %>%
  inner_join(senti1, by = "word")

sentiments_counts <- sentiments1 %>%
  count(sentiment) %>%
  arrange(-n)

negative_freqs <- sentiments_counts %>%
  filter(sentiment == "negative")

print(sentiments_counts)

sentiments1 %>%
  count(sentiment, sort = TRUE) %>%
  filter(n > 6) %>%
  mutate(sentiment = reorder(sentiment, n)) %>%
  ggplot(aes(sentiment, n)) +
  geom_col() +
  xlab(NULL) +
  geom_col(fill="dodgerblue4") +
  coord_flip() +
  theme_minimal()

sentiments2 <- clean2 %>%
  inner_join(senti2, by = "word")

sentiments2 %>%
  count(value, sort = TRUE) %>%
  filter(n > 6) %>%
  mutate(value = reorder(value, n)) %>%
  ggplot(aes(value, n)) +
  geom_col() +
  xlab(NULL) +
  geom_col(fill="dodgerblue4") +
  coord_flip() +
  theme_minimal()

sentiments3 <- clean2 %>%
  inner_join(senti3, by = "word")

sentiments3 %>%
  count(sentiment, sort = TRUE) %>%
  filter(n > 6) %>%
  mutate(sentiment = reorder(sentiment, n)) %>%
  ggplot(aes(sentiment, n)) +
  geom_col() +
  xlab(NULL) +
  geom_col(fill="dodgerblue4") +
  coord_flip() +
  theme_minimal()

sentiments4 <- clean2 %>%
  inner_join(senti4, by = "word")

sentiments4 <- clean2 %>%
  inner_join(senti4, by = "word")

sentiments4 %>%
  count(sentiment, sort = TRUE) %>%
  filter(n > 6) %>%
  mutate(sentiment = reorder(sentiment, n)) %>%
  ggplot(aes(sentiment, n)) +
  geom_col() +
  xlab(NULL) +
  geom_col(fill="dodgerblue4") +
  coord_flip() +
  theme_minimal()
