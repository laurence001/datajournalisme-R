#install.packages(“rvest”)
library(rvest)

#Pour augmenter la mémoire
getOption("max.print")
options(max.print=9999999)

jo <- read_html("https://fr.wikipedia.org/wiki/Sportifs_les_plus_m%C3%A9daill%C3%A9s_aux_Jeux_olympiques")
jo

#plusieurs URL's
#à la main
#urls <- c("https://fr.wikipedia.org/wiki/Sportifs_les_plus_m%C3%A9daill%C3%A9s_aux_Jeux_olympiques",
         # "https://fr.wikipedia.org/wiki/Sportifs_les_plus_m%C3%A9daill%C3%A9s_aux_Jeux_olympiques2",
          #"https://fr.wikipedia.org/wiki/Sportifs_les_plus_m%C3%A9daill%C3%A9s_aux_Jeux_olympiques3")


texte <- jo %>%
  html_nodes("p") %>%
  html_text()
texte

image <- jo %>%
  html_nodes("img") %>%
  html_attr("src")
image

table <- jo %>% 
  html_table(fill = TRUE)
View(table)

first_table <- table[[1]]
medailles <- as.data.frame(first_table)
str(medailles)

View(first_table)

write.csv(first_table,'medailles.csv')

#Documentation https://www.rdocumentation.org/packages/rvest/versions/0.3.6

###Scraper Twitter

library(tidyverse)
library(lubridate)
library(scales)

#install.packages("rtweet")
library(rtweet)

#doc
#https://cran.r-project.org/web/packages/rtweet/vignettes/auth.html
#https://cran.r-project.org/web/packages/rtweet/rtweet.pdf
#https://cran.r-project.org/web/packages/rtweet/vignettes/intro.html

api_key <- "macle"
api_secret_key <- "maclesecrete"

token <- create_token(
  app = "laurencefirstapp",
  consumer_key = api_key,
  consumer_secret = api_secret_key)

token

#Créer un compte sur Twitter Dev et récupérer les code + créer app
access_token <- "xxx"
access_token_secret <- "xxx"


token <- create_token(
  app = "laurencefirstapp",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

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


#WordCloud
#Voir https://towardsdatascience.com/create-a-word-cloud-with-r-bde3e7422e8a


###Extraire données Google Scholar
#install.packages("scholar")
library(scholar)

id <- '0N9TTo0AAAAJ&hl'
l <- get_profile(id)
l$name
p <- get_publications(id)
head(p, 3)
View(p)

#Graphique historique des citations
library(ggplot2)
ct <- get_citation_history(id)
ggplot(ct, aes(year, cites)) + geom_line() + geom_point()

#DOC https://rdrr.io/github/jkeirstead/scholar/

##Copier-coller des données d'un tableau
install.packages("datapasata")
library(datapasta)
library(tibble)

mavariable <- #action : copier le tableau (depuis une page web, un tableaur) puis cliquer sur "Addins" (Paste as tibble, par exemple)
str(mavariable)
