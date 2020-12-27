# ucitavanje rtweeet biblioteke - trenutno se vise koristi nego twitteR biblioteka
library(rtweet)
library(ggplot2)
#Ucitavanje dplyr bibiloteke - gramatika manipulacije podacima(skup glagola) 
library(dplyr)
# ucitavanje bibiloteke za rudarenje teksta
library(tidytext)


## spremanje api kljuceva sa twitter dev aplikacije
api_key <- "hHD8Mlv95a92O3MYnEt6SHKgW"
api_secret_key <- "ZklfW2atzF16iTZ4rSDqUhsDyuBnvtAf08PeO8S5kVxLEU5sLj"
access_token <- "964604508373319680-qbWw7yozBFBnwYY0emWOhdQPj49tLuJ"
access_token_secret <- "tS0IIqhq8XMk3x3ga4t9B68k3QvP4rER5vBds14kjVl76"

## autentikacija putem web browsera, 
##create_token funk. sprema token kao enviroment varijablu 
token <- create_token(
  app = "Fran Gudek Twitter Analiza",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

# u novoj sesiji token ucitamo pomocu ove funkcije
get_token()

#funkcija koja vraca tweetove koji odgovaraju korisnicki zadanom upitu 
tweets_USA <- search_tweets(q = "#USA", n = 5000, lang = "en", include_rts = FALSE)

head(tweets_USA$text)


# manualno uklanjanje http elemenata
tweets_USA$stripped_text <- gsub("http.*","",  tweets_USA$text)
tweets_USA$stripped_text <- gsub("https.*","", tweets_USA$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
# uklanjanje interpunkcija, promjena u mala slova, dodavanje id-a za svaki tweet
tweets_USA_potrebno <- tweets_USA %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(word, stripped_text)

data("stop_words")

head(stop_words)

nrow(tweets_USA_potrebno)

# uklanjanje stopwords (the, are, to, and...)
potrebne_tweet_rijeci <- tweets_USA_potrebno %>%
  anti_join(stop_words)

# vidimo da ima manje rijeci
nrow(potrebne_tweet_rijeci)



# graf koji sadrzi stopwords
tweets_USA_potrebno %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Specificne rijeci",
       y = "Broj",
       title = "Broj specificnih rijeci pronadenih u tweetovima")

# graf koji ne sadrzi stop words
potrebne_tweet_rijeci %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Broj",
       x = "Specificne rijeci",
       title = "Broj specificnih rijeci pronadenih u tweetovima",
       subtitle = "Stopwords uklonjene iz liste Stopwords")

pkgs <-c("ggplot2", "dplyr", "tidyverse", "tidytext", "topicmodels", "stringr",
         "sentimentr", "tm", "devtools", "directlabels")
#removed: "igraph" "ggraph" "directlabels"
for(p in pkgs) if(p %in% rownames(installed.packages()) == FALSE) {install.packages(p)}
for(p in pkgs) suppressPackageStartupMessages(library(p, quietly=TRUE, character.only=TRUE))

library(devtools)
install_github("dgrtwo/widyr")
library(widyr)

# micanje interpunkcijskih znakova, prebacivanje u mala slova i dodaje se id svakom tweetu
tweets_USA_paired_words <- tweets_USA %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(paired_words, stripped_text, token = "ngrams", n = 2)

tweets_USA_paired_words %>%
  count(paired_words, sort = TRUE)



tweets_USA_separated_words <- tweets_USA_paired_words %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

tweets_USA_filtered <- tweets_USA_separated_words %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
tweets_USA_counts <- tweets_USA_filtered %>%
  count(word1, word2, sort = TRUE)

head(tweets_USA_counts)
  
# ucitavanje biblioteke za mreznu vizualizaciju podataka
library(igraph)
library(ggraph)

# Prikaz mreze rijeci koje se spominju u tweetovima sa #USA, sto su pojmovi blizi to znaci 
# da se cesto spominju u zajednickom kontekstu

tweets_USA_counts %>%
  filter(n >= 24) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  #geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Mreza rijeci: #USA ",
       subtitle = "Rudarenje teksta twitter podataka",
       x = "", y = "") 


#Pronalazak lokacije tweetova i probrojavanje
tweets_USA %>%
  ggplot(aes(location)) +
  geom_bar() + coord_flip() +
  labs(x = "Count",
       y = "Location",
       title = "Twitter users ")

tweets_USA %>%
  dplyr::count(location, sort = TRUE) %>%
  mutate(location = reorder(location,n)) %>%
  na.omit() %>%
  top_n(15) %>%
  ggplot(aes(x = location,y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Location",
       y = "Count",
       title = "Twitter korisnici po Gradovima/Regiji")



#Izrada wordclouda
library(tm)
library(stringr)
library(qdapRegex)
library(wordcloud2)
library(RColorBrewer)

wordcloud_tweets <- tweets_USA
text <- str_c(wordcloud_tweets$text, collapse = "")

# continue cleaning the text
text <- 
  text %>%
  str_remove("\\n") %>%                   # remove linebreaks
  rm_twitter_url() %>%                    # Remove URLS Uklanjanje URL
  rm_url() %>%
  str_remove_all("#\\S+") %>%             # Remove any hashtags Uklanjanje hashtagova
  str_remove_all("@\\S+") %>%             # Remove any @ mentions uklanjanje @ spomena drugih korisnika 
  removeWords(stopwords("english")) %>%   # Remove common words (a, the, it etc.) uklanjanje stopwords
  removeNumbers() %>%
  stripWhitespace() %>%
  removeWords(c("amp"))                   # Final cleanup of other small changes

# Convert the data into a summary table
textCorpus <- 
  Corpus(VectorSource(text)) %>%
  TermDocumentMatrix() %>%
  as.matrix()

textCorpus <- sort(rowSums(textCorpus), decreasing=TRUE)
textCorpus <- data.frame(word = names(textCorpus), freq=textCorpus, row.names = NULL)

# build wordcloud 
wordcloud <- wordcloud2(data = textCorpus, minRotation = 0, maxRotation = 0, ellipticity = 0.6)
wordcloud



sentiments
get_sentiments("bing")

# join sentiment classification to the tweet words
bing_word_counts <- tweets_USA_potrebno %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment tweetova o Americi tijekom Covid-19 epidemije",
       y = "Doprinos sentimentu",
       x = NULL) +
  coord_flip()

bing_word_counts %>%
  filter(n > 30) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment))+
  geom_col() +
  coord_flip() +
  labs(y = "Sentiment Score")
