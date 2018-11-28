#### Intro ####

library(rtweet) # interface with Twitter API
library(tidytext) # sentiment analysis
library(tidyverse) # ggplot
library(wordcloud) # wordclouds 
library(igraph) # networks
library(ggraph) # graphs
library(tm) # text mining 


# Get connected: app.twitter.com 
create_token(app = "rladies_cville",
             consumer_key = "BUdtul9S5Iv5GZPJrXZPsVp2p", 
             consumer_secret = "K0Y9PZhQEncA08EOQr1MpK3m1TwtHIkSXwSFkv3VVk9M08IX0Z",
             access_token = "975786175830155265-AwAlPT5xlEFr8f143jO23G363uN2wHS",
             access_secret = "fD9SOQx7DJR5SiJG7tKWPIq8FbqxKw8OriGH2JZTvfRQD")


# EX. Get 10,000 non-retweets containing rstats hashtag:
rt <- search_tweets(
    "#rstats", n = 10000, include_rts = FALSE)
View(rt)

# Get tweets: 
dt_tweets <- get_timeline("realDonaldTrump", 3200) %>%
    select(created_at, text) %>%
    mutate(id = c(1:3193), text = tolower(text))

# returns 3k tweets from Dec 2017 - Nov 2018 with
# each tweet as a separate row. Max func can return is 3.2k

# Reprex:
write.csv(dt_tweets, "dt_tweets.csv")
dt_tweets <- read_csv("dt_tweets.csv")




range(dt_tweets$created_at) # 12/19/2017 - 11/26/2018

#### Tidy ####

# one item per row:
dt_tidy <- dt_tweets %>%
    unnest_tokens(word, text) 

# remove stop words: 
data(stop_words)
stop_words <- data.frame(word = c("https", "http", "amp", "t.co"), 
           lexicon = "custom") %>%
    bind_rows(stop_words)
dt_tidy <- dt_tidy %>%
    anti_join(stop_words)

#### Explore ####
dt_tidy %>%
    count(word, sort = TRUE)# lots of tweets about himself

dt_tidy %>%
    count(word, sort = TRUE) %>%
    filter(n > 150) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip()

#### NRC #### 

# The nrc lexicon categorizes words in a binary fashion (“yes”/“no”) into 
# categories of positive, negative, anger, anticipation, disgust, fear, joy, 
# sadness, surprise, and trust.
# http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm

# anger
nrc_anger <- get_sentiments("nrc") %>% 
    filter(sentiment == "anger")

dt_tidy %>%
    inner_join(nrc_anger) %>%
    count(word, sort = TRUE) # yikes: crime, collusion, witch, bad, phony, illegal

# joy
nrc_joy <- get_sentiments("nrc") %>%
    filter(sentiment == "joy")

dt_tidy %>%
    inner_join(nrc_joy) %>%
    count(word, sort = TRUE) # hmm... deal, white, love, pay, happy

# all nrc
nrc <- get_sentiments("nrc")

dt_tidy %>%
    inner_join(nrc) %>%
    count(word, sentiment, sort = TRUE) %>%
    filter(n > 50) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col(aes(fill = sentiment)) +
    xlab(NULL) +
    coord_flip() # so vote spans all sentiments? 

#### Bing ####
# The bing lexicon categorizes words in a binary fashion into positive 
# and negative categories.  
# https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html

# Get score per tweet:
bing_sentiment <- dt_tidy %>%
    inner_join(get_sentiments("bing")) %>%
    count(id, created_at, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative)

# View as timeline:
ggplot(bing_sentiment, aes(created_at, sentiment)) +
    geom_line() # roller coaster of emotions

# View in date:
bing_sentiment %>%
    mutate(month = lubridate::month(created_at))%>% # extracts month
    ggplot(aes(sentiment, group = month, color = month)) +
    geom_density() +
    scale_color_viridis_c() 

#### Wordclouds ####

dt_tidy %>%
    anti_join(stop_words) %>%
    count(word) %>%
    with(wordcloud(word, n, max.words = 100, ordered.colors = TRUE))

#### Frequency Counts ####

# get the bing association (pos/neg) and count of each word:
bing_word_counts <- dt_tidy %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    ungroup()

# plot it:
bing_word_counts %>%
    group_by(sentiment) %>%
    top_n(10) %>%
    ungroup() %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y") +
    labs(y = "Contribution to sentiment",
         x = NULL) +
    coord_flip() # again, mostly tweeting about himself and accusing others 
# emotional negative and neutral positive. what is this fake stuff?  

#### n-grams ####

# create bigrams:
trump_bigrams <- dt_tweets %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2)

trump_bigrams %>%
    count(bigram, sort = TRUE)

# remove stop words:
bigrams_separated <- trump_bigrams %>%
    separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
    count(word1, word2, sort = TRUE)

bigram_counts # fake news, witch hunt, crooked hillary

# visualize associations:
bigram_graph <- bigram_counts %>%
    filter(n > 20) %>%
    graph_from_data_frame()
bigram_graph

ggraph(bigram_graph, layout = "fr") +
    geom_edge_link() +
    geom_node_point() +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1)

