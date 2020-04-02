#######################
## Project_20200331_Function(Twitter Sentiment Analysis and Visualization using R
#######################
install.packages("tidytext")
library(rtweet)
library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(sentimentr)

setwd('C:/ITWILL/R_SE_P')
train_twt <- read.csv("C:/ITWILL/R_SE_P/tweet-sentiment-extraction/train.csv")
str(train_twt)

train_txt <- as.character(train_twt$text)
train_sel <- as.character(train_twt$selected_text)
train_txt[1:2]
train_sel[1:2]
str(train_txt)

test_twt <- read.csv("C:/ITWILL/R_SE_P/tweet-sentiment-extraction/test.csv")
str(test_twt)
test_txt <- as.character(test_twt$text)
test_txt[1:2] 
str(test_txt)

submission <- read.csv("C:/ITWILL/R_SE_P/tweet-sentiment-extraction/sample_submission.csv")

data(sentiments)
table(sentiments)

get_sentiments("bing") %>%
  filter(sentiment == "positive") 
get_sentiments("bing") %>%
  filter(sentiment == "negative")

get_sentiments("afinn") %>%
  filter(value == "1")
get_sentiments("afinn") %>%
  filter(value == "-1")

train_txt

titles <- c("train_txt", "train_sel", "test_txt")
texts <- list(train_txt,
              train_sel,
              test_txt)
str(texts)
series <- tibble()


for(i in seq_along(titles)){
  clean <- tibble(textsNum = seq_along(texts[[i]]),
                  text = texts[[i]]) %>%
    unnest_tokens(word,
                  text) %>%
    mutate(title = titles[i]) %>%
    select(title,
           everything())
  series <- rbind(series,
                  clean)
}

series$title <- factor(series$title,
                      levels = rev(titles))
str(series)

series_ncr <- series %>%
  right_join(get_sentiments("nrc"),
             by = "word") %>%
  filter(!is.na(sentiment)) %>%
  count(sentiment,
        sort = TRUE)
series_ncr

series %>%
  group_by(title) %>%
  mutate(word_count = 1:n(),
         index = word_count %/% 10 + 1) %>%
  inner_join(get_sentiments("bing"),
             by = "word") %>%
  count(title,
        index = index,
        sentiment) %>%
  ungroup() %>%
  spread(sentiment,
         n,
         fill = 0) %>%
  mutate(sentiment = positive - negative,
         title = factor(title,
                        levels = titles)) %>%
  ggplot(aes(index, sentiment, fill = title)) +
  geom_bar(alpha = 0.5, stat = 'identity', show.legend = FALSE) +
  facet_wrap(~ title, ncol = 2, scales = "free_x")

get_sentiments(lexicon = c("afinn"))

afinn <- series %>%
  group_by(title) %>%
  mutate(word_count = 1:n(),
         index = word_count %/% 10 + 1) %>%
  inner_join(get_sentiments("afinn"),
             by = "word") %>%
  group_by(title, index) %>%
  summarise(sentiment = sum(value)) %>%
  mutate(method = "AFINN")
afinn

bing_and_nrc <- bind_rows(series %>%
                            group_by(title) %>%
                            mutate(word_count = 1:n(),
                                   index = word_count %/% 10 + 1) %>%
                            inner_join(get_sentiments("bing"),
                                       by = "word") %>%
                            mutate(method = "Bing"),
                          series %>%
                            group_by(title) %>%
                            mutate(word_count = 1:n(),
                                   index = word_count %/% 10 + 1) %>%
                            inner_join(get_sentiments("nrc") %>%
                                         filter(sentiment %in% c("positive", "negative")),
                                       by = "word") %>%
                            mutate(method = "NRC")) %>%
  count(title, method, index = index, sentiment) %>%
  ungroup() %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  select(title, index, method, sentiment)
bing_and_nrc

bind_rows(afinn,
          bing_and_nrc) %>%
  ungroup() %>%
  mutate(title = factor(title, levels = titles)) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_grid(title ~ method)

bing_word_counts <- series %>%
  inner_join(get_sentiments("bing"),
             by = "word") %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
bing_word_counts

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ggplot(aes(reorder(word, n),
             n,
             fill = sentiment)) +
  geom_bar(alpha = 0.8,
           stat = "identity",
           show.legend = FALSE) +
  facet_wrap(~ sentiment,
             scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

tibble(text = train_txt) %>%
  unnest_tokens(sentence, text, token = 'sentences')
tibble(text = train_sel) %>%
  unnest_tokens(sentence, text, token = 'sentences')
tibble(text = test_txt) %>%
  unnest_tokens(sentence, text, token = 'sentences')

ps_sentences_train <- tibble(chapter = 1:length(train_txt),
                       text = train_txt) %>%
  unnest_tokens(sentence, text, token = 'sentences')
ps_sentences_train

ps_sentences_sel <- tibble(chapter = 1:length(train_sel),
                             text = train_sel) %>%
  unnest_tokens(sentence, text, token = 'sentences')
ps_sentences_sel

ps_sentences_test <- tibble(chapter = 1:length(test_txt),
                             text = test_txt) %>%
  unnest_tokens(sentence, text, token = 'sentences')
ps_sentences_test

train_sent <- ps_sentences_train %>%
  group_by(chapter) %>%
  mutate(sentence_num = 1:n(),
         index = round(sentence_num / n(), 2)) %>%
  unnest_tokens(word, sentence) %>%
  inner_join(get_sentiments("afinn"),
             by = "word") %>%
  group_by(chapter) %>%
  summarise(sentiment = sum(value, na.rm = FALSE)) 

train_sent
dim(train_sent)
dim(train_twt)
length(train_txt)
