# Project_20200330

# install.packages("SentimentAnalysis")
# install.packages('tidyverse')
# install.packages("sentimentr")
# install.packages('stringdist')
# install.packages('sentimentr')

library(tidyverse)
library(stringdist)
library(sentimentr)
library(stringr)
library(slam) 
library(tm)  
library(SnowballC) 
library(e1071)

getwd()
setwd('C:/ITWILL/R_SE_P')

sys_time <- Sys.time()

train_twt <- read.csv('C:/ITWILL/R_SE_P/tweet-sentiment-extraction/train.csv')
dim(train_twt) # 27486     4
str(train_twt)

test_twt <- read.csv('C:/ITWILL/R_SE_P/tweet-sentiment-extraction/test.csv')
dim(test_twt) # 3535    3
str(test_twt)

sub_twt <- read.csv('C:/ITWILL/R_SE_P/tweet-sentiment-extraction/sample_submission.csv')
dim(sub_twt)

sys_time2 <- Sys.time()
print(sys_time2 - sys_time)

names(train_twt) # "textID" "text" "selected_text" "sentiment"   

# sentiment
tr <- train_twt
tr$sentiment <- as.factor(tr$sentiment)
plot(tr$sentiment)


# text
tr$text <- as.character(tr$text)
tr$selected_text <- as.character(tr$selected_text)

tr$length <- str_length(tr$text)
tr$length_sel <- str_length(tr$selected_text)

tr <- dplyr::filter(tr,
                    length > 0)

summary(tr$length)
hist(tr$length,
     main = 'Training Set')

summary(tr$length_sel)
hist(tr$length_sel,
     main = 'Training Set2')

tr$length_ratio <- tr$length_sel / tr$length
hist(tr$length_ratio)


fun_find_first <- function(text, subtext) {
  foo <- stringr::str_locate(text, fixed(subtext))
  return(foo[1])
}

fun_find_second <- function(text, subtext) {
  foo <- stringr::str_locate(text, fixed(subtext))
  return(foo[2])
}

n <- nrow(tr)
as <- 1:n
bs <- 1:n

for (i in 1:n) {
  # print(i)
  text <- tr$text[i]
  subtext <- tr$selected_text[i]
  a <- fun_find_first(text,
                      subtext)
  b <- fun_find_second(text,
                       subtext)
  as[i] <- a
  bs[i] <- b
}

tr$begin <- as
tr$end <- bs

tr$begin_rel <- tr$begin / tr$length
tr$begin_rel
tr$end_rel <- tr$end / tr$length
tr$end_rel

summary(tr$begin_rel)
hist(tr$begin_rel)

summary(df$end_rel)
hist(df$end_rel)

# sentiment analysis
sentiment_train <- sentimentr::sentiment_by(get_sentences(tr$text))
tr$sentiment_score <- sentiment_train$ave_sentiment
tr$word_count <- sentiment_train$word_count

head(sentiment_train,
     10)

# same for selected text
sentiment_train_sel <- sentimentr::sentiment_by(get_sentences(tr$selected_text))
tr$sentiment_score_sel <- sentiment_train_sel$ave_sentiment
tr$word_count_sel <- sentiment_train_sel$word_count

head(sentiment_train_sel, 
     10)

sent_cor <- round(cor(tr$sentiment_score,
                      tr$sentiment_score_sel),
                  3)
plot(tr$sentiment_score,
     tr$sentiment_score_sel,
     col = '#00000040',
     pch = 16,
     main = paste0('Sentiment selected text vs full text; cor=',
                   sent_cor)
     )
grid()

stats_train <- dplyr::group_by(tr,
                               sentiment) %>%
  summarise(n = n(),
            mean_words = mean(word_count),
            mean_words_sel = mean(word_count_sel),
            mean_length = mean(length),
            mean_length_sel = mean(length_sel),
            mean_ratio = mean(length_ratio),
            median_ratio = median(length_ratio),
            mean_begin_rel = mean(begin_rel),
            mean_end_rel = mean(end_rel),
            mean_sentiment_score = mean(sentiment_score),
            mean_sentiment_score_sel = mean(sentiment_score_sel)
            )

stats_train <- as.data.frame(stats_train)

# show result
stats_train

tr_train_neutral <- dplyr::filter(tr, sentiment=='neutral')
tr_train_positive <- dplyr::filter(tr, sentiment=='positive')
tr_train_negative <- dplyr::filter(tr, sentiment=='negative')


plot(ecdf(tr_train_negative$length),
     col = 'red',
     main = 'Training - Length of text by sentiment',
     xlab = 'Length of Text')

plot(ecdf(tr_train_positive$length),
     col = 'green',
     add = TRUE)

plot(ecdf(tr_train_neutral$length),
     col = 'blue',
     add = TRUE)

grid()

legend('topleft',
       text.width = 25,
       legend = c('Negative','Positive','Neutral'),
       col = c('red','green','blue'),
       pch = 16)

plot(ecdf(tr_train_negative$length_sel),
     col = 'red',
     main = 'Training - Length of selected text by sentiment',
     xlab = 'Length of Selected Text')

plot(ecdf(tr_train_positive$length_sel),
     col = 'green',
     add = TRUE)

plot(ecdf(tr_train_neutral$length_sel),
     col = 'blue',
     add = TRUE)

grid()

legend('topleft',
       text.width = 25,
       legend = c('Negative','Positive','Neutral'),
       col = c('red','green','blue'),
       pch = 16)

plot(ecdf(tr_train_negative$length_ratio),
     col = 'red',
     main = 'Training - Length Ratio Selected/Full Text',
     xlab = 'Ratio')

plot(ecdf(tr_train_positive$length_ratio),
     col = 'green',
     add = TRUE)

plot(ecdf(tr_train_neutral$length_ratio),
     col = 'blue',
     add = TRUE)

grid()

legend('topleft',
       text.width = 0.2,
       legend = c('Negative','Positive','Neutral'),
       col = c('red','green','blue'),
       pch = 16)

hist(tr_train_neutral$sentiment_score,
     50,
     col = 'blue',
     main = 'Sentiment Score - Sentiment=Neutral')

hist(tr_train_positive$sentiment_score,
     50,
     col = 'green',
     main = 'Sentiment Score - Sentiment=Positive')

hist(tr_train_positive$sentiment_score_sel,
     50,
     col = 'darkgreen',
     main = 'Sentiment Score Selected Text - Sentiment=Positive')

hist(tr_train_negative$sentiment_score,
     50,
     col = 'red',
     main = 'Sentiment Score - Sentiment=Negative')

hist(tr_train_negative$sentiment_score_sel,
     50,
     col = 'darkred',
     main = 'Sentiment Score Selected Text - Sentiment=Negative')

smoothScatter(tr_train_negative$length,
              tr_train_negative$length_sel,
              xlab = 'Length',
              ylab = 'Length Selected Text',
              main = 'Training - Sentiment Negative')

smoothScatter(tr_train_positive$length,
              tr_train_positive$length_sel,
              xlab = 'Length',
              ylab = 'Length Selected Text',
              main = 'Training - Sentiment Positive')

smoothScatter(tr_train_neutral$length,
              tr_train_neutral$length_sel,
              xlab = 'Length',
              ylab = 'Length Selected Text',
              main = 'Training - Sentiment Neutral')

sent_cor <- round(cor(tr_train_neutral$sentiment_score,
                      tr_train_neutral$sentiment_score_sel),
                  3)
plot(tr_train_neutral$sentiment_score,
     tr_train_neutral$sentiment_score_sel,
     col = '#0000ff40',
     pch = 16,
     main = paste0('NEUTRAL - Sentiment selected text vs full text; cor=',
                   sent_cor)
     )

grid()

sent_cor <- round(cor(tr_train_positive$sentiment_score,
                      tr_train_positive$sentiment_score_sel),
                  3)
plot(tr_train_positive$sentiment_score,
     tr_train_positive$sentiment_score_sel,
     col = '#00990040',
     pch = 16,
     main = paste0('POSITIVE - Sentiment selected text vs full text; cor=',
                   sent_cor)
     )
grid()

sent_cor <- round(cor(tr_train_negative$sentiment_score,
                      tr_train_negative$sentiment_score_sel),
                  3)
plot(tr_train_negative$sentiment_score,
     tr_train_negative$sentiment_score_sel,
     col = '#99000040',
     pch = 16,
     main = paste0('NEGATIVE - Sentiment selected text vs full text; cor=',
                   sent_cor)
     )
grid()

