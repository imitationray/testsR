##################################################
## 배째ver
##################################################

# 필요 패키지 in memory
library(dplyr)
library(tm)
library(tidyverse)
library(ggplot2)
library(tidytext)
library(tidyr)
library(e1071)
data()
sentiments

# 1. 데이터 가져오기 
setwd('C:/ITWILL/R_SE_P')

train_twt <- read.csv("C:/ITWILL/R_SE_P/tweet-sentiment-extraction/train.csv")

test_twt <- read.csv("C:/ITWILL/R_SE_P/tweet-sentiment-extraction/test.csv")

submission <- read.csv("C:/ITWILL/R_SE_P/tweet-sentiment-extraction/sample_submission.csv")

# 2. 데이터 전처리
 

 # paste


# 3. 긍부정 단어/스코어 열람
get_sentiments("bing") %>%
  filter(sentiment == "positive") 
#   A tibble: 2,005 x 2
#    word        sentiment
#    <chr>       <chr>    
#  1 abound      positive 
#  2 abounds     positive 
#  3 abundance   positive 
#  4 abundant    positive 
#  5 accessable  positive 
#  6 accessible  positive 
#  7 acclaim     positive 
#  8 acclaimed   positive 
#  9 acclamation positive 
# 10 accolade    positive 
#  ... with 1,995 more rows

get_sentiments("bing") %>%
  filter(sentiment == "negative")
#   A tibble: 4,781 x 2
#    word        sentiment
#    <chr>       <chr>    
#  1 2-faces     negative 
#  2 abnormal    negative 
#  3 abolish     negative 
#  4 abominable  negative 
#  5 abominably  negative 
#  6 abominate   negative 
#  7 abomination negative 
#  8 abort       negative 
#  9 aborted     negative 
# 10 aborts      negative 
#   ... with 4,771 more rows

get_sentiments("afinn") %>%
  filter(value == "1")
#   A tibble: 208 x 2
#    word       value
#    <chr>      <dbl>
#  1 aboard         1
#  2 absorbed       1
#  3 accept         1
#  4 accepted       1
#  5 accepting      1
#  6 accepts        1
#  7 achievable     1
#  8 active         1
#  9 adequate       1
# 10 adopt          1
#  ... with 198 more rows

get_sentiments("afinn") %>%
  filter(value == "-1")

get_sentiments("afinn") %>%
  filter(value == "2")

get_sentiments("afinn") %>%
  filter(value == "-2")
#   A tibble: 966 x 2
#    word         value
#    <chr>        <dbl>
#  1 abandon         -2
#  2 abandoned       -2
#  3 abandons        -2
#  4 abducted        -2
#  5 abduction       -2
#  6 abductions      -2
#  7 accident        -2
#  8 accidental      -2
#  9 accidentally    -2
# 10 accidents       -2
#   ... with 956 more rows


# 3. 감성 분석을 위한 데이터 테이블 생성
train_txt <- as.character(train_twt$text) # train text 추출
train_sel <- as.character(train_twt$selected_text) # selelcted text 추출
test_txt <- as.character(test_twt$text) # test text 추출

titles <- c("train_txt", # 제목 객체 생성
            "train_sel",
            "test_txt")
texts <- list(train_txt, # 내용 객체 생성
              train_sel,
              test_txt)
twitts <- tibble() # tibble data frame 생성
index <- data.frame() 

# 단어장 만들기
for(i in seq_along(titles)){ # i <- 객체의 갯수 만큼 in
  imsi <- tibble(textsNum = seq_along(texts[[i]]), # texts의 주소값 i를 참조하여 주소값을 입력할 변수 textsNum 생성      
                  text = texts[[i]]) %>% # texts의 주소값 i를 참조한 내용을 추가할 변수 text 생성 
    unnest_tokens(word, # unnest_tokens 함수를 이용하여 토큰화(디폴트)
                  text) %>%
    mutate(title = titles[i]) %>% # 제목 객체의 주소값을 참조하여 제목 변수에 mutate
    select(title, # title을 기준으로 하여 모든 내용 불러온 후 출력 
           everything()
           ) # 모든 수행 내용을 변수 imsi에 저장
  twitts <- rbind(twitts,
                  imsi) # tibble data frame 'twitts 데이터 프레임과 imsi 데이터를 행병합하여 twittes에 저장 후 완료.
}
twitts # 트위터 단어장 완성

# 단어장 색인 만들기 
for(i in seq_along(titles)){ # i <- 객체의 갯수 만큼 in
  imsi <- tibble(textsNum = seq_along(texts[[i]]), # texts의 주소값 i를 참조하여 주소값을 입력할 변수 textsNum 생성      
                  text = texts[[i]]) %>%
    mutate(title = titles[i])
  index <- rbind(index,
                 imsi)
}
index[index$title == "train_txt",]
index[index$title == "train_sel",]
index[index$title == "test_txt",] 
# 색인 완성

twitts$title <- factor(twitts$title, 
                       levels = rev(titles)) # title 칼럼 factor변환
str(twitts)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	1798932 obs. of  3 variables:
# $ title   : Factor w/ 3 levels "test_txt","train_sel",..: 3 3 3 3 3 3 3 3 3 3 ...
# $ textsNum: int  1 1 1 1 1 1 1 1 1 1 ...
# $ word    : chr  "spent" "the" "entire" "morning" ...


# 4. 단어장 감정 분석
twitts_ncr <- twitts %>%
  right_join(get_sentiments("nrc"), # ncr 감정분석
             by = "word") %>%
  filter(!is.na(sentiment)) %>%
  count(sentiment,
        sort = TRUE)
twitts_ncr

twitts %>%
  inner_join(get_sentiments("bing"), # bing 감성분석
             by = "word") %>%
  count(title,
        sentiment) %>%
  spread(sentiment, # 스프레드 시트 형태로 변경
         n,
         fill = 0) %>%
  mutate(sentiment = positive - negative)


twitts %>%
  inner_join(get_sentiments("afinn"), # afinn 감성분석
             by = "word") %>%
  count(title,
        value) %>%
  spread(value, # 스프레드 시트 형태로 변경
         n,
         fill = 0)

twitts %>%
  group_by(title) %>%
  mutate(word_count = 1:n(),
         index = word_count %/% 50 + 1) %>%
  inner_join(get_sentiments("bing"), # bing 감정분석
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
  ggplot(aes(index, # 시각화
             sentiment,
             fill = title)) +
  geom_bar(alpha = 0.5,
           stat = 'identity',
           show.legend = FALSE) +
  facet_wrap(~ title,
             ncol = 2,
             scales = "free_x")


# affin 감성분석 
get_sentiments(lexicon = c("afinn")) 

afinn <- twitts %>%
  group_by(title) %>%
  inner_join(get_sentiments("afinn"),
             by = "word") %>%
  group_by(title, title) %>%
  summarise(value = sum(value))
afinn

twitts %>%
  inner_join(get_sentiments("bing"),
             by = "word") %>%
  
  inner_join(get_sentiments("afinn"),
             by = "word") %>%
  group_by(title, textsNum) %>%
  summarise(value = sum(value))

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
