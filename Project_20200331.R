#######################
## Project_20200331
#######################

setwd('C:/ITWILL/R_SE_P')
train_twt <- read.csv("C:/ITWILL/R_SE_P/tweet-sentiment-extraction/train.csv",
                  stringsAsFactors = F)  # 'data.frame':   27486 obs. of  4 variables:
# $ textID       : Factor w/ 27486 levels
# $ text         : Factor w/ 27486 levels
# $ selected_text: Factor w/ 22580 levels
# $ sentiment    : Factor w/ 3 levels
str(train)

test_twt <- read.csv("C:/ITWILL/R_SE_P/tweet-sentiment-extraction/test.csv",
                     stringsAsFactors = F)  # 'data.frame':   3535 obs. of  3 variables:
# $ textID   : Factor w/ 3535 levels
# $ text     : Factor w/ 3535 levels
# $ sentiment: Factor w/ 3 levels
submission <- read.csv("C:/ITWILL/R_SE_P/tweet-sentiment-extraction/sample_submission.csv")

practice.eda <- train$text
practice.eda
str(practice.eda)
practice.eda <- as.matrix(practice.eda)
practice.eda[100:120]
practice.eda
practice.eda <- gsub("[가-힣]", "", practice.eda)
practice.eda[100:120]
practice.eda <- gsub('[[:cntrl:]]', '', practice.eda)
practice.eda[100:120]
practice.eda <- gsub("http\\S+", "", practice.eda)
practice.eda[1:10]


# 전처리
practice.eda <- tolower(practice.eda)
practice.eda[100:120]
practice.eda <- removeNumbers(practice.eda)
practice.eda[100:120]
practice.eda <- removePunctuation(practice.eda)
library(tm)

train.corpus <- Corpus(VectorSource(practice.eda))
train.corpus <- tm_map(train.corpus, removeWords, stopwords("en"))                  
train.corpus <- tm_map(train.corpus, stripWhitespace)  
train.corpus <- tm_map(train.corpus, content_transformer(stemDocument), language = "english")
inspect(train.corpus[1:10])
# train.corpus <- gsub('[[:cntrl:]]', '', train.corpus)  이거하면오류남
# train.corpus <- gsub("[[가-힣]]", "", train.corpus)  이것도
inspect(train.corpus[100:120])
# train.corpus = gsub("[가-힣]", "", train.corpus)  # 평서문에만 적용 가능한 함수.


# 코퍼스 -> 다시 벡터로 변환하기(데이타프레임 거쳐감)

train.df <- data.frame(text=sapply(train.corpus, identity),
                       stringsAsFactors=F)
train.df
str(train.df)

train.vec <- as.vector(train.df$text)
train.vec
str(train.vec)
length(train.vec)  # 27486




# install.packages("dplyr")
library(dplyr)
# install.packages("tidytext")
library(tidytext)
# install.packages("textdata")
library(textdata)

posDic <- get_sentiments("bing") %>% filter(sentiment=='positive')
negDic <- get_sentiments("bing") %>% filter(sentiment=='negative')
# get_sentiments("bing")%>% filter(sentiment=='neutral')
# table(get_sentiments("bing"))
str(posDic)


get_sentiments("afinn")
get_sentiments("afinn")%>%filter(value==1)
get_sentiments("afinn")%>%filter(value==-1)  
get_sentiments("afinn")%>%filter(value==0)
get_sentiments("afinn")%>%filter(value==5)
get_sentiments("afinn")%>%filter(value==6)
get_sentiments("afinn")%>%filter(value==-5)

# train.vec을 긍정, 부정어사전에 적용

sentimental = function(sentences, posDic, negDic){
  
  scores = lapply(sentences, function(sentence, posDic, negDic) {
    
    # 문장 전처리 gsub('패턴')
    sentence = gsub('[[:punct:]]', '', sentence) #문장부호 제거
    sentence = gsub('[[:cntrl:]]', '', sentence) #특수문자 제거
    sentence = gsub('\\d+', '', sentence) # 숫자 제거
    sentence = tolower(sentence) # 모두 소문자로 변경(단어가 모두 소문자 임)
    
    # 공백을 기준으로 문장을 -> 단어로(엉어니까)
    word.list = str_split(sentence, '\\s+') # 공백 기준으로 단어 생성 -> \\s+ : 공백 정규식, +(1개 이상)
    words = unlist(word.list) # unlist() : list를 vector 객체로 구조변경
    
    # 단어와 사전 매치(사전에 있는지 없는지)
    pos.matches = match(words, posDic) # words의 단어를 posDic에서 matching
    neg.matches = match(words, negDic)
    
    # 매칭된 단어 추출
    pos.matches = !is.na(pos.matches) # NA 제거, 위치(숫자)만 추출
    neg.matches = !is.na(neg.matches)
    
    score = sum(pos.matches) - sum(neg.matches) # 긍정 - 부정    
    return(score)    #  양이면 긍정, 음이면 부정
  }, posDic, negDic)                       # inner function
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}                                          # outer function


# 감성 분석
result <- sentimental(train.vec, posDic, negDic)
result
library(stringr)
library(plyr)


