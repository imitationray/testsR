

install.packages("textstem")
# [libraries]
library(tm)
library(dplyr)
library(wordcloud)
library(textstem)
library(stringr)
library(tidytext)
library(textdata)
library(plyr)


setwd("C:/ITWILL/Work/2_Rwork/!_Project/semi_kaggle_TweetSentimentExtraction/Work")
train <- read.csv("C:/ITWILL/R_SE_P/tweet-sentiment-extraction/train.csv", stringsAsFactors = F)

# 전처리(gsub는 평서문에만 적용 가능.)
practice.eda <- train$text
practice.eda <-as.matrix(practice.eda)

practice.eda <- gsub("[가-힣]", "", practice.eda)
practice.eda <- gsub('[[:cntrl:]]', '', practice.eda)
practice.eda <- gsub("http\\S+", "", practice.eda)

train.corpus <- Corpus(VectorSource(practice.eda))

inspect(train.corpus[1:10])
inspect(train.corpus[100:120])

# 전처리
train.corpus <- tm_map(train.corpus, tolower)
train.corpus <- tm_map(train.corpus, removeNumbers)
train.corpus <- tm_map(train.corpus, removeWords, "'s")
train.corpus <- tm_map(train.corpus, removeWords, stopwords("SMART"))
train.corpus <- tm_map(train.corpus, removeWords, stopwords("en"))
train.corpus <- tm_map(train.corpus, removePunctuation)
stopwords("SMART")
stopwords('english')
train.corpus <- tm_map(train.corpus, lemmatize_strings)
train.corpus <- tm_map(train.corpus, stripWhitespace)    
inspect(train.corpus[1:10])
inspect(train.corpus[100:120])

# 코퍼스 -> 다시 벡터로 변환하기(데이타프레임 거쳐감)

train.df <- data.frame(text=sapply(train.corpus, identity),
                       stringsAsFactors=F)

train.vec <- as.vector(train.df$text)
train.vec
str(train.vec)
length(train.vec)  # 27486

train.vec

train.df$idx <- 1:length(train.vec)
train.df


get_sentiments("afinn")
posDic1 <- get_sentiments("afinn")%>%filter(value==1)                      
posDic2 <- get_sentiments("afinn")%>%filter(value==2)  
posDic3 <- get_sentiments("afinn")%>%filter(value==3)
posDic4 <- get_sentiments("afinn")%>%filter(value==4)
posDic5 <- get_sentiments("afinn")%>%filter(value==5)
neuDic <- get_sentiments("afinn")%>%filter(value==0)
negDic1 <- get_sentiments("afinn")%>%filter(value==-1)
negDic2 <- get_sentiments("afinn")%>%filter(value==-2)
negDic3 <- get_sentiments("afinn")%>%filter(value==-3)
negDic4 <- get_sentiments("afinn")%>%filter(value==-4)
negDic5 <- get_sentiments("afinn")%>%filter(value==-5)


sentimental2 = function(sentences, posDic1, posDic2, posDic3, posDic4, posDic5, neuDic, negDic1, negDic2, negDic3, negDic4, negDic5){
  
  scores = laply(sentences, function(sentence, posDic1, posDic2, posDic3, posDic4, posDic5, neuDic, negDic1, negDic2, negDic3, negDic4, negDic5) {
    
    # 공백을 기준으로 문장을 -> 단어로(엉어니까)
    word.list = str_split(sentence, '\\s+') # 공백 기준으로 단어 생성 -> \\s+ : 공백 정규식, +(1개 이상)
    words = unlist(word.list) # unlist() : list를 vector 객체로 구조변경
    cat('words')
    print(words)
    
    # 단어와 사전 매치(사전에 있는지 없는지)
    pos.matches1 = match(words, posDic1) # words의 단어를 posDic에서 matching
    pos.matches2 = match(words, posDic2)
    pos.matches3 = match(words, posDic3)
    pos.matches4 = match(words, posDic4)
    pos.matches5 = match(words, posDic5)
    neu.matches = match(words, neuDic)
    neg.matches1 = match(words, negDic1)
    neg.matches2 = match(words, negDic2)
    neg.matches3 = match(words, negDic3)
    neg.matches4 = match(words, negDic4)
    neg.matches5 = match(words, negDic5)
    cat('pos.matches1')
    print(pos.matches1)
    
    # 매칭된 단어 추출
    pos.matches1 = !is.na(pos.matches1) # NA 제거, 위치(숫자)만 추출
    pos.matches2 = !is.na(pos.matches2)
    pos.matches3 = !is.na(pos.matches3)
    pos.matches4 = !is.na(pos.matches4)
    pos.matches5 = !is.na(pos.matches5)
    neu.matches = !is.na(neu.matches)
    neg.matches1 = !is.na(neg.matches1)
    neg.matches2 = !is.na(neg.matches2)
    neg.matches3 = !is.na(neg.matches3)
    neg.matches4 = !is.na(neg.matches4)
    neg.matches5 = !is.na(neg.matches5)
    cat('pos.matches1')
    print(pos.matches1)
    
    
    score = (sum(pos.matches1)*1 + sum(pos.matches2)*2 + sum(pos.matches3)*3 + sum(pos.matches4)*4 + sum(pos.matches5)*5) - (sum(neg.matches1)*1 + sum(neg.matches2)*2 + sum(neg.matches3)*3 + sum(neg.matches4)*4 + sum(neg.matches5)*5)  # 긍정 - 부정    
    return(score)    #  양이면 긍정, 음이면 부정
    
  }, posDic1, posDic2, posDic3, posDic4, posDic5, neuDic, negDic1, negDic2, negDic3, negDic4, negDic5)             # inner function
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}                                          # outer function


# 감성 분석
texts <- train.vec[1:10]
sample_for_test <- sentimental2(texts, posDic1$word, posDic2$word, posDic3$word, posDic4$word, posDic5$word, neuDic$word, negDic1$word, negDic2$word, negDic3$word, negDic4$word, negDic5$word)
sample_for_test

result2 <- sentimental2(train.vec, posDic1$word, posDic2$word, posDic3$word, posDic4$word, posDic5$word, neuDic$word, negDic1$word, negDic2$word, negDic3$word, negDic4$word, negDic5$word)
head(result2)


# 두 번째 예측치(pred)를 원래 train 데이터셋에 추가하기
sentimental_pred <- result2$score
sentimental_pred2 <- ifelse(sentimental_pred>=0,
                            ifelse(sentimental_pred>0,"positive", "neutral"),
                            "negative")





train$pred2 <- sentimental_pred2


# 예측치와 실제값 비교
table(train$sentiment)

table(train$pred2)

tab2 <- table(train$pred2, train$sentiment)


# model평가
acc <- (tab2[1,1]+tab2[2,2]+tab2[3,3])/sum(tab2)
cat('accruacy for our second model with rough EDA & complicated Sentiment Extract function is', acc)




############################################################################################
##################기존 find.___.words 함수를 if문을 사용해서 합치기#########################
############################################################################################
find.selected.words = function(sentences, posDic1, posDic2, posDic3, posDic4, posDic5, neuDic, negDic1, negDic2, negDic3, negDic4, negDic5){
  
  # 전체 문장의 best word save
  selected_words <- character()
  sel_idx <- 0 # index
  
  selected_words2 = laply(sentences, function(sentence, posDic1, posDic2, posDic3, posDic4, posDic5, neuDic, negDic1, negDic2, negDic3, negDic4, negDic5) {
    
    sel_idx <- sel_idx + 1
    # 공백을 기준으로 문장을 -> 단어로(엉어니까)
    word.list = str_split(sentence, '\\s+') # 공백 기준으로 단어 생성 -> \\s+ : 공백 정규식, +(1개 이상)
    words = unlist(word.list) # unlist() : list를 vector 객체로 구조변경
    cat('words')
    print(words)
    
    # 단어와 사전 매치(사전에 있는지 없는지)
    pos.matches1 = match(words, posDic1) # words의 단어를 posDic에서 matching
    pos.matches2 = match(words, posDic2)
    pos.matches3 = match(words, posDic3)
    pos.matches4 = match(words, posDic4)
    pos.matches5 = match(words, posDic5)
    
    neu.matches = match(words, neuDic)
    neg.matches1 = match(words, negDic1)
    neg.matches2 = match(words, negDic2)
    neg.matches3 = match(words, negDic3)
    neg.matches4 = match(words, negDic4)
    neg.matches5 = match(words, negDic5)
    cat('pos.matches1')
    print(pos.matches1)
    
    # 매칭된 단어 추출
    pos.matches1 = !is.na(pos.matches1) # NA 제거, 위치(숫자)만 추출
    pos.matches2 = !is.na(pos.matches2)
    pos.matches3 = !is.na(pos.matches3)
    pos.matches4 = !is.na(pos.matches4)
    pos.matches5 = !is.na(pos.matches5)
    neu.matches = !is.na(neu.matches)
    neg.matches1 = !is.na(neg.matches1)
    neg.matches2 = !is.na(neg.matches2)
    neg.matches3 = !is.na(neg.matches3)
    neg.matches4 = !is.na(neg.matches4)
    neg.matches5 = !is.na(neg.matches5)
    
    score = (sum(pos.matches1)*1 + sum(pos.matches2)*2 + sum(pos.matches3)*3 + sum(pos.matches4)*4 + sum(pos.matches5)*5) - (sum(neg.matches1)*1 + sum(neg.matches2)*2 + sum(neg.matches3)*3 + sum(neg.matches4)*4 + sum(neg.matches5)*5)  # 긍정 - 부정    
    
    
    # 1개 문장에서 best word
    best_word <- character()
    if(score>0){              ###### 긍정들만
      idx <- c()
      
      cat('\ndic class ==>')
      if(sum(pos.matches5,na.rm = T)){ # NA  NA  index(5):words NA  NA -> sum(index(5)) -> 합계(TRUE) or 0(FALSE)
        cat('dic5 : ')
        idx <- which(pos.matches5 == TRUE)
        print(words[idx])
        best_word <- words[idx]
      }else if(sum(pos.matches4,na.rm = T)){
        cat('dic4 : ')
        idx <- which(pos.matches4 == TRUE)
        print(words[idx])
        best_word <- words[idx]
      }else if(sum(pos.matches3,na.rm = T)){
        cat('dic3 : ')
        idx <- which(pos.matches3 == TRUE)
        print(words[idx])
        best_word <- words[idx]
      }else if(sum(pos.matches2,na.rm = T)){
        cat('dic2 : ')
        idx <- which(pos.matches2 == TRUE)
        print(words[idx])
        best_word <- words[idx]
      }else if(sum(pos.matches1,na.rm = T)){
        cat('dic1 : ')
        idx <- which(pos.matches1 == TRUE)
        print(words[idx])
        best_word <- words[idx]
      }else{
        cat('없음\n')  
        best_word <- '없음'
      }
      
      # best word save
      selected_words[sel_idx] <- best_word
      
    } else if(score<0) {
      idx <- c()
      
      cat('\ndic class ==>')
      if(sum(neg.matches5,na.rm = T)){ # NA  NA  index(5):words NA  NA -> sum(index(5)) -> 합계(TRUE) or 0(FALSE)
        cat('dic5 : ')
        idx <- which(neg.matches5 == TRUE)
        print(words[idx])
        best_word <- words[idx]
      }else if(sum(neg.matches4,na.rm = T)){
        cat('dic4 : ')
        idx <- which(neg.matches4 == TRUE)
        print(words[idx])
        best_word <- words[idx]
      }else if(sum(neg.matches3,na.rm = T)){
        cat('dic3 : ')
        idx <- which(neg.matches3 == TRUE)
        print(words[idx])
        best_word <- words[idx]
      }else if(sum(neg.matches2,na.rm = T)){
        cat('dic2 : ')
        idx <- which(neg.matches2 == TRUE)
        print(words[idx])
        best_word <- words[idx]
      }else if(sum(neg.matches1,na.rm = T)){
        cat('dic1 : ')
        idx <- which(neg.matches1 == TRUE)
        print(words[idx])
        best_word <- words[idx]
      }else{
        cat('없음\n')  
        best_word <- '없음'
      }
      
      # best word save
      selected_words[sel_idx] <- best_word
      
    } else {
      
      best_word <- paste(words, collapse = ' ')  
      # best word save
      selected_words[sel_idx] <- best_word
    }
    
    return(selected_words)    #  양이면 긍정, 음이면 부정
    
  }, posDic1, posDic2, posDic3, posDic4, posDic5, neuDic, negDic1, negDic2, negDic3, negDic4, negDic5)             # inner function
  
  return(selected_words2)
}                                               # outer function

# 함수호출
texts <- train.vec[1:10]
texts
sample_for_test <- find.selected.words(texts, posDic1$word, posDic2$word, posDic3$word, posDic4$word, posDic5$word, neuDic$word, negDic1$word, negDic2$word, negDic3$word, negDic4$word, negDic5$word)
sample_for_test

selected.words <-find.selected.words(train.vec, posDic1$word, posDic2$word, posDic3$word, posDic4$word, posDic5$word, neuDic$word, negDic1$word, negDic2$word, negDic3$word, negDic4$word, negDic5$word)
selected.words
str(selected.words)
length(selected.words)



##################################################
## Naive Bayes 응용, 0401 내용 정리
##################################################

# 필요 패키지 in memory

library(e1071)


# 1. 데이터 가져오기 
test <- read.csv("C:/ITWILL/R_SE_P/tweet-sentiment-extraction/test.csv")
test.vec <- test$text
head(test)

# 2. 데이터 생성/전처리
result3 <- sentimental2(test.vec, posDic1$word, posDic2$word, posDic3$word, posDic4$word, posDic5$word, neuDic$word, negDic1$word, negDic2$word, negDic3$word, negDic4$word, negDic5$word)
result3

sentimental_test <- result3$score
sentimental_test2 <- ifelse(sentimental_test>=0,
                            ifelse(sentimental_test>0,"positive", "neutral"),
                            "negative")


test$pred2 <- sentimental_test2



train_df <- data.frame(score = result2$score,
                       result = as.character(train$pred2),
                       sentiment = as.factor(train$sentiment))
str(train_df)

# 3. 분류모델(분류기) 생성 : train data 이용    
# 형식2) niveBayes(y변수 ~ x변수, data)
model <- naiveBayes(sentiment ~ .,
                    data = train_df)
model

# 4. 분류모델 평가(예측기) : test data 이용 
# 형식) predict(model, test)
test_df <- data.frame(score = result3$score,
                      result = as.character(test$pred2),
                      sentiment = as.factor(test$sentiment))
str(test_df)

p <- predict(model, test_df)
tab <- table(p, test_df$sentiment)
tab

# 5. 분류정확도 
(tab[1,1] + tab[2,2] + tab[3,3]) / sum(tab)

