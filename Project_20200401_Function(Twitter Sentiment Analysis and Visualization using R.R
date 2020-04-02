#######################
## Project_20200401_Function(Twitter Sentiment Analysis and Visualization using R
#######################


library(dplyr)
library(ggplot2)


setwd('C:/ITWILL/R_SE_P')
train_twt <- read.csv("C:/ITWILL/R_SE_P/tweet-sentiment-extraction/train.csv")
str(train_twt)

test_twt <- read.csv("C:/ITWILL/R_SE_P/tweet-sentiment-extraction/test.csv")
str(test_twt)

submission <- read.csv("C:/ITWILL/R_SE_P/tweet-sentiment-extraction/sample_submission.csv")


library(SentimentAnalysis)
?SentimentAnalysis

sentiment <- analyzeSentiment("Yeah, this was a great soccer game for the German team!")
convertToBinaryResponse(sentiment)$SentimentQDAP
# [1] positive
# Levels: negative positive

data()
# DictionaryGI : Dictionary with opinionated words from the
#                Harvard-IV dictionary as used in the General
#                Inquirer software
# DictionaryHE : Dictionary with opinionated words from Henry's
#                Financial dictionary
# DictionaryLM : Dictionary with opinionated words from
#                Loughran-McDonald Financial dictionary
# QDAP : All Data

train_file <- paste(train_twt$text, train_twt$selected_text, train_twt$sentiment,
                    sep = " / ") 

head(train_file)

train_an <- analyzeSentiment(train_file)
table(is.na(train_an$SentimentQDAP))
test_score <- train_an$SentimentQDAP


# train_txt <- as.character(train_twt$text)
# sentiment <- analyzeSentiment(train_txt[1])
# sentiment$SentimentQDAP # 0.1818182

# sentiment_trn <- analyzeSentiment(train_txt)
# sentiment_trn$SentimentQDAP
# length(sentiment_trn$SentimentQDAP) # 27486
# table(is.na(sentiment_trn$SentimentQDAP))
# sentiment_trn$SentimentQDAP[is.na(sentiment_trn$SentimentQDAP) != 0] <- 0
# text_score <- sentiment_trn$SentimentQDAP

# train_sel <- as.character(train_twt$selected_text)
# sentiment_sel <- analyzeSentiment(train_sel)
# length(sentiment_sel$SentimentQDAP) #2 7486
# table(is.na(sentiment_sel$SentimentQDAP))
# sentiment_sel$SentimentQDAP[is.na(sentiment_sel$SentimentQDAP) != 0] <- 0
# text_score <- sentiment_sel$SentimentQDAP




library(e1071)

train <- data.frame(text_score,
                    train_twt$sentiment)
head(train)
str(train)
train[train$train_twt.sentiment == "negative",]

model <- naiveBayes(train_twt$sentiment ~ .,
                    data = train) 
model

# test_txt <- as.character(test_twt$text)
# sentiment_test <- analyzeSentiment(test_txt)
# length(sentiment_test$SentimentQDAP) # 3535
# table(is.na(sentiment_test$SentimentQDAP))
# sentiment_test$SentimentQDAP[is.na(sentiment_test$SentimentQDAP) != 0] <- 0
# text_score <- sentiment_test$SentimentQDAP
test_file <- paste(test_twt$text, test$test_twt.sentiment,
                   sep = " / ")

test_an <- analyzeSentiment(test_file)
table(is.na(test_an$SentimentQDAP))
text_score <- test_an$SentimentQDAP
test <- data.frame(text_score,
                   test_twt$sentiment)
head(test)
str(test)

nrow(train) # 27486
nrow(test) # 3535

pred <- predict(model, test)
pred

tab <- table(test$test_twt.sentiment,
             pred)
(tab[1,1] + tab[2,2] + tab[3,3]) / sum(tab) # 0.7196605



##################################################
## Naive Bayes 응용, 0401 내용 정리
##################################################

# 필요 패키지 in memory
library(dplyr)
library(ggplot2)
library(SentimentAnalysis)
library(e1071)


# 1. 데이터 가져오기 
setwd('C:/ITWILL/R_SE_P')

train_twt <- read.csv("C:/ITWILL/R_SE_P/tweet-sentiment-extraction/train.csv")

test_twt <- read.csv("C:/ITWILL/R_SE_P/tweet-sentiment-extraction/test.csv")

submission <- read.csv("C:/ITWILL/R_SE_P/tweet-sentiment-extraction/sample_submission.csv")

# 2. 데이터 생성/전처리
head(train_twt)
head(test_twt)
document_before <- paste(train_twt$text, # row data
                         train_twt$selected_text, # selected data
                         train_twt$sentiment,
                         sep = " -> ") # result -> merge
standard <- as.character(train_twt$sentiment) # 주어진 감정 기준 추출
standardization <- analyzeSentiment(standard) # 표준화
# 모든 사전의 데이터를 종합하여 계산하는 QDAP 지표를 사용 
head(data.frame(train_twt$sentiment, standardization$SentimentQDAP), 10) # 간단한 비교

response <- standardization$SentimentQDAP # 학습을 위한 표준 데이터 추출

dict_lern <- generateDictionary(document_before, response) # 사전 학습
dict_lern
summary(dict_lern) # Standard deviation: 0.7979223

write(dict_lern, file = 'dictionary.dict') # 사전 등록
dict_lern <- read('dictionary.dict') # 사전 불러오기


train_an <- analyzeSentiment(as.character(train_twt$text))
table(is.na(train_an$SentimentQDAP)) # 결측치 확인


# 3. 분류모델(분류기) 생성 : train data 이용    
# 형식2) niveBayes(y변수 ~ x변수, data)
model <- naiveBayes(RainTomorrow ~ ., data = train_w)
model

# 4. 분류모델 평가(예측기) : test data 이용 
# 형식) predict(model, test, type='class')
p <- predict(model, test_w)
tab <- table(p, test_w$RainTomorrow)
# p       No  Yes
#   No  7153  964
#   Yes 1137 1615

# No  Yes 
# 8290 2579 

# 5. 분류정확도 
(tab[1,1] + tab[2,2]) / sum(tab) # 0.8066979
tab[1,1] / sum(tab[1,]) # 0.8812369
tab[2,2] / sum(tab[2,]) # 0.5868459
table(test_w$RainTomorrow)


##########################
### 희소행렬 데이터 적용 
##########################

setwd("C:/ITWILL/2_Rwork/Part-IV")
sms_data <- read.csv('sms_dtm_df.csv')
dim(sms_data) # 5558 6123
str(sms_data)

# y변수 : $ sms_data.type
sms_data$sms_data.type

# 1. train과 test 데이터 셋 생성 (7:3)
idx <- sample(nrow(sms_data), nrow(sms_data)*0.7)
train_sms <- sms_data[idx, ]
test_sms <- sms_data[-idx, ]

# 2. model 생성 - train_sms
model <- naiveBayes(sms_data.type ~ ., 
                    data = train_sms)
model

# 3. 예측치 생성 - test_sms
pred <- predict(model, test_sms)
tab <- table(test_sms$sms_data.type, pred)
tab
# pred
#       ham spam
# ham  1428   12
# spam   17  211

# 4. 정분류율(Accuracy)
acc <- (tab[1,1] + tab[2,2]) / sum(tab)
acc # 0.9826139

# 5. F measure(f1 score)
r <- tab[2,2] / sum(tab[2,]) # 0.9356984(관측치)
p <- tab[2,2] / sum(tab[,2]) # 0.9461883(예측치)

F1 <- 2*((r*p) / (r+p))
F1 # 0.9356984