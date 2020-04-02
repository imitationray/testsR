#######################
## Project_20200330_2
#######################
# 1. train_twt 단어별 출현빈도 가중치
# 2. 

library(tidyverse)
library(stringdist)
library(sentimentr)
library(stringr)
library(slam) 
library(tm)  
library(SnowballC) 
library(e1071)
library(SentimentAnalysis)

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

head(train_twt)

docu_twt <- as.character(train_twt$text)

sentiment <- analyzeSentiment(docu_twt)

str(sentiment)
table(is.na(sentiment))

table(sentiment$WordCount, train_twt$sentiment)


# 3. 감성 분석 함수 정의 - sentimental

# (1) 문자열 처리를 위한 패키지 로딩 
install.packages("plyr")
library(plyr) # laply()함수 제공
library(stringr) # str_split()함수 제공

# (2) 감성분석을 위한 함수 정의
sentimental = function(sentences, posDic, negDic){
  
  scores = laply(sentences, function(sentence, posDic, negDic) {
    
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

# 4. 감성 분석 : 두번째 변수(review) 전체 레코드 대상 감성분석
result<-sentimental(data[,2], posDic.final, negDic.final)
result
names(result) # "score" "text" 
dim(result) # 100   2
result$text
result$score # 100 줄 단위로 긍정어/부정어 사전을 적용한 점수 합계

# score값을 대상으로 color 칼럼 추가
result$color[result$score >=1] <- "blue"
result$color[result$score ==0] <- "green"
result$color[result$score < 0] <- "red"

# 감성분석 결과 차트보기
plot(result$score, col=result$color) # 산포도 색생 적용
barplot(result$score, col=result$color, main ="감성분석 결과화면") # 막대차트


# 5. 단어의 긍정/부정 분석 

# (1) 감성분석 빈도수 
table(result$color)

# (2) score 칼럼 리코딩 
result$remark[result$score >=1] <- "긍정"
result$remark[result$score ==0] <- "중립"
result$remark[result$score < 0] <- "부정"

sentiment_result<- table(result$remark)
sentiment_result

# (3) 제목, 색상, 원크기
pie(sentiment_result, main="감성분석 결과", 
    col=c("blue","red","green"), radius=0.8) # ->  1.2




model_twt <- naiveBayes(train_twt[-4], train_twt$sentiment)
summary(model_twt)
str(model_twt)


pred_twt <- predict(model_twt, test_twt)
pred_twt


table(pred_twt, test_twt$sentiment) # 예측결과, 원형 test의 y변수   


# 분류 정확도
(13 + 16 + 12) / nrow(test) # 0.9111111



##################################################
## Naive Bayes 응용실습 : 기상데이터 분석
##################################################

# 1. 데이터 가져오기 
setwd("c:/ITWILL/2_Rwork/Part-IV")
weatherAUS <- read.csv('weatherAUS.csv')
weatherAUS <- weatherAUS[, c(-1,-2, -22, -23)] # 칼럼 제외 

# 2. 데이터 생성/전처리  
set.seed(415)
idx <- sample(1:nrow(weatherAUS), 0.7*nrow(weatherAUS))
train_w <- weatherAUS[idx, ]
test_w  <- weatherAUS[-idx, ]

head(train_w)
head(test_w)
dim(train_w) # [1] 25816    20
dim(test_w) # [1] 11065    20


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