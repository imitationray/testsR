#################################
## <제1장 연습문제>
#################################

#01. 현재 작업 공간을 확인하고 "D:/ITWILL/2_Rwork/output"로 변경하시오.
getwd() 
setwd("C:/ITWILL/2_Rwork/output")
getwd()

#02. 다음 조건에 맞게 name, age, address 변수를 생성하고 출력하시오.
#조건1) name, age, address 변수에 자신의 이름, 나이, 주소를 만들고 출력한다. 
#조건2) mode() 함수를 이용하여 각 변수의 자료형(data type)을 확인한다.
name <- c('장민석')
age <- c(27) # 만으로 25
address <- c('경기도 안산시')
name; age; address
mode(name)
mode(age)
mode(address)

#03. 다음 rain 변수는 비 유무를 나타내는 변수이다. 이 변수를 요인형으로 변경하시오.  
rain <- c('YES', 'No', 'YES', 'YES', 'NO')
rain <- as.factor(rain)
rain
mode(rain)

#04. R에서 제공하는 women 데이터 셋을 다음과 같이 처리하시오.
data("women")

#조건1) women은 어떤 데이터 셋 인지를 쓰시오?
help(women) # Average Heights and Weights for American Women 1975년도 30대 미국여성의 평균 키와 몸무게
View(women)
dim(women) # [1] 15(row) 2(col)
dim(iris3) # [1] 50(row) 4 3

#조건2) women 데이터 셋의 자료형은 무엇인가?
mode(women) # "list"

#조건3) plot() 함수를 이용하여 기본 차트 그리기
plot(women)

#05. R에서 제공하는 c()함수를 이용하여 벡터를 생성하고, 데이터를 처리하시오.
#조건1) 1~10까지 벡터를 생성한다.
va <- c(1:10)

#조건2) 생성된 벡터를 대상으로 평균을 구한다.
mean(va) # 5.5

#06. R 프로그래밍 언어의 특징에서 in memory computing에 대해서 설명하시오.
data() # 모든 연산이 메모리에서 처리된다는 의미