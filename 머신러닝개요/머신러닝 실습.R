rm(list=ls())
library(dplyr)
library(psych)
library(car)
library(randomForest)
data<-read.csv("D:/머신러닝실습/insurance2.csv")
#데이터를 불러오면 구조랑 각종 값들 먼저 확인
str(data)
#범주형 변수 factor로 변경
data[ , c("sex","smoker","region")] = lapply(data[ , c("sex","smoker","region")], factor)

summary(data)
head(data)

#결측치처리 - 평균
hist(data$bmi)
data$bmi[is.na(data$bmi)]<-mean(data$bmi,na.rm=T)

#상당히 skewed되어있음 , 이상치로 볼것이냐?
hist(data$charges)

#범주형 변수들 확인
table(data$sex) ; table(data$smoker) ; table(data$region)
with(data,boxplot(charges~sex))
with(data,boxplot(charges~smoker))
with(data,boxplot(charges~region))


pairs(data[,unlist(lapply(data,is.numeric))])
pairs.panels(data[,unlist(lapply(data,is.numeric))]) #bmi 일정값 이상부터 요금이 급격히 커지는데 또 갈라짐

data$bmi30<-as.factor(ifelse(data$bmi>=30,"비만","정상"))

boxplot(data$charges~data$bmi30) #비만인 사람이 좀더 비용이 많아보이긴 하지만 그닥?
boxplot(data$charges~data$bmi30*data$smoker) # 하지만 흡연과 같이 본다면?
#데이터분할 랜덤하게 6:2:2
set.seed(123)
index<-sample(c("train","valid","test"),size=nrow(data),replace=T,prob=c(0.6, 0.2, 0.2))
table(index)

train<-data[index=="train",]
valid<-data[index=="valid",]
test<-data[index=="test",]

#범주형 변수 reduced rank 방식 모델매트릭스 but, 알아서 해준다~
model.matrix(~., data = train)

#원래 변수 회귀모형 fiiting
fit<-lm(charges~age+sex+bmi+children+smoker+region,data=train)
summary(fit) #잔차 median이 -999인걸로 보아 등분산 가정 안맞을 가능성 농후

par(mfrow=c(2,2))
plot(fit)
summary(train[train$charges<=20000,]) ;summary(train[train$charges>20000,]) #앞서 산점도 확인한것처럼 갈림

par(mfrow=c(1,2))
hist(data$charges) ; hist(log(data$charges)) 

#로그변환 및 bmi와 흡연 interaction 추가
fit2<-lm(log(charges)~age+sex+children+region+bmi30*smoker,data=train)
summary(fit2)

par(mfrow=c(2,2))
plot(fit2)

vif(fit2)
Anova(fit2,type=3)

#평가지표 rmse, 겁나 못만든 모델이구나~
rmse<-sqrt(sum((exp(predict(fit2,newdata=train))-train$charges)^2)/fit2$df.residual)
rmse ; summary(train$charges)

#validation에 rmse가 더 작은 이유는 train에 있던 이상치가 없어서인 것 같음.
valid_rmse<-sqrt(sum((exp(predict(fit2,newdata=valid))-valid$charges)^2)/(nrow(valid)-fit2$rank))
valid_rmse ; summary(valid$charges) ; summary(train$charges)

par(mfrow=c(1,1))
plot(predict(fit2,newdata=valid),log(valid$charges))
plot(exp(predict(fit2,newdata=valid)),valid$charges)
#x=y직선에 근접해있지 않음.
abline(a=0,b=1,col="red")     

#랜덤포레스트
set.seed(123)
rf.fit<-randomForest(charges~age+sex+children+smoker+region+bmi30*smoker,data=train)
rf.fit
sqrt(26970068)

rf.pre<-predict(rf.fit,newdata=valid)
#회귀분석이 아니면 n으로 나눔
rf.rmse<-sqrt(sum((rf.pre-valid$charges)^2)/nrow(valid))
rf.rmse
plot(rf.pre,valid$charges)
abline(a=0,b=1,col="red")     

#랜덤포레스트로 최종모형 결정
nrow(test)
final_pre<-predict(rf.fit,newdata=test)
sqrt(sum((final_pre-test$charges)^2)/nrow(test))
plot(final_pre,test$charges)
abline(a=0,b=1,col="red")     
