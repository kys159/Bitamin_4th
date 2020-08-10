### parallel 패키지
library(parallel)

# parLapply, parSapply

# 코어 개수 획득
numCores <- parallel::detectCores() - 1

# 클러스터 초기화 -> 백엔드 결과 추가하기(작업관리자)
myCluster <- parallel::makeCluster(numCores)

setwd("C:/Users/82104/Desktop")

iseq <- seq(1, 10000, 1)

parLapply(myCluster, iseq, function(y){
  write(y, "progress.txt", append=T)
}
)

# 클러스터 중지
parallel::stopCluster(myCluster)


# CPU 병렬처리
a = parallel::parLapply(cl = myCluster, X = 2:4, fun = function(x) {2^x})

# 순서 다름

setwd("C:/Users/82104/Desktop")

iseq <- seq(1, 10000, 1)

parLapply(myCluster, iseq, function(y){
  write(y, "progress.txt", append=T)
}
)

#순서맞추기
results <- unlist(clusterCall(myCluster, iseq, function(y){
  write(y, "progress.txt", append=T)
}
))


# 클러스터 중지
parallel::stopCluster(myCluster)

2^999


# 변수 스코프
# 코어 개수 획득 
numCores <- parallel::detectCores() - 1

# 클러스터 초기화

myCluster <- parallel::makeCluster(numCores)

# 변수 등록
base <- 2
parallel::clusterExport(myCluster, "base")
# CPU 병렬처리
parallel::parLapply(cl = myCluster,
                    X = 2:4,
                    fun = function(x) {
                      base^x
                    })
# 클러스터 중지
parallel::stopCluster(myCluster)



###foreach 패키지
library(foreach)
library(doParallel)

# 코어 개수 획득
numCores <- parallel::detectCores() - 1

# 클러스터 초기화
myCluster <- parallel::makeCluster(numCores)
doParallel::registerDoParallel(myCluster)

# 변수 등록, 안해도 상관없음
base <- 2
parallel::clusterExport(myCluster, "base")

# CPU 병렬처리 , c는 cbind 느낌
foreach::foreach(exponent = 2:4, .combine = c)  %dopar% {
  aa = base^exponent
}



# 코어 개수 획득
numCores <- parallel::detectCores() - 1

# 클러스터 초기화
myCluster <- parallel::makeCluster(numCores)
doParallel::registerDoParallel(myCluster)

set.seed(1234)
folds = createFolds(iris$Sepal.Length,k=3)

td_tmp = foreach::foreach(k = 1:3,
                 .combine = rbind,
                 .packages=c("dplyr","broom","caret"),
                 .inorder = TRUE) %dopar% {
                     tmp = iris[-unlist(folds[k]),] %>% group_by(Species) %>% 
                     do(fit = lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = .))
                     tidy(tmp,fit)
                 }

# 클러스터 중지
parallel::stopCluster(myCluster)


# rbind는 행으로 붙힘
#foreach::foreach(exponent = 2:4, .combine = rbind)  %dopar% {
#  base^exponent
#}





# foreach() 함수를 별도의 외부 함수로 정의할 경우 오류가 나므로 외부변수를
#사용하는데 있어 불편함을 해소하기 위하여 export 옵션 제공
test <- function (exponent) {
  foreach::foreach(exponent = 2:4, 
                   .combine = c,
                   .export = "base") %dopar% {
                     base^exponent
                   }
}

test()



# 하나의 클러스터가 fail이 났다고 해서 전체 연산을 중지시키면 좀..
# try를 이용하여 에러를 catch하고 발생원인 설명하는 텍스트 반환

# 코어 개수 획득
numCores <- parallel::detectCores() - 1

# 클러스터 초기화
myCluster <- parallel::makeCluster(numCores, type = "PSOCK")

# CPU 병렬처리
foreach(x=list(1, 2, "a"))  %dopar% {
  tryCatch({
    c(1/x, x, 2^x)
  }, error = function(e) {
    return(paste0("The variable '", x, "'", " caused the error: '", e, "'"))
  })
}

# 클러스터 중지
parallel::stopCluster(myCluster)





### 시간 비교

# 소인수 분해 함수
eratosthenes<-function(n){
  residue<-2:n
  while(n %in% residue){
    p<-residue[1]
    residue<-residue[as.logical(residue%%p)]
  }
  return(p)
}

set.seed(1234)
test<-sample(2*1:10^5+1,1000)

# 일반 for문 소인수분해 
system.time({
  for(n in test){
    eratosthenes(n)
  }
})

# 병렬처리 소인수분해
numCores <- detectCores() -1
myCluster <- makeCluster(numCores)
registerDoParallel(myCluster)

system.time({
  foreach(n = test, .combine = c) %dopar% {
    eratosthenes(n)
  }
})

stopCluster(myCluster)



### 병렬처리가 늦을때도 있다!!
system.time({for(i in 1:10000){
  i+5
}})

n_core = detectCores()
cl = makeCluster(n_core-1)
registerDoParallel(cl)
system.time(
  {foreach(i = 1:10000) %dopar%{
    i+5
  }}
)
stopCluster(cl)


### h2o 활용한 병렬처리
library(dplyr)
library(caret)

flights_data = readRDS("C:/Users/82104/Desktop/flights.RDS")

head(flights_data)
str(flights_data)

flights_data$target <- ifelse((is.na(flights_data$dep_delay) | (flights_data$dep_delay<=30 & flights_data$dep_delay >= -30)) & 
                                (is.na(flights_data$arr_delay) | (flights_data$arr_delay<=30 & flights_data$arr_delay >= -30)), "normal", "delay")
table(flights_data$target)




final_data <- flights_data %>% select("month","carrier","flight","dest","air_time","distance","target")
str(final_data)

final_data$carrier <- as.factor(final_data$carrier)
final_data$dest <- as.factor(final_data$dest)
final_data$target <- as.factor(final_data$target)



##train, test 나누기
set.seed(1234)
train_idx <- createDataPartition(final_data$target, p=0.7, list = F)
train<-final_data[train_idx, ]
test<-final_data[-train_idx, ]

str(train)

#randomForest modeling
#library(randomForest)
#set.seed(1234)
#rf.fit.500 = randomForest(transaction_real_price ~ ., data = train, mtry = floor(ncol(train) / 3), ntree = 500, importance = T)


#install.packages("h2o")


library(h2o)
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk-13.0.2")
h2o.init(nthreads = 15, max_mem_size = "10g")
#h2o::h2o.shutdown(prompt = F)




#http://localhost:54321  : 웹 접속

train_data_h2o <- as.h2o(train, destination_frame = "train_data_h2o")
test_data_h2o <- as.h2o(test, destination_frame = "test_data_h2o")


str(train)

target <- "target"
features <- names(train)[!names(train) %in% target]
target ; features

rf_model <- h2o.randomForest(x = features, y = target, training_frame = train_data_h2o, 
                             model_id = "rf_model", ntrees = 500, seed = 1234, mtries = floor(ncol(train) / 3), verbose = F)
rf_model

test_predict<-h2o.predict(rf_model,newdata=test_data_h2o)
test_predict
summary(test_predict)


h2o.confusionMatrix(rf_model,newdata = test_data_h2o, metrics = "accuracy", thresholds = 0.5)


plot(rf_model)

h2o.varimp_plot(rf_model, num_of_features = 6)

rf_model_correct <- h2o.randomForest(x = features, y = target, training_frame = train_data_h2o, 
                             model_id = "rf_model", ntrees = 213, seed = 1234, mtries = floor(ncol(train) / 3), verbose = T)

rf_model_correct

test_predict2<-predict(rf_model_correct,newdata=test_data_h2o)


