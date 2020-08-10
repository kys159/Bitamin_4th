rm(list=ls())
library(HSAUR)
library(car)

data(heptathlon)

# hurdles : 110m 허들 , highjump : 높이뛰기 , shot : 포환던지기 , run200m : 200m 달리기
# longjump : 멀리뛰기, javelin : 창던지기, run800m : 800m 달리기 , score : 타갯

head(heptathlon)
str(heptathlon)

plot(heptathlon)
cor(heptathlon)

# 다른변수와 마찬가지로 양의 상관관계를 갖도록 값 변경
heptathlon$hurdles <- max( heptathlon$hurdles ) - heptathlon$hurdles
heptathlon$run200m <- max( heptathlon$run200m ) - heptathlon$run200m
heptathlon$run800m <- max( heptathlon$run800m ) - heptathlon$run800m

# 다중공선성 예시로 회귀모형 fit
fit<-lm(score~.,data=heptathlon)
summary(fit)

# 다중공선성 존재
vif(fit)

# cor ; 공분산행렬 쓸 것인지, 상관계수 행렬 쓸 것인지, scores : 주성분 점수, loading : 주성분계수 (고유벡터)
pc.fit<-princomp(subset(heptathlon,select=-score),cor = T, scores = T)

pc.fit$scores # 새로 계산된 행렬을 의미 (Z행렬)
cor(pc.fit$scores) # 상관계수 0
pc.fit$loadings # 주성분 계수 , 0에 가까운값은 빈칸, SS loadings 이부분은 요인분석 내용

# 누적 분산 및 분산 비율 확인
summary(pc.fit)
screeplot(pc.fit , type = "l" , pch = 19 , main = "screeplot") # 2개만 선택

# 해석을 위한 그림, scale은 정규화해서 볼것인지에 대한 여부, 이상치에 대한 여부도 확인 가능
#가까운 거리와 방향일수록 변수들의 상관성이 높아지게 된다. 
# pc1은 창던지기를 제외한 모든 변수가 큰 값을 가짐 - 전체적인 운동능력 지표 변수
# pc2 는 창던지기가 매우 큰 값을 가짐 이에대한 변수일 것으로 예상되나 해석 어렵..
biplot(pc.fit, scale = F , cex = 1)

# pca한 데이터
new_data<-as.data.frame(cbind(pc.fit$scores[,1:2],heptathlon$score))
head(new_data)
colnames(new_data)[3]<-"score"

# pca 한 데이터로 회귀모형 fitting
pc.lm<-lm(score~.,data=new_data)
summary(pc.lm)

# 성능 차이 거의 없음
summary(fit)$adj.r.squared
summary(pc.lm)$adj.r.squared

# 다중공선성 문제 해결
vif(pc.lm) ; cor(new_data[,1:2])


#prcomp 함수
# 공분산행렬의 고유값을 이용하는것이 아닌, 
#원 데이터에 대해 SVD(Singular Value Decomposition : 특이값분해)
#를 수행하여 계산한다. 이 방법이 통상 정확도 면에서 더 선호된다고 한다.
# 고유값분해는 특이값 분해의 특이한 케이스 이므로 두 가지 방법은 큰 차이는 없다.
# center는 평균을 0으로 , scale은 분산을 1로
svd.fit<-prcomp(subset(heptathlon,select=-score),center=T, scale = T)
svd.fit
svd.fit$rotation 
pc.fit$loadings 

# 고유벡터 loadings와 같음
# prcomp와 비교하면 계수가 반대로 되어있는 애들이 있음,, svd를 해서 차이가 나는게 아닐까?

svd.fit$x # 주성분 점수, scores와 같음

vars <- apply(svd.fit$x, 2, var)  
props <- vars / sum(vars)
props
cumsum(props)


screeplot(svd.fit,type="l",pch=19)

biplot(svd.fit) # 방향만 반대로 된 것을 알 수 있음.

