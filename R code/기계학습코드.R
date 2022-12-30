library(dplyr)

comment <- "
### datause.csv
loadfile <- file.choose()
datause <- read.csv(loadfile,header=T)
View(datause)
### Factor요소로 바꾸기
datause$yn <- datause$yn %>% as.factor()
datause$hosun <- datause$hosun %>% as.factor()
datause %>% str()
### 구분할 수 있는 행 이름으로 바꾸기
rownames(datause)<-paste0(datause$hosun,datause$name)
rn <- rownames(datause)

# train, test data 분할해서 csv로 저장하기
#idx <- sample(2, nrow(datause), replace=T, prob=c(0.7, 0.3))
#data.train <- datause[idx==1, ]
#data.test <- datause[idx==2, ]
#write.csv(data.train, file='train.csv', row.names = F)
#write.csv(data.test, 'test.csv', row.names = F)
"

# train, test data 불러오기
trainfile <- file.choose()
testfile <- file.choose()
data.train <- read.csv(trainfile, header=T)
data.test <- read.csv(testfile, header=T)

#의사결정나무
library(rpart)
library(rpart.plot)
fit.rpart <- rpart(yn ~ hosun+avgprice+size_under+size_over+승차+하차, data=data.train)
prp(fit.rpart, type=2, extra=4)
rpart.plot(fit.rpart)
pred.rpart <- predict(fit.rpart, data.test, type="class")
table(data.test$yn, pred.rpart)
#accuracy = 0.76
confusionMatrix(refer=data.test$yn, data=pred.rpart, positive="1")
roc.curve(data.test$yn, pred.rpart)

#변수중요도 
model <- train(yn ~ hosun+avgprice+승차+하차, data=data.train, method="rpart")
importance <- varImp(model, scale=F)
plot(importance, cex.lab=0.5)




##############################################################
# 로지스틱 회귀분석
data.train %>% summary()
data.test %>% summary()
data.train$yn <- as.factor(data.train$yn)
data.test$yn <- as.factor(data.test$yn)

#fit.lm <- lm(yn ~ avgprice+size_under+size_over+승차+하차, data=data.train, na.rm=T)
#summary(fit.lm)
library(ROSE)
fit.glm <- glm(yn ~ hosun+avgprice+size_under+size_over+승차+하차, data=data.train, family="binomial")
summary(fit.glm)
#avgprice만 유의함

pred.prob <- predict(fit.glm, data.test)
pred.glm <- ifelse(pred.prob > 0.25, 1, 0) %>% factor()

table(data.test$yn, pred.glm)
caret::confusionMatrix(reference=data.test$yn, data=pred.glm, positive="1") 
#accuracy = 0.7042

#ROC curve & AUC = 0.878
ROSE::roc.curve(data.test$yn, pred.glm)





##############################################################
#서포트 벡터 머신 SVM
#알아서 스케일 변환하므로 preProcess 안해도 됨
library(e1071)
fit.svm <- svm(yn ~ hosun+avgprice+승차+하차, data=data.train,
               type="C-classification", kernel="radial", gamma=0.1, cost=1)
pred.svm <- predict(fit.svm, data.test)
table(data.test$yn, pred.svm)
confusionMatrix(refer=data.test$yn, data=pred.svm, positive = "1")
#auc 
roc.curve(data.test$yn, pred.svm, main="ROC curve - SVM")

#변수중요도
library(kernlab)
model <- train(yn ~ hosun+avgprice+승차+하차, data=data.train, method="svmRadial")
#importance <- varImp(model, scale=T)





###################################################################################
#신경망
#표준화 스케일 변환 필요
data.pre.pro <- preProcess(data.train, method=c("center", "scale")) #평균=0, sd=1
data.train <- predict(data.pre.pro, data.train) #스케일 변환된 데이터 저장
data.test <- predict(data.pre.pro, data.test)
data.train %>% summary()
data.test %>% summary()

library(nnet)
### 모형적합
fit.nnet <- nnet(yn ~ hosun+avgprice+size_under+size_over+승차+하차, data=data.train, size=10, maxit=1000) # size: hidden unit 수

### 검정자료 예측
pred.nnet <- predict(fit.nnet, data.test, type="class") %>% factor()
pred.nnet

### 정오분류표
table(data.test$yn, pred.nnet)

### accuracy
caret::confusionMatrix(refer=data.test$yn, data=pred.nnet, positive="1")

### ROC & AUC
roc.curve(data.test$yn, pred.nnet)

### 변수중요도
library(NeuralNetTools)
garson(fit.nnet) + coord_flip()


#              Accuracy   AUC
# 로지스틱 회귀 0.7042   0.702
# 의사결정나무  0.7606   0.758
# SVM           0.7183   0.716
# 신경망        0.6479   0.648