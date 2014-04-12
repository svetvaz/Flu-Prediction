set.seed(pi)
rm(list=ls())
require(klaR)
library(MASS)
library(randomForest)
library(rpart.plot)  
library(rattle)   
library(RColorBrewer) 
library(party) 
library(partykit)  
library(caret) 
library(ggplot2)
library(DAAG)
library("parcor")
library(lars)
library(ridge)
library(class)
#setwd('data')
library(e1071)
# read in the data set
fluData <- read.table('FinalDataSet_3_3.csv',sep=',',header=TRUE)
train<-fluData[1:520,1:11]
# Store the first 9 columns (exclude flu positive pct) into train_x
train_x<-train[,1:9]
# Store the 11th col ILI Severity as the result
train_y<-train[,11]
par(mar = rep(4, 4))
#Try out knn before scaling
out <- knn.cv(train_x,train_y,k=1)
print('Error with KNN before scaling:')
(1-sum(abs(train_y == out))/length(out))
train2 <- train_x
#Now scale the data
for(i in seq(from = 1, to = ncol(train_x))){
  v = var(train_x[,i])
  m = mean(train_x[,i])
  train2[,i] <- (train_x[,i]-m)/sqrt(v)
}
(out <- knn.cv(train2,train_y,k=1))
print('Error with KNN with k=1 after scaling:')
(1-sum(train_y == out)/length(out))
Err <- rep(0,40)
for(kk in seq(from=1,to=40)){
  out <- knn.cv(train2,train_y,k=kk)
  Error <- 1-sum(abs(train_y == out))/length(out)
  Err[kk] <- Error   
}
print('Best k value : ')
(which.min(Err))
print('min Error with Knn : ')
(min(Err))
# plot(Err,pch=c(15,15),col="red",xlab = 'K value', ylab='Error')

#Now Divide into trainset & testset selected randomly so works just like cross validation
index <- 1:nrow(train2)
train2[,10]<-train_y
testindex <- sample(index, trunc(length(index)/3))
testset <- train2[testindex, ]
trainset <- train2[-testindex, ]

# Try decision trees
library(rpart)
rpart.model <- rpart(V10 ~ ., data = trainset,control=rpart.control(maxdepth=5, minsplit=2))
prp(rpart.model) 
fancyRpartPlot(rpart.model)
rpart.pred <- predict(rpart.model, testset[, -10])
print('Error with decision trees')
(1-(sum(rpart.pred == testset[,10])/length(testset[,10])))

# Try random forests
trainset[,10] <- as.factor(trainset[, 10])
modelRF <- randomForest(V10 ~ ., data = trainset)
modelRF
plot(modelRF, log="y")
legend("topright", legend=unique(trainset$V10), col=unique(trainset$V10), pch=19)
(predRF <- predict(modelRF, testset[, -10]))
print('Error with Random forest : ')
(1-(sum(predRF == testset[,10])/length(testset[,10])))


#Try SVM
svm.model <- svm(V10 ~ ., data = trainset, cost = 100, gamma = 1)
svm.pred <- predict(svm.model, testset[, -10])
print('Error with SVM before tuning')
(1-sum(svm.pred == testset[,10])/length(testset[,10]))
# tune the model
obj <- tune(svm, V10~., data = trainset,
            ranges = list(gamma = 2^(-12:2), cost = 5^(2:4)),
            tunecontrol = tune.control(sampling = "cross"))
obj$best.parameters
plot(obj)
# retry with the best parameters after tuning
svm.model <- svm(V10 ~ ., data = trainset, cost = 625, gamma = 0.03125)
plot(svm.model, trainset, WeightedILIPCT ~ Region, svSymbol = 1, dataSymbol = 2, 
     color.palette = cm.colors)
svm.pred <- predict(svm.model, testset[, -10])
print('Error with SVM after tuning : ')
(1-sum(svm.pred == testset[,10])/length(testset[,10]))
plot(svm.pred)

# Linear regression
lmflu <- lm(V10~., data = trainset)
print("Predicting error using above model")
lmFit <- predict(lmflu,newdata = testset[,-10])
accuracy<-sum(round(lmFit)== testset[,10])/length(testset[,10])
print('Error with linear regression : ')
(1-accuracy)
plot(round(lmFit) ~ WeightedILIPCT, data = testset,ylab='ILI Severity', xlab = '% Weighted ILI', main='Plot of ILI Severity & % Weighted ILI',pch=c(16,16),col="red")

#ridge regression
# library(glmnet)
# grid=10^seq(10,-2,length=100)
# X <- as.matrix(trainset[,-10])
# ridge.mod=glmnet(X,train2[,10],alpha=0,lambda=grid)
# pred<-predict(ridge.mod,s=50,type="coefficients")[1:10,]
# pred
# plot(ridge.mod)



# Visualize the flu data set
library(gridExtra)
colnames(fluData)=c('Region','Year','Week','Season','Total No. Patients','No. Of Providers','PositiveWeightedPct','% Positive Unweighted','Total Influenza Test Specimens','InfluenzaPositive','ILISeverity','FluSeverity')
m<-qplot(Region,Week, data = fluData, color = ILISeverity, size=InfluenzaPositive,na.rm = TRUE)
n<-m+scale_y_continuous(breaks=1:52)+scale_x_discrete(labels = c("CT, ME, MA.. ","NJ, NY..","DE, DC, MD..","AL, FL, GA..","IL, IN, MI..","AR, LA, NM..","IA, KS, MO..","CO, MT, ND..","AZ, CA,NV..","AK, ID, OR.."))
n

ggplot(fluData, aes(x=PositiveWeightedPct)) +
  geom_histogram(position="dodge", binwidth=3) +
  scale_fill_brewer()+scale_x_discrete(breaks=0:8)

