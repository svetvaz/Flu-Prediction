#setwd('data')
fluData <- read.table('FinalDataSet_3_3.csv',sep=',',header=TRUE)
train<-fluData[1:520,1:12]
# Use columns 1-9 as training data
train_x<-train[,1:9]
# The last column Flu Severity is the result
train_y<-train[,12]
par(mar = rep(4, 4))
out <- knn.cv(train_x,train_y,k=1)
print('Error with KNN before scaling:')
(1-sum(abs(train_y == out))/length(out))
train2 <- train_x
for(i in seq(from = 1, to = ncol(train_x))){
  v = var(train_x[,i])
  m = mean(train_x[,i])
  train2[,i] <- (train_x[,i]-m)/sqrt(v)
}


train2[,10]<-train_y
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
plot(Err,pch=c(15,15),col="red",xlab = 'K value', ylab='Error')

# Now include flu postive pct & ILI severity, cos decision trees dont seem to do very well without these attributes
train_x<-train[,1:11]
train_y<-train[,12]
# Not re scaling as it doesn't make much of a change for the other methods
train2 <- train_x
# for(i in seq(from = 1, to = ncol(train_x))){
#   v = var(train_x[,i])
#   m = mean(train_x[,i])
#   train2[,i] <- (train_x[,i]-m)/sqrt(v)
# }
index <- 1:nrow(train2)
train2[,12]<-train_y
#find the test set
testindex <- sample(index, trunc(length(index)/3))
testset <- train2[testindex, ]
trainset <- train2[-testindex, ]

# Predict using decision trees
library(rpart)
rpart.model <- rpart(V12 ~ ., data = trainset,control=rpart.control(maxdepth=5, minsplit=2))
prp(rpart.model) 
fancyRpartPlot(rpart.model)
rpart.pred <- predict(rpart.model, testset[, -12])
print('Error with decision trees')
(1-(sum(rpart.pred == testset[,12])/length(testset[,12])))

# Treat the result as factor and predict with random forests
trainset[,12] <- as.factor(trainset[, 12])
modelRF <- randomForest(V12 ~ ., data = trainset)
modelRF
plot(modelRF, log="y")
legend("topright", legend=unique(trainset$V12), col=unique(trainset$V12), pch=19)
varImpPlot(modelRF)
(predRF <- predict(modelRF, testset[, -12]))
print('Error with Random forest : ')
(1-(sum(predRF == testset[,12])/length(testset[,12])))

# Predict with SVM
svm.model <- svm(V12 ~ ., data = trainset, cost = 100, gamma = 1)
svm.pred <- predict(svm.model, testset[, -12])
print('Error with SVM before tuning')
(1-sum(svm.pred == testset[,12])/length(testset[,12]))
obj <- tune(svm, V12~., data = trainset,
            ranges = list(gamma = 2^(-12:2), cost = 5^(2:4)),
            tunecontrol = tune.control(sampling = "cross"))

obj$best.parameters
plot(obj)
svm.model <- svm(V12 ~ ., data = trainset, cost = 625, gamma = 0.00390625)
plot(svm.model, trainset, WeightedILIPCT ~ Region, svSymbol = 1, dataSymbol = 2, 
     color.palette = cm.colors)
svm.pred <- predict(svm.model, testset[, -12])
print('Error with SVM after tuning : ')
(1-sum(svm.pred == testset[,12])/length(testset[,12]))

# Linear regression
lmflu <- lm(V12~., data = trainset)
print("Predicting error using above model")
lmFit <- predict(lmflu,newdata = testset[,-12])
accuracy<-sum(round(lmFit)== testset[,12])/length(testset[,12])
print('Error with linear regression : ')
(1-accuracy)
plot(round(lmFit) ~ WeightedILIPCT, data = testset,ylab='Flu Severity', xlab = '% Weighted ILI', main='Plot of Flu Severity & % Weighted ILI',pch=c(16,16),col="red")

