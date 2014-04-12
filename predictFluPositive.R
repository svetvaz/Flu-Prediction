#setwd('data')
# read in the data set
fluData <- read.table('FinalDataSet_3_3.csv',sep=',',header=TRUE)
train<-fluData[1:520,1:10]
# 1st 9 columns are attributes
train_x<-train[,1:9]
#10th column (Flu Positive Pct) is the result
train_y<-as.numeric(train[,10])

#Tried out KNN but didnt get good predictions. I'm not scaling the data as I did not see changes in the predictions while using other models
train2 <- train_x
# # for(i in seq(from = 1, to = ncol(train_x))){
# #   v = var(train_x[,i])
# #   m = mean(train_x[,i])
# #   train2[,i] <- (train_x[,i]-m)/sqrt(v)
# # }
# 
# #Try out knn after scaling
# (out <- knn.cv(train2,train_y,k=1))
# print('Error with KNN with k=1 after scaling:')
# (1-sum(train_y == out)/length(out))

# Now try dividing into train & test sets using random sampling
index <- 1:nrow(train2)
train2[,10]<-train_y
testindex <- sample(index, trunc(length(index)/3))
testset <- train2[testindex, ]
trainset <- train2[-testindex, ]

# Try out decision trees
library(rpart)
rpart.model <- rpart(V10 ~ ., data = trainset,control=rpart.control(maxdepth=5, minsplit=2))
prp(rpart.model) 
fancyRpartPlot(rpart.model)
yhat <- predict(rpart.model, trainset[, -10])
dY <- trainset[,10] - yhat
print('Train Error with decision trees')
(trainError <- sqrt(sum(dY*dY))/(length(trainset[,10]))) 
yhat <- predict(rpart.model, testset[, -10])
dY <- testset[,10] - yhat
print('Test Error with decision trees')
(testError <- sqrt(sum(dY*dY))/(length(testset[,10]))) 

# Try out random forest
modelRF <- randomForest(V10 ~ ., data = trainset)
modelRF
plot(modelRF, log="y")
varImpPlot(modelRF)
(yhat <- predict(modelRF, testset[, -10]))
dY <- testset[,10] - yhat
print('Test Error with random forests')
(testError <- sqrt(sum(dY*dY))/(length(testset[,10]))) 


# SVM
svm.model <- svm(V10 ~ ., data = trainset, cost = 100, gamma = 1)
yhat<- predict(svm.model, testset[, -10])
dY <- testset[,10] - yhat
print('Test Error with SVM before tuning')
(testError <- sqrt(sum(dY*dY))/(length(testset[,10]))) 
# tune the model
obj <- tune(svm, V10~., data = trainset,
            ranges = list(gamma = 2^(-12:2), cost = 5^(2:4)),
            tunecontrol = tune.control(sampling = "cross"))

obj$best.parameters
plot(obj)
# re build model using the best parameters
svm.model <- svm(V10 ~ ., data = trainset, cost = 25, gamma = 0.125)
plot(svm.model, trainset, FluSpecimens ~ Region,color.palette = cm.colors)
yhat<- predict(svm.model, trainset[, -10])
dY <- trainset[,10] - yhat
print('Train Error with SVM after tuning')
(trainError <- sqrt(sum(dY*dY))/(length(trainset[,10]))) 

#test error
yhat<- predict(svm.model, testset[, -10])
dY <- testset[,10] - yhat
print('Test Error with SVM after tuning')
(testError <- sqrt(sum(dY*dY))/(length(testset[,10]))) 


# Linear regression
lmflu <- lm(V10~., data = trainset)
print("Predicting error using above model")
lmFit <- predict(lmflu,newdata = testset[,-10])
dY <- round(testset[,10]) - round(lmFit)
(testError <- sqrt(sum(dY*dY))/(length(testset[,10]))) 
print('Error with linear regression : ')
testError
plot(round(lmFit) ~ WeightedILIPCT, data = testset,ylab='% Flu Positive', xlab = '% Weighted ILI', main='Plot of % Flu Positive & % Weighted ILI',pch=c(16,16),col="red")
