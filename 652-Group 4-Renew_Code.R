library(mvnormtest)
library(openxlsx)
library(mice)
library(nortest)
library(car)
library(corrplot)
library(MASS)#bayes
library(class)#knn
library(klaR)#bayes
library(ROCR)
library(gplots)
library(ggplot2)
library(caret)
library(DiagrammeR)
install.packages("DiagrammeR")
########################data preparation######################################
###select variables
TP <- read.xlsx("/Users/jason/Documents/BIA study/Multivariate Data Analysis/Team project/Prepare/shot_logs.xlsx", sheet ="shot_logs")
TP2 <- subset(TP, select = c(-GAME_ID1,-GAME_ID2,-W,-FINAL_MARGIN,-PTS_TYPE,-SHOT_RESULT,-CLOSEST_DEFENDER_PLAYER_ID,-CLOSEST_DEFENDER,-player_name,-player_id,-PTS))
TP2$LOCATION[TP2$LOCATION=="A"]=1
TP2$LOCATION[TP2$LOCATION=="H"]=2
TP2$LOCATION<-as.numeric(TP2$LOCATION)
str(TP2)
head(TP2)
###
### deal with missing value 
md.pattern(TP2)#see how much missing value
TP2[is.na(TP2)]=0 #replace missing value with 0
md.pattern(TP2)#check how much missing value
###
### find outliers
boxplot(TP4,range = 16,names)
length(TP2$TOUCH_TIME[TP2$TOUCH_TIME<0]) #see how many value below 0 
mean(TP2$TOUCH_TIME) #average value of touch time
TP2$TOUCH_TIME[TP2$TOUCH_TIME<0] <- 2.768#
length(TP2$TOUCH_TIME[TP2$PERIOD<0]) #check
#check others
length(TP2$TOUCH_TIME[TP2$FGM>1])
TP2$SHOT_NUMBER[TP2$SHOT_NUMBER>30]
TP2$PERIOD[TP2$PERIOD>7]
TP2$DRIBBLES[TP2$DRIBBLES>30]
TP2$CLOSE_DEF_DIST[TP2$CLOSE_DEF_DIST>28] #the basketball playground is 84 feet(28 meter) long and 50 feet(15 meter) wide
#final check
boxplot(TP2,range = 17,names)
###
### test Multivariate normal distribution
#  p-value< α(0.05) ,refuse normal distribution   , value more close to 0  is more likely nomal
lillie.test(TP2[1:60000,6])
lillie.test(TP2[1:60000,7])
lillie.test(TP2[1:30000,8])
lillie.test(TP2[1:30000,9])
###
###Check the ratio of the dependent output value 
table(TP2$FGM)
###
###standerdize data
TP3<-scale(TP2[1:9],center = TRUE, scale = TRUE)
TP3<-data.frame(TP3)
###
###correlation matrix
library(corrplot)
cor<-cor(TP4)
corrplot(cor,method = "color")
###
###seek the charactor
#1. see the relationship between SHOT_DIST and CLOSE_DEF_DIST
ggplot(data=TP2, aes(x=SHOT_DIST, y=CLOSE_DEF_DIST)) + geom_point(aes(color=factor(FGM)))
#2.  find the best players and the worst players
offdata <-aggregate(FGM ~ player_name, data=TP, FUN=mean, na.rm=TRUE)
head(offdata[order(offdata$FGM), ])
head(offdata[order(-offdata$FGM), ])
#3.

###

###separate training and testing dataset
TP4<-data.frame(TP3,TP2$FGM)
set.seed(342)
train_ind <- sample.int(n = nrow(TP4), size = floor(.75*nrow(TP4)))
train.total <- TP4[train_ind, ]
test.total <- TP4[-train_ind, ]
train<- train.total[1:9]
test<- test.total[1:9]
train.fgm<- train.total$TP2.FGM
test.fgm<- test.total$TP2.FGM
###
###PCA
train.PCA <- prcomp(train, center = TRUE, scale = FALSE)
train.PCA <-data.frame(train.PCA$x)
screeplot(train.PCA,type="lines",main="scree plot")
summary(train.PCA)

test.PCA <- prcomp(test, center = TRUE, scale = FALSE)
test.PCA <-data.frame(test.PCA$x)
screeplot(test.PCA,type="lines",main="scree plot")
summary(test.PCA)
trytrytry<-data.frame(train.PCA,train.fgm)
###
########################claasification######################################
###Linear Discriminant Analysis
fit.LDA2<-lda(TP2.FGM ~ . ,data = train.total)
summary(fit.LDA2)
Y2<-predict(fit.LDA2,test.total)
table(Y2$class==test.fgm)
confusionMatrix(Y2$class,as.factor(test.fgm))
Y3<-predict(fit.LDA2,train.total)
table(Y3$class==train.fgm)
confusionMatrix(Y3$class,as.factor(train.fgm))
###

###
#kNN
fit.knn<-knn(train[,4:9],test[,4:9],train.fgm,k=15)
table(fit.knn==test.fgm)
confusionMatrix(fit.knn,as.factor(test.fgm))
###

###Naïve Bayes Classifications 
NB.model<-NaiveBayes(as.factor(TP2.FGM)~. ,data=train.total)
pred_Bayes<- predict(NB.model,test)
table(pred_Bayes$class==test.fgm)
confusionMatrix(pred_Bayes$class,as.factor(test.fgm))

pred_Bayes2<- predict(NB.model,train)
table(pred_Bayes2$class==train.fgm)
confusionMatrix(pred_Bayes2$class,as.factor(train.fgm))
###
###Logistic Regression
#Without PCA
fit.logi<-glm(TP2.FGM ~., data=train.total,family = "binomial")
summary(fit.logi)
pre5<-predict(fit.logi,test)
pre6<-ifelse(pre5>0.5,1,0)
table(pre6==test.fgm)
confusionMatrix(as.factor(pre6),as.factor(test.fgm))

#plot ROC
pred=prediction(pre5,as.vector(test.fgm))
performance(pred,'auc')@y.values
perf=performance(pred,'tpr','fpr')
plot(perf)
###
###xgboost
install.packages("xgboost")
library(xgboost)
library(readr)
library(caret)
library(car)
library(Matrix)
train2<-as.matrix(train[1:9])
dtrain <- xgb.DMatrix(train2, label = train.fgm)
bstSparse2 <- xgboost(dtrain, max.depth = 6, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic")

bstSparse <- ?xgboost(train2, label = train.fgm, max.depth = 6, eta = 0.01, nthread = 3, nrounds = 2,silent=1,
                     min_child_weight=0.0001, objective = "binary:logistic")
pred9<- predict(bstSparse, as.matrix(test[1:9]))
pre8<-ifelse(pred9>0.5,1,0)
table(pre8==test.fgm)
confusionMatrix(as.factor(pre8),as.factor(test.fgm))

xgb.plot.tree(model = bstSparse) #plot the tree
importance_matrix <- xgb.importance( model = bstSparse)# show the importance of the variables
xgb.plot.importance(importance_matrix)

###
###random forest
library(randomForest)
library(magrittr)
spam.rf <- randomForest(TP2.FGM~.,data=train.total,importance=TRUE,proximity=TRUE)
pred22<- predict(spam.rf,test,type="class")
??confusionMatrix(pred22,df.test[,58])
###
########################ensemble######################################
library(caret)
library(caretEnsemble)
library(caTools)

train.total$TP2.FGM <- as.factor(train.total$TP2.FGM)
levels(train.total$TP2.FGM) <- c("first_class", "second_class")
ctrl <- trainControl(  method = "cv", number = 3, savePredictions = 'final',classProbs = T)

#Training decision tree
dt.ensemble <-train(TP2.FGM~., data=train.total, method="rpart",trControl=ctrl, tuneLength=2)
#Training logistic regression
logit.ensemble <-train(TP2.FGM~., data=train.total, method="glm",trControl=ctrl, tuneLength=2)
#Training lda model
lda.ensemble <-train(TP2.FGM~., data=train.total, method="lda",trControl=ctrl,tuneLength=2)
#Training bayes model
bayes.ensemble <-train(TP2.FGM~., data=train.total, method="naive_bayes",trControl=ctrl,tuneLength=2)
#Check Correlation Matrix of Accuracy
results <- resamples(list(dt.ensemble, logit.ensemble, bayes.ensemble,lda.ensemble))
modelCor(results)

#Predicting probabilities for testing data
test$OOF_dt<- predict(dt.ensemble,test,type='prob')$second_class
temp<-ifelse(test$OOF_dt>0.5,1,0)
confusionMatrix(as.factor(temp),as.factor(test.fgm))
colAUC(test$OOF_dt, test.fgm,plotROC=TRUE)
# 0.61  AUC: 0.59 
test$OOF_logit<-predict(logit.ensemble,test,type='prob')$second_class
temp2<-ifelse(test$OOF_logit>0.5,1,0)
confusionMatrix(as.factor(temp2),as.factor(test.fgm))
colAUC(test$OOF_logit, test.fgm,plotROC=TRUE)
# 0.6085  AUC:0.6321962
test$OOF_lda<-predict(lda.ensemble,test,type='prob')$second_class
temp3<-ifelse(test$OOF_lda>0.5,1,0)
confusionMatrix(as.factor(temp3),as.factor(test.fgm))
colAUC(test$OOF_lda, test.fgm,plotROC=TRUE)
#0.6087 AUC:0.632153
test$OOF_bayes<-predict(bayes.ensemble,test,type='prob')$second_class
temp4<-ifelse(test$OOF_bayes>0.5,1,0)
confusionMatrix(as.factor(temp4),as.factor(test.fgm))
colAUC(test$OOF_bayes, test.fgm,test$OOF_lda,plotROC=TRUE)
# 0.5941 AUC:0.6076
colAUC(c(test$OOF_lda,test$OOF_dt,test$OOF_logit,test$OOF_bayes), test.fgm,plotROC=TRUE)

#Predicting the out of fold prediction probabilities for training data
#In this case, level2 is event
#rowindex : row numbers of the data used in k-fold
#Sorting by rowindex
train.total$OOF_dt<-dt.ensemble$pred$second_class[order(dt.ensemble$pred$rowIndex)]
train.total$OOF_logit<-logit.ensemble$pred$second_class[order(logit.ensemble$pred$rowIndex)]
train.total$OOF_lda<-lda.ensemble$pred$second_class[order(lda.ensemble$pred$rowIndex)]
train.total$OOF_bayes<-bayes.ensemble$pred$second_class[order(bayes.ensemble$pred$rowIndex)]

predictors_top<-c('OOF_dt','OOF_logit','OOF_lda')
model_gbm<- train(train.total[,predictors_top],train.total[,"TP2.FGM"],method='gbm',trControl=ctrl,tuneLength=3)
gbm_stacked<-predict(model_gbm,test[,predictors_top],type = 'prob')
temp5<-ifelse(gbm_stacked$second_class>0.5,1,0)
confusionMatrix(as.factor(temp5),as.factor(test.fgm))
colAUC(gbm_stacked$second_class, test.fgm)


model_glm<- train(train.total[,predictors_top],train.total[,"TP2.FGM"],method='glm',trControl=ctrl,tuneLength=3)
glm_stacked<-predict(model_glm,test[,predictors_top],type = 'prob')
colAUC(glm_stacked$second_class, test.fgm)


