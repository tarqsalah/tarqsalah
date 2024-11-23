library(e1071)
library(randomForest)
library(caret)
library(xgboost)
library(naivebayes)

library(pROC)
library(fastDummies)
library(rpart)
library(scales)
library(dplyr)
library(rpart.plot)
library(wconf)
library(Metrics)
library(vip)



data <- read.csv("complete_clusters_data.csv") %>% select(-c(7,8,9))


data <- data %>% mutate(Cluster_1 = ifelse(Cluster==1,1,0),Cluster_2 = ifelse(Cluster==2,1,0),Cluster_3 = ifelse(Cluster==3,1,0))

## rf & svm model type (classification,regression) is based on response variable type.  

data$Cluster<-as.factor(data$Cluster)

data$Cluster_1<-as.factor(data$Cluster_1)
data$Cluster_2<-as.factor(data$Cluster_2)
data$Cluster_3<-as.factor(data$Cluster_3)

data<-dummy_cols(data,select_columns =c("Aquifer_type","SoilDrainage","Vulnerability",
                                        "Permeability","DTB","Transmissivity"), remove_selected_columns =T)

data[,1:3] <- lapply(data[,1:3], rescale)

set.seed(123)

##split data into a train and test set:
splits   <- createDataPartition(data$Cluster_2,p=0.7, list = FALSE)
trainset <- data[splits,-c(1,2,4)]
testset  <- data[-splits,-c(1,2,4)]


## svm
## cost=100 is a general penalizing parameter (classification) and
## gamma is the radial basis function-specific kernel parameter.
## svm will scale dataset by default...
svm.model <- svm(Cluster~. , data=trainset, cost=100, gamma= 1)
svm.pred  <- predict(svm.model, testset[-1])
## svm performance
svm.tab <- table(pred= svm.pred, true=testset[,1])
svm.conf<-confusionMatrix(svm.tab)

wconfusionmatrix(confusionMatrix(svm.tab),weight.type = "arithmetic", print.weighted.accuracy = TRUE)

recalls<- svm.conf$byClass[, "Sensitivity"]
#macro_average_recall
mean(recalls)

support<-table(testset[,1])
#weighted_average_recall
sum(recalls * support)/sum(support)


##Decision Trees
tree.model <- rpart(Cluster_3~., data=trainset,  method = "class")
tree.pred <- predict(tree.model, testset[,-1], type = "class")


## tree performance
## type 2 is decision tree display type, 
#extra=101 is to display number& percentage in the node.

tree.tab <- table(pred= tree.pred, true=testset[,1])
tree.conf<-confusionMatrix(tree.tab)
rpart.plot(tree.model, type=2, extra= 101)
vip(tree.model)

wconfusionmatrix(confusionMatrix(tree.tab),weight.type = "arithmetic", print.weighted.accuracy = TRUE)

recalls<- tree.conf$byClass[, "Sensitivity"]

#macro_average_recall
mean(recalls)
support<-table(testset[,1])

#weighted_average_recall
sum(recalls * support)/sum(support)



## Random Forest
rf.model <- randomForest(Cluster_2~., data=trainset)
rf.pred  <-  predict(rf.model,   testset[,-1])

## rf performance
rf.tab <- table(pred= rf.pred, true= testset[,1])
rf.conf<-confusionMatrix(rf.tab)
varImpPlot(rf.model)
vip(rf.model)

recalls<- rf.conf$byClass[, "Sensitivity"]

#macro_average_recall
mean(recalls)
support<-table(testset[,1])
#weighted_average_recall
sum(recalls * support)/sum(support)
wconfusionmatrix(confusionMatrix(rf.tab),weight.type = "arithmetic", print.weighted.accuracy = TRUE)








#extreme Gradient Boosting (xgboost)
## XGB only handle xgb matrix

dtrain.x=as.matrix(trainset[,-1])
dtest.x =as.matrix(testset[,-1])

dtrain.y=as.numeric(trainset$Cluster)-1
dtest.y =as.numeric(testset$Cluster)-1

parameters<- list(eta=0.3,
                  max_depth =6,
                  subsample= 1,
                  colsample_bytree =1,
                  min_child_weight = 1,
                  gamma =0,
                  objective= "multi:softmax",
                  num_class=3,
                  eval_metric="auc",
                  booster ="gbtree"
                 )

# watchlist = list(train=dtrain, test=dtest)

xgb.model <- xgboost(data=dtrain.x, label =dtrain.y, params = parameters, nround=100)
xgb.pred <- predict(xgb.model,newdata=dtest.x, type="class")


## xgb performance
xgb.tab <- table(pred=xgb.pred, true=dtest.y)
xgb.conf<-confusionMatrix(xgb.tab)

xgb.plot.shap(data= dtest.x, model= xgb.model, top_n=5)

wconfusionmatrix(confusionMatrix(xgb.tab),weight.type = "arithmetic", print.weighted.accuracy = TRUE)

recalls<- xgb.conf$byClass[, "Sensitivity"]

#macro_average_recall
mean(recalls)
support<-table(testset[,1])
#weighted_average_recall
sum(recalls * support)/sum(support)











## naive bayes multinational (Works with classification problems) 

## independent variable should be in matrix format, preparing the matrix.

train.matrx <- as.matrix(trainset[,-c(1)])
test.matrx <- as.matrix(testset[,-c(1)])
nb.model<- multinomial_naive_bayes(train.matrx,trainset[,1])
nb.pred <- predict(nb.model, newdata = test.matrx, type="class")



## naive bayes performance
nb.tab <- table(pred=nb.pred, true=testset[,1])
nb.conf<-confusionMatrix(nb.tab)
vip(nb.model)

wconfusionmatrix(confusionMatrix(nb.tab),weight.type = "arithmetic", print.weighted.accuracy = TRUE)

recalls<- nb.conf$byClass[, "Sensitivity"]

#macro_average_recall
mean(recalls)
support<-table(testset[,1])
#weighted_average_recall
sum(recalls * support)/sum(support)





#ROC and AUC graph:



par(pty="s")

roc.1 <- roc(trainset$Cluster_1, rf.model$votes[,1], plot=TRUE, legacy.axes=TRUE, 
    percent=TRUE,xlab="False Positive Percentage.", ylab= "True Positive Percentage", col="#3182bd", 
    lwd=4, print.auc=TRUE)


roc.2 <- plot.roc(trainset$Cluster_2, rf.model$votes[,1],legacy.axes=TRUE, 
    percent=TRUE,xlab="False Positive Percentage.", ylab= "True Positive Percentage", col="#4daf4a", 
    lwd=4, print.auc=TRUE, add= TRUE, print.auc.y=40)

roc.3 <- plot.roc(trainset$Cluster_3, rf.model$votes[,1],legacy.axes=TRUE, 
         percent=TRUE,xlab="False Positive Percentage.", ylab= "True Positive Percentage", col="#de2d26", 
         lwd=4, print.auc=TRUE, add= TRUE, print.auc.y=60)


legend("bottomright", legend=c("Cluster I","Cluster II","Cluster III"), col=c("#3182bd","#4daf4a","#de2d26"),lwd=4)
