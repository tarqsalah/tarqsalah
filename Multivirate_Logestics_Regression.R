# Load the library for logistic regression
library(MASS)
library(dplyr)
library('dplyr')
library(fastDummies)
library(caret)
library(randomForest)



# Define function for stepwise logistic regression
logistic_regression_stepwise <- function(X, y) {
  # Combine the response variable with the predictors
  data_combined <- cbind(y, X)
  
  # Fit the full model
  full_model <- glm(y ~ ., data = data_combined, family = binomial)
  
  # Perform stepwise regression
  step_model <- step(full_model, direction = "both", trace = FALSE)
  
  return(step_model)
}

Data <- read.csv("GWL_100m_Statistical analysis.csv")
Data$Cluster<- as.factor(Data$Cluster)

Data_dummy <-dummy_cols(Data$Cluster)



# Extract input variables:

categorical<-c("SoilDraina","AQUIFER_CAT","VUL_CAT","SSPERM", "T", "DTB")
Data[categorical] <- lapply(Data[categorical], factor)
X<- Data[,c("MEDIAN_Slope","MEDIAN_DEM","MEDIAN_HAND","SoilDraina","AQUIFER_CAT","VUL_CAT","SSPERM", "T", "DTB")]


#Response variables:
y_cluster <- as.factor(Data$Cluster)

y_cluster1 <- as.factor(Data_dummy$.data_1)
y_cluster2 <- as.factor(Data_dummy$.data_2)
y_cluster3 <- as.factor(Data_dummy$.data_3)


# Perform logistic regression for cluster
result_cluster <- logistic_regression_stepwise(X, y_cluster)


result_cluster <- logistic_regression_stepwise(X, y_cluster1)
result_cluster <- logistic_regression_stepwise(X, y_cluster2)
result_cluster <- logistic_regression_stepwise(X, y_cluster3)



# Print results
summary(result_cluster)
exp(coef(result_cluster))
exp(confint(result_cluster))



summary(result_cluster1)
exp(coef(result_cluster1))
exp(confint(result_cluster1))

summary(result_cluster3)
exp(coef(result_cluster3))
exp(confint(result_cluster3))



exp(coef(result_cluster2))
exp(coef(result_cluster3))





# Random Forest_Feature Selection Method:

#----
# Data preparation & splitting
Data <- read.csv("GWL_100m_Statistical analysis.csv")

# Convert specified columns to factors (categorical variables)
categorical <- c("Cluster", "SoilDraina", "AQUIFER_CAT", "VUL_CAT", "SSPERM", "T", "DTB")
Data[categorical] <- lapply(Data[categorical], factor)

# Select specific columns for analysis
Data <- Data[, c("Cluster", "MEDIAN_Slope", "MEDIAN_DEM", "MEDIAN_HAND", "SoilDraina", 
                 "AQUIFER_CAT", "VUL_CAT", "SSPERM", "T", "DTB")]

# Set a seed for reproducibility
set.seed(123)
splits <- createDataPartition(Data$Cluster, p = 0.75, list = FALSE)

Data <-dummy_cols(Data, select_columns="Cluster",remove_selected_columns=TRUE)

Data$Cluster_1 <- as.factor(Data$Cluster_1)
Data$Cluster_2 <- as.factor(Data$Cluster_2)
Data$Cluster_3 <- as.factor(Data$Cluster_3)

# # Create training and testing datasets
# train <- Data[splits, ]
# test  <- Data[-splits, ]

# Extract input variables:

#Model training
RF_model_1<- randomForest(Data$Cluster_1~.,data = Data[,1:9], ntree=500)
RF_model_2<- randomForest(Data$Cluster_2~.,data = Data[,1:9], ntree=500)
RF_model_3<- randomForest(Data$Cluster_3~.,data = Data[,1:9], ntree=500)

#Evaluate variable importance from RF_model:

RF_feature_1 <-importance(RF_model_1)
RF_feature_2 <-importance(RF_model_2)
RF_feature_3 <-importance(RF_model_3)





# Higher values indicate more important features in determining the outcome.
# Lower values suggest less importance, though they still contribute to the model.












