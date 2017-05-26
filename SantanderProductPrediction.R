#nk <- 20000
#ncount <- countLines("train_ver2.csv")
#santanderTrain20<- read.csv("train_ver2.csv", header=FALSE, skip=ncount-nk)
#head(santanderTrainExtract)
#write.csv(santanderTrain20,"santanderTrain20.csv")

#nk <- 20000
#ncount <- countLines("test_ver2.csv")
#santanderTest20<- read.csv("test_ver2.csv", header=FALSE, skip=ncount-nk)
#head(santanderTrainExtract)
#write.csv(santanderTest20,"santanderTest20.csv")

#libraries Used
library(dplyr)
library(corrplot)
library(xgboost)


#Data Loading
santanderTrainData <- read.csv("SantanderTrain20.csv") 
santanderTrain <- santanderTrainData[1:15000,]
santanderValidation <- santanderTrainData[15001:20000,]
santanderTest <- read.csv("SantanderTest20.csv")
santanderTest <- santanderTest[1:15000,]

#Make sure the test, train and test have distinct customers
trainCustomers <- santanderTrain$V2
validationCustomers <- santanderValidation$V2

nrow(santanderTrain)
nrow(santanderValidation)
nrow(santanderTest)


santanderTrain <- filter(santanderTrain, !santanderTrain$V2 %in% validationCustomers)
nrow(santanderTrain)
santanderValidation <- filter(santanderValidation, !santanderValidation$V2 %in% trainCustomers)
nrow(santanderValidation)
santanderTest <- filter(santanderTest, !santanderTest$V2 %in% trainCustomers)
nrow(santanderTest)

#Check the dimensions of the train, validation and test data
dim(santanderTrain)
dim(santanderValidation)
dim(santanderTest)

#Random selection of 15 features out of 25(dropped the date related column for simplified analysis) that seem to be important from the data description
santanderTrain <- santanderTrain[,c(5,6,7,9,10,13,14,16,18,21,23,24,25,38,48,49,47)]
trainColNames <- c("Residence","Sex","Age","NewCustomerIndex","CusSeniority","CusTypeAtMonBeg","CusRelType","ForIndex","Channel","ProvinceCode","ActivityIndex","GrossIncome","Segment","eAccount","Pension2","DirectDebit","Payroll")
colnames(santanderTrain) <- trainColNames

santanderValidation <- santanderValidation[,c(5,6,7,9,10,13,14,16,18,21,23,24,25,38,49,48,47)]
validationColNames <- c("Residence","Sex","Age","NewCustomerIndex","CusSeniority","CusTypeAtMonBeg","CusRelType","ForIndex","Channel","ProvinceCode","ActivityIndex","GrossIncome","Segment","eAccount","Pension2","DirectDebit","Payroll")
colnames(santanderValidation) <- validationColNames

santanderTest <- santanderTest[,c(5,6,7,9,10,13,14,16,18,21,23,24,25)]
testColNames <- c("Residence","Sex","Age","NewCustomerIndex","CusSeniority","CusTypeAtMonBeg","CusRelType","ForIndex","Channel","ProvinceCode","ActivityIndex","GrossIncome","Segment")
colnames(santanderTest) <- testColNames

#Convert the columns into numeric

santanderTrain$Residence <- as.numeric(santanderTrain$Residence)
santanderTrain$Sex <- as.numeric(santanderTrain$Sex)
santanderTrain$Age <- as.numeric(santanderTrain$Age)
santanderTrain$NewCustomerIndex <- as.numeric(santanderTrain$NewCustomerIndex)
santanderTrain$CusSeniority <- as.numeric(santanderTrain$CusSeniority)
santanderTrain$CusTypeAtMonBeg <- as.numeric(santanderTrain$CusTypeAtMonBeg)
santanderTrain$CusRelType <- as.numeric(santanderTrain$CusRelType)
santanderTrain$ForIndex <- as.numeric(santanderTrain$ForIndex)
santanderTrain$Channel <- as.numeric(santanderTrain$Channel)
santanderTrain$ProvinceCode <- as.numeric(santanderTrain$ProvinceCode)
santanderTrain$ActivityIndex <- as.numeric(santanderTrain$ActivityIndex)
santanderTrain$GrossIncome <- as.numeric(santanderTrain$GrossIncome)
santanderTrain$Segment <- as.numeric(santanderTrain$Segment)
santanderTrain$eAccount <- as.numeric(santanderTrain$eAccount)
santanderTrain$Payroll <- as.numeric(santanderTrain$Payroll)
santanderTrain$DirectDebit <- as.numeric(santanderTrain$DirectDebit)
santanderTrain$Pension2 <- as.numeric(santanderTrain$Pension2)



santanderValidation$Residence <- as.numeric(santanderValidation$Residence)
santanderValidation$Sex <- as.numeric(santanderValidation$Sex)
santanderValidation$Age <- as.numeric(santanderValidation$Age)
santanderValidation$NewCustomerIndex <- as.numeric(santanderValidation$NewCustomerIndex)
santanderValidation$CusSeniority <- as.numeric(santanderValidation$CusSeniority)
santanderValidation$CusTypeAtMonBeg <- as.numeric(santanderValidation$CusTypeAtMonBeg)
santanderValidation$CusRelType <- as.numeric(santanderValidation$CusRelType)
santanderValidation$ForIndex <- as.numeric(santanderValidation$ForIndex)
santanderValidation$Channel <- as.numeric(santanderValidation$Channel)
santanderValidation$ProvinceCode <- as.numeric(santanderValidation$ProvinceCode)
santanderValidation$ActivityIndex <- as.numeric(santanderValidation$ActivityIndex)
santanderValidation$GrossIncome <- as.numeric(santanderValidation$GrossIncome)
santanderValidation$Segment <- as.numeric(santanderValidation$Segment)
santanderValidation$eAccount <- as.numeric(santanderValidation$eAccount)
santanderValidation$Payroll <- as.numeric(santanderValidation$Payroll)
santanderValidation$DirectDebit <- as.numeric(santanderValidation$DirectDebit)
santanderValidation$Pension2 <- as.numeric(santanderValidation$Pension2)

santanderTest$Residence <- as.numeric(santanderTest$Residence)
santanderTest$Sex <- as.numeric(santanderTest$Sex)
santanderTest$Age <- as.numeric(santanderTest$Age)
santanderTest$NewCustomerIndex <- as.numeric(santanderTest$NewCustomerIndex)
santanderTest$CusSeniority <- as.numeric(santanderTest$CusSeniority)
santanderTest$CusTypeAtMonBeg <- as.numeric(santanderTest$CusTypeAtMonBeg)
santanderTest$CusRelType <- as.numeric(santanderTest$CusRelType)
santanderTest$ForIndex <- as.numeric(santanderTest$ForIndex)
santanderTest$Channel <- as.numeric(santanderTest$Channel)
santanderTest$ProvinceCode <- as.numeric(santanderTest$ProvinceCode)
santanderTest$ActivityIndex <- as.numeric(santanderTest$ActivityIndex)
santanderTest$GrossIncome <- as.numeric(santanderTrain$GrossIncome)
santanderTest$Segment <- as.numeric(santanderTest$Segment)

#Omit all the rows having NAs

santanderTrain <- na.omit(santanderTrain)
santanderValidation <- na.omit(santanderValidation)
santanderTest <- na.omit(santanderTest)

nrow(santanderTrain)
nrow(santanderValidation)
nrow(santanderTest)

#Correlation among the features
santanderFeatureCorrelation <- cor(santanderTrain[2:13])
windows()
corrplot(santanderFeatureCorrelation)

#Define the parameters to be used by XGBoost

param       = list("objective" = "binary:logistic", # binary classification
                   #"num_class"= 2 ,  		# Number of classes in the dependent variable.
                   "eval_metric" = "error",  	 # evaluation metric 
                   "nthread" = 8,   			 # number of threads to be used 
                   "max_depth" = 6,    		 # maximum depth of tree 
                   "eta" = 0.3,    			 # step size shrinkage 
                   "gamma" = 0    			 # minimum loss reduction 
                   #"subsample" = 0.7,    		 # part of data instances to grow tree 
                   #"colsample_bytree" = 1, 		 # subsample ratio of columns when constructing each tree 
                   #"min_child_weight" = 12  		 # minimum sum of instance weight needed in a child 
)

#Model 1 (Model with features)

#Define the predictors
predictors <- colnames(santanderTrain[,1:13]) 
label <- as.numeric(santanderTrain[,ncol(santanderTrain)]) #XGBoost wants the labels to be numeric
print(table(label))
length(label)

#Cross Validation to determine the minimum error
cv.nround = 50;

bst.cv = xgb.cv(
  param=param,
  data = as.matrix(santanderTrain[,predictors]),
  label = label,
  nfold = 5,
  nrounds=cv.nround,
  prediction=T)

#min.auc = which.min(bst.cv$dt[,test.auc.mean])
min.error = which.min(bst.cv$dt[,test.error.mean])
cat ("Minimum error occurred in round : ", min.error, "\n")
print(bst.cv$dt[min.error,])


dtrain1 <- xgb.DMatrix(data = as.matrix(santanderTrain[,1:13]), label = santanderTrain$Payroll)
bst1 <- xgboost(data = dtrain1, max_depth = 4, eta = 0.3, nrounds = min.error, nthread = 2, objective = "binary:logistic", verbose = 2)

pred <- predict(bst1, as.matrix(santanderValidation[,1:13]))
err <- mean(as.numeric(pred > 0.5) != santanderValidation$Payroll)
print(paste("validation-error=", err))
print("Most important features (look at column Gain):")

imp_matrix <- xgb.importance(feature_names = colnames(santanderTrain[,1:13]), model = bst1)
print(imp_matrix)

xgb.plot.importance(imp_matrix[1:5])

#Model 2  (Model with features and existing products)

#Define the predictors
predictors <- colnames(santanderTrain[-ncol(santanderTrain)]) 
label <- as.numeric(santanderTrain[,ncol(santanderTrain)]) #XGBoost wants the labels to be numeric
print(table(label))
length(label)

#Cross Validation to determine the minimum error
cv.nround = 50;

bst.cv = xgb.cv(
  param=param,
  data = as.matrix(santanderTrain[,predictors]),
  label = label,
  nfold = 5,
  nrounds=cv.nround,
  prediction=T)

min.error = which.min(bst.cv$dt[,test.error.mean])
cat ("Minimum error occurred in round : ", min.error, "\n")
print(bst.cv$dt[min.error,])

# Build the XGBoost Model with features and products

dtrain2 <- xgb.DMatrix(data = as.matrix(santanderTrain[,-ncol(santanderTrain)]), label = santanderTrain$Payroll)
bst2 <- xgboost(data = dtrain2, max_depth = 4, eta = 0.3, nrounds = min.error, nthread = 2, objective = "binary:logistic", verbose = 2)

pred <- predict(bst2, as.matrix(santanderValidation[,-ncol(santanderValidation)]))
err <- mean(as.numeric(pred > 0.5) != santanderValidation$Payroll)
print(paste("validation-error=", err))
print("Most important features (look at column Gain):")

imp_matrix <- xgb.importance(feature_names = colnames(santanderTrain[,-ncol(santanderTrain)]), model = bst2)
print(imp_matrix)

xgb.plot.importance(imp_matrix)


# Use Model 1 to predict the products for test data

#eAccount
dtrain3 <- xgb.DMatrix(data = as.matrix(santanderTrain[,1:13]), label = santanderTrain$eAccount)
bst3 <- xgboost(data = dtrain3, max_depth = 8, eta = 0.3, nrounds = 2, nthread = 2, objective = "binary:logistic", verbose = 2)

pred1 <- predict(bst3, as.matrix(santanderTest[,1:13]))
pred1 <- as.numeric(pred1 > 0.3)
print("Most important features (look at column Gain):")
imp_matrix <- xgb.importance(feature_names = colnames(santanderTrain[,1:13]), model = bst3)
print(imp_matrix)

xgb.plot.importance(imp_matrix)

nrow(santanderTest)
length(pred1)
santanderTestPredicted <- data.frame()
santanderTestPredicted <- cbind(santanderTest,pred1)

#DirectDebit
dtrain4 <- xgb.DMatrix(data = as.matrix(santanderTrain[,1:13]), label = santanderTrain$DirectDebit)
bst4 <- xgboost(data = dtrain4, max_depth = 8, eta = 0.3, nrounds = 2, nthread = 2, objective = "binary:logistic", verbose = 2)

pred2 <- predict(bst4, as.matrix(santanderTest[,1:13]))
pred2 <- as.numeric(pred2 > 0.5)
print("Most important features (look at column Gain):")
imp_matrix <- xgb.importance(feature_names = colnames(santanderTrain[,1:13]), model = bst4)
print(imp_matrix)

xgb.plot.importance(imp_matrix[1:5])

nrow(santanderTest)
length(pred2)

santanderTestPredicted <- cbind(santanderTestPredicted,pred2)

#Pension2
dtrain5 <- xgb.DMatrix(data = as.matrix(santanderTrain[,1:13]), label = santanderTrain$Pension2)
bst5 <- xgboost(data = dtrain4, max_depth = 8, eta = 0.3, nrounds = 2, nthread = 2, objective = "binary:logistic", verbose = 2)

pred3 <- predict(bst5, as.matrix(santanderTest[,1:13]))
pred3 <- as.numeric(pred3 > 0.4)
print("Most important features (look at column Gain):")
imp_matrix <- xgb.importance(feature_names = colnames(santanderTrain[,1:13]), model = bst5)
print(imp_matrix)

xgb.plot.importance(imp_matrix[1:5])

nrow(santanderTest)
length(pred3)

santanderTestPredicted <- cbind(santanderTestPredicted,pred3)

#predicting the payroll product for customers using the features and predicted eAccount,Direct Debit and Pension2

dtrain6 <- xgb.DMatrix(data = as.matrix(santanderTrain[,-ncol(santanderTrain)]), label = santanderTrain$Payroll)
bst6 <- xgboost(data = dtrain6, max_depth = 8, eta = 0.3, nrounds = 2, nthread = 2, objective = "binary:logistic", verbose = 2)
pred4 <- predict(bst6, as.matrix(santanderTest))
print("Most important features (look at column Gain):")
imp_matrix <- xgb.importance(feature_names = colnames(santanderTrain[,-ncol(santanderTrain)]), model = bst6)
print(imp_matrix)

xgb.plot.importance(imp_matrix)
