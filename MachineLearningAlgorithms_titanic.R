library(caret)
library(doSNOW)
library(AppliedPredictiveModeling)
transparentTheme(trans = .4)


train_data<-read.csv("train.csv", stringsAsFactors = FALSE)
head(train_data)
str(train_data)
View(train_data)

#Replace missing values in Embarked column with mode
table(train_data$Embarked)
train_data$Embarked[train_data$Embarked==""]<-"S"

#Track missing values in Age by creating a new feature
summary(train_data$Age)
train_data$MissingAge<- ifelse(is.na(train_data$Age),"Y","N")
table(train_data$MissingAge)

#Add a feature for family size
train_data$Familysize<-1+train_data$SibSp+train_data$Parch


#Transform the variables as factors
train_data$Survived<- as.factor(train_data$Survived)
train_data$Pclass<- as.factor(train_data$Pclass)
train_data$Sex<- as.factor(train_data$Sex)
train_data$Embarked<- as.factor(train_data$Embarked)
train_data$MissingAge<- as.factor(train_data$MissingAge)

#Subsetting data to features we wish to use
features<-c("Survived","Pclass","Sex","Age","SibSp","Parch","Fare","Embarked","MissingAge","Familysize")
train_data<-train_data[,features]
str(train_data)

#Transforming all features to dummy variables
dummy.vars<- dummyVars(~ .,data=train_data[,-1])
train.dummy<- predict(dummy.vars,train_data[,-1])
View(train.dummy)

#Imputation Model
preProcValues <- preProcess(train.dummy,method="bagImpute")
imputed.data<-predict(preProcValues, train.dummy)
View(imputed.data)

train_data$Age <- imputed.data[,6]
View(train_data)

#Splitting the training data (80:20), 
#maintaining the proportions of the survived class label the same across the splits
set.seed(42)
indexes <- createDataPartition(train_data$Survived, times= 1, p=0.8, list=FALSE)
titanic_train<- train_data[indexes,]
titanic_test <-train_data[-indexes,]

nrow(titanic_train)
nrow(titanic_test)

prop.table(table(titanic_train$Survived))
prop.table(table(titanic_test$Survived))

#10-fold cross validation repeated 3 times and use the grid search for optimal model parameter values
train_control<-trainControl(method="repeatedcv",
                            number=10,
                            repeats=3,
                            search="grid")

#leverage a grid search of hyperparameters for xgboost.
tune_grid <- expand.grid(eta=c(0.05,0.075,0.1),
                         nrounds=c(50,75,100),
                         max_depth=6:8,
                         min_child_weight=c(2.0,2.25,2.5),
                         colsample_bytree=c(0.3,0.4,0.5),
                         gamma=0,
                         subsample=1)
view(tune_grid)

cl<-makeCluster(3,type="SOCK")
#Register cluster so that caret will know to train in parallel
registerDoSNOW(cl)

#Train the xgboost model using the 10-fold c repeated 3 times
#and a hyperparameter grid search to train the optimal model
crossval<-train(Survived~.,
                     data=titanic_train,
                     method="xgbTree",
                     tuneGrid=tune_grid,
                     trControl=train_control)

stopCluster(cl)

#Examine caret's preprocessing results
crossval

#Make predictions on the test set using xgboost model trained on the training set using the found optimal hyperparameter values
preds <- predict(crossval,titanic.test)

#Confusion matrix to estimate the effectiveness of this model on unseen, new data
confusionMatrix(preds,titanic.test$Survived)
