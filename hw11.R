##Assignment 11

library(ISLR)
library(class)
library(tree)
library(car)
library(randomForest)
library(caret)
library(gbm)

#Reading the two data files
CancerData <- read.csv("C:/Users/Hiren/OneDrive/OneDrive - Texas A&M University/TAMU/Spring'20/Data Analysis/HW/Mid-term/CancerData.csv")
CancerHoldoutData <- read.csv("C:/Users/Hiren/OneDrive/OneDrive - Texas A&M University/TAMU/Spring'20/Data Analysis/HW/Mid-term/CancerHoldoutData.csv")

##In Geography the number of levels are way too many, thereby is is required to work on the variable data to make it useful
length(levels(CancerData$Geography))
##We will reduce the number of levels by replacing counties with states and performing data analysis
CancerData$Geography = as.character(CancerData$Geography)
for ( i in 1:length(CancerData$Geography))
{
  CancerData$Geography[i] = unlist(strsplit(CancerData$Geography[i], split=', ', fixed=TRUE))[2]
}
CancerData$Geography<-factor(CancerData$Geography)
levels(CancerData$Geography)
###The levels have been reduced to 51, thereby it becomes easier to predict data now for reduced levels
###Thereby the data has been cleaned

##In Geography (test data set) the number of levels are way too many, thereby is is required to work on the variable data to make it useful
length(levels(CancerHoldoutData$Geography))
##We will reduce the number of levels by replacing counties with states and performing data analysis
CancerHoldoutData$Geography = as.character(CancerHoldoutData$Geography)
for ( i in 1:length(CancerHoldoutData$Geography))
{
  CancerHoldoutData$Geography[i] = unlist(strsplit(CancerHoldoutData$Geography[i], split=', ', fixed=TRUE))[2]
}
CancerHoldoutData$Geography<-factor(CancerHoldoutData$Geography)
levels(CancerHoldoutData$Geography)
###The levels have been reduced, thereby it becomes easier to predict data now for reduced levels
###Thereby the data has been cleaned


#1B: Outliers
##Performance after adressing outlers, i.e. assigning mean values to MedianAge
#For CancerData
##The Age data set also has some issues attached, the below analysis would highlight it better.
max(CancerData$MedianAge)
max(CancerData$MedianAgeFemale)
max(CancerData$MedianAgeMale)
##The above results show a huge discrepancy. It also provides us with some insight about the incorectness of the MedianAge Data set
##The median age data set needs to be searched through, to find possible errors in the data.
sum(CancerData$MedianAge>100)
##This suggests that there are 23 data entries with incorrect/inflated MedianAge
##For these data entries, we will take the average of the MedianAgeFemal and MedianAgeMale
for (i in 1:length(CancerData$MedianAge))
{ 
  if (CancerData$MedianAge[i] > 100)
  {
    CancerData$MedianAge[i] = mean(CancerData$MedianAgeMale[i],CancerData$MedianAgeFemale[i])
  }
}
###In the above part the assignment has been completed
sum(CancerData$MedianAge>100)
###Thereby the data has been cleaned

#ForCancerHoldOut
##The Age data set also has some issues attached, the below analysis would highlight it better.
max(CancerHoldoutData$MedianAge)
max(CancerHoldoutData$MedianAgeFemale)
max(CancerHoldoutData$MedianAgeMale)
##The above results show a huge discrepancy. It also provides us with some insight about the incorectness of the MedianAge Data set
##The median age data set needs to be searched through, to find possible errors in the data.
sum(CancerHoldoutData$MedianAge>100)
##This suggests that there are 23 data entries with incorrect/inflated MedianAge
##For these data entries, we will take the average of the MedianAgeFemal and MedianAgeMale
for (i in 1:length(CancerHoldoutData$MedianAge))
{
  if (CancerHoldoutData$MedianAge[i] > 100)
  {
    CancerHoldoutData$MedianAge[i] = mean(CancerHoldoutData$MedianAgeMale[i],CancerHoldoutData$MedianAgeFemale[i])
  }
}
###In the above part the assignment has been completed
sum(CancerHoldoutData$MedianAge>100)
###Thereby the data has been cleaned for CancerHoldout Data set


#1C Missing Values in test Data set

##Cleaning the CancerData set
###From analyzing the data it can be seen that PctSomeCol18_24 has lots of missing data values, therby a decision has been made to opmit the dataset from analysis
sum(is.na(CancerData$PctSomeCol18_24))
length(CancerData$PctSomeCol18_24)
##From above it can be seem that about 75% of the data is missing, therby rendering the data useless, thus we compute the data from neighboring data
CancerData$PctSomeCol18_24<- 100 - CancerData$PctNoHS18_24 - CancerData$PctHS18_24 - CancerData$PctBachDeg18_24
CancerData$PctSomeCol18_24

##Cleaning the CancerHoldoutData set
###From analyzing the data it can be seen that PctSomeCol18_24 has lots of missing data values, therby a decision has been made to opmit the dataset from analysis
sum(is.na(CancerHoldoutData$PctSomeCol18_24))
length(CancerHoldoutData$PctSomeCol18_24)
##From above it can be seem that about 80% of the data is missing, therby rendering the data useless, thus we compute the data from neighboring data
CancerHoldoutData$PctSomeCol18_24<- 100 - CancerHoldoutData$PctNoHS18_24 - CancerHoldoutData$PctHS18_24 - CancerHoldoutData$PctBachDeg18_24
names(CancerHoldoutData)
CancerHoldoutData$PctSomeCol18_24

# Model performance after the removing the missing value column
linear.cancer.miss = lm(TARGET_deathRate~.-PctSomeCol18_24, data = CancerData)
summary(linear.cancer.miss)

#1D Checking the collinearity between variables
##We check this for the model developed above
vif(linear.cancer.miss)
##We remove MedianAge and MedianAgeFemale from the model due to their high VIF values, i.e. VIF>5
linear.cancer.v = lm(TARGET_deathRate~.-PctSomeCol18_24-MedianAgeMale-MedianAgeFemale, data = CancerData)
summary(linear.cancer.v)
vif(linear.cancer.v)


##Removing the noise predictors and making new data frame for train and test

##Splitting the data into different data sets
Train.split = sample(nrow(CancerData), 0.7*nrow(CancerData))

### The data has been splitted into two parts
Train.CanD = CancerData[Train.split,]
Test.CanD = CancerData[-Train.split,]


##Problem 1

##Part A
##Tree Pruning
set.seed(3)
train.tree<-tree(TARGET_deathRate~.-MedianAgeMale-MedianAgeFemale-Geography,CancerData)
summary(train.tree)
plot(train.tree)
text(train.tree,pretty = 0.25)

##Testing on test data to see the MSE value without tree pruning
test.tree<-predict(train.tree,CancerHoldoutData)
mse<- mean((test.tree - CancerHoldoutData$TARGET_deathRate)^2)
mse

##Using Cross-validation to determine the best value for tree size
cv.tree.train<-cv.tree(train.tree, FUN = prune.tree)
names(cv.tree.train)
cv.tree.train
min(cv.tree.train$dev)
plot(cv.tree.train$size,cv.tree.train$dev,'b')

##Tree Pruning
prune.train.tree = prune.tree(train.tree, best=8)
plot(prune.train.tree)
text(prune.train.tree,pretty = 0.25)

##Prediction
pred.test.tree = predict(prune.train.tree,CancerHoldoutData)
mean((pred.test.tree - CancerHoldoutData$TARGET_deathRate)^2)

##Part B

##Random Forest
##Hyper parameter tuning with 10 fold cross-validation will be done
# Hyper parameter tuning using Caret
#Caret package is not taking ntree as control
?expand.grid
caretGrid <- expand.grid( mtry = 5:13)
num_tree <- c(200,300,400,500)
trainControl <- trainControl(method="cv", number=10)
mse.rf<- c(0,0,0,0)
set.seed(2)
for (i in 1:length(num_tree))
{
  rf.caret.CancerData <- train(TARGET_deathRate~.-Geography, data=CancerData ,method="rf",
                  trControl=trainControl, verbose=FALSE, ntree=num_tree[i],
                  tuneGrid=caretGrid, metric='RMSE')   
  print(rf.caret.CancerData)
  cancer.rf.predict <- predict(rf.caret.CancerData,CancerHoldoutData, type = "raw")
  mse.rf[i]<-mean((cancer.rf.predict-CancerHoldoutData$TARGET_deathRate)^2)
}
##The mean square error rate for different ntrees are:
mse.rf
##Computing Random Forest for the best parameter combination
randomforest.cancerdata = randomForest(TARGET_deathRate~.-Geography,data =CancerData, mtry= 9 , ntree =400, importance = TRUE)
randomforest.predict = predict(randomforest.cancerdata,CancerHoldoutData)
varImpPlot(randomforest.cancerdata)  
importance(randomforest.cancerdata)



##Part C
##Boosting
##Hyper parameter tuning with 10 fold cross-validation will be done
caretGrid <- expand.grid(interaction.depth=c(1, 3, 5), n.trees = (1:10)*100,
                         shrinkage=c(0.01, 0.001),
                         n.minobsinnode=100)

trainControl <- trainControl(method="cv", number=10)
set.seed(2)
gbm.caret.cancerdata <- train(TARGET_deathRate ~.-Geography, data= CancerData , distribution="gaussian", method="gbm",
                   trControl=trainControl, verbose=FALSE, 
                   tuneGrid=caretGrid, metric='RMSE', bag.fraction=0.75)   

print(gbm.caret.cancerdata)
gbm.caret.predict <- predict(gbm.caret.cancerdata,CancerHoldoutData, type = "raw")
mean((gbm.caret.predict-CancerHoldoutData$TARGET_deathRate)^2)
summary(gbm.caret.cancerdata)
