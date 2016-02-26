#######  
#  I.	Dataset: 
#   YaleB
#  Outcome: 1-38
#  2414 obs and 1024 dimension

#  II.	Method:
#   (1)	 Data Processing
#   Center and L2 Normalize data by deducting mean and divided by square root of variances for each column

#   (2)	 Training/Testing
#   Permute the whole dataset and split the whole datasets into 80% training and 20% testing datasets.

#   (3)	Model Training—Using Training Datasets
#   a)	Classifier: Both linear SVM and non-linear SVM with Gaussian Radial Basis Function  Kernel
#   b)	Model Parameters:  Cost and Gamma 
#   According to previous papers, we set a list of cost and gamma values (cost from 2^-2 to 2^5, gamma from 2^-9 to 2^1)
#   We use 5-fold CV to select the best cost and gamma value by measuring the accuracy.
#   Final Model: Linear SVM with Cost =2^4

#  (4)	Model Evaluation—Using training and testing Datasets
#   a)	Using training datasets to run best SVM model and predict on test dataset
# b)	Measure test dataset accuracy
#########


library("e1701")
library("caret")

## Data Processing- We centralized and scaled the datasets
x<-scale(YaleB_X,center=T,scale=T);
dataset<-data.frame(y=YaleB_Y,x=x);
colnames(dataset)[1]='y'
dataset$y <- factor(dataset$y)

n.obs <- nrow(dataset); # no. of observations 
s = sample(n.obs);

## We split the dataset into traning and testing dataset
train=dataset[s[1:1800],]
test=dataset[s[1801:2414],];

## set parameters 
C=c(2^-2,2^-1,2^0,2^1,2^2,2^3,2^5,2^6)
gamma=c(2^-10,2^-9,2^-8,2^-7,2^-6,2^-5,2^-4,2^-3,2^-2,2^-1,2^0,2^1,2^2,2^3)

####Linear SVM
k.fold.cv <- function(data, y, k.fold=5) {
  var<-ncol(data);
  n.obs <- nrow(data) # no. of observations 
  s = sample(n.obs)
  pred=NULL
  acc = matrix(0,nrow=5,ncol=8)
  probs = NULL
  actuals = NULL

  for (j in 1:8 ){
    for (k in 1:k.fold) {
      test.idx = which(s %% k.fold == (k-1) ) # use modular operator
      train.set = data[-test.idx,]
      test.set = data[test.idx,]
      m <- svm(as.factor(y) ~., data = train.set, cost = C[j])
      pred<-predict(m,test.set)
      actual = test.set$y;
      CM <- confusionMatrix(pred, actual);
      accuracy <- CM$overall['Accuracy'];
      acc[k,j] = accuracy;
      
    }
  }
  print(acc)
  return(acc)
}

accuracy=k.fold.cv(train,y,5)

##Final Model
a=svm(as.factor(y)~.,data=train,cost=2^4)
b=predict(a,test)
actual = test$y;
CM <- confusionMatrix(b, actual);
accuracy <- CM$overall['Accuracy'];


###Kernel SVM

C=c(2^-2,2^-1,2^0,2^1,2^2,2^3,2^4,2^5)
G=c(2^-13,2^-12,2^-11)

Svm.CV <- function(data, y, k.fold=5) {
  var<-ncol(data);
  n.obs <- nrow(data) # no. of observations 
  s = sample(n.obs)
  pred=NULL
  acc = matrix(0,nrow=3,ncol=8)
  probs = NULL
  actuals = NULL
  accCV=NULL
  for (j in 1:8 ){
    for(u in 1:3){
    for (k in 1:k.fold) {
      test.idx = which(s %% k.fold == (k-1) ) # use modular operator
      train.set = data[-test.idx,]
      test.set = data[test.idx,]
      m <- svm(as.factor(y) ~., data = train.set, cost = C[j],kernel ='radial', gamma=G[u])
      pred<-predict(m,test.set)
      actual = test.set$y;
      CM <- confusionMatrix(pred, actual);
      accuracy <- CM$overall['Accuracy'];
      accCV[k] = accuracy;
     }
      acc[u,j]=mean(accCV);
    }
  }
  print(acc)
  return(acc)
}

accuracy=Svm.CV(train,y,5)

##Final Model

a<-svm(as.factor(y)~.,data=train,kernel="radial", gamma=2^-11,cost=2^4)
pre=predict(a,test)
CM <- confusionMatrix(pre,actual);
accuracy <- CM$overall['Accuracy'];
