sensor1<-data[,c(1,2,3,13,14,15,25,26,27,37,38,39,49,53,54,55,65)]
sensor2<-data[,c(4,5,6,16,17,18,28,29,30,40,41,42,50,56,57,58,65)]
sensor3<-data[,c(7,8,9,19,20,21,31,32,33,43,44,45,51,59,60,61,65)]
sensor4<-data[,c(10,11,12,22,23,24,34,35,36,46,47,48,52,62,63,64,65)]

do.classification <- function(train.set, test.set,cl.name, verbose=F) {
  ## note: to plot ROC later, we want the raw probabilities,
  ## not binary decisions
  switch(cl.name,
         knn1= { # here we test k=1; you should evaluate different k's
           prob = knn(train.set[,-65], test.set[,-65], cl=train.set[,65], k = 1, prob=F)
           #attr(prob,"prob")[prob==0] = 1-attr(prob,"prob")[prob==0] #modified
           #prob = attr(prob,"prob")
           #print(cbind(prob,as.character(test.set$y)))
           prob
         },
         
         randomForest = {
           model <- randomForest(class ~.,data = train.set)
           if (verbose) {
             print(summary(model)) # detailed summary of splits
             ## plot the tree
             plot(model, main=" Random Forest")
           }           
           
           prob = predict(model, newdata=test.set,probability=F)
           prob
         },
         randomForest1 = {
           model <- randomForest(class ~aad.x4+avg.z1+avg.z2+aad.y4+range.z1+avg.y2+dist1+aad.y1+aad.x1+aad.x3,data = train.set)
           if (verbose) {
             print(summary(model)) # detailed summary of splits
             ## plot the tree
             plot(model, main=" Random Forest")
           }           
           
           prob = predict(model, newdata=test.set,probability=F)
           prob
         },
         
         randomForest2 = {
           model <- randomForest(class ~aad.x4+avg.z1+avg.z2+aad.y4+range.z1+avg.y2+dist1+aad.y1+aad.x1+aad.x3+aad.z1+ std.x3 +std.y1+aad.y3 +aad.z3+std.x4+range.z3+aad.z2 +dist3 +std.z1,data = train.set)
           if (verbose) {
             print(summary(model)) # detailed summary of splits
             ## plot the tree
             plot(model, main=" Random Forest")
           }           
           
           prob = predict(model, newdata=test.set,probability=F)
           prob
         },
         
         
         svm = {
           model = svm(class~., data=train.set, probability=F)
           prob = predict(model, newdata=test.set, probability=F)
           #prob = attr(prob,"probabilities")
           #print(cbind(prob,as.character(test.set$y)))
           #print(dim(prob))
           #prob = prob[,which(colnames(prob)==1)]/rowSums(prob)
           prob
         },
         
         svm.1 = {
           model = svm(class~aad.x4+avg.z1+avg.z2+aad.y4+range.z1+avg.y2+dist1+aad.y1+aad.x1+aad.x3, data=train.set, probability=F)
           prob = predict(model, newdata=test.set, probability=F)
           #prob = attr(prob,"probabilities")
           #print(cbind(prob,as.character(test.set$y)))
           #print(dim(prob))
           #prob = prob[,which(colnames(prob)==1)]/rowSums(prob)
           prob
         },
        
         svm.2 = {
           model = svm(class~aad.x4+avg.z1+avg.z2+aad.y4+range.z1+avg.y2+dist1+aad.y1+aad.x1+aad.x3+aad.z1+ std.x3 +std.y1+aad.y3 +aad.z3+std.x4+range.z3+aad.z2 +dist3 +std.z1, data=train.set, probability=F)
           prob = predict(model, newdata=test.set, probability=F)
           #prob = attr(prob,"probabilities")
           #print(cbind(prob,as.character(test.set$y)))
           #print(dim(prob))
           #prob = prob[,which(colnames(prob)==1)]/rowSums(prob)
           prob
         }, 
         
  ) 
}

k.fold.cv <- function(dataset, cl.name, k.fold=10, prob.cutoff=0.5) {
  ## default: 10-fold CV, cut-off 0.5 
  n.obs <- nrow(dataset) # no. of observations 
  s = sample(n.obs)
  accuracy<-dim(10)
  kappa<-dim(10)
  actuals = NULL
  for (k in 1:k.fold) {
    test.idx = which(s %% k.fold == (k-1) ) # use modular operator
    train.set = dataset[-test.idx,]
    test.set = dataset[test.idx,]
    cat(k.fold,'-fold CV run',k,cl.name,':',
        '#training:',nrow(train.set),
        '#testing',nrow(test.set),'\n')
    prob = do.classification(train.set, test.set, cl.name)
    actual = test.set$class
    matrix<-confusionMatrix(prob,actual,dnn = c("Prediction", "Reference"))
    accuracy[k]<-as.numeric(matrix$overall[1])
    kappa[k]<-as.numeric(matrix$overall[2])
  }
  acc<-mean(accuracy)
  kap<-mean(kappa)
  print(acc)
  print(kap)
}
sensor1.random<-k.fold.cv(sensor1,'randomForest')
sensor2.random<-k.fold.cv(sensor2,'randomForest')
sensor3.random<-k.fold.cv(sensor3,'randomForest')
sensor4.random<-k.fold.cv(sensor4,'randomForest')

