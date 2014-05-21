pr2<-function(t)
{
  
  t$name<-NULL
  t$ticket<-NULL
  t$cabin<-NULL
  
  t$survived<-as.factor(t$survived)
  t$pclass<-as.factor(t$pclass)
  t$sex<-as.factor(t$sex)
  t$sibsp<-as.factor(t$sibsp)
  t$parch<-as.factor(t$parch)
  t$embarked<-as.factor(t$embarked)
  
  
  survived<-t[,1] #response vector
  predictors<-t[,-1] 
  capture.output(result<-rfImpute(x=predictors,y=survived))  #suppressed output of rfImpute
  
  tfix<-result
  tforest<-randomForest(survived~sex+pclass+age+sibsp+parch+fare+embarked,data=tfix,prox=TRUE)
  tforest
  
  Predictions<-predict(tforest, newdata=tfix)
  print(table(tfix$survived,Predictions))
  
  #Additional output
  #print("Number of correct predictions: ")
  #table(testPred == tfix$survived)
}