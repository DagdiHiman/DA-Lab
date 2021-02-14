table <- read.csv('q6.csv',header=TRUE,sep=',')
table

getPred<-function(x,k){
  distance=vector()
  age<-x[1]
  loan<-x[2]
  length(distance)=nrow(table)
  for(i in 1:nrow(table))
  {
    distance[i]=sqrt(((age-table[i,1])^2)+((loan-table[i,2])^2))
  }
  #min(distance)
  temp_table <- table
  temp_table$dist <- distance
  temp_table <- temp_table[order(temp_table$dist),]
  n_classes <- temp_table[1:k,3]
  count_table <- table(n_classes)
  classes <- names(count_table)
  return(classes[which.max(count_table)])
}

x_test <- data.frame("Age"=c(5,20,80),"Loan"=c(5,10000,300000))
pred1 <- apply(x_test,1,function(x) getPred(x,1))
pred2 <- apply(x_test,1,function(x) getPred(x,2))
pred3 <- apply(x_test,1,function(x) getPred(x,3))

x_test[1]
#Library Function
library(class)
y_train <- table[,3]
x_train <- table[,1:2]
pred_lib1 <- knn(train = x_train,test=x_test,cl=y_train, k = 1)
pred_lib2 <- knn(train = x_train,test=x_test,cl=y_train, k = 2) 
pred_lib3 <- knn(train = x_train,test=x_test,cl=y_train, k = 3) 

pred_lib1
pred_lib2
pred_lib3
#comparison table
library(grid)
library(gridExtra)
par(mfrow=c(2,3))
result1 <- data.frame("Age"=x_test[,1],"Loan"=x_test[,2],"Prediction"=pred1,"Prediction_Lib"=pred_lib1)
result2 <- data.frame("Age"=x_test[,1],"Loan"=x_test[,2],"Prediction"=pred2,"Prediction_Lib"=pred_lib2)
result3 <- data.frame("Age"=x_test[,1],"Loan"=x_test[,2],"Prediction"=pred3,"Prediction_Lib"=pred_lib3)

grid.arrange(tableGrob("k=1"),tableGrob(result1),tableGrob("k=2"),tableGrob(result2),tableGrob("k=3"),tableGrob(result3))

