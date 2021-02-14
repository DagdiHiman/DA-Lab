
bank<-read.csv("q6.csv")
bank
#x<- readline(prompt="Enter Age: ")
#x <- as.integer(x)
#y <- readline(prompt="Enter Loan Amount: ")
#y <- as.integer(y)
x=100
y=2000

dis <- transform(bank, distance= sqrt((x-bank$Age)^2+(y-bank$Loan)^2 ) )
dis

odis<- dis[order(dis$distance),c(2,3)]
odis

#k<- readline(prompt="Enter k: ")
# convert character into integer
#k <- as.integer(k)
k=3

nn<-head(odis,k)
knn<-table(nn$Default)
# finding which class has got max occurences
t<-names(which(table(nn$Default)==max(table(nn$Default))))  
cat("class:",t[[1]][1])

#Library Function
x_test <- data.frame("Age"=c(5,20,80),"Loan"=c(5,10000,300000))
library(class)
y_train <- table[,3]
x_train <- table[,1:2]
pred_lib1 <- knn(train = x_train,test=x_test,cl=y_train, k = 1)
pred_lib2 <- knn(train = x_train,test=x_test,cl=y_train, k = 2) 
pred_lib3 <- knn(train = x_train,test=x_test,cl=y_train, k = 3) 

pred_lib1
pred_lib2
pred_lib3
