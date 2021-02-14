dataset<- read.table('q4.csv',header=TRUE,sep=',')

x_avg<-(sum(dataset$budget))/nrow(dataset)
y_avg<-(sum(dataset$sales))/nrow(dataset)

func1<-function(x){
  return(x-x_avg)
}
func2<-function(y){
  return(y-y_avg)
}
func3<-function(x){
  return((x-x_avg)*(x-x_avg))
}

col1<- sapply(dataset$budget,func1)
col2<- sapply(dataset$sales,func2)
col3<- col1*col2
col4<-sapply(dataset$budget,func3)

numerator<-sum(col3)
denominator<-sum(col4)

beta1<- numerator/denominator
beta0<- y_avg-(beta1*x_avg)

print(paste("Beta0=",beta0))
print(paste("Beta1=",beta1))

pred1<-beta0+(beta1*3000)
pred2<-beta0+(beta1*7000)
pred3<-beta0+(beta1*8000)
print(paste("When budget=3000 then sales=",pred1))
print(paste("When budget=7000 then sales=",pred2))
print(paste("When budget=8000 then sales=",pred3))

#Using library functions
linearMod<-lm(sales~budget,data=dataset)
summary(linearMod)
x_test = data.frame("budget"=c(3000,7000,8000))
predictions_lib<-predict(linearMod,x_test)
predictions_lib
