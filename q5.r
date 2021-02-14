data <- read.csv('salesmlr.csv')

x <- data[,2:3]
x$intercept <- rep(1,nrow(data))
x <- as.matrix(x)

y <- as.matrix(data$totalUnits)

#Find coefficients
b <- solve(t(x)%*%x)%*%t(x)%*%y  #coefficient vector beta = inv(x'*x)*x'*y
b

getPred <- function(x){
  x <- c(x,1)
  return(x%*%b)
}

x_test = data.frame("budgetNews"=c(150,300,450,600,750),"budgetTv"=c(3000,4000,5000,6000,7000))

predicted<-apply(x_test,1,getPred)
predicted

#using library function for mlr 
model <- lm(totalUnits ~ budgetNews+budgetTv,data=data)
summary(model)
predictions_lib<-predict(model,x_test)
predictions_lib
