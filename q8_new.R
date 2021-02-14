data <- read.csv('bankdetails.csv')
data

#one-hot encoding
for(i in 1:nrow(data)){
  if(data[i,1]=='Y')
    data[i,1] <- '1'
  else
    data[i,1] <- '0'
}

for(i in 1:nrow(data)){
  if(data[i,4]=='Y')
    data[i,4] <- '1'
  else
    data[i,4] <- '0'
}

for(i in 1:nrow(data)){
  if(data[i,2]=='Stud')
    data[i,2] <- '1'
  else
    data[i,2] <- '0'
}
data$credit <- as.numeric(data$credit)
data$qual <- as.numeric(data$qual)
data$Default <- as.numeric(data$Default)


#normalizing balance
max_bal <- max(data$balance)
min_bal <- min(data$balance)
max_bal
min_bal
for(i in 1:nrow(data)){
  data[i,3] <- (data[i,3]-min_bal)/(max_bal-min_bal)
}
data


##Using pre-defined function:
logModel <- glm(Default~credit+qual+balance, data=data,binomial)
modelSummary <- summary(logModel)
print(modelSummary)

#test data
test1 <- as.data.frame(list(0,0,0.999))
colnames(test1) <- c('credit','qual','balance')

#predict
pred <- predict(logModel, newdata = test1)
print(pred)
if(pred<0.5){
  print("N")
}
if (pred>=0.5){
  print("Y")
}

##Writing user-defined version

#calculating averages
x1_avg <- sum(data$credit)/nrow(data)
x2_avg <- sum(data$qual)/nrow(data)
x3_avg <- sum(data$balance)/nrow(data)
y_avg <- sum(data$Default)/nrow(data)
x1_avg
x2_avg
x3_avg

#functions for calculating betas
func1 <- function(x){
  return(x-x1_avg)
}

func11 <- function(x){
  return((x-x1_avg)*(x-x1_avg))
}

func2 <- function(x){
  return(x-x2_avg)
}

func21 <- function(x){
  return((x-x2_avg)*(x-x2_avg))
}

func3 <- function(x){
  return(x-x3_avg)
}

func31 <- function(x){
  return((x-x3_avg)*(x-x3_avg))
}

func_y <- function(y){
  return(y-y_avg)
}


#calculating beta1
num1 <- sapply(data$credit, func1)
num2 <- sapply(data$Default, func_y)
num3 <- num1*num2
num <- sum(num3)

deno <- sapply(data$credit, func11)
den <- sum(deno)

beta1 <- num/den
beta1

#calculating beta2
num1 <- sapply(data$qual, func2)
num2 <- sapply(data$Default, func_y)
num3 <- num1*num2
num <- sum(num3)

deno <- sapply(data$qual, func21)
den <- sum(deno)

beta2 <- num/den
beta2

#calculating beta3
num1 <- sapply(data$balance, func3)
num2 <- sapply(data$Default, func_y)
num3 <- num1*num2
num <- sum(num3)

deno <- sapply(data$balance, func31)
den <- sum(deno)

beta3 <- num/den
beta3

#calculating beta0
beta0 <- y_avg - (beta1*x1_avg +beta2*x2_avg + beta3*x3_avg)
beta0

#Logistic Regression
e<-2.71828

temp1 <- beta0+beta1*0+beta2*0+beta3*0.999
prob <- ((e**temp1)/(1+e**temp1))
prob

#Calcutating logit
logit <- log(prob/(1-prob))
logit

