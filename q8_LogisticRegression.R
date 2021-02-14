#Simple Logistic Regression without library methods
#Set working directory
setwd("/home/sathvikkp/Data_Analytics_R_Programs/1MS17CS143_Sathvik_K_P_Logistic_LDA_QDA")
survey<- read.table('default.csv',header=TRUE,sep=',')


#Calculating mean values
xbar<-(sum(survey$Loan))/6
ybar<-(sum(survey$Default))/6

#functions for beta calc
findComp1<-function(x){
  return(x-xbar)
}
findComp2<-function(y){
  return(y-ybar)
}
findComp3<-function(x){
  return((x-xbar)*(x-xbar))
}
#independent variable
num1<- sapply(survey$Loan,findComp1)
num1
#dependent variable
num2<- sapply(survey$Default,findComp2)
num2

num3<- num1*num2
num3
#finding numerator
numerator<-sum(num3)
numerator
#finding denominator
den<-sapply(survey$Loan,findComp3)
den
deno<-sum(den)
deno

#finding betaone and betazero
betaOne<- numerator/deno
betaOne
betaZero<- ybar-(betaOne*xbar)
betaZero


e<-2.71828
xnew<-250000
#For a new value of loan predicting the probability of defaulting
prob<-(e**(betaZero+(betaOne*xnew)))/(1+(e**(betaZero+(betaOne*xnew))))
prob
#Since probability is 0.785 therefore 78.5% the person will default
#Calculating logit
logit<- log(prob/(1-prob))
logit



#Using library function
survey$Default<- factor(survey$Default)
mylogit <- glm(Default ~ Loan, data = survey, family = "binomial")
summary(mylogit)
#new dataframe
x <- data.frame("Loan" = c(250000), "Default" = c(1))
x
#predicting using glm
x$Defaultp <- predict(mylogit, newdata = x, type = "response")
x

