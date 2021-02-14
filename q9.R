#LDA 
#install.packages('ISLR')

library(ISLR) #credit card dataset
library(MASS)
dfr = Default
dfr

index = sample(x = 1:nrow(dfr), size = round(nrow(dfr) * 0.5))
index

train_dfr = dfr[index, ]
test_dfr = dfr[-index, ]
train_dfr
test_dfr

model = lda(default ~ student + balance + income, data = train_dfr)
summary(model)

pred = predict(model, test_dfr)
pred
test_dfr$pred = pred$class
test_dfr

table(test_dfr$pred, test_dfr$default)

head(dfr)


#QDA

library(ISLR)  #credit card - banking dataset 
library(MASS)

dfr = Default
head(dfr)
dfr$student = as.numeric(dfr$student)
dfr$student = as.factor(dfr$student)
head(dfr)
index = sample(1:nrow(dfr), round(nrow(dfr) * 0.5))
train_dfr = dfr[index, ]
test_dfr = dfr[-index, ]

model = qda(default ~ student + balance + income, train_dfr)
summary(model)

pred = predict(model, test_dfr)
test_dfr$pred = pred$class
test_dfr

table(test_dfr$pred, test_dfr$default)
