loans <- read.csv('loan_data.csv')
str(loans)

loans$purpose <- as.factor(loans$purpose)
loans$credit.policy <- as.factor(loans$credit.policy)
loans$inq.last.6mths <- as.factor(loans$inq.last.6mths)
loans$delinq.2yrs <- as.factor(loans$delinq.2yrs)
loans$pub.rec <- as.factor(loans$pub.rec)
loans$not.fully.paid <- as.factor(loans$not.fully.paid)

str(loans)

library(ggplot2)

pl1 <- ggplot(loans, aes(fico)) + geom_histogram(aes(fill=not.fully.paid), color='black', bins=40, alpha=0.5) + theme_bw()
pl2 <- pl1 + scale_fill_manual(values=c('green','red'))
pl2

pl1 <- ggplot(loans, aes(x=factor(purpose))) + geom_bar(aes(fill=not.fully.paid), position = 'dodge') + theme_bw()
pl1

pl1 <- ggplot(loans, aes(x=int.rate, y=fico)) + geom_point(aes(color=not.fully.paid), alpha=0.4) + theme_bw()
pl1

library(caTools)
set.seed(101)

sample <- sample.split(loans$not.fully.paid, SplitRatio=0.7)
train <- subset(loans, sample == T)
test <- subset(loans, sample == F)

library(e1071)

model <- svm(not.fully.paid~., data=train)
summary(model)

pred.values <- predict(model, test[1:13])
table(pred.values, test$not.fully.paid)

tuned.results <- tune(svm, train.x = not.fully.paid~., data=train, kernel='radial', ranges = list(cost=c(100,200),gamma=c(0.1)))
summary(tuned.results)

tuned.model <- svm(not.fully.paid~., data=train, cost=100, gamma=0.1)

tuned.predictions <- predict(tuned.model, test[1:13])
table(tuned.predictions,test$not.fully.paid)


