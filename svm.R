# Lending club data for SVM project.

loans <- read.csv("loan_data.csv")

str(loans)

summary(loans)

# Need to factorize certain variables

loans$credit.policy <- factor(loans$credit.policy)
loans$inq.last.6mths <- factor(loans$inq.last.6mths)
loans$delinq.2yrs <- factor(loans$delinq.2yrs)
loans$pub.rec <- factor(loans$pub.rec)
loans$not.fully.paid <- factor(loans$not.fully.paid)

# Exploratory Data Analysis

library(ggplot2)

# histogram of fico scores, colored by not.fully.paid

pl <- ggplot(loans, aes(x = fico))

pl <- pl + geom_histogram(aes(fill = not.fully.paid), color = "black", bins = 40,
                          alpha = 0.5)

pl + scale_fill_manual(values = c('green', "red"))+
  theme_bw()

# barplot of purpose counts, colored by not.fully.paid

pl <- ggplot(loans, aes(x = factor(purpose)))

pl <- pl + geom_bar(aes(fill = not.fully.paid), position = "dodge")

pl + theme_bw()+theme(axis.text.x = element_text(angle = 90, hjust = 1))


# scatter plot of fico scores vs int.rate

ggplot(loans, aes(x = int.rate, y = fico))+
  geom_point()+
  theme_bw()

ggplot(data = loans, aes(x = int.rate, y = fico))+
  geom_point(aes(color = not.fully.paid), alpha = 0.3)+
  theme_bw()

### Building the model ###

# train and test sets

library(caTools)

set.seed(101)

spl = sample.split(loans$not.fully.paid, 0.7)

train = subset(loans, spl == T)
test = subset(loans, spl == F)

# call e1071 library

library(e1071)

model <- svm(not.fully.paid ~ . , data = train)

summary(model)

# use predict to predict new values from the test set using model

predicted.values <- predict(model, test[1:13])

table(predicted.values, test$not.fully.paid)

# Yikes, everything was classified in one group

# Tuning the model

tune.results <- tune(svm, train.x = not.fully.paid ~ .,
                     data = train,
                     kernel = 'radial',
                     ranges = list(cost = c(1,10), gamma = c(0.1,1)))

model <- svm(not.fully.paid~.,
             data = train,
             cost = 10, 
             gamma = 0.1)

predicted.values <- predict(model, test[1:13])

table(predicted.values, test$not.fully.paid)

# Much better after tuning