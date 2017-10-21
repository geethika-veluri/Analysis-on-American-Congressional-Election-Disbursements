
# install.packages('C50')
library(C50)
data(churn)
## Two objects are loaded: churnTrain and churnTest
str(churnTrain)
table(churnTrain$Class)

# This table is at customer day level. The x variables show the call attributes of a customer at day level. 
# It also has customer attributes.

table(churnTrain$churn)
churn_rate <- nrow(churnTrain[churnTrain$churn == 'yes', ])/nrow(churnTrain)
churn_rate  

# write.csv(churnTrain,file="V:\\UTD\\ABI with R\\churnTrain_raw.csv")


# install.packages('corrplot')
library(corrplot)
my_data <- churnTrain[,seq(6,19,1)]
z <- cor(my_data)
corrplot(z,method='circle')

churnTrain$churn_flag <- ifelse(churnTrain$churn == 'yes',1,0)
model_data <- churnTrain
model_data[ ,c('total_day_charge','total_eve_charge','total_night_charge','total_intl_charge',
               'total_day_calls','total_eve_calls','total_night_calls','churn')] <- NULL
model <- glm(churn_flag ~.,family=binomial(link='logit'),data=model_data)
summary(model)

install.packages('car')
library(car)
vif(model)

prediction <- predict(model,churnTest,type='response')
head(prediction,10)

Final_pred <- data.frame(pred=prediction, actual=as.character(churnTest$churn))

as.character(Final_pred$actual)

Final_pred$bin_pred <- ifelse(Final_pred$pred>0.5,'yes','no')
table(Final_pred$bin_pred)
table(churnTrain$churn)

Final_pred$comp <- ifelse(Final_pred$actual == Final_pred$bin_pred,1,0)
str(Final_pred)

table(Final_pred$comp)
