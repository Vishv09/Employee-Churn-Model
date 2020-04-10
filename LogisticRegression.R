emp_churn <- read.csv("C:/Users/Amisha Sancheti/Desktop/MITA sem2/Multivariate Analysis/Project/attritiondata.csv")

##########Exploring the Dataset###########
str(emp_churn)
library(data.table) 
setDT(emp_churn)
class(emp_churn)

library(caTools)

#we will partition the data into test and train with 80:20 ratio.
partn = floor(0.80*nrow(emp_churn))

set.seed(123) 
train_ind = sample(seq_len(nrow(emp_churn)),size = partn)

empchurn_train = emp_churn[train_ind,]
empchurn_test= emp_churn[-train_ind,]

dim(emp_churn)
dim(empchurn_train)

empchurn_train[is.na(empchurn_train)] <- 0

## applying logistic regression:
logreg_model <- glm(Attrition ~.,family=binomial(link='logit'),na.action = na.exclude,data=empchurn_train)
summary(logreg_model)
round(exp(coef(logreg_model)), 2)

logreg_pred <- predict(logreg_model,empchurn_test,type="response" )
logreg_pred

confusion_matrix <- table(empchurn_test$Attrition, logreg_pred > 0.5)
confusion_matrix

#so we have 240 true positive and true negative 24, 6 false negatives and 24 false positives
#Now we will see the accuracy of our classification.

accuracy <- (sum(diag(confusion_matrix)) / sum(confusion_matrix))
accuracy
# So our model gives 89.79% of accuracy which is good for our data.

#install.packages("pROC", lib="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library(pROC)

logreg_roc <- roc(empchurn_test$Attrition, logreg_pred) 
plot.roc(logreg_roc)

##it is a plot of true positive rate against false positive rate. The graph shows that we have pretty good accuracy for our data.
