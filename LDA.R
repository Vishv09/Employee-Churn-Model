emp_churn <- read.csv("C:/Users/vishv/Downloads/ibm-hr-analytics-attrition-dataset/WA_Fn-UseC_-HR-Employee-Attrition.csv", header=TRUE, stringsAsFactors=FALSE)

##########Exploring the Dataset###########
str(emp_churn)
### There is an anomaly with the name of the Age column, we will change that name for proper identification.
names(emp_churn)[1]<- "Age"

str(emp_churn)
library(data.table) 
setDT(emp_churn)
class(emp_churn)

is.na(emp_churn)
table(is.na(emp_churn)) ##The output is false, hence we don't have any null values in our data.

####### Now we will look for erroneous data in our table, column wise.########

unique(emp_churn$Attrition) #the output is yes and no. There is no other deformed value.
unique(emp_churn$BusinessTravel) #the output is 'Non-Travel, Travel_Frequently, Travel_Rarely'. There is no other deformed value.

#####Like wise, our data has only integer values and factors with defined labels in accordance with miller's law. 

unique(emp_churn$Over18) #yes is the only anwers hence it will have no effect on attrition. There is no variation 
# Hence we can drop this.
unique(emp_churn$StandardHours) #no variation. 80 for all
unique(emp_churn$EmployeeCount) #no variation. 1 for all employees.

#Hence we can drop these three columns: Over18, Standard hours, Employee count
emp_churn = subset(emp_churn, select = -c(Over18, StandardHours, EmployeeCount, JobLevel) )

emp_churn[,1:31][emp_churn[,1:31] == "Yes"] = 1
emp_churn[,1:31][emp_churn[,1:31] == "No"] = 0

emp_churn$Gender[emp_churn$Gender=="Male"]=1;
emp_churn$Gender[emp_churn$Gender=="Female"]=0;

emp_churn$MaritalStatus[emp_churn$MaritalStatus=="Non-Travel"]=0;
emp_churn$MaritalStatus[emp_churn$MaritalStatus=="Travel_Rarely"]=1;
emp_churn$MaritalStatus[emp_churn$MaritalStatus=="Travel_Frequently"]=2;

emp_churn$BusinessTravel[emp_churn$BusinessTravel=="Single"]=0;
emp_churn$BusinessTravel[emp_churn$BusinessTravel=="Married"]=1;
emp_churn$BusinessTravel[emp_churn$BusinessTravel=="Divorced"]=2;

emp_churn = subset(emp_churn, select = -c(JobRole,Department,EducationField) )

emp_churn$Attrition <- as.numeric(emp_churn$Attrition)
emp_churn$Gender <- as.numeric(emp_churn$Gender)
emp_churn$MaritalStatus <- as.numeric(emp_churn$MaritalStatus)
emp_churn$OverTime <- as.numeric(emp_churn$OverTime)
emp_churn$BusinessTravel <- as.numeric(emp_churn$BusinessTravel)

#Splitting into test and train data
library(caTools)

partn = floor(0.80*nrow(emp_churn))

set.seed(123) 
train_ind = sample(seq_len(nrow(emp_churn)),size = partn)

empchurn_train = emp_churn[train_ind,]
empchurn_test= emp_churn[-train_ind,]

#dim(emp_churn)
#dim(empchurn_train)

empchurn_train[is.na(empchurn_train)] <- 0

library(MASS)

empchurn_train = subset(empchurn_train, select = -c(BusinessTravel,MaritalStatus) )

#Executing LDA Model
lda1 <- lda(formula=empchurn_train$Attrition ~., data = empchurn_train)

#Making prediction on test data
pred1 <- predict(lda1, newdata=empchurn_test, type ="response")

lda.pred<- pred1$x

#confusion matrix
cm<-table(pred1$class, empchurn_test$Attrition)
cm

#overall accuracy
accuracy <- (sum(diag(cm)) / sum(cm))
accuracy

library(pROC)

#area under the curve
r <- roc(empchurn_test$Attrition, pred1$posterior[,2]) 
plot.roc(r)
auc(r)

#The model accuracy for Linear Discriminant Analysis is about 87% and the area under the curve for the same is 0.85. 

