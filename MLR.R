emp_churn <- read.csv("C:/Users/Amisha Sancheti/Desktop/MITA sem2/Multivariate Analysis/Project/attritiondata.csv", header=TRUE, stringsAsFactors=FALSE)

str(emp_churn)
library(data.table) 
setDT(emp_churn)

class(emp_churn)

library(dplyr)
library(ggplot2)
library(ggpubr)
library(car)
emp_churn

emp_churn[,1:35][emp_churn[,1:35] == "Yes"] = 1
emp_churn[,1:35][emp_churn[,1:35] == "No"] = 0

emp_churn$Gender[emp_churn$Gender=="Male"]=1;
emp_churn$Gender[emp_churn$Gender=="Female"]=0;

emp_churn$MaritalStatus[emp_churn$MaritalStatus=="Single"]=0;
emp_churn$MaritalStatus[emp_churn$MaritalStatus=="Married"]=1;
emp_churn$MaritalStatus[emp_churn$MaritalStatus=="Divorced"]=2;



x=lm(Attrition~ï..Age+BusinessTravel+DailyRate+DistanceFromHome+EnvironmentSatisfaction+MaritalStatus+HourlyRate+JobInvolvement+JobLevel+JobSatisfaction+MaritalStatus+MonthlyIncome+OverTime+PercentSalaryHike+StandardHours+TotalWorkingYears+WorkLifeBalance+YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager, data=emp_churn)
x
summary(x)
coefficients(x)
confint(x,level=0.95)

# Predicted Values
fitted(x)
residuals(x)
#Anova Table
anova(x)
vcov(x)
cov2cor(vcov(x))
temp <- influence.measures(x)
temp

#diagnostic plots
plot(x)

# Assessing Outliers
outlierTest(x)
qqPlot(x, main="QQ Plot")


# Cook's D plot
# identify D values > 4/(n-k-1)
cutoff <- 4/((nrow(emp_churn)-length(x$coefficients)-2))
plot(x, which=4, cook.levels=cutoff)
# Influence Plot
influencePlot(x, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
# Normality of Residuals
# qq plot for studentized resid
qqPlot(x, main="QQ Plot")
# distribution of studentized residuals
library(MASS)
sresid <- studres(x)
hist(sresid, freq=FALSE,
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit)
#Non-constant Error Variance
# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(x)
# plot studentized residuals vs. fitted values
spreadLevelPlot(x)

#Non-independence of Errors
# Test for Autocorrelated Errors
durbinWatsonTest(x)

summary(x)
x1 <- x
x2 <- lm(Attrition~ï..Age+BusinessTravel+DailyRate+DistanceFromHome+EnvironmentSatisfaction+MaritalStatus+HourlyRate+JobInvolvement+JobLevel+JobSatisfaction+MaritalStatus+MonthlyIncome+OverTime+PercentSalaryHike+StandardHours+TotalWorkingYears+WorkLifeBalance+YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager, data=emp_churn)
# compare models
anova(x1, x2)

step <- stepAIC(x, direction="both")
step$anova # display results
install.packages("leaps", lib="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library(leaps)
leaps<-regsubsets(Attrition~ï..Age+BusinessTravel+DailyRate+DistanceFromHome+EnvironmentSatisfaction+MaritalStatus+HourlyRate+JobInvolvement+JobLevel+JobSatisfaction+MaritalStatus+MonthlyIncome+OverTime+PercentSalaryHike+StandardHours+TotalWorkingYears+WorkLifeBalance+YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager, data=emp_churn,nbest=10)
# view results
summary(leaps)
# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
plot(leaps)
plot(leaps,scale="r2")

# All Subsets Regression
plot(leaps,scale="bic")
summary(leaps)
regsubsets
summary(leaps)
leaps
coef(leaps,1:10)


summary(x)



#Interpretation 

#We have implemented a multiple linear regression model and have drawn certain conclusions. 
#By implementing the model, we can say that, 
# 1.) We are 99.99% sure that age is a highly significant variable in predicting attrition. 
# 2.) We are 99.99% sure that frequent travel also contribute a lot in predicitng attrition. 
# 3.) Similarly Environment satisfaction, Marital status, Job Involvement, Overtime and promotion also contribute to the prediction of attrition. 


#Also, based on the estimate values, we can write the equation and prediction the attrition value where 1 is yes and 0 is no. 
