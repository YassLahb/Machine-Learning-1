
#### This is a DRAFT ### 

# Clear everything in the workspace:
rm(list=ls())

#########################################
# QUESTION 1 : 
#########################################

# ----- Question 1-A ------

# Importing the "salaries" dataset.
salaries <- read.csv("salaries.csv")

# Exploring the datase to see the variables : 
salaries
head(salaries)
tail(salaries)
# We have 2 variables : 
# 1- Salary which gives the salary of people.
# 2 - years of experience which gives the years that a person has as an experience in the professional domain.
plot(salaries$YearsExperience,salaries$Salary,
     pch = 16,
     col = "blue",
     main = "Scatterplot of Salary vs. Years of Experience",
     xlab = "Years of Experience",
     ylab = "Salary")

# Yes, i think there's a huge correlation between the 2 variables because we can see that 
# there's a linear regression and a proportional relationship between both variables.


# ----- Question 1-B ------ 

x = salaries$YearsExperience
y = salaries$Salary

# Finding n : 
n <- length(x)

# Finding the pieces we need : 
S_xy <- sum(x * y) - (sum(x) * sum(y)) / n
S_xx <- sum(x^2) - sum(x)^2 / n
S_yy <- sum(y^2) - sum(y)^2 / n

b <- S_xy / S_xx
a <- mean(y) - b*mean(x)
abline(a, b, col = "red", lwd = 2)

b # Slope 
a # Intercept

lm(y ~ x) # To check if we have approximately the same results.

# Interpretation : 
# The intercept means that for 0 year
# of experience we can hope to have a salary of
# 25792 euros for this company.
# The slope means that for each additionnal year of experience 
# you can earn approximately 9449 euros more.


# ----- Question 1-C ------

# H0 : Hypothesis where B1 = 0. 
# H1 : Hypothesis where B1 =/= 0.
# Significence level of alpha = 0.05

cor.test(x,y)

# Conclusion : We can fairly say that the employee's statement is wrong 
# because we can clearly see that the p-value is < alpha = 0.05 and 0 is not in the confidence interval.
# Therefore, we reject H0.
cat("We reject H0 and conclude that B1 is not equal to 0")

# To check our results : 
model <- lm(y ~ x)
summary(model)


# ----- Question 1-D ------

# Using the same variances we used before :
r_squared = (S_xy^2) / (S_xx * S_yy)
r_squared

# To check our results : 
model <- lm(y ~ x)
summary(model)

# Conclusion : This is a useful model because we can clearly see 
# that we have a strong correlation as r_squared is equal to approximately 0.957 nearly approaching 1.

# ----- Question 1-E ------

sixhalfyear_lecturer  <- data.frame(x =6.5)
new_lecturer <- data.frame(x = 0)

# Prediction interval :
predict(model,newdata = new_lecturer,interval = "predict")

# Confidence interval :
predict(model,newdata = sixhalfyear_lecturer,interval = "confidence")

# Point prediction : 
predict(model,newdata = sixhalfyear_lecturer)

# Interpretation : We can clearly tell to the new employee that : 
# 1. The point prediction value is the salary they should expect to earn in the college. 
# 2. The 95% confidence Interval gives an interval with values that takes incertitude in count to give the mean salary of people with 6.5 years of experience.
# 3. The 95% prediction Interval gives an interval with predicted values for the salary of a new lecturer.

# ----- Question 1-F ------

thirtyyear_lecturer  <- data.frame(x =30)

# Point prediction : 
predict(model,newdata = thirtyyear_lecturer)

# Interpretation :  Our model isn't efficient enough because our model is quite simple 
# but in reality we need a lot more factors like : discipline, rank, experiences,...
# So our model isn't perfect and accurate to have a precise idea of the salary.

# ----- Question 1-G ------

# Plotting using ggplot2.
library(ggplot2)

temp_var <- predict(model,interval ="prediction")
new_df <- cbind(salaries,temp_var)

# 95% confidence and prediction intervals. 
ggplot(new_df, aes(YearsExperience, Salary,color = Salary))+
        geom_point() +
        geom_line(aes(y=lwr), color = "red", linetype = "dashed")+
        geom_line(aes(y=upr), color = "red", linetype = "dashed")+
        geom_smooth(method=lm, se=TRUE) +
        xlab("Years of Experience")+
        ylab("Salary")+
        ggtitle("Plot of Salaries by Years of Experience with 95% confidence and prediction intervals")+
        scale_y_continuous(name="Salary", limits=c(10000, 150000))
        
#########################################
# QUESTION 2 : 
#########################################
# Clear everything in the workspace:
rm(list=ls())

# Loading libraries that we will need : 
library(psych)
library(ggplot2)

# ----- STEP 1 : Load the dataset  ------

insurance <- read.csv("medical-expenses.csv")
head(insurance)
tail(insurance)

# Finding the data type of our variables : 
str(insurance)

# ----- STEP 2 : Explore the dataset  ------

# Finding NA values :
table(is.na(insurance))

# Having a more deeper look at our data :
describe(insurance)
summary(insurance)
boxplot(insurance)

# Since the dependant variable is expenses
# Let's take a look to see how it is distributed 
summary(insurance$expenses)

# We can clearly see that the mean value is greater than the median, 
# this implies that the distribution of insurance expenses is right-skewed.
# Let's confirm that visually :
hist(insurance$expenses) # Data falls on the right.
table(insurance$region)


# Let's now explore the relationships among our data : Correlation matrix 
round(cor(insurance[c("age", "bmi", "children", "expenses")]),2)

# We can also see it in another way using the scatterplot matrix : 
pairs(insurance[c("age", "bmi", "children", "expenses")])
# We can also use the simple plot(insurance) function but there's a lot of variables that we don't need.

# A more useful way to visualize data :
pairs.panels(insurance[c("age", "bmi", "children", "expenses")])


# ----- STEP 3 : Train the regression model  ------

# We are going to train the model on the training dataset, 
# and predict the values on the test dataset : 

# Split data into training(70%) and testing(30%) sets
set.seed(1313)
n <- length(insurance$expenses)
index <- sample(1:n,floor(n*0.7))
length(index)
index
head(index)

train <- insurance[index,]
test <- insurance[-index,]

dim(train)

# Train the model : 
mod1 <- lm(expenses~.,data = train)

print(mod1)

summary(mod1)


# ----- STEP 4 : Validating regression coefficients and models  ------
model<-lm(expenses~age+children+bmi+sex+smoker+region,data=insurance)
model
summary(model)

# AIC and BIC used for choosing the best predictor subsets and for comparing the different models that we could have : 
AIC(mod1)
BIC(mod1)


names(mod1)
mod1$fitted.values


# ----- STEP 5 : Checking assumptions of linear regression  ------

# 1. Errors should follow a normal distribution
hist(mod1$residuals, col = "grey")

# Looks pretty much fine. 

# 2. There should be no heteroscedasticity : 
# There should be no patterns when we look at residuals vs. fitted values
plot(mod1$fitted.values, mod1$residuals)

# It looks like there is no relationship/pattern. 

# 3. There should be no multicollinearity : 
library(car)
vif(mod1)

# 4. There should be no auto serial correlation : 
library("lmtest")
dwtest(mod1)

# ----- STEP 6 : Using the model to make predictions  ------

# Predicting price for the test dataset :
test$predicted.expenses <- predict(mod1, test)

# Printing top 6 rows of actual and predicted expenses :
head(test)

########################################

#  Training a model on the dat
insurance_model <- lm(expenses ~ ., data = insurance )


summary(insurance_model)
# Multiple R-squared <- Correlation


# 1 - Improving the model by adding non linear relationships : 
insurance$age2<-insurance$age^2

# The effect of age on medical expenditures may not
# be constant throughout all age values; the treatment may become disproportionately expensive 
# for the oldest populations

# 2 - Converting a numerical value into binary value
insurance$bmi30<-ifelse(insurance$bmi>=30,1,0) # From numeric variable to a binary indicator

# Medical expenses will only be affected by bmi if it is abnormal i.e. >30, this will make our model more efficient

# 3 - Smoking and obesity may have harmful effects seperately, but it is reasonable to assume that their combined effect may be worse than the sum of each one alone.

# Putting all these together to have an improved regression model : 
insurance_model_2 <- lm(expenses ~ age + age2 + children + bmi + sex + bmi30*smoker + region, data = insurance)

summary(insurance_model_2)
