#############################
#                           #
#    LINEAR REGRESSION      #
#                           #
#############################
#  PREDICTIVE MODELLING     #  
#############################

# Machine Learning: LINEAR REGRESSION
# Predicting medical expenses for an insurance company using linear regression 
# Shivam Panchal


### Exploring and preparing the data

insurance <- read.csv("insurance.csv", stringsAsFactors = TRUE) 
str(insurance)
# Since the dependent variable is charges, 
# let's take a look to see how it is distributed
summary(insurance$charges)

# Because the mean value is greater than the median, 
# this implies that the distribution of insurance charges is right-skewed. 
# We can confirm this visually using a histogram:
hist(insurance$charges)
table(insurance$region)

## Exploring relationships among  features - the correlation matrix
cor(insurance[c("age", "bmi", "children", "charges")])

## Visualizing relationships among  features - the scatterplot matrix 

pairs(insurance[c("age", "bmi", "children", "charges")])
# If we add more information to the plot, it can be even more useful.
library(psych)
pairs.panels(insurance[c("age", "bmi", "children", "charges")])



#  Training a model on the dat
insurance_model <- lm(charges ~ age + children + bmi + sex + smoker + region, data = insurance )
# or ins_model <- lm(charges ~ ., data = insurance), if you are time savvy

summary(insurance_model)
# Multiple R-squared <- Corelation

# Improving model performance 

# adding non-linear relationships, In linear regression, the relationship between an 
# independent variable and the dependent variable is assumed to be linear, yet this may 
# not necessarily be true.  For example, the effect of age on medical expenditures may not
# be constant throughout all age values; the treatment may become disproportionately expensive 
# for the oldest populations

insurance$age2 <- insurance$age^2

# Transformation - converting a numeric variable to  a binary indicator 

insurance$bmi30 <- ifelse(insurance$bmi <= 30, 1, 0)
# think logically, medical expenses will only be affected by bmi if it is abnormal i.e. >30, this will make our model more efficient


# Model specification - adding interaction effects
#  For instance, smoking and obesity may have harmful effects separately, but it is reasonable to assume that their combined effect may be worse than the sum of each one alone. 
# It is braught by *



# Putting it all together - an improved regression model 

# 1 Added a non-linear term for age 
# 2 Created an indicator for obesity 
# 3 Specified an interaction between obesity and smoking 

insurance_model_2 <- lm(charges ~ age + age2 + children + bmi + sex + bmi30*smoker + region, data = insurance)


#####################################################################
#                                                                   #
#  Relative to our first model, the R-squared value has improved    #
#  from 0.75 to about 0.87. Our model is now explaining 87 percent  #
#  of the variation in medical treatment costs.                     #
#                                                                   #
#####################################################################

summary(insurance_model_2)




