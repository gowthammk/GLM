# Task 1: Read the data unto R using seperate vectors Gender, Severe, Information, Age, 
# number of people, number of people that changed behaviour. Apply appropriate numeric codes.
#Dataset Creation
Gender <- rep(c(1, 0), each = 16)
Severe <- rep(c(1, 0), each = 8, times = 2)
Information <- rep(c(1,0), each = 4, times = 4)
Age <-  rep(c(1,2,3,4), times = 8)
n <- c(18, 23, 22, 17, 
       19, 35, 30, 22, 
       24, 37, 29, 24, 
       28, 42, 37, 30,
       11, 25, 12, 8,
       14, 34, 22, 21,
       18, 28, 24, 8,
       28, 47, 45, 30)
Changed_behaviour <- c(11, 14, 11, 5,
                       4, 15, 8, 8,
                       10, 13, 8, 6,
                       11, 14, 15, 9,
                       6, 13, 7, 8,
                       7, 15, 8, 5,
                       12, 15, 7,1,
                       13, 21, 11, 6)
Proportion <- Changed_behaviour/n

#Task 2: Conduct primilinary analyses and draw tentative conclusions.
# Preliminary Analysis
par(mfrow = c(2, 2))
plot(Age, Proportion)
#1) As the age group increases they are less likely to change their behaviour
plot(Severe, Proportion)
#2) People who do not believe in consequence of contracting swine flu are less likely to
# change their behiour
plot(Gender, Proportion)
#3) Female are more likely to change their behaviour
plot(Information, Proportion)
#4) The person who believes there is more information available about swine
#flu are more likely to change their behaviour

#From the plot, we can see there is a potential outlier

#Task 3:  Fit a logistic regression model to the numbers of behaviour changes using 
#the other four variables as explanatory variables. Use forward selection to consider 
#all possible two-way interactions. Use likelihood ratio test to assess the
# significance of these interactions.
#Logistic model

y <- cbind(Changed_behaviour, n - Changed_behaviour)
#Additive model
glm1 <-
  glm(
    y ~ factor(Gender) + factor(Severe) + factor(Information) + factor(Age),
    family = binomial(link = logit)
  )
summary(glm1)

#Forward selection
#Interaction Term Gender and Severe
glminteraction1 <-
  glm(
    y ~ factor(Gender) + factor(Severe) + factor(Information) + factor(Age) + factor(Gender) * factor(Severe),
    family = binomial(link = logit)
  )
summary(glminteraction1)
#Also the p-value is not significant (ie, 0.83763 > 0.05)
anova(glm1, glminteraction1)
#Df deviance is lesser than the chi-square value in Table (ie)3.841 > 0.041994
#So remove the interaction term factor(Gender) * factor(Severe) for the next model


#Interaction Term Gender and Information
glminteraction2 <-
  glm(
    y ~ factor(Gender) + factor(Severe) + factor(Information) + factor(Age) + factor(Gender) * factor(Information),
    family = binomial(link = logit)
  )
summary(glminteraction2)
#Also the p-value is not significant (ie, 0.25108 > 0.05)
anova(glm1, glminteraction2)
#Df deviance is lesser than the chi-square value in Table (ie)3.841 > 1.3182
#So remove the interaction term factor(Gender) * factor(Information) for the next model



#Interaction Term Gender and Age
glminteraction3 <-
  glm(
    y ~ factor(Gender) + factor(Severe) + factor(Information) + factor(Age) + factor(Gender) * factor(Age),
    family = binomial(link = logit)
  )
summary(glminteraction3)
#Also all the p-value is not significant
anova(glm1, glminteraction3)
#Df deviance is lesser than the chi-square value in Table (ie)3.841 > 2.7202
#So remove the interaction term factor(Gender) * factor(Age) for the next model


#Interaction Term Severe and Information
glminteraction4 <-
  glm(
    y ~ factor(Gender) + factor(Severe) + factor(Information) + factor(Age) + factor(Severe) * factor(Information),
    family = binomial(link = logit)
  )
summary(glminteraction4)
#Also all the p-value is significant ie 0.02263 < 0.05
anova(glm1, glminteraction4)
#Df deviance is greater than the chi-square value in Table (ie)3.841 < 5.2312
#So keep the interaction term factor(Severe) * factor(Information) for the next model



#Interaction Term Severe and Age added
glminteraction5 <-
  glm(
    y ~ factor(Gender) + factor(Severe) + factor(Information) + factor(Age) + factor(Severe) * factor(Information) + factor(Severe) * factor(Age),
    family = binomial(link = logit)
  )
summary(glminteraction5)
#Also all the p-value are not significant
anova(glminteraction4, glminteraction5)
#Df deviance is lesser than the chi-square value in Table (ie) 7.815 > 2.9344
#So remove the interaction term factor(Severe) * factor(Age) for the next model


#Interaction Term Severe and Age added
glminteraction6 <-
  glm(
    y ~ factor(Gender) + factor(Severe) + factor(Information) + factor(Age) + factor(Severe) * factor(Information) + factor(Information) * factor(Age),
    family = binomial(link = logit)
  )
summary(glminteraction6)
#Also all the p-value are not significant
anova(glminteraction4, glminteraction6)
#Df deviance is lesser than the chi-square value in Table (ie) 7.815 > 0.85032
#So remove the interaction term factor(Severe) * factor(Age) for the next model

# Task 4: Present the final model.
#The final model is the 4th model -  glminteraction4

# Task 5: Perform diagnostics, presenting appropriate measures and graphics in an appendix. 
# Interpret and suggest appropriate actions, if any. 
# Are there any issues with overdispersion or sparse data? 
# Interpret in terms of your final model(significance, direction and size of effect).
#Diagnostics for the final model

h <- lm.influence(glminteraction4)$hat
rpear <- residuals(glminteraction4, "pearson") / sqrt(1 - h)
rdev <- residuals(glminteraction4, "deviance") / sqrt(1 - h)
phat <- glminteraction4$fitted.values
UsingFitted <- n * phat
DiagonisticsDF <-
  data.frame(Changed_behaviour, UsingFitted, n, phat, h, rpear, rdev)

par(mfrow = c(1, 1))
plot(rpear, main = "Index plot of Pearson Residuals")
plot(rdev, main = "Index plot of Deviance Residuals")

plot(glminteraction4$linear.predictors, rpear, main = "Plot of Pearson Residuals vs Linear Predictors")
plot(glminteraction4$linear.predictors, rdev, main = "Plot of Pearson Residuals vs Linear Predictors")

#High leverage cases
plot(h, ylim = c(0, 0.5))
abline(h = 0.5)
#No high leverage cases

#High influence cases
#Cook's distance
D <- rpear * rpear * h / (2 * (1 - h))
plot(D)
identify(D)
#Case 15 and 20 are high influence cases

#Outlier
plot(rpear)
abline(h = c(-2,2))
identify(rpear)
#Case 20 is an outlier
#Case 15 is almost an outlier
rpear[5]
rpear[15]
rpear[20]
#This case had all members changed behaviours

summary(glminteraction4)
#chi square value is 36.415 > than summary value 32.582
#This, Multiplicative model which has (factor(Severe) * factor(Information)) is a good fit.

#overdispersion
#Formula for overdispersion is deviance/ n-p

32.582 / (32-7)
# 1.30328 the value is almost equal to 1. Thus there is no overdispersion

#Sparse data
summary(glminteraction4)
#Number of Fisher Scoring iterations: 4
#The std. Errors are smaller
#Thus, there is no sparse data


#Interpretation of significance, direction, size of effect of final model
summary(glminteraction4)
#Gender
exp(-0.20409)
1 - exp(-0.20409)
#Males are 18.46% less likely to change the behavior than female
#p-value is 0.16863, which is greater than 0.05. Thus not significant

#Age
exp(-0.08784)
1 - exp(-0.08784)
# People of age group between 40 and 50 are 8.409% less likely to 
# change their behaviour than people of age group between 0 and 40
# P-value is 0.66543, which is greater than 0.05. Thus not significant

exp(-0.50742)
1 - exp(-0.50742)
# People of age group between 50 and 60 are 39.79% less likely to 
# change their behaviour than people of age group between 0 and 40
#P-value is 0.01856, which is less than 0.05. Thus it is significant

exp(-0.68213)
1 - exp(-0.68213)
# People of age group above 60 are 49.44% less likely to 
# change their behaviour than people of age group between 0 and 40
# P-value is 0.00407, which is less than 0.05. Thus it is significant.

#interaction term
exp(0.68915)
exp(0.68915) - 1
# People who believed that contracting swine flu were severe and who believed the information
# about swine flu was adequate are 99.202% more likely to change their behavior than
# people who believed that contracting swine flu were not severe and who believed the
# information about swine flu was not adequate
