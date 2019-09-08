install.packages("Matching")
install.packages("MatchIt")

library(tableone)
library(Matching)
library(MatchIt)

data(lalonde)

# Q1 Find the standardized differences for all of the confounding variables (pre-matching). What is the standardized difference for married (to nearest hundredth)?

q1 <- CreateTableOne(colnames(lalonde)[2:10], strata = "treat", data = lalonde, test = F, smd = T)
summary(q1)
# Q2 What is the raw (unadjusted) mean of real earnings in 1978 for treated subjects minus the mean of real earnings in 1978 for untreated subjects?

q1$ContTable$`0` # untreated
q1$ContTable$`1` # treated

q2 <- as.data.frame(q1$ContTable$`1`)$mean[rownames(as.data.frame(q1$ContTable$`1`))=="re78"] - as.data.frame(q1$ContTable$`0`)$mean[rownames(as.data.frame(q1$ContTable$`0`))=="re78"]

# Q3 Fit a propensity score model. Use a logistic regression model, where the outcome is treatment. Include the 8 confounding variables in the model as predictors, with no interaction terms or non-linear terms (such as squared terms). Obtain the propensity score for each subject.

q3.model <- glm(treat ~ age + educ + black + hispan + married + nodegree + re74 + re75, data = lalonde, family = binomial())

min(q3.model$fitted.values)
max(q3.model$fitted.values)

# Q4, Q5
set.seed(931139)
matched_data <- Match(Tr=lalonde$treat, X = q3.model$fitted.values, replace = F)
matched.lalonde <- lalonde[c(matched_data$index.treated, matched_data$index.control),]

q4 <- CreateTableOne(colnames(lalonde)[2:10], strata = "treat", data = matched.lalonde, test = F, smd = T)
summary(q4)



# Q6
set.seed(931139)
matched_data.caliper <- Match(Tr=lalonde$treat, X = q3.model$fitted.values, replace = F, caliper = 0.1)
matched_data.caliper$wnobs
matched.lalonde.caliper <- lalonde[c(matched_data.caliper$index.treated, matched_data.caliper$index.control),]

q6 <- CreateTableOne(colnames(lalonde)[2:10], strata = "treat", data = matched.lalonde.caliper, test = F, smd = T)

as.data.frame(q6$ContTable$`1`)$mean[rownames(as.data.frame(q6$ContTable$`1`))=="re78"] - as.data.frame(q6$ContTable$`0`)$mean[rownames(as.data.frame(q6$ContTable$`0`))=="re78"]

t.test(re78 ~ treat, data = matched.lalonde.caliper, paired = TRUE)
