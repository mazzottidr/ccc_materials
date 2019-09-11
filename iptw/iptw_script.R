library(tableone)
library(ipw)
library(sandwich)
library(survey)
library(MatchIt)


data("lalonde")

# fit model of covariates on treatment to get propensity scores
psmodel <- glm(treat ~ age + educ + black + hispan + married + nodegree + re74 + re75, data = lalonde, family = binomial)
summary(psmodel)

ps <- predict(psmodel, type = "response")

# Create weigths
weight <- ifelse(lalonde$treat==1, 1/(ps), 1/(1-ps))
summary(weight)

weighteddata <- svydesign(ids= ~ 1, data = lalonde, weights = ~ weight)
weightedtables <-  svyCreateTableOne(vars = colnames(weighteddata)[2:9], strata = "treat", data = weighteddata, test = F)
print(weightedtables, smd=T)

# In a model with biinary outcome
# Get causal relative risk using weighted GLM
glm.obj <- glm(died ~ treatment, weights = weight, family = binomial(link = log))

# get IPTW coefficients
betaiptw <- coef(glm.obj)

# Calculate asymptotic variance
SE <- sqrt(diag(vcovHC(glm.obj, type = "HC0")))

# Get estimate and CI for relative risk
causalrr <- exp(betaiptw[2])
lcl <- exp(betaiptw[2]-1.96*SE[2])
ucl <- exp(betaiptw[2]+1.96*SE[2])
c(lcl, causalrr, ucl)


# For risk difference (identity versus logit)
glm.obj <- glm(died ~ treatment, weights = weight, family = binomial(link = "identity"))



# Using ipw package
weightmodel <- ipwpoint(exposure = treatment, family = "binomial", link = "logit", denominator = ~ xcovars, data = mydata)

# plot
ipwplot(weights = weightmodel$ipw.weights, logscale = F, main = "weights")

# Fit marginal structural models

msm <- (svyglm(re78 ~ treat, design = svydesign(~ 1, weights = ~ weight, data=lalonde)))
coef(msm)
confint(msm)

truncweght <- replace(weight, weight>10,10)

# Then use truncaded weights as weights in glm, or specify truncation using ipw package (trunc=0.01 1st and 99th percetile)
# weightmodel$weights.trunc     

weightmodel <- ipwpoint(exposure = treat, family = "binomial", link = "logit", numerator = ~ 1, denominator = ~ age + educ + black + hispan + married + nodegree + re74 + re75, data = lalonde, trunc = 0.01)

msm_trunc <- (svyglm(re78 ~ treat, design = svydesign(~ 1, weights = ~ weightmodel$weights.trunc, data=lalonde)))
coef(msm_trunc)
confint(msm_trunc)


