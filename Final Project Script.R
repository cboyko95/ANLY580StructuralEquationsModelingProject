# Libraries
library(readxl)
library(corrplot)
library(psych)
library(car)
library(lavaan)
library(knitr)

# Setting directory
setwd("C:\\Users\\cboyk\\OneDrive\\Desktop\\HU Analytics\\580 Structural Equation Modeling\\Final Project")

# Reading in data
data <- read_excel("Combined Data.xlsx")

# Removing missing data because ones that are missing are missing for a reason
data2 <- na.omit(data)

# Outliers
mahal <- mahalanobis(data2[, 4:15],
                     colMeans(data2[, 4:15]),
                     cov(data2[, 4:15], use = 'pairwise.complete.obs'))
cutoff <- qchisq(p = 1- 0.001, df = ncol(data2[, 4:15]))
cutoff
summary(mahal < cutoff)
data2[mahal > cutoff, ]
data3 <- data2[ mahal < cutoff, ]

# Additivity for Correlations
corrplot(cor(data3[,4:15])) # Government Integrity is high, but may be ok
vif(lm(rchisq(nrow(data3[,4:15]), 5) ~ ., data3[,4:15])) # all below 10 which is good


# Setting up next assumptions
set.seed(52)
random_variable <- rchisq(nrow(data3[, 4:15]), 13)
fake_model <- lm(random_variable ~ . , data = data3[, 4:15])
standardized <- rstudent(fake_model)
fitvalues <- scale(fake_model$fitted.values)

# Normality
hist(standardized) # Mostly normal with values between -2 and 2, some right skew

# Linearity
plot(fake_model, 2) # Looks pretty good with most values bwtween -2 and 2 on the line

# Homogeneity and Homoscedastic
{plot(standardized, fitvalues) # Looks good!
  abline(v = 0)
  abline(h = 0)}

# Finding number of factors
number_factors <- fa.parallel(data3[, 4:15], fm = 'ml', fa = 'fa') # 3
number_factors$fa.values
sum(number_factors$fa.values > 1) # 1
sum(number_factors$fa.values > 0.7) # 1

# 1 Factor Model
fa1.model <- fa(data3[, 4:15], fm = 'ml', nfactors = 1, rotate = 'oblimin')
fa1.model # taxburden is low so remove that
fa1.model2 <- fa(data3[, c(4, 5, 6, 8, 9, 10, 11, 12, 13, 14, 15)], fm = 'ml', nfactors = 1, rotate = 'oblimin')
fa1.model2 # Fiscal health is low so that is removed
fa1.model3 <- fa(data3[, c(4, 5, 6, 8, 10, 11, 12, 13, 14, 15)], fm = 'ml', nfactors = 1, rotate = 'oblimin')
fa1.model3



# 3 Factor model
fa3.model <- fa(data3[, 4:15], fm = 'ml', nfactors = 3, rotate = 'oblimin')
fa3.model # drop trade freedom first for cross loading
fa3.model2 <- fa(data3[, c(4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 15)], fm = 'ml', nfactors = 3, rotate = 'oblimin')
fa3.model2 # drop govt spending and fiscal health
fa3.model3 <- fa(data3[, c(4, 5, 6, 7, 10, 11, 12, 14, 15)], fm = 'ml', nfactors = 3, rotate = 'oblimin')
fa3.model3 # drop labor
fa3.model4 <- fa(data3[, c(4, 5, 6, 7, 10, 12, 14, 15)], fm = 'ml', nfactors = 3, rotate = 'oblimin')
fa3.model4 # only 1 thing is left on factor 3 so it's not really that good

# 2 Factor model for the fun of it
fa2.model <- fa(data3[, 4:15], fm = 'ml', nfactors = 2, rotate = 'oblimin')
fa2.model # dropping taxburden, fiscalhealth
fa2.model2 <- fa(data3[, c(4, 5, 6, 8, 10, 11, 12, 13, 14, 15)], fm = 'ml', nfactors = 2, rotate = 'oblimin')
fa2.model2 # drop trade freedom
fa2.model3 <- fa(data3[, c(4, 5, 6, 8, 10, 11, 12, 14, 15)], fm = 'ml', nfactors = 2, rotate = 'oblimin')
fa2.model3

fa4.model <- "
GovernmentSize =~ TaxBurden + GovtSpending + FiscalHealth
RuleofLaw =~ PropertyRights + JudicalEffectiveness + GovernmentIntegrity
RegulatoryEffeciency =~ BusinessFreedom + LaborFreedom + MonetaryFreedom
OpenMarkets =~ TradeFreedom + InvestmentFreedom + FinancialFreedom
"
fa4.fit <- cfa(fa4.model, data = data3)
summary(fa4.fit, rsquare = T, fit.measures = T, standardized = T)


# Fit Measures for 1 factor (overall OK, not great model)
fa1.model2$rms
fa1.model2$RMSEA
1 - ((fa1.model2$STATISTIC-fa1.model2$dof)/
       (fa1.model2$null.chisq-fa1.model2$null.dof))

# Fit Measures for 2 Factor (All good!)
fa2.model3$rms
fa2.model3$RMSEA
1 - ((fa2.model3$STATISTIC-fa2.model3$dof)/
       (fa2.model3$null.chisq-fa2.model3$null.dof))

# CFA for 1 factor and 2 factor model to see which one is better
Factor1.model <- "
Freedom =~ PropertyRights + JudicalEffectiveness + GovernmentIntegrity + GovtSpending + FiscalHealth + BusinessFreedom + LaborFreedom + MonetaryFreedom + TradeFreedom + InvestmentFreedom + FinancialFreedom
"
Factor1.fit <- cfa(Factor1.model, data = data3)
summary(Factor1.fit, rsquare = T, fit.measures = T, standardized = T)

factor2.model <- "
Legal =~ PropertyRights + JudicalEffectiveness + GovernmentIntegrity + GovtSpending + BusinessFreedom + LaborFreedom
Money =~ MonetaryFreedom + InvestmentFreedom + FinancialFreedom
"
Factor2.fit <- cfa(factor2.model, data = data3)
summary(Factor2.fit, rsquare = T, fit.measures = T, standardized = T)

anova(Factor1.fit, Factor2.fit)
fitmeasures(Factor1.fit, c("aic", "ecvi"))
fitmeasures(Factor2.fit, c("aic", "ecvi"))

modindices(Factor2.fit, sort. = TRUE, minimum.value = 30.00000)



# Overall Model
Overall.model <- "
Legal =~ PropertyRights + JudicalEffectiveness + GovernmentIntegrity + GovtSpending + BusinessFreedom + LaborFreedom
Money =~ MonetaryFreedom + InvestmentFreedom + FinancialFreedom
"
Overall.fit <- cfa(Overall.model, data = data3, meanstructure = T)
summary(Overall.fit, rsquare = T, fit.measures = T, standardized = T)
table_fit <- matrix(NA, nrow = 7, ncol = 6)
colnames(table_fit) = c("Model", "X2", "df", "CFI", "RMSEA", "SRMR")
table_fit[1, ] <- c("Overall Model", round(fitmeasures(Overall.fit, c("chisq", "df", "cfi", "rmsea", "srmr")),3))
kable(table_fit)

# 2020 Model
model2020.fit <- cfa(Overall.model, data = data3[data3$Year == 2020,], meanstructure = T)
summary(model2020.fit, rsquare = T, fit.measures = T, standardized = T)
table_fit[2, ] <- c("2020 Model", round(fitmeasures(model2020.fit, c("chisq", "df", "cfi", "rmsea", "srmr")),3))
kable(table_fit)

# 2019 Model
model2019.fit <- cfa(Overall.model, data = data3[data3$Year == 2019,], meanstructure = T)
summary(model2019.fit, rsquare = T, fit.measures = T, standardized = T)
table_fit[3, ] <- c("2019 Model", round(fitmeasures(model2019.fit, c("chisq", "df", "cfi", "rmsea", "srmr")),3))
kable(table_fit)

# Configural Model
configural.fit <- cfa(Overall.model, data = data3, meanstructure = T, group = "Year")
summary(configural.fit, rsquare = T, fit.measures = T, standardized = T)
table_fit[4, ] <- c("Configural Model", round(fitmeasures(configural.fit, c("chisq", "df", "cfi", "rmsea", "srmr")),3))
kable(table_fit)

# Metric Model
metric.fit <- cfa(Overall.model, data = data3, meanstructure = T, group = "Year",
                  group.equal = c("loadings"))
summary(metric.fit, rsquare = T, fit.measures = T, standardized = T)
table_fit[5, ] <- c("Metric Model", round(fitmeasures(metric.fit, c("chisq", "df", "cfi", "rmsea", "srmr")),3))
kable(table_fit)

# Scalar Model
scalar.fit <- cfa(Overall.model, data = data3, meanstructure = T, group = "Year",
                  group.equal = c("loadings", "intercepts"))
summary(scalar.fit, rsquare = T, fit.measures = T, standardized = T)
table_fit[6, ] <- c("Scalar Model", round(fitmeasures(scalar.fit, c("chisq", "df", "cfi", "rmsea", "srmr")),3))
kable(table_fit)

# Strict Model
strict.fit <- cfa(Overall.model, data = data3, meanstructure = T, group = "Year",
                  group.equal = c("loadings", "intercepts", "residuals"))
summary(strict.fit, rsquare = T, fit.measures = T, standardized = T)
table_fit[7, ] <- c("Strict Model", round(fitmeasures(strict.fit, c("chisq", "df", "cfi", "rmsea", "srmr")),3))
kable(table_fit)







