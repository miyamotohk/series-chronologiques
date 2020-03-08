# TP Séries chronologiques
# Henrique Miyamoto et Abderrahim Mehdaoui

# Clear workspace
rm(list = ls())
.rs.restartR()

# Load libraries
library("lmtest")
library("forecast")

# Load data
data = read.csv(file='030320_data.csv', sep=';', encoding = 'UTF-8')
data = data[1:711,]   # Delete void lines

##########################################################################
## Partie 1: Estimation du modèle

# Question 1
# Add earning yield
colnames(data)[7] = "earningyield"
data[,7] = data$earnings/data$price

# Convert into time series type
ts.ey = ts(data$earningyield, start=c(1960,1), end=c(2019,3), frequency=12)
ts.r = ts(data$rates, start=c(1960,1), end=c(2019,3), frequency=12)

# Plot earning yield and rate
par(mfrow=c(1,1))
png(file='./Figures/Figure1.png', width=480, height=350)
plot(ts.ey, ylab='Earning yield', main='Évolution de l\'earning yield')
dev.off()

par(mfrow=c(1,1))
png(file='./Figures/Figure2.png', width=480, height=350)
plot(ts.r, ylab='Taux sans risque', main='Évolution du taux sans risque')
dev.off()

# Scatter plot
png(file='./Figures/Figure3.png', width=480, height=350)
plot(x=ts.r, y=ts.ey, xlab='Taux sans risque', ylab='Earning yield', main='Earning yield vs taux')
abline(lm(ts.ey~ts.r), col="red")
dev.off()

# Question 4
# Fit by least squares
ls.model = lm(formula = ts.ey ~ ts.r)
summary(ls.model)
ls.model$coefficients

# Plot fitted data with orignal data
png(file='./Figures/Figure4.png', width=480, height=350)
ts.eyfit = ts(ls.model$fitted.values, start=c(1960,1), end=c(2019,3), frequency=12)
ts.plot(ts.ey, ts.eyfit, gpars = list(ylab='Earning yield', col=c("black", "red")),  main='Earning yield original et issue du MCO')
legend(x=2000, y=0.14, legend=c("Original data", "Fitted data"), col=c("black", "red"), lty=c(1,1), cex=0.8)
dev.off()

# Student's test
t.test(ts.ey, ls.model$fitted.values)
# Fisher test
fisher.test(ts.ey[0:10], ls.model$fitted.values[0:10])
# R-suared value (https://www.investopedia.com/terms/r/r-squared.asp)
summary(ls.model)$r.squared

# Question 5
# Study of residuals
# Density
png(file='./Figures/Figure5.png', width=480, height=350)
plot(density(ls.model$residuals), main='Dénsité des résidus')
dev.off()
# Normality
png(file='./Figures/Figure6.png', width=480, height=350)
qqnorm(ls.model$residuals)
qqline(ls.model$residuals, col='red')
dev.off()
# Autocorrelation
png(file='./Figures/Figure7.png', width=480, height=350)
acf(ls.model$residuals, main='Autocorrélation des résidus')
dev.off()
# Heteroscedasticity
# https://www.r-bloggers.com/how-to-detect-heteroscedasticity-and-rectify-it/
# If there is no heteroscedasticity we should see a flat line on the graph residauls vs fitted
png(file='./Figures/Figure8.png', width=480, height=350)
par(mfrow=c(2,2))
plot(ls.model)
dev.off()
# A p-Value > 0.05 indicates that the null hypothesis (the variance is unchanging in the residual) can be rejected
# and therefore heterscedasticity exists.
# https://stats.stackexchange.com/questions/239060/interpretation-of-breusch-pagan-test-bptest-in-r
bptest(ls.model)

# Question 6

# Extract subsequences
ts.ey1 = window(ts.ey, start=c(1961,1), end=c(2001,12), frequency=12)
ts.r1 = window(ts.r, start=c(1961,1), end=c(2001,12), frequency=12)
ts.ey2 = window(ts.ey, start=c(2002,1), end=c(2019,3), frequency=12)
ts.r2 = window(ts.r, start=c(2002,1), end=c(2019,3), frequency=12)

# Plot two trajectories
par(mfrow=c(1,2))
png(file='./Figures/Figure9.png', width=480, height=350)
plot(ts.ey1, ylab='Earning yield', main='Earning yield')
dev.off()
png(file='./Figures/Figure10.png', width=480, height=350)
plot(ts.ey2, ylab='Earning yield', main='Earning yield')
dev.off()

# Fit each subsequence by least squares
ls.model1 = lm(formula = ts.ey1 ~ ts.r1)
summary(ls.model1)

ls.model2 = lm(formula = ts.ey2 ~ ts.r2)
summary(ls.model2)

# Plot two scatter plots
par(mfrow=c(1,2))
png(file='./Figures/Figure11.png', width=480, height=350)
plot(x=ts.r1, y=ts.ey1, xlab='Taux sans risque', ylab='Earning yield', main='Earning yield vs taux')
abline(lm(ts.ey1~ts.r1), col="red")
dev.off()

png(file='./Figures/Figure12.png', width=480, height=350)
plot(x=ts.r2, y=ts.ey2, xlab='Taux sans risque', ylab='Earning yield', main='Earning yield vs taux')
abline(lm(ts.ey2~ts.r2), col="red")
dev.off()

##########################################################################
## Partie 2: Estimation d'une nouvelle spécification et comparaison

# Question 7

# Add real rate
colnames(data)[8] = "deflated"
data$deflated[13:nrow(data)] = (data$cpi[1:(nrow(data)-12)]-data$cpi[13:nrow(data)])*100/data$cpi[13:nrow(data)]

colnames(data)[9] = "realrate"
data$realrate = data$rates - data$deflated

ts.rreal = ts(data$realrate, start=c(1960,1), end=c(2019,3), frequency=12)
ts.rreal1 = window(ts.rreal, start=c(1961,1), end=c(2001,12), frequency=12)
ts.rreal2 = window(ts.rreal, start=c(2002,1), end=c(2019,3), frequency=12)

# Least square methods
ls.model.new = lm(formula = ts.ey ~ ts.rreal)
summary(ls.model.new)

ls.model.new1 = lm(formula = ts.ey1 ~ ts.rreal1)
summary(ls.model.new1)

ls.model.new2 = lm(formula = ts.ey2 ~ ts.rreal2)
summary(ls.model.new2)

# Scatter plots
par(mfrow=c(1,1))
png(file='./Figures/Figure13.png', width=480, height=350)
plot(x=ts.rreal, y=ts.ey, xlab='Taux réel', ylab='Earning yield', main='Earning yield vs taux')
abline(lm(ts.ey~ts.rreal), col="red")
dev.off()

par(mfrow=c(1,2))
png(file='./Figures/Figure14.png', width=480, height=350)
plot(x=ts.rreal1, y=ts.ey1, xlab='Taux réel', ylab='Earning yield', main='Earning yield vs taux')
abline(lm(ts.ey1~ts.rreal1), col="red")
dev.off()

png(file='./Figures/Figure15.png', width=480, height=350)
plot(x=ts.rreal2, y=ts.ey2, xlab='Taux réel', ylab='Earning yield', main='Earning yield vs taux')
abline(lm(ts.ey2~ts.rreal2), col="red")
dev.off()

##########################################################################
## Partie 3: Estimation d'une nouvelle spécification modèle ARMA (p,q)

# Question 9
# Order identification
# i) Autocorrelation plot and the partial autocorrelation plots
# https://en.wikipedia.org/wiki/Box%E2%80%93Jenkins_method

png(file='./Figures/Figure16.png', width=480, height=350)
par(mfrow=c(1,2))
acf(data$earningyield, main='Autocorrelation')
pacf(data$earningyield, main='Partial autocorrelation')
dev.off()

# Since the autocorrelation does not decay to zero, we infer the model is not stationnary and we differentiate
ey.diff = diff(data$earningyield, differences=1)
ts.ey.diff = ts(ey.diff, start=c(1960,1), end=c(2019,3), frequency=12)

png(file='./Figures/Figure17.png', width=480, height=350)
par(mfrow=c(1,2))
acf(ts.ey.diff, main='Autocorrelation')
pacf(ts.ey.diff, main='Partial autocorrelation')
dev.off()
# The partial autocorrelation of an AR(p) process becomes zero at lag p+1 and greater => AR(4)
# The autocorrelation function of a MA(q) process becomes zero at lag q+1 and greater => MA(5)
# We estimate a model ARMA(4,5)

# ii) Akaike Information Criterion (AIC)
pmax = 6; qmax = 6;
AIC = matrix(0, nrow=pmax+1, ncol=qmax+1)
for (p in 0:pmax){
  for (q in 0:qmax){
    model = arima(ts.ey.diff, order=c(p,0,q), method="ML")
    AIC[p,q] = model$aic
  }
}
AIC
which(AIC == min(AIC), arr.ind = TRUE)
# ARMA(6,3)

# Question 10
# Returns best ARIMA model according to either AIC, AICc or BIC value.
# TO DO: vérifier la qualité de votre estimation
myarima = auto.arima(ts.ey)
summary(myarima)

# Question 11
prediction = predict(myarima, 3, se.fit=TRUE)

# Question 12
#MSE(prediction$pred, data$earningyield[(nrow(data)-2):nrow(data)])
#RMSE(prediction$pred, data$earningyield[(nrow(data)-2):nrow(data)])

##########################################################################
## Partie 4: Stabilité du modèle

# Question 13
# Question 14