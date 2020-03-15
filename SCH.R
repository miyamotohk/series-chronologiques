# TP Séries chronologiques
# Henrique Miyamoto et Abderrahim Mehdaoui

# Clear workspace
rm(list = ls())
#.rs.restartR()

# Load libraries
library('lmtest')
library('forecast')
library('Metrics')
library('strucchange')

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
png(file='./Figures/Figure3.png', width=400, height=400)
plot(x=ts.r, y=ts.ey, xlab='Taux sans risque', ylab='Earning yield', main='Earning yield vs taux')
abline(lm(ts.ey~ts.r), col="red")
dev.off()

# Question 4
# Fit by least squares
ls.model = lm(ts.ey ~ ts.r)
summary(ls.model)
ls.model$coefficients

# Plot fitted data with orignal data
png(file='./Figures/Figure4.png', width=480, height=350)
ts.eyfit = ts(ls.model$fitted.values, start=c(1960,1), end=c(2019,3), frequency=12)
ts.plot(ts.ey, ts.eyfit, gpars = list(ylab='Earning yield', col=c("black", "red")),  main='Earning yield original et issue du MCO')
legend(x=2000, y=0.14, legend=c("Original data", "Fitted data"), col=c("black", "red"), lty=c(1,1), cex=0.8)
dev.off()

# Student's test
t.test(data$earningyield, ls.model$fitted.values)
# F-statistic
summary(ls.model)$fstatistic
# Coeffcient of determination
summary(ls.model)$r.squared

# Question 5
# Study of residuals
# Density
png(file='./Figures/Figure5.png', width=480, height=350)
plot(density(ls.model$residuals), main='Dénsité des résidus')
dev.off()
# Normality
png(file='./Figures/Figure6.png', width=400, height=400)
qqnorm(ls.model$residuals)
qqline(ls.model$residuals, col='red')
dev.off()
# Autocorrelation
png(file='./Figures/Figure7.png', width=480, height=350)
acf(ls.model$residuals, main='Autocorrélation des résidus')
dev.off()
# Heteroscedasticity
png(file='./Figures/Figure8.png', width=600, height=600)
par(mfrow=c(2,2))
plot(ls.model)
dev.off()
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
png(file='./Figures/Figure11.png', width=400, height=400)
plot(x=ts.r1, y=ts.ey1, xlab='Taux sans risque', ylab='Earning yield', main='Earning yield vs taux')
abline(lm(ts.ey1~ts.r1), col="red")
dev.off()

png(file='./Figures/Figure12.png', width=400, height=400)
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
png(file='./Figures/Figure13.png', width=400, height=400)
plot(x=ts.rreal, y=ts.ey, xlab='Taux réel', ylab='Earning yield', main='Earning yield vs taux')
abline(lm(ts.ey~ts.rreal), col="red")
dev.off()

par(mfrow=c(1,2))
png(file='./Figures/Figure14.png', width=400, height=400)
plot(x=ts.rreal1, y=ts.ey1, xlab='Taux réel', ylab='Earning yield', main='Earning yield vs taux')
abline(lm(ts.ey1~ts.rreal1), col="red")
dev.off()

png(file='./Figures/Figure15.png', width=400, height=400)
plot(x=ts.rreal2, y=ts.ey2, xlab='Taux réel', ylab='Earning yield', main='Earning yield vs taux')
abline(lm(ts.ey2~ts.rreal2), col="red")
dev.off()

##########################################################################
## Partie 3: Estimation d'une nouvelle spécification modèle ARMA (p,q)

# Question 9
# Order identification
# i) Autocorrelation plot and the partial autocorrelation plots

par(mfrow=c(1,2))
png(file='./Figures/Figure16.png', width=480, height=350)
acf(data$earningyield, main='Autocorrelation')
dev.off()

png(file='./Figures/Figure17.png', width=480, height=350)
pacf(data$earningyield, main='Partial autocorrelation')
dev.off()

# Since the autocorrelation does not decay to zero, we infer the model is not stationnary and we differentiate
ey.diff = diff(data$earningyield, differences=1)
ts.ey.diff = ts(ey.diff, start=c(1960,1), end=c(2019,3), frequency=12)

par(mfrow=c(1,2))
png(file='./Figures/Figure18.png', width=480, height=350)
acf(ey.diff, main='Autocorrelation')
dev.off()

png(file='./Figures/Figure19.png', width=480, height=350)
pacf(ey.diff, main='Partial autocorrelation')
dev.off()

# The partial autocorrelation of an AR(p) process becomes zero at lag p+1 and greater => AR(1)
# The autocorrelation function of a MA(q) process becomes zero at lag q+1 and greater => MA(1)
# We estimate a model ARMA(1,1,1)

# ii) Akaike Information Criterion (AIC)
pmax = 6; qmax = 6;
AIC = matrix(0, nrow=pmax+1, ncol=qmax+1)
for (p in 0:pmax){
  for (q in 0:qmax){
    model = arima(ts.ey, order=c(p,1,q), method="ML")
    AIC[p,q] = model$aic
  }
}
AIC
which(AIC == min(AIC), arr.ind = TRUE)
# ARIMA(6,1,3)

# Question 10
# Returns best ARIMA model according to either AIC, AICc or BIC value.
myarima = auto.arima(ts.ey)
summary(myarima)
# ARIMA(1,1,2)

# Plot model data and real data
ts.arimafit = ts(myarima$fitted, start=c(1960,1), end=c(2019,3), frequency=12)

par(mfrow=c(1,1))
png(file='./Figures/Figure20.png', width=480, height=350)
ts.plot(ts.ey, ts.arimafit, gpars = list(ylab='Earning yield', col=c("black", "red"), lty=c("solid", "dotted")), main="Earning yield original et issue de l'ARIMA")
legend(x=2000, y=0.14, legend=c("Original data", "Fitted data"), col=c("black", "red"), lty=c("solid", "dotted"), cex=0.8)
dev.off()

# Question 11
# Prediction for 3 periods
prediction = predict(myarima, 3, interval="confidence")
ts.pred = ts(prediction$pred, start=c(2019,4), frequency=12)

myforecast = forecast(myarima, level = c(95), h = 3)

par(mfrow=c(1,1))
png(file='./Figures/Figure21.png', width=480, height=350)
plot(myforecast, include=250, main="Prédiction ARIMA")
dev.off()

par(mfrow=c(1,1))
png(file='./Figures/Figure22.png', width=480, height=350)
plot(myforecast, include=50, main="Prédiction ARIMA")
dev.off()

# Question 12
# To do the prediction and compare with real data, we will predict the last 12 moths of the dataset

ts.actual = window(ts.ey, start=c(2018,4), end=c(2019,3), frequency=12)
arima1 = arima(window(ts.ey, end=c(2018,3)), order=c(1,1,2))
arima2 = arima(window(ts.ey, end=c(2018,3)), order=c(1,1,1))
arima3 = arima(window(ts.ey, end=c(2018,3)), order=c(6,1,3))

pred1 = predict(arima1, 12)
pred2 = predict(arima2, 12)
pred3 = predict(arima3, 12)

png(file='./Figures/Figure23.png', width=480, height=350)
ts.plot(ts.actual, pred1$pred, pred2$pred, pred3$pred, gpars = list(ylab='Earning yield', 
        col=c("black", "red", "blue", "purple")), main="Prédictions de l'earning yield")
legend(x=2018.8, y=0.052, legend=c("Ground truth", "ARIMA(1,1,2)", "ARIMA(1,1,1)", "ARIMA(6,1,3)"), 
        col=c("black", "red", "blue", "purple"), lty=c(1,1,1), cex=0.8)
dev.off()

mae(ts.actual, pred1$pred)
rmse(ts.actual, pred1$pred)

mae(ts.actual, pred2$pred)
rmse(ts.actual, pred2$pred)

mae(ts.actual, pred3$pred)
rmse(ts.actual, pred3$pred)

# Question 11 bis

##########################################################################
## Partie 4: Stabilité du modèle

# Question 13
# Sliding method
beta1 = numeric(0)
ubeta1 = numeric(0)
lbeta1 = numeric(0)

beta2 = numeric(0)
ubeta2 = numeric(0)
lbeta2 = numeric(0)

years = seq(from=1960, to=2010, by=10)
for (year in years){
  ey.aux = window(ts.ey, start=c(year,1), end=c(min(year+10,2018),12), frequency=12)
  r.aux = window(ts.r, start=c(year,1), end=c(min(year+10,2018),12), frequency=12)
  rreal.aux = window(ts.rreal, start=c(year,1), end=c(min(year+10,2018),12), frequency=12)
  
  ls.model1 = lm(ey.aux ~ r.aux)
  ls.model2 = lm(ey.aux ~ rreal.aux)
  
  beta1 = c( beta1, ls.model1$coefficients[2] )
  ubeta1 = c( ubeta1, ls.model1$coefficients[2] + 1.96*sqrt(vcov(ls.model1)[2,2]) )
  lbeta1 = c( lbeta1, ls.model1$coefficients[2] - 1.96*sqrt(vcov(ls.model1)[2,2]) )
  
  beta2 = c( beta2, ls.model2$coefficients[2] )
  ubeta2 = c( ubeta2, ls.model2$coefficients[2] + 1.96*sqrt(vcov(ls.model2)[2,2]) )
  lbeta2 = c( lbeta2, ls.model2$coefficients[2] - 1.96*sqrt(vcov(ls.model2)[2,2]) )
}

par(mfrow=c(1,1))
png(file='./Figures/Figure24.png', width=480, height=350)
plot(years, beta1, type="l", ylim=c(-0.01,0.015), main="Évolution de beta")
lines(years, ubeta1, lty=2)
lines(years, lbeta1, lty=2)
dev.off()

par(mfrow=c(1,1))
png(file='./Figures/Figure25.png', width=480, height=350)
plot(years, beta2, type="l", ylim=c(-0.01,0.015), main="Évolution de beta")
lines(years, ubeta2, lty=2)
lines(years, lbeta2, lty=2)
dev.off()

# Question 14
cusum = efp(formula = ts.ey ~ ts.r)
sctest(cusum)
summary(cusum)

png(file='./Figures/Figure26.png', width=480, height=350)
plot(cusum)
dev.off()