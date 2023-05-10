# Time Series and Interrupted Time Series
# Coding recap (rough notes form tutorial example - see the learnr tutorials for more developed examples)

# Example based on the wineind dataframe from the forecast package
library(forecast)
?wineind
is.ts(wineind)

# Some basic functions
start(wineind)
end(wineind)
summary(wineind)

frequency(wineind)

plot(wineind)

plot(decompose(wineind))


# Statistical properties of time series?
# 1. Stationary - constant mean + constant variance
# 2. Seasonality
# 3. Autocorrelation

# transformations of the data
plot(log(wineind)) # Taking the log can stabilise the time series
plot(diff(log(wineind))) # Taking the first order difference removes the trend
plot(diff(diff(log(wineind)), 12)) # Taking the 12th order difference removes the annual seasonality
plot(diff(diff(wineind), 12)) # After differencing, it looks like we don't need the log - this will make interpretation easier

# Tests of stationarity for different transformation  (p<0.05 implies Not Stationary)
tseries::kpss.test(log(wineind))
tseries::kpss.test(diff(log(wineind)))
tseries::kpss.test(diff(diff(log(wineind)), 12))
tseries::kpss.test(diff(diff(wineind), 12))

# [AR][I][MA](p,d,q)(P,D,Q)

# Autoregressive (p)
# Integration aka differencing (d)
# Moving average (q)

# Seasonal autoregressive (P)
# Seasonal integration aka differencing (D)
# Seasonal moving average (Q)

# using the auto.arima function to find model fit for p, q and P, Q
auto.arima(log(wineind), d=1, D=1)

# Refitting the model suggested by auto.arima using sarima() - this provides nice residual plots
library(astsa)
sarima(log(wineind), p=1, d=1, q=1, D=1, Q=1, S=12)

###########################
# Interrupted time series #
###########################

# Creating new variables for ITS.

# Time change
time <- seq(1, length(wineind))
time

# Step change
step <- ifelse(time(wineind)<'1986',0,1)
step

# Change in slope
slope <- append(rep(0,sum(time(wineind)<'1986.0')), seq(1,sum(time(wineind)>='1986.0')))
slope


# Using the variables in segmented regression
modSegReg <- lm(wineind ~ time + step + slope + seasonaldummy(wineind))
summary(modSegReg)

# Using the variables in ARIMA model
sarima(wineind, p=1, d=1, q=1, D=1, Q=1, S=12, xreg=cbind(step, slope))





