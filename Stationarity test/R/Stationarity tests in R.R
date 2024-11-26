#-------------------------------#
# Stationarity of a time series #
#-------------------------------#

# simulate two times series (first = stationary, second = non stationary)
set.seed(2024)

# parameters
n = 100; phi = 0.7; sigma = 1 # number of observation, AR(1) parameter value and s.d. of WN

# stationary time series: AR(1)
t1 = numeric(n)
epsilon = rnorm(n, mean = 0, sd = sigma)  # White noise

for (t in 2:n) {
  t1[t] = phi * t1[t - 1] + epsilon[t]
}

# non-stationary time series: A random walk
t2 = numeric(n)
epsilon = rnorm(n, mean = 0, sd = sigma)  # White noise

for (t in 2:n) {
  t2[t] = t2[t - 1] + epsilon[t]
}

# plots
par(mfrow = c(2, 1))  
plot(t1, type = "l", col = "darkred", main = "Stationary Time Series (AR(1))",
     ylab = "Value", xlab = "Time")
plot(t2, type = "l", col = "red", main = "Non-Stationary Time Series (Random Walk)",
     ylab = "Value", xlab = "Time")

# augmented Dickey-Fuller test
library(tseries)
adf.test(t1) # p-value: 0.0321; Ho: the series is non-stationary, is rejected
adf.test(t2) # p-value: 0.8317; Ho: the series is non-stationary, is not rejected 

# KPSS test
kpss.test(t1, null="Trend") # p-value: 0.0764; Ho: the series is trend stationary, is not rejected
kpss.test(t2, null="Trend") # p-value: 0.01; Ho: the series is trend stationary, is rejected 

#-----#
# end #
#-----#