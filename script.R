#-0.31	0.41	0.51	-0.20	0.61	0.20	0.61	-0.30	-0.10	-0.20	-0.51	0.41
#-0.51	0.61	-0.20	0.10	-0.10	0.30	0.00	0.20	-0.30	0.00	-0.10	0.81
#-0.60	0.40	0.50	0.10	-0.10	0.00	0.20	0.10	-0.10	0.10	0.10	0.60
#-0.20	0.60	0.59	0.00	0.00	0.10	0.20	0.10	0.20	0.00	0.20	0.19
#-0.10	0.68	0.58	-0.19	0.00	-0.19	0.39	0.38	0.10	0.00	0.10	0.29
#-0.48	0.57	0.48	-0.47	0.38	0.09	0.47	0.00	0.00	-0.19	0.19	0.38
#-0.56	0.47	0.28	-0.19	-0.09	0.28	0.28	0.00	0.00	-0.28	0.00	0.00
#-1.03	0.85	0.47	0.00	0.09	-0.09	0.19	0.00	-0.19	0.00	0.09	-0.09
#-0.84	0.38	0.75	-0.37	0.28	0.09	0.28	0.00	0.09	0.19	0.09	0.74
#-0.64	0.65	0.18	0.00	-0.18	0.18	0.37	0.09	0.09	0.00	


data = scan()

germaninfl = ts(data,start=2008,frequency = 12)

plot(germaninfl,ylab='Inflation rate',xlab='Year',main='Monthly Inflation rate of Germany')

# Old Decomposition method
plot(decompose(germaninfl))

# New decomposition method
plot(stl(germaninfl,s.window = 7))

# Decomposition forecasting
plot(stlf(germaninfl,method='ets'))

autoplot(stlf(germaninfl,method='ets'))

# Seasonal ARIMA
auto.arima(germaninfl,stepwise = T,
           approximation = F,trace = T)

germaninflarima = auto.arima(germaninfl,stepwise = T,
                             approximation = F,trace = T)

# Forecast
forec = forecast(germaninflarima)
autoplot(forec)

#Exponential Smoothing

ets(germaninfl)

germaninflets = ets(germaninfl)

plot(forecast(germaninflets,h=60))

# Time Series Cross Validation

forecastets = function(x,h){
  forecast(ets(x),h=h)
}

forecastarima = function(x,h){
  forecast(auto.arima(x),
           stepwise=T,
           approximation=F,
           h=h)
}

etserror = tsCV(germaninfl,forecastets,h=1)
arimaerror = tsCV(germaninfl,forecastarima,h=1)

etsmse = mean(etserror^2,na.rm=TRUE)
arimamse = mean(arimaerror^2,na.rm=TRUE)

names = c('Exponential Smoothing','ARIMA')

barplot(c(etsmse,arimamse),names.arg = names,xlab='Models',ylab='MSE',main='Model Accuracy for different models')
