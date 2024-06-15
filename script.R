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
                             apSproximation = F,trace = T)

# Forecast
forec = forecast(germaninflarima)
autoplot(forec)
