## Creating smoothes time series and how to use it for "technical trading"
# Using packages quantmod, 
# install.packages("quantmod")
library(quantmod)
library(zoo)
library(xts)
library(quantmod)
getSymbols("AAPL")
head("AAPL")

# to extract columns, we use Op, Hi, Lo, Cl, Vo and Ad

Open <- Op(AAPL)   #Open Price
High <- Hi(AAPL)    # High price
Low <- Lo(AAPL)  # Low price
Close<- Cl(AAPL)   #Close Price
Volume <- Vo(AAPL)   #Volume
AdjClose <- Ad(AAPL) # Adjusted close

## Creating some plots
# Line plot
chartSeries(AAPL,
            type="line",
            subset='2007',
            theme=chartTheme('white'))

# Bar plot between dates
chartSeries(AAPL,
            type="bar",
            subset='2007-05::2007-06',
            theme=chartTheme('white'))

chartSeries(AAPL,
            type="candlesticks",
            subset='2007-05',
            theme=chartTheme('white'))


# Adding Smoother: aka Tech Indicators

# simple moving average
sma <- SMA(Cl(AAPL),n=20)
tail(sma,n=5)

# Implementation of adding the tech indicators
chartSeries(AAPL,
            subset='2007-05::2009-01',
            theme=chartTheme('white'))

# Add to the plot
addSMA(n=30,on=1,col = "blue")

# Add to the plot
addSMA(n=200,on = 1,col = "red")

# Add to the plot
addSMA(n=30,on=1,col = "blue")

addBBands(n=20,sd=2)

addMomentum(n=1)

getSymbols("MSFT")

price = Cl(MSFT) # P(t)
r = price/Lag(price, k = 1) - 1 # P(t)/P(t-1) - 1
delta = 0.005 # 0.5 % return > delta 
signal <-c(0) # first date has no signal

#Loop over all trading days (except the first)
# Trading Strategy: Create signal
for (i in 2 :length(price)){
  if (r[i] > delta)
    signal [i] = 1
  else
    signal[i] = 0
}

# Each data is not attached with time
head(signal, n=20)
# re-attach date to signal (from price series)
signal = reclass(signal,price)

# Charting with Trading rule
chartSeries(MSFT,
            type = 'line',
            subset="2009-08::2009-09-15",
            theme=chartTheme('white'))

# add a custom indicator
addTA(signal,type='S',col='red')

# You can always consider trading on yesterdays signal
trade = Lag(signal, k = 1)

# Buy at open
# Sell at close
# Trading size: ALL IN
# Daily return: daily_Ret = (Close-Open)/Open

ret <- dailyReturn(MSFT)*trade
# rename quantity
names(ret) <- "filter"

#Performance Summary
charts.PerformanceSummary(ret, main="Naive Buy Rule")

## Let's change our trading rule
# 1. Buy/Sell signal based on a single simple filter rule
# BUY: when P(t)/P(t-1) > 1 + delta (delta is an arbitrary threshold)
# SELL: when P(t)/P(t-1) < 1 + delta (delta is an arbitrary threshold)

# Trading Strategy: Create signal
for (i in 2:length(price)){
  if (r[i] > delta){
    signal[i] = 1
  } else if (r[i] < -delta){
    signal[i] = -1
  } else {
    signal[i] = 0
  }
}

signal<-reclass(signal,Cl(MSFT))

trade1 = Lag(signal, k = 1)
ret1 = dailyReturn(MSFT)*trade1
names(ret1) = "Naive"
charts.PerformanceSummary(ret1, main="Naive Buy-Sell and Hold Rule")

# Charting with Trading rule
chartSeries(MSFT,
            type = 'line',
            subset="2009-08::2009-11",
            theme=chartTheme('white'))

addTA(signal,type='S',col='red')
addEMA(n=30,on=1,col = "pink")
addEMA(n=7,on=1,col = "blue")
addRSI(n = 12, maType = "EMA", wilder = TRUE)

# New trading strategy
# RSI
# Buy one unit (share) if RSI < 30 and
# otherwise do not trade
day <-14
price <- Cl(MSFT)
signal <- c()                    #initialize vector
rsi <- RSI(price, day)     #rsi is the lag of RSI
signal [1:day+1] <- 0            #0 because no signal until day+1

for (i in (day+1): length(price)){
  if (rsi[i] < 30){             #buy if rsi < 30
    signal[i] <- 1
  }else {                       #no trade all if rsi > 30
    signal[i] <- 0
  }
}
signal<-reclass(signal,Cl(MSFT))
trade2 <- Lag(signal)

#construct a new variable ret1
ret1 <- dailyReturn(MSFT)*trade1
names(ret1) <- 'Naive'
# construct a new variable ret2
ret2 <- dailyReturn(MSFT)*trade2
names(ret2) <- 'RSI'

retall <- cbind(ret1, ret2)
charts.PerformanceSummary(retall,main="Naive v.s. RSI")