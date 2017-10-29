# Project for Lecture 4: Yield Curve Spread Trades


library(data.table)
library(lubridate) #process time
library(magrittr) #pipe
library(tidyverse)
library(xts)
library(ggplot2)


# Nelson-Siegel-Svensson model to calculate yield
NSS <- function(t,beta0,beta1,beta2,beta3,tau1,tau2){
  t1=t/tau1
  t2=t/tau2
  rt=beta0+beta1*(1-exp(-t1))/t1+beta2*((1-exp(-t1))/t1-exp(-t1))+beta3*((1-exp(-t2))/t2-exp(-t2))
  return (rt)
}

# use yield to calculate price
C_price <- function(ytm,t) {
  #price = 100/((1+ytm)^t)
  price=100*exp(-t*ytm/100)
  return (price)
}
#use price to calculate DV01 Duration
C_duration <- function(price,ytm,t){
  #M_duration <- t/(1+ytm)
  # M_duration <- -1/price*(-t/100)*exp(-t*ytm/100)
  #DV01 <- price*t/10000
  DV01 <- -(-t)*exp(-t*ytm/100)/10000
  return (DV01)
}
#calculate hedge ratio
C_x <- function(DV_10,DV_2) {
  x=DV_2/DV_10
  return (x)
}

data <- fread('https://raw.githubusercontent.com/mmmdi/investment-hw-4/new_branch/data.csv',header=TRUE)
# data.table to xts from 1983-12-30 to 2017-06-30
tmp <- double(8437)
for(i in data[,1]){ tmp<-as.Date(i,format='%Y-%m-%d')}
data[,1]=tmp
xtsdata <- as.xts.data.table(data)
xtsdata <- xtsdata["1983-12-30::2017-06-30"]
enddata<-xtsdata[endpoints(xtsdata,'weeks')]
# ignore na in 2008-3-28
enddata<-enddata[-which(is.na(enddata))]

#store the begin price
enddata$p2=C_price(enddata[,1],2)
enddata$p10=C_price(enddata[,2],10)
#calculate the duration
enddata$DV01_2=C_duration(enddata$p2,enddata[,1],2)
enddata$DV01_10=C_duration(enddata$p10,enddata[,2],10)
#calculate the new yield
enddata$e_r2=NSS(1+358/365,enddata[,3],enddata[,4],enddata[,5],enddata[,6],enddata[,7],enddata[,8])
enddata$e_r10=NSS(9+358/365,enddata[,3],enddata[,4],enddata[,5],enddata[,6],enddata[,7],enddata[,8])
#calculate the end price
enddata$e_p2=C_price(enddata$e_r2,1+358/365)
enddata$e_p10=C_price(enddata$e_r10,9+358/365)
#calculate the hedge ratio
enddata$xratio=C_x(enddata$DV01_10,enddata$DV01_2)
#store the capital
enddata$capital=c(rep(1000000,length(enddata$BETA0)))
#store the change of capital
enddata$revenue=c(rep(0,length(enddata$BETA0)))
#store the cash
enddata$cash=c(rep(0,length(enddata$BETA0)))


for(i in 1:(length(enddata$BETA0)-1)){
  units=enddata$capital[i]*10/(enddata$p2[i]+enddata$x[i]*enddata$p10[i])
  enddata$revenue[i]=units*((enddata$e_p10[i]-enddata$p10[i])*enddata$xratio[i]-(enddata$e_p2[i]-enddata$p2[i]))
  enddata$cash[i]=units*enddata$p2[i]-units*enddata$x[i]*enddata$p10[i]+enddata$capital[i]
  riskfreerate=NSS(7/365,enddata[i,3],enddata[i,4],enddata[i,5],enddata[i,6],enddata[i,7],enddata[i,8])
  interest=enddata$cash[i]*exp(riskfreerate/100*7/365)-enddata$cash[i]
  enddata$capital[i+1]=enddata$capital[i]+enddata$revenue[i]+interest
}
# (1)
return = enddata$capital
return$cumulative_return = (return$capital/1000000 - 1)*100
plot.xts(return$cumulative_return,
         main = "Cumulative Return of Flattener",
         major.ticks= "years", grid.ticks.on = "years", col = "red")

# (2) 
t=10 #? not sure
enddata$ct10=t^2/100*exp(-t*enddata[,2]/100)/enddata$p10
plot.xts(enddata$ct10,ylim = range(0,0.025), col='red',major.ticks= "years", grid.ticks.on = "years")
enddata$dp <- 1/2*enddata$ct_10*enddata$p10*(0.1/100)^2

# (3)





