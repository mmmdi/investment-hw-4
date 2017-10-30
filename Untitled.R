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
#store the return
enddata$return=c(rep(0,length(enddata$BETA0)))
#store the cumulative return
enddata$cumu=c(rep(0,length(enddata$BETA0)))
# store the units
enddata$units=c(rep(0,length(enddata$BETA0)))
enddata$dif2=lag.xts(enddata$e_p2,k=-1)-enddata$p2
enddata$dif10=lag.xts(enddata$e_p10,k=-1)-enddata$p10
enddata$interest=c(rep(0,length(enddata$BETA0)))
enddata$rf=c(rep(0,length(enddata$BETA0)))

for(i in 1:(length(enddata$BETA0)-1)){
  enddata$units[i]=enddata$capital[i]*10/(enddata$p2[i]+enddata$xratio[i]*enddata$p10[i])
  enddata$revenue[i]=enddata$units[i]*(enddata$dif10[i]*enddata$xratio[i]-enddata$dif2[i])
  enddata$cash[i]=enddata$units[i]*enddata$p2[i]-enddata$units[i]*enddata$xratio[i]*enddata$p10[i]+enddata$capital[i]
  enddata$rf[i]=NSS(7/365,enddata[i,3],enddata[i,4],enddata[i,5],enddata[i,6],enddata[i,7],enddata[i,8])
  enddata$interest[i]=enddata$cash[i]*exp(enddata$rf[i]/100*7/365)-enddata$cash[i]
  enddata$capital[i+1]=enddata$capital[i]+enddata$revenue[i]+enddata$interest[i]
  enddata$return[i]=enddata$revenue[i]+enddata$interest[i]
}
# (1)
# return = enddata$capital
enddata$cumulative_return = (enddata$capital/1000000 - 1)*100
plot.xts(enddata$cumulative_return,
         main = "Cumulative Return of Flattener",
         major.ticks= "years", grid.ticks.on = "years", col = "red")

# (2) 
t=10 #? not sure
enddata$ct10=t^2/100*exp(-t*enddata[,2]/100)/enddata$p10
plot.xts(enddata$ct10,ylim = range(0,0.025), col='red',major.ticks= "years", grid.ticks.on = "years")
enddata$dp <- 1/2*enddata$ct10*enddata$p10*(0.1/100)^2


# (3a)
enddata$y2_change = enddata$e_r2 - enddata[,1]
enddata$y10_change = enddata$e_r10 - enddata[,2]
enddata$Sreturn_2 = (- 100*enddata$y2_change*enddata$DV01_2)*enddata$units
enddata$Sreturn_10 = (- 100*enddata$y10_change*enddata$DV01_10)*enddata$units*enddata$xratio
cum_spread = sum(enddata$Sreturn_2)-sum(enddata$Sreturn_10)
# plot(enddata$Sreturn_2-enddata$Sreturn_10)
 
# (3b)
enddata$ct2=2^2/100*exp(-2*enddata[,1]/100)/enddata$p2
enddata$Creturn_2 = (1/2*enddata$p2*enddata$ct2*(enddata$y2_change/100)^2)*enddata$units
# enddata$ct10=t^2/100*exp(-t*enddata[,2]/100)/enddata$p10
enddata$Creturn_10 = (1/2*enddata$p10*enddata$ct10*(enddata$y10_change/100)^2)*enddata$units
cum_con = sum(enddata$Creturn_2)+sum(enddata$Creturn_10)
# plot(enddata$Creturn_2+enddata$Creturn_10)
# (3c)
enddata$timer_2 = (enddata$p2-enddata$e_p2)*enddata$units
enddata$timer_10 = (-enddata$p10+enddata$e_p10)*enddata$units*enddata$xratio
cum_timer = sum(enddata$timer_2)+sum(enddata$timer_10)
# plot.xts(enddata$interest)
cum_int = sum(enddata$interest)

#(3d)
summation = cum_spread + cum_con + cum_timer + cum_int
totalreturn = sum(enddata$return)
enddata$residual = enddata$return-(enddata$Sreturn_2-enddata$Sreturn_10) - (enddata$Creturn_2+enddata$Creturn_10) -enddata$interest - (enddata$timer_2 + enddata$timer_10)
residual=sum(enddata$residual)

plot.xts(enddata$cumulative_return,
         main = "Cumulative Return of Flattener",
         major.ticks= "years", grid.ticks.on = "years", col = "red",ylim=c(-50, 30))
lines(enddata$Sreturn_2-enddata$Sreturn_10,major.ticks= "years", grid.ticks.on = "years", col = "blue")
lines(enddata$Creturn_2+enddata$Creturn_10,major.ticks= "years", grid.ticks.on = "years",col='green')

plot.xts(enddata$residual,major.ticks= "years", grid.ticks.on = "years",col='pink')
lines(enddata$interest,major.ticks= "years", grid.ticks.on = "years",col='orange')

#4
enddata2=enddata
for(i in 1:(length(enddata2$BETA0)-1)){
  enddata2$units[i]=enddata2$capital[i]*50/(enddata2$p2[i]+enddata2$xratio[i]*enddata2$p10[i])
  enddata2$revenue[i]=enddata2$units[i]*(enddata2$dif10[i]*enddata2$xratio[i]-enddata2$dif2[i])
  enddata2$cash[i]=enddata2$units[i]*enddata2$p2[i]-enddata2$units[i]*enddata2$xratio[i]*enddata2$p10[i]+enddata2$capital[i]
  enddata2$rf[i]=NSS(7/365,enddata2[i,3],enddata2[i,4],enddata2[i,5],enddata2[i,6],enddata2[i,7],enddata2[i,8])
  enddata2$interest[i]=enddata2$cash[i]*exp(enddata2$rf[i]/100*7/365)-enddata2$cash[i]
  enddata2$capital[i+1]=enddata2$capital[i]+enddata2$revenue[i]+enddata2$interest[i]
  enddata2$return[i]=enddata2$revenue[i]+enddata2$interest[i]
}

enddata2$cumulative_return = (enddata2$capital/1000000 - 1)*100
plot.xts(enddata$cumulative_return,
         main = "Cumulative Return of Flattener",
         major.ticks= "years", grid.ticks.on = "years", col = "red",ylim=c(-120,50))

lines(enddata2$cumulative_return,
         main = "Cumulative Return of Flattener",
         major.ticks= "years", grid.ticks.on = "years", col = "blue")
 