
rm(list=ls())

library(Quandl)
library(xts)
library(forecast)
library(TSclust)
library(dendextend)
library(ggplot2)
library(reshape2)
library(dtw)
library(TSdist)

list_of_codes <- c('CHRIS/CME_HG6.1','CHRIS/CME_C2.1','CHRIS/CME_GC4.1','CHRIS/CME_W3.1','CHRIS/CME_BZ2.1','CHRIS/CME_S2.1','CHRIS/CME_SI1.1','CHRIS/CME_PL2.1','CHRIS/LIFFE_C2.1','CHRIS/CME_CL5.1', 'CHRIS/EUREX_FESX2.1','CHRIS/CME_RP1.1','CHRIS/CME_AD1.1') #'CHRIS/CME_N1Y1.1', 'CHRIS/CME_RU1.1'
full_data <- Quandl(list_of_codes, start_date = '2015-12-31', end_date = '2020-02-10',collapse = "weekly")
colnames(full_data) <- c('Date','Copper','Corn','Gold','Wheat','Brent Crude','Soy','Ag','Pt','Coco','Crude Oil', 'EU FX','GBLb','Au Dol') #'Yen','Ruble'
full_data$Date <- NULL
dates = seq(as.Date("2016-01-03"),length = nrow(full_data),by="weeks")
ts_data = xts(full_data,order.by = dates)
ts_data_clean = apply(ts_data, 2, tsclean)
xts_data_clean = xts(ts_data_clean,order.by = dates)
xts_diff =  diff(log(xts_data_clean),1)
#remove first row
xts_diff = xts_diff[-1,]
rev_data = xts_diff


#gold <- arima.sim(model=list(ar=c(.9,-.2)),n=100)
#ag <- arima.sim(model=list(ar=c(.2,-.8)),n=100)

gold = ts(rev_data$Gold, start=c(2016,01,04), end=c(2017,01,04), frequency = 365)
ag = ts(rev_data$Ag, start=c(2016,01,04), end=c(2017,01,04), frequency = 365)

#Generic implementation of a constrain optimization algorithm.
library(GA)

set.seed(1)
#gold <- arima.sim(model=list(ar=c(.9,-.2)),n=100)
#ag <- arima.sim(model=list(ar=c(.2,-.8)),n=100)

a <- gold
b <- ag
#win_size = 60
epsilon = .25

f <- function(x, series1, series2){
  window_size = x[3]
  return (diss.EUCL((series1[x[1]:(x[1]+window_size)]),(series2[x[2]:(x[2]+window_size)])) / window_size)
}

# c1 <- function(x, series1, series2, epsilon){
#   return ((diss.EUCL((series1[x[1]:(x[1]+x[3])]),(series2[x[2]:(x[2]+x[3])])) / x[3]) - epsilon)
# }

fitness <- function(x,a,b, epsilon) {
  #f <- -1*f(x,a,b,win_size) - epsilon  # we need to maximise -f(x)
  f <- -1*f(x,a,b)  # we need to maximise -f(x)
  #pen <- sqrt(.Machine$double.xmax) # penalty term
  #penalty1 <- max(c1(x,a,b,epsilon),0) #*pen
  #f <- f - penalty1
  return (f) # fitness function value
}

GA <- ga("real-valued", function(x) fitness(x,a,b, epsilon),
         lower = c(3,3,3), 
         #upper = c(nrow(as.matrix(a))-max(win_size)-1, nrow(as.matrix(b))-max(win_size)-1, max(win_size)),
         upper = c(nrow(as.matrix(a))-win_size-1, nrow(as.matrix(b))-win_size-1, win_size),
         maxiter = 1000, run = 500)

summary(GA)

ga = GA@solution
par(mfrow = c(dim(ga)[1],1))
n=1
while (n <= dim(ga)[1]){
  plot(ceiling(ga[n,1]):(ceiling(ga[n,1])+ceiling(ga[n,3])), 
       gold[ceiling(ga[n,1]):(ceiling(ga[n,1])+ceiling(ga[n,3]))],type = "l")
  par(new=TRUE)
  plot(ceiling(ga[n,2]):(ceiling(ga[n,2])+ceiling(ga[n,3])), 
       ag[ceiling(ga[n,2]):(ceiling(ga[n,2])+ceiling(ga[n,3]))],type = "l", col.axis="red", col = "red")
  n=n+1
}

# par(mfrow = c(1,1))
# plot(gold[21:(21+win_size)],type = "l")
# par(new=TRUE)
# plot(ag[28:(28+win_size)],type = "l", col.axis="red", col = "red")

n=1

print (diss.EUCL(ag[ceiling(ga[n,2]):(ceiling(ga[n,2])+ceiling(ga[n,3])-1)],
                 gold[ceiling(ga[n,1]):(ceiling(ga[n,1])+ceiling(ga[n,3])-1)] )/ga[n,3])
