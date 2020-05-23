
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

#Generic implementation of a constrain optimization algorithm.
library(GA)

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
rev_data = xts_data_clean #xts_diff


gold_xts <- xts_diff$Soy[1:100]
ag_xts <- xts_diff$Wheat[1:100]

gold = ts(gold_xts)
ag = ts(ag_xts)

# set.seed(1)
# gold <- arima.sim(model=list(ar=c(.9,-.2)),n=100)
# ag <- arima.sim(model=list(ar=c(.2,-.8)),n=100)

a <- gold
b <- ag
win_size = 20
epsilon = 2

f <- function(x, series1, series2, win_size)
{return (diss.EUCL((series1[x[1]:(x[1]+win_size)]),(series2[x[2]:(x[2]+win_size)])) / win_size)}

# c1 <- function(x, series2, series1, win_size)
# {return ((diss.EUCL((series1[x[1]:(x[1]+win_size)]),(series2[x[2]:(x[2]+win_size)])) / win_size) - epsilon)
# }

fitness <- function(x,a,b,win_size, epsilon)
{
  f <- -1*f(x,a,b,win_size) - epsilon  # we need to maximise -f(x)
  # pen <- sqrt(.Machine$double.xmax) # penalty term
  # penalty1 <- max(c1(x,a,b, win_size),0)*pen # penalisation for 1st inequality constraint
  # f <- f-penalty1
  return (f) # fitness function value
}

GA <- ga("real-valued", function(x) fitness(x,a,b,win_size, epsilon),
         lower = c(1,1), upper = c(nrow(as.matrix(a))-win_size-1, nrow(as.matrix(b))-win_size-1),
         maxiter = 2000, run = 1000)

summary(GA)

ga = GA@solution
par(mfrow = c(dim(ga)[1],1))
par(mfrow = c(2,1))
n=1
while (n <= dim(ga)[1]){
  plot(ceiling(ga[n,1]):(ceiling(ga[n,1])+win_size), 
       gold[ceiling(ga[n,1]):(ceiling(ga[n,1])+win_size)],type = "l", col="black")
  par(new=TRUE)
  plot(ceiling(ga[n,2]):(ceiling(ga[n,2])+win_size), 
       ag[ceiling(ga[n,2]):(ceiling(ga[n,2])+win_size)],type = "l", col.axis="red", col = "red")
  n=n+1
}

# par(mfrow = c(1,1))
# plot(gold[26:(26+win_size)],type = "l")
# par(new=TRUE)
# plot(ag[69:(69+win_size)],type = "l", col.axis="red", col = "red")

n=1
print (diss.EUCL(ag[ceiling(ga[n,2]):(ceiling(ga[n,2])+win_size-1)],
                 gold[ceiling(ga[n,1]):(ceiling(ga[n,1])+win_size-1)] )/win_size)

