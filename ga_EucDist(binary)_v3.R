
rm(list=ls())
library(ggfortify)
library(TSclust)
library(ggplot2)
library(GA)
library(gridExtra)
library(TSdist)

library(Quandl)
library(xts)
library(forecast)
library(dplyr)

# set.seed(1)
# test_ts = (arima.sim(model=list(ar=c(-.7,-0.3)),n=50))
# test_ts1 = (arima.sim(model=list(ar=c(-.6,-0.3)),n=50))
# soy <- arima.sim(model=list(ar=c(.9,-.2)),n=150)
# gblb <- arima.sim(model=list(ar=c(.2,-.8)),n=150)
# gold <- arima.sim(model=list(ar=c(.4,-.6)),n=150)
# ag <- arima.sim(model=list(ar=c(.7,-.2)),n=150)
# new_soy = ts(c(test_ts,soy))
# new_gb = ts(c(gblb,test_ts))
# new_gold = ts(c(test_ts1, gold))
# new_ag = ts(c(ag, test_ts1))
# 
# set.seed(1)
# a <- new_gb
# b <- new_soy

list_of_codes <- c('CHRIS/CME_HG6.1','CHRIS/CME_C2.1','CHRIS/CME_GC4.1','CHRIS/CME_W3.1','CHRIS/CME_BZ2.1','CHRIS/CME_S2.1','CHRIS/CME_SI1.1','CHRIS/CME_PL2.1','CHRIS/LIFFE_C2.1','CHRIS/CME_CL5.1', 'CHRIS/EUREX_FESX2.1','CHRIS/CME_RP1.1','CHRIS/CME_AD1.1') #'CHRIS/CME_N1Y1.1', 'CHRIS/CME_RU1.1'
full_data <- Quandl(list_of_codes, start_date = '2015-12-31', end_date = '2020-02-10',collapse = "weekly")
Quandl.api_key('pyja4MxF_1bEXcZkUSBY')
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


gold_xts <- xts_diff$Crude[1:100]
ag_xts <- xts_diff$Wheat[1:100]

gold <- ts(gold_xts)
ag <- ts(ag_xts)

a <- gold
b <- ag

win_bit_size = 20

#full_data = data.frame(gb=new_gb, soy=new_soy)
full_data = data.frame(gold=a, ag=b)
epsilon = 5

decode_bin <- function(x) 
{ 
  x <- gray2binary(x)
  a_item <- binary2decimal(x[1:len1])
  b_item <- binary2decimal(x[(len1+1):(len1+len2)])
  out <- structure(c(a_item,b_item), names = c("a", "b"))
  return(out)
}

go_fitness <- function(x)
{ 
  x <- decode_bin(x)
  if (length(a[x[1]:(x[1]+win_bit_size)]) != length(b[x[2]:(x[2]+win_bit_size)])){
    print("what?!")
    return(-100)
  }else if (x[1] + win_bit_size > length(a)){
    return(-100)
  }else if (x[2] + win_bit_size > length(b)){
    return(-100)
  }else{
    f <- diss.EUCL((a[x[1]:(x[1]+win_bit_size)]),(b[x[2]:(x[2]+win_bit_size)]))
    #f <- diss.EUCL((data[x[1]:(x[1]+win_bit_size)]),(data[x[2]:(x[2]+win_bit_size)])) / win_bit_size
    print("yes")
    return(-1*f)
  }
}

#n1  <- nrow(as.matrix(a))-win_bit_size  
n1 = 1:length(a) # range of values to search
b1 <- decimal2binary(max(n1)) # max number of bits requires
len1 <- length(b1) 

#n2  <- nrow(as.matrix(b))-win_bit_size 
n2 = 1:length(b) # range of values to search
b2 <- decimal2binary(max(n2)) # max number of bits requires
len2 <- length(b2) 

GA <- ga("binary",fitness = go_fitness, 
         nBits = len1+len2,
         keepBest = TRUE,
         popSize = 100,
         maxiter = 1000, run = 700) # seed = 123)

par(mfrow=c(1,1))
plot(GA)

sols <- as.data.frame(t(decode_bin(GA@solution)))
View(sols)



ga = GA@solution
print(decode_bin(ga))
par(mfrow = c(dim(ga)[1],1))
par(mfrow = c(2,1))

plot(decode_bin(ga)[1]:(decode_bin(ga)[1]+win_bit_size), 
     a[decode_bin(ga)[1]:(decode_bin(ga)[1]+win_bit_size)],type = "l", col="black")
par(new=TRUE)
plot(decode_bin(ga)[2]:(decode_bin(ga)[2]+win_bit_size), 
     b[decode_bin(ga)[2]:(decode_bin(ga)[2]+win_bit_size)],type = "o", col.axis="red", ann = FALSE, col = "red")
legend("bottomright", legend = c("a", "b"), lwd = 3, col = c("black", "red"))


print (diss.EUCL(a[decode_bin(ga)[1]:(decode_bin(ga)[1]+win_bit_size)],
                 b[decode_bin(ga)[2]:(decode_bin(ga)[2]+win_bit_size)] )/win_bit_size)
