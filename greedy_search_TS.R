
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
library(dygraphs)
library(tseries) 

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

win_size = 20
#epsilon = .002335
#epsilon = 217.195
#epsilon = .00362
epsilon = .0048

num_windows = length(gold) - win_size
min_series = NULL
table_vals = NULL
table_indices <- NULL

queryIndex = 0
refIndex = 0
i=1
k=0
while(i<num_windows){
  ref = gold[i:(win_size+i-1)]
  j=1
  while (j < num_windows){
    sum = 0
    query = ag[j:(win_size + j-1)] #we will evaluate gbLb for windows of size __ throughout its time series
    
    alignment <- diss.EUCL(query,ref)
    sum = (alignment)/win_size
    
    # alignment = dtw(query,ref)
    # sum = (alignment$distance)/win_size
    
    if (sum < epsilon){
      tabl_vals_intermed = cbind(ag[j:(win_size + j-1)], gold[i:(win_size+i-1)])
      table_vals = rbind(table_vals, as.table(tabl_vals_intermed))
      time_x <- j:(j+win_size-1)
      time_y <- i:(i+win_size-1)
      tabl_indices_intermed<- cbind(time_x,time_y)
      table_indices <- rbind(table_indices,as.table(tabl_indices_intermed))
      
      k = k+1
      print(k)
    }
    j=j+1
  }
  i=i+1
}

par(mfrow = c(k,1))
n=1
while (n < nrow(table_indices)){
  print(plot(gold[table_indices[(n:(n+win_size-1)),2]],main="Soy", ylab="Value", xlab = "Time index", type = "l"))
  par(new=TRUE)
  print(plot(ag[table_indices[(n:(n+win_size-1)),1]],main="Wheat", ylab = "Value", xlab = "Time index", type = "l", col.axis="red", col="red"))
  n = n + win_size
}

print(diss.EUCL(ag[tabl_indices_intermed[1]:(tabl_indices_intermed[1]+win_size-1)],
                gold[tabl_indices_intermed[2]:(tabl_indices_intermed[2]+win_size-1)])/win_size)

