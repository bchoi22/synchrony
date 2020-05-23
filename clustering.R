rm(list=ls())
library(Quandl)
library(xts)
library(forecast)
library(TSclust)
library(dendextend)
library(ggplot2)
library(reshape2)
library(dtw)

get_lower_tri<-function(b){
   b[upper.tri(b)] <- NA
   return(b)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(b){
   b[lower.tri(b)]<- NA
   return(b)
}


list_of_codes <- c('CHRIS/CME_HG6.1','CHRIS/CME_C2.1','CHRIS/CME_GC4.1','CHRIS/CME_W3.1','CHRIS/CME_BZ2.1','CHRIS/CME_S2.1','CHRIS/CME_SI1.1','CHRIS/CME_PL2.1','CHRIS/LIFFE_C2.1','CHRIS/CME_CL5.1', 'CHRIS/EUREX_FESX2.1','CHRIS/CME_RP1.1','CHRIS/CME_AD1.1') #'CHRIS/CME_N1Y1.1', 'CHRIS/CME_RU1.1'

full_data <- Quandl(list_of_codes, start_date = '2015-12-31', end_date = '2020-02-10',collapse = "weekly")
colnames(full_data) <- c('Date','Copper','Corn','Gold','Wheat','Brent Crude','Soy','Ag','Pt','Coco','Crude Oil', 'EU FX','GBLb','Au Dol') #'Yen','Ruble'
full_data$Date <- NULL
dates = seq(as.Date("2016-01-03"),length = nrow(full_data),by="weeks")
ts_data = xts(full_data,order.by = dates)
head(ts_data)
dim(ts_data)
summary(full_data[-1])
ts_data_clean = apply(ts_data, 2, tsclean)
xts_data_clean = xts(ts_data_clean,order.by = dates)
xts_diff =  diff(log(xts_data_clean),1)
#remove first row
xts_diff = xts_diff[-1,]

ts_data_clean = ts(ts_data_clean,start=c(2016, 1, 3), end=c(2020, 02,10), frequency=365)
ts_data_diff = ts(xts_diff, start=c(2016,01,04), end=c(2020,02,16))


dist_dtw = diss(ts_data_clean, METHOD = "DTWARP")
upper_tri_dtw <- get_upper_tri(as.matrix(dist_dtw))
melted_cormat_dtw <- melt(upper_tri_dtw, na.rm = TRUE)
colnames(melted_cormat_dtw) <- c('Product_x', 'Product_X', 'DTW_Dist')
ggplot(data = melted_cormat_dtw, aes(x=Product_x, y=Product_X, fill =  DTW_Dist)) +
   geom_tile(color = "white")

diss_ACF = diss(ts_data_clean, METHOD = "ACF")

upper_tri_ACF <- get_upper_tri(as.matrix(diss_ACF))
melted_cormat_ACF <- melt(upper_tri_ACF, na.rm = TRUE)
colnames(melted_cormat_ACF) <- c('Product_x', 'Product_X', 'ACF_Dist')
ggplot(data = melted_cormat_ACF, aes(x=Product_x, y=Product_X, fill = ACF_Dist)) +
   geom_tile(color = "white")



diss_Per = diss(ts_data_clean, METHOD = "PER")

upper_tri_Per <- get_upper_tri(as.matrix(diss_Per))
melted_cormat_Per <- melt(upper_tri_Per, na.rm = TRUE)
colnames(melted_cormat_Per) <- c('Product_x', 'Product_X', 'Per_Dist')
ggplot(data = melted_cormat_Per, aes(x=Product_x, y=Product_X, fill = Per_Dist)) +
   geom_tile(color = "white")




diss_Euc = diss(ts_data_clean, METHOD = "EUCL")

upper_tri_Euc <- get_upper_tri(as.matrix(diss_Euc))
melted_cormat_Euc <- melt(upper_tri_Euc, na.rm = TRUE)
colnames(melted_cormat_Euc) <- c('Product_x', 'Product_X', 'Euc_Dist')
ggplot(data = melted_cormat_Euc, aes(x=Product_x, y=Product_X, fill = Euc_Dist)) +
   geom_tile(color = "white")

hclust_dtw = hclust(dist_dtw, method = "ward.D")
plot(as.dendrogram(hclust_dtw),)

#hclust_dwt = hclust(diss_dwt, method = "ward.D")
#plot(hclust_dwt)

hclust_ACF = hclust(diss_ACF, method = "ward.D")
plot(hclust_ACF)

hclust_Per = hclust(diss_Per, method = "ward.D")
plot(hclust_Per)

hclust_Euc = hclust(diss_Euc, method = "ward.D")
plot(hclust_Euc)




#Differenced Data

dist_dtw = diss(ts_data_diff, METHOD = "DTWARP")
upper_tri_dtw <- get_upper_tri(as.matrix(dist_dtw))
melted_cormat_dtw <- melt(upper_tri_dtw, na.rm = TRUE)
colnames(melted_cormat_dtw) <- c('Product_x', 'Product_X', 'DTW_Dist')
ggplot(data = melted_cormat_dtw, aes(x=Product_x, y=Product_X, fill =  DTW_Dist)) +
   geom_tile(color = "white")

diss_ACF = diss(ts_data_diff, METHOD = "ACF")

upper_tri_ACF <- get_upper_tri(as.matrix(diss_ACF))
melted_cormat_ACF <- melt(upper_tri_ACF, na.rm = TRUE)
colnames(melted_cormat_ACF) <- c('Product_x', 'Product_X', 'ACF_Dist')
ggplot(data = melted_cormat_ACF, aes(x=Product_x, y=Product_X, fill = ACF_Dist)) +
   geom_tile(color = "white")



diss_Per = diss(ts_data_diff, METHOD = "PER")

upper_tri_Per <- get_upper_tri(as.matrix(diss_Per))
melted_cormat_Per <- melt(upper_tri_Per, na.rm = TRUE)
colnames(melted_cormat_Per) <- c('Product_x', 'Product_X', 'Per_Dist')
ggplot(data = melted_cormat_Per, aes(x=Product_x, y=Product_X, fill = Per_Dist)) +
   geom_tile(color = "white")




diss_Euc = diss(ts_data_diff, METHOD = "EUCL")

upper_tri_Euc <- get_upper_tri(as.matrix(diss_Euc))
melted_cormat_Euc <- melt(upper_tri_Euc, na.rm = TRUE)
colnames(melted_cormat_Euc) <- c('Product_x', 'Product_X', 'Euc_Dist')
ggplot(data = melted_cormat_Euc, aes(x=Product_x, y=Product_X, fill = Euc_Dist)) +
   geom_tile(color = "white")


hclust_dtw = hclust(dist_dtw, method = "ward.D")
plot(hclust_dtw)

#hclust_dwt = hclust(diss_dwt, method = "ward.D")
#plot(hclust_dwt)

hclust_ACF = hclust(diss_ACF, method = "ward.D")
plot(hclust_ACF)

hclust_Per = hclust(diss_Per, method = "ward.D")
plot(hclust_Per)

hclust_Euc = hclust(diss_Euc, method = "ward.D")
plot(hclust_Euc)

#------------------------------------------------------------

