library(lattice)
library(plyr)

## Create a record and some random data for every 5 seconds 
## over two days for two hosts.
dates <- seq(as.POSIXct("2011-01-01 00:00", tz = "GMT"),
             as.POSIXct("2011-01-02 23:59", tz = "GMT"),
             by = 60)

head(dates)

dates<-as.data.frame(dates)


Store_data1 <- Store_data[Store_data$refProductId == 6,]


Store_data2 <- data.frame(Id = seq(1:360) , refProductId = 6 , refStoreId = 30 ,City="NewYork" , SalesRevenue =  sample(1000:95662, 360, replace=T) ,
                          CreateDate = seq(as.POSIXct("2016-01-01 00:00", tz = "GMT"),
                                           as.POSIXct("2016-01-15 23:00", tz = "GMT"),
                                           by = "hour") )


seq(as.POSIXct("2016-01-01 00:00", tz = "GMT"),
    as.POSIXct("2016-01-15 23:00", tz = "GMT"),
    by = "hour")


SnowFall <- data.frame(Id = seq(1:360) , AverageSnowFall  =  sample(10:952, 360, replace=T) ,CreateDate = seq(as.POSIXct("2016-01-01 00:00", tz = "GMT"),
                                                                                                              as.POSIXct("2016-01-15 23:00", tz = "GMT"),
                                                                                                              by = "hour") )

GasPrice_tb <- data.frame(Id = seq(1:360) , AverageGasPrice  =  sample(10:952, 360, replace=T) ,CreateDate = seq(as.POSIXct("2016-01-01 00:00", tz = "GMT"),
                                                                                                                   as.POSIXct("2016-01-15 23:00", tz = "GMT"),
                                                                                                                   by = "hour") )
                                               


##Throw these tables in sql 
str(SnowFall)

SnowFall$AverageSnowFall <- as.numeric(SnowFall$AverageSnowFall)
SnowFall$Id <- as.numeric(SnowFall$Id)

SnowFall$CreateDate <- as.POSIXlt(SnowFall$CreateDate)


sqlSave(channel , SnowFall , "SnowFall_tb" , append = TRUE, rownames = F)



str(GasPrice_tb)

#SnowFall$AverageSnowFall <- as.numeric(SnowFall$AverageSnowFall)
#SnowFall$Id <- as.numeric(SnowFall$Id)

GasPrice_tb$CreateDate <- as.POSIXlt(GasPrice_tb$CreateDate)



sqlSave(channel , GasPrice_tb , "GasPrices_tb" , append = TRUE, rownames = F)

 

str(Store_data2)

Store_data2$CreateDate <- as.POSIXlt(Store_data2$CreateDate)

sqlSave(channel , Store_data2 , "SalesByCity_tb" , append = TRUE, rownames = F)


library(dplyr)

inner_join()

