#set directory
getwd()
 
setwd("E:\\Projects\\Store_Analytics")

getwd()

#install.packages("xts")



##load the library
library(mxnet)
library(sqldf)
library(zoo)
library(xts)
library(Metrics)


library(tsoutliers)
library(expsmooth)
library(fma)
##Data access for data base

args1<-commandArgs(TRUE)

args2<-commandArgs(TRUE)

args3<-commandArgs(TRUE)

#args4<-commandArgs(TRUE)

x<-args1

y<-args2

z<- args3





print(z)

#storeID
#x <- 20

#ProductID
#y <- 5

#Activation function

#Activation_f <- c("relu" , "sigmoid" , "softrelu", "tanh")

if(z==1)
{
  Activation_f<- "relu"
}
  
if(z==2)
{
  Activation_f<- "sigmoid"
}
  
#str(Activation_f)



#number of layers
 
layers= 2


#numberofneurons
neurons = 50


library(RODBC)
channel<- odbcConnect("StoreAnalytics", uid="ved", pwd="pwd")
Store_data<- sqlQuery(channel, paste("SELECT * FROM ModelData where StoreID in (", x, ") and ProductID in (",y,")" , sep="") )

#Store_completedata<-sqlQuery(channel , "select * from ModelData")

Rainfall_data<- sqlQuery(channel,paste("SELECT * FROM [model_data2] where StoreID in (", x, ") and ProductID in (",y,")" , sep=""))



close(channel)

data <- as.xts(Rainfall_data$Rainfall,order.by=as.Date(Rainfall_data$Date))
weekly <- apply.weekly(data,sum)
temp<-as.data.frame(weekly)
temp$Date<-index(weekly)
colnames(temp)[1] <- "Rainfall"



######
data1 <- as.xts(Rainfall_data$GasPrice,order.by=as.Date(Rainfall_data$Date))
weekly1 <- apply.weekly(data1,sum)
temp1<-as.data.frame(weekly1)
temp1$Date<-index(weekly1)
colnames(temp1)[1] <- "GasPrice"


###
 i<-1
 
if((((Store_data$Date[i+1] - Store_data$Date[i]) ==1) &&  (temp$Date[i+1] - temp$Date[i]) ==7) )
  
{ 

for(i in length(temp1$Date):1 )
{
  a<-1/7*temp1[i,1]
  
  b<-1/7*temp[i,1]
  
  Store_data$X1<- ifelse(Store_data$Date <= temp1$Date[i] , a ,Store_data$X1) 
  Store_data$X2<- ifelse(Store_data$Date <= temp$Date[i] , b ,Store_data$X2) 
  
}

}

 if(((Store_data$Date[i+1] - Store_data$Date[i]) ==1) &&  (temp$Date[i+1] - temp$Date[i]) ==30) 
   
 { 
   
   for(i in length(temp1$Date):1 )
   {
     a<-1/30*temp1[i,1]
     
     b<-1/30*temp[i,1]
     
     Store_data$X1<- ifelse(Store_data$Date <= temp1$Date[i] , a ,Store_data$X1) 
     Store_data$X2<- ifelse(Store_data$Date <= temp$Date[i] , b ,Store_data$X2) 
     
   }
   
 }
 
 

 if((((Store_data$Date[i+1] - Store_data$Date[i]) ==7) &&  ((temp$Date[i+1] - temp$Date[i]) ==30 || (temp$Date[i+1] - temp$Date[i]) ==31 )))
   
 { 
   
   for(i in length(temp1$Date):1 )
   {
     a<-1/30*temp1[i,1]
     
     b<-1/30*temp[i,1]
     
     Store_data$X1<- ifelse(Store_data$Date <= temp1$Date[i] , a ,Store_data$X1) 
     Store_data$X2<- ifelse(Store_data$Date <= temp$Date[i] , b ,Store_data$X2) 
     
   }
   
 }
 



##summary and str
summary(Store_data)
str(Store_data)

Store_data[Store_data[, 6] < 1.2 ,6] <- NA

Store_data[Store_data[, 7] < 27 ,7] <- NA

final_model<-NULL


source("Store_analytics_mxnet_v1.R",echo=TRUE,verbose = F)
 
List_Pred<-pred(Store_data,Activation_f,layers,neurons) 


