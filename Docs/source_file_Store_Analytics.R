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

args4<-commandArgs(TRUE)

args5<-commandArgs(TRUE)

x<-args1

y<-args2

z<- args3








#storeID
#x <- 20

#ProductID
#y <- 5

#Activation function

#Activation_f <- c("relu" , "sigmoid" , "softrelu", "tanh")

#Activation_f<-"relu"



#str(Activation_f)



#number of l

l= args4
#l<-2

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
#summary(Store_data)
#str(Store_data)

Store_data[Store_data[, 6] < 1.2 ,6] <- NA

Store_data[Store_data[, 7] < 27 ,7] <- NA

final_model<-NULL


#source("Store_analytics_mxnet_v1.R",echo=TRUE,verbose = F)

#List_Pred<-pred(Store_data,z,l,neurons) 


final_model<-NULL







##############################################################################################
##############################################################################################
##############################################################################################



funcLag <- function(x,y){
  z<- y$Sales
  for(i in 1:30)
  {
    
    y1 <- lag(x, -i, na.pad = TRUE)
    z <- cbind(z,y1)
    ##print(y)
    
  }
  
  return (z)
}




for (i in 1:length(unique(Store_data$StoreID))) {
  
  SID<-unique(Store_data$StoreID)[i]
  ##subset of storeId
  SID_subset<-subset(Store_data, StoreID==SID)
  
  for (j in unique(SID_subset$ProductID)){
    PID_subset<-subset(SID_subset, ProductID==j)
    
    #order the dates
    PID_subset<- PID_subset[order(PID_subset$Date) ,]
    
    #summary(PID_subset)
    ### ##Check the data if it is daily or weekly or monthly
    if((PID_subset$Date[i+1] - PID_subset$Date[i]) ==1)
    {
      print("Data is Daily Sales")
      ##I should know which var is weekly
      ##PID_subset[ , 6] <-
      colname<-"Rainfall"
      #weekly_column<- c("Rainfall" , "Temp")
      #for (colname in  )) {
      PID_subset[colname] <-1/7*(PID_subset[colname])
      #}
    }
    if((PID_subset$Date[i+1] - PID_subset$Date[i]) ==7)
    {
      print("Data is Weekly Sales")
    }
    if((PID_subset$Date[i+1] - PID_subset$Date[i]) ==30 | (PID_subset$Date[i+1] - PID_subset$Date[i]) ==31)
    {
      print("Data is Monthly Sales")
    }  
    
    
    ####Handle missing value issue by their avg value
    for (colname in 1:length(PID_subset)) {
      PID_subset[is.na(PID_subset[colname]),colname] <-mean(PID_subset[!is.na(PID_subset[colname]),colname])
    }
    
    
    #handel outlier issue
    
    data2 <- as.xts(PID_subset$Sales,order.by=as.Date(PID_subset$Date))
    weekly2 <- apply.weekly(data2,colMeans)
    temp2<-as.data.frame(weekly2)
    temp2$Date<-index(weekly2)
    colnames(temp2)[1] <- "Sales"
    
    
    t<-1
    
    for (i in 1:length(temp2$Date)) {
      
      
      
      for (g in t:length(PID_subset$Sales)) {
        
        if(PID_subset$Sales[g]> 1.25*temp2$Sales[i] && PID_subset$Date[g] <=temp2$Date[i])
        {
          PID_subset$Sales[g] = temp2$Sales[i]
          print("This is cool")
        }
        
        
        
      }
      
      t<-g
      
    }
    
    
    
    
    
    
    n<-30
    
    #create zoo class object
    x <- zoo(PID_subset$Sales)
    
    ##call the function for lag creation
    z <- funcLag(x,PID_subset)
    colnames(z) <- c("Sales","lag1", "lag2","lag3", "lag4","lag5", "lag6","lag7",
                     "lag8","lag9" ,"lag10",
                     "lag11","lag12", "lag13","lag14", "lag15","lag16",
                     "lag17","lag18","lag19", "lag20","lag21",
                     "lag22","lag23", "lag24","lag25", "lag26","lag27",
                     "lag28","lag29","lag30")
    
    #Keep only complete records from data
    z <- z[complete.cases(z),]
    temp<-as.data.frame(z)
    PID_subset <- PID_subset[31:nrow(PID_subset) ,]
    features<-colnames(PID_subset)
    PID_Date<-PID_subset
    train.x1 <- PID_subset[,features[6:(ncol(PID_subset)-1)]]
    PID_subset <- cbind(train.x1 , temp)
    
    ##data sampling
    index <- sample(1:nrow(PID_subset),round(0.9*nrow(PID_subset)))
    train <- PID_subset[index,]
    test <- PID_subset[-index,]
    Date_df <- PID_Date[-index,]
    #features<-colnames(train)
    #train.x1 <- train[,features]
    train.y<-train[ ,"Sales"]
    train[ ,"Sales"] <-NULL
    train.x<-data.matrix(train[ ,])
    test.y<-test[ ,"Sales"]
    test[ ,"Sales"]<-NULL
    test.x<-data.matrix(test[, ])
    
    # Define the input data
    #data <- mx.symbol.Variable("data")
    # A fully connected hidden layer
    # data: input source
    # num_hidden: number of neurons in this hidden layer
    #fc1 <- mx.symbol.FullyConnected(data, num_hidden=1)
    # Use linear regression for the output layer
    #lro <- mx.symbol.LinearRegressionOutput(fc1)
    #mx.set.seed(0)
    # model <- mx.model.FeedForward.create(lro, X=train.x, y=train.y,
    #                                     ctx=mx.cpu(), num.round=50, array.batch.size=20,
    #                                     learning.rate=2e-6, momentum=0.9, eval.metric=mx.metric.rmse)
    x <-train.y
    #train.y<-x
    train.y<-(x-min(x))/(max(x)-min(x))
    
    
    y<-test.y
    test.y<-(y-min(y))/(max(y)-min(y))
    
    
    p<-NULL
    
    
    
    hidden_node_l = NULL
    s<- as.vector(neurons)
    for (p in 1:l) {
      if(p<=l )
      {
        p<-p+1
        hidden_node_l<-as.vector(c(hidden_node_l, s))
        s<- s+20
        print(s)
        #hidden_node_l
        #as.vector(hidden_node_l)
        #hidden_node_l<-(rbind(hidden_node_l, s))
        
        mx.metric.rmse_one <- mx.metric.custom("rmse", function(label, pred) {
          res <- sqrt(mean((label-pred)^2))
          df<-(res)
          return(res)
        })
        
        t<-sink("a_file.txt")
        
        
        
        mx.set.seed(1000)
        model2 <- mx.mlp(data = train.x,
                         label = train.y,
                         hidden_node = hidden_node_l ,
                         out_node = 1,
                         dropout = c(0.6),
                         optimizer = "sgd",
                         activation = "relu" ,
                         out_activation = "rmse",
                         learning.rate = 2e-6,
                         momentum = 0.9,
                         eval.metric = mx.metric.rmse_one,
                         array.layout = "rowmajor" ,
                         num.round=10, array.batch.size=60)
        
        
        sink()
        
        #capture.output(summary(model2),file="captureoutput.txt")
        
        
        #out <- capture.output(summary(model2))
        #model2$aux.params
        #
        #print(eval.metric$get)
        #model2$aux.params
        #model2$symbol
        #model$arg.params
        
        #graph.viz(model2$symbol$as.json())
        
        preds = as.numeric(predict(model2, test.x ))
        denormalised = (preds)*(max(x)-min(x))+min(x)
        
        res<-NULL
        res <-  denormalised
        res<-as.data.frame(res)
        colnames(res) <- "predicted"
        res$Actual <- y
        res$Date <- Date_df$Date
        res$StoreID<-test$StoreID
        res$ProductID<-test$ProductID
        #res$Date<-test$Date
        res$Activation_f<- "relu"
        res$Numberof_neurons<- list(hidden_node_l)
        
        res$Numberof_hiddenl<- length(hidden_node_l)
        
        res$RMSE <- rmse(actual = round(test.y, 3) , predicted = round(preds,3))
        
        #sqrt(mean((denormalised-test.y)^2))
        
        final_model<-rbind(as.data.frame(final_model), res)
        
        
        
      }
    }
  }
}
#return(final_model)






