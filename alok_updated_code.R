## E:\Projects\Store_Analytics\source_file_Store_Analytics.R


##C:\Program Files\R\R-3.3.1\bin

#parameter sequence
# storeid 
# productid
# activation function
# no of layers
# no of neurons
# temperature
# snowfall
#gasprice
# interst rate
# userid
#training
#forecasting
#"Rainfall" "var1" "var2" "var3" "var4" "var5" "var6"

# "ID" "StoreID" "ProductID" "Date" "Day" "Temp" "Sales" "var1" "var2" "var3" "var4" "var5" "var6" "var7"











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



##Data access for data base


args1<-commandArgs(TRUE)

args2<-commandArgs(TRUE)

args3<-commandArgs(TRUE)

args4<-commandArgs(TRUE)

args5<-commandArgs(TRUE)

args6<-commandArgs(TRUE)

args7<-commandArgs(TRUE)

args8<-commandArgs(TRUE)

args9<-commandArgs(TRUE)

args10<-commandArgs(TRUE)

args11<-commandArgs(TRUE)

args12<-commandArgs(TRUE)

args13 <- commandArgs(TRUE)

args14 <- commandArgs(TRUE)

#uid<-

f<-args10[10]

print(f)

x<-args1[1]

y<-args2[2]

z<- args3


t<-z[3]


#print(z)

print(t)
#storeID
#x <- 20

#ProductID
#y <- 5

#Activation function

#i<-t

Activation_f <- c("relu" , "sigmoid" , "tanh")


if(t==1){ 
  Activation_f<-Activation_f[1]
  
  print(Activation_f)
  
}

if(t==2){ 
  Activation_f<-Activation_f[2]
  
  print(Activation_f)
  
}


print(args13[13])

#c<-Activation_f[t]

#str(z)

#str(m)
#str(Activation_f)

#print(Activation_f)

print(args4)

#number of layers

l= args4[4]


#numberofneurons
n1 = args5[5]


library(RODBC)
channel<- odbcConnect("StoreAnalytics", uid="ved", pwd="pwd")

y1<- args14[20] 
y2<- args14[21] 
y3<- args14[22]
y4<- args14[23] 
y5<- args14[24] 
y6<- args14[25] 
y7<- args14[26] 
y8<- args14[27] 
y9<- args14[29] 
y10<- args14[30] 
y11<- args14[31] 
y12<- args14[32] 
y13<- args14[33] 
y14<- args14[34] 



if(args14[20]== 0 )
{
  y1<- args14[20] 
  y1<- NULL
  
}

if(args14[21]== 0 )
{
  y2<- args14[21] 
  y2<- NULL
}

if(args14[22]== 0 )
{
  y3<- args14[22] 
  y3<- NULL
}
if(args14[23]== 0 )
{
  y4<- args14[23] 
  y4<- NULL
}
if(args14[24]== 0 )
{
  y5<- args14[24] 
  y5<- NULL
}
if(args14[25]== 0 )
{
  y6<- args14[25] 
  y6<- NULL
}
if(args14[26]== 0 )
{
  y7<- args14[26] 
  y7<- NULL
}

if(args14[27]== 0 )
{
  y8<- args14[27] 
  y8<- NULL
}

if(args14[28]== 0 )
{
  y9<-args14[28] 
  y9<- NULL
  
}

if(args14[29]== 0 )
{
  y10<-args14[29] 
  y10<- NULL
  
}
if(args14[30]== 0 )
{
  y11<-args14[30] 
  y11<- NULL
  
}
if(args14[31]== 0 )
{
  y12<-args14[31]
  y12<- NULL
  
}
if(args14[32]== 0 )
{
  y13<-args14[32] 
  y13<- NULL
  
}
if(args14[33]== 0 )
{
  y14<-args14[33] 
  y14<- NULL
  
}





if(x!="all")
{  
Store_data<- sqlQuery(channel, paste("SELECT * FROM ModelData where StoreID in (", x, ") and ProductID in (",y,")" , sep="") )

Store_data<-Store_data[,c(args14[27],args14[28] ,args14[29] ,args14[30] ,args14[31],args14[32],args14[33]   )]

}


if(x=="all")
{  
Store_data<-sqlQuery(channel , "select * from ModelData")

Store_data<-Store_data[,c(args14[27],args14[28] ,args14[29] ,args14[30] ,args14[31],args14[32],args14[33]  )]

}

#Store_completedata<-sqlQuery(channel , "select * from ModelData")


x1<-args14[14] 
x2<-args14[15] 
x3<-args14[16] 
x4<-args14[17]
x5<-args14[18] 
x6<-args14[19] 
 






if(args14[14]== 0)
{
  x1<-args14[14] 
  x1<-NULL
}


if(args14[15]== 0 )
{
  x2<-args14[15] 
  x2<- NULL
  
}
if(args14[16]== 0 )
{
  x3<-args14[16] 
  x3<- NULL
}
if(args14[17]== 0 )
{
  x4<-args14[17] 
  x4<- NULL
}
if(args14[18]== 0)
{
  x5<-args14[18] 
  x5<- NULL
}
if(args14[19]== 0)
{
  x6<-args14[19] 
  x6<- NULL
} 


u<-0

if(args6[6] == 1)
{ 
  Rainfall_data<- sqlQuery(channel,"SELECT * FROM [temp]")
  
  Rainfall_data<- Rainfall_data[,c(args13[13] , "Date", x1,x2,x3,x4,x5,x6)]
  
  u<-Rainfall_data
}

if(args8[7] == 1)
{ 
  Snowfall_data<- sqlQuery(channel,"SELECT * FROM [temp_snowfall]")
  
  Snowfall_data$SnowFall<-as.numeric(Snowfall_data$SnowFall)
  
  u<-Snowfall_data
  
}

if(args8[8] == 1)
{ 
  GasPrice_data<- sqlQuery(channel,"SELECT * FROM [temp_gas]")
  u<-GasPrice_data
  
}

if(args9[9] == 1)
{ 
  Interest_data<- sqlQuery(channel,"SELECT * FROM [temp_interest]")
  u<-Interest_data
  
}





#close(channel)

#data <- as.xts(Rainfall_data$Rainfall,order.by=as.Date(Rainfall_data$Date))
#weekly <- apply.weekly(data,sum)
#temp<-as.data.frame(weekly)
#temp$Date<-index(weekly)
#colnames(temp)[1] <- "Rainfall"

#write.csv(temp,"temp.csv" , row.names = F)


######
#data1 <- as.xts(Rainfall_data$GasPrice,order.by=as.Date(Rainfall_data$Date))
#weekly1 <- apply.weekly(data1,sum)
#temp1<-as.data.frame(weekly1)
#temp1$Date<-index(weekly1)
#colnames(temp1)[1] <- "GasPrice"

#write.csv(temp1,"temp1.csv" , row.names = F)

###
i<-1

if(u!=0)
{  
  
  if((((Store_data$Date[i+1] - Store_data$Date[i]) ==1) &&  (u$Date[i+1] - u$Date[i]) ==7) )
    
  { 
    
    for(i in length(u$Date):1 )
    {
      
      
      
      if(args6[6] == 1)
      { 
        a<-1/7*Rainfall_data[i,1]
        Store_data$X1<- ifelse(Store_data$Date <= Rainfall_data$Date[i] , a ,Store_data$X1) 
      }
      
      
      
      
      if(args7[7] == 1)
      { 
        b<-1/7*Snowfall_data[i,1]
        Store_data$X2<- ifelse(Store_data$Date <= Snowfall_data$Date[i] , b ,Store_data$X2) 
      }
      
      
      
      
      if(args8[8] == 1)
      { 
        c<-1/7*GasPrice_data[i,1]
        Store_data$X3<- ifelse(Store_data$Date <= GasPrice_data$Date[i] , c ,Store_data$X3) 
      }
      
      if(args9[9] == 1)
      { 
        d<-1/7*Interest_data[i,1]
        Store_data$X4<- ifelse(Store_data$Date <= Interest_data$Date[i] , d ,Store_data$X4) 
      }
      
    }
    
  }
  
  
  
  
  if(((Store_data$Date[i+1] - Store_data$Date[i]) ==1) &&  (u$Date[i+1] - u$Date[i]) ==30 || (u$Date[i+1] - u$Date[i]) ==31 ) 
    
  { 
    
    for(i in length(u$Date):1 )
    {
      
      
      if(args6[6] == 1)
      { 
        a<-1/30*Rainfall_data[i,1]
        Store_data$X1<- ifelse(Store_data$Date <= Rainfall_data$Date[i] , a ,Store_data$X1) 
      }
      
      
      
      
      if(args7[7] == 1)
      { 
        b<-1/30*Snowfall_data[i,1]
        Store_data$X2<- ifelse(Store_data$Date <= Snowfall_data$Date[i] , b ,Store_data$X2) 
      }
      
      
      
      
      if(args8[8] == 1)
      { 
        c<-1/30*GasPrice_data[i,1]
        Store_data$X3<- ifelse(Store_data$Date <= GasPrice_data$Date[i] , c ,Store_data$X3) 
      }
      
      if(args9[9] == 1)
      { 
        d<-1/30*Interest_data[i,1]
        Store_data$X4<- ifelse(Store_data$Date <= Interest_data$Date[i] , d ,Store_data$X4) 
      }
      
      
      
    }
    
  }
  
}




##summary and str
#summary(Store_data)
#str(Store_data)

#Store_data[Store_data[, 6] < 1.2 ,6] <- NA

#Store_data[Store_data[, 7] < 27 ,7] <- NA

final_model<-NULL


#source("Store_analytics_mxnet_v1.R",echo=TRUE,verbose = F)

#List_Pred<-pred(Store_data,Activation_f,l,n) 













######################################################################


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
      #colname<-"Rainfall"
      #weekly_column<- c("Rainfall" , "Temp")
      #for (colname in  )) {
      #PID_subset[colname] <-1/7*(PID_subset[colname])
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
    
    
    #o_lier<-args13[13]
    
    #if(o_lier==1)
       
    data2 <- as.xts(PID_subset$Sales,order.by=as.Date(PID_subset$Date))
    weekly2 <- apply.weekly(data2,colMeans)
    temp2<-as.data.frame(weekly2)
    temp2$Date<-index(weekly2)
    colnames(temp2)[1] <- "Sales"
    
    
    t<-1
    
    for (i in 1:length(temp2$Date)) {
      
      Outliers<-"null"
      
      
      for (g in t:length(PID_subset$Sales)) {
        
        if(PID_subset$Sales[g] > 1.25*temp2$Sales[i] && PID_subset$Date[g] <=temp2$Date[i])
        {
          
          Outliers<-as.data.frame(PID_subset$Sales[g])
          colnames(Outliers) <- "Outliers"
          
          Outliers$Date<-PID_subset$Date[g]
        
          
          PID_subset$Sales[g] = temp2$Sales[i]
          #print("This is cool")
          
          Outliers$Date <- as.POSIXlt(Outliers$Date)
          
          Outliers$UID <- f 
          sqlSave(channel , Outliers ,"Outliers2" , append = TRUE , rownames = F)
          
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
    PID_subset <- PID_subset[(n+1):nrow(PID_subset) ,]
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
    
    
    
    hidden_node_layers = NULL
    s<- as.numeric(n1)
    for (p in 1:l) {
      if(p<=l )
      {
        p<-p+1
        hidden_node_layers<-as.vector(c(hidden_node_layers, s))
        s<- s+20
        print(s)
        #hidden_node_layers
        #as.vector(hidden_node_layers)
        #hidden_node_layers<-(rbind(hidden_node_layers, s))
        
        
        
        
        mx.metric.rmse_one <- mx.metric.custom("rmse", function(label, pred) {
          res <- sqrt(mean((label-pred)^2))
          df<-(res)
          return(res)
        })
        
        
        
        t<-sink("a_file.txt")
        
        yz<-args11[11]
        
        if(yz == 1)
        {
          mx.set.seed(1000)
          model2 <- mx.mlp(data = train.x,
                           label = train.y,
                           hidden_node = hidden_node_layers ,
                           out_node = 1,
                           dropout = c(0.6),
                           optimizer = "sgd",
                           activation = Activation_f ,
                           out_activation = "rmse",
                           learning.rate = 2e-6,
                           momentum = 0.9,
                           eval.metric = mx.metric.rmse_one,
                           array.layout = "rowmajor" ,
                           num.round=10, array.batch.size=60)
          
          
          
          mx.model.save(model2, "model_save", iteration = 10)
          
          preds = as.numeric(predict(model2, test.x , array.layout = "rowmajor"))
          
        }
       
        
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
        
        xy<-args12[12]
      
        if(xy==1)
        {
          
          
          mod2<-mx.model.load("model_save", 10)
          
          
          
          preds = as.numeric(predict(mod2, test.x , array.layout = "rowmajor" ))
          
          
        }
        
      
        
        
       
        denormalised = (preds)*(max(x)-min(x))+min(x)
        
        res<-NULL
        res <-  denormalised
        res<-as.data.frame(res)
        colnames(res) <- "predicted"
        
        #g<- res
          
        res$Actual <- y
        res$Date <- Date_df$Date
        res$StoreID<-test$StoreID
        res$ProductID<-test$ProductID
        #res$Date<-test$Date
        res$Activation_f<- Activation_f
        res$Numberof_neurons<- list(hidden_node_layers)
        
        res$Numberof_hiddenlayers<- length(hidden_node_layers)
        
        res$RMSE <- rmse(actual = round(test.y, 3) , predicted = round(preds,3))
        
        #g$df<-f 
        
        res$uid<-f
        
        
        
        ###negetive values
        
       # if()
      #    res$predicted<-abs(mean(res$predicted))
          
        
        ##Outliers in Forecasted values
          
          
          if(Outliers != "null")
          {
            res$Outliers_flag<-ifelse((res$predicted > min(Outliers$Outliers) || res$predicted < max(Outliers$Outliers)),"Yes","No")
            
          }
          
          if(Outliers == "null")
          {
            res$Outliers_flag<-ifelse((res$predicted < min(temp2$Sales) || res$predicted > max(temp2$Sales)),"Yes","No")
            
          }
          
        
        
          
          
          #Snowfall_data$SnowFall[Snowfall_data$SnowFall < 200 ]
        
        #sqrt(mean((denormalised-test.y)^2))
        
        final_model<-rbind(as.data.frame(final_model), res)
        
        final_model$Date <- as.POSIXlt(final_model$Date)
        
        final_model<-final_model[order(final_model$predicted , decreasing = T), ]
        
        
      }
    }
  }
}

#f<- "re"

#final_model$UserId <- f

sqlSave(channel , final_model ,"model4" , append = TRUE)

#sqlSave(channel , gt ,"gt" , append = TRUE)

#exit (0)
