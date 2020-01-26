###################################
####Data Science Final Project#####
####Team A-06######################
####2019-10-13#####################

#read the data
data<-read.csv('Online Retail.csv',stringsAsFactors = FALSE,
               col.names = c("InvoiceNo","StockCode","Description","Quantity","InvoiceDate","UnitPrice","CustomerID","Country"))

#know the data
summary(data)
n_occur <- data.frame(table(data$CustomerID))
length(unique(data$CustomerID))

#remove duplicate data
data_nona<-data[!duplicated(data),]

#Removed transactions with missing Customer ID
data_nona<-data_nona[!is.na(data_nona$CustomerID),]

#Convert all the Date time data from “character” to “POSIXct” 
install.packages('anytime')
library(anytime)
data_nona$InvoiceDate<-anytime(data_nona$InvoiceDate)
class(data_nona$InvoiceDate)

#convert and edit invoiceNo into numeric
class(data_nona$InvoiceNo)
data_nona$InvoiceNo<-as.numeric(data_nona$InvoiceNo)
#check Stock codes having alphabets
# less than a percent no point going through the complications 
#delete them
class(data_nona$StockCode)
alphabet<-grep("^[[:alpha:]]*$", data_nona$StockCode)
alphabet2<-grep("C2", data_nona$StockCode)
alphabet3<-grep("BANK CHARGES", data_nona$StockCode)

data_nona<-data_nona[-alphabet,]
data_nona<-data_nona[-alphabet2,]
data_nona<-data_nona[-alphabet3,]
#nrow(alphabet)/nrow(data_nona)

#ADD A row named REVENUE
data_nona$Revenue<-data_nona$UnitPrice*data_nona$Quantity


#############################################
###Exploratory Analysis on original data#####
#########################################
library(tidyverse)
library(lubridate)

#count of customer per unique txn count
customer<-data_nona%>% 
  group_by(CustomerID)%>% 
  summarise(TransactionNo=unique(n()))
####histogram
hist(customer$TransactionNo, xlim = c(min(customer$TransactionNo),1000), breaks = 500)

ggplot(customer,aes(x=TransactionNo))+geom_histogram(binwidth = 0.5, col = "black", fill = "cornflowerblue")+
  xlab('Total Transaction for 1 year')+ylab('Count of Customers')

#Transactions (unique Invoice number) per month across the 12-month window 
data_nona$InvoiceMonth<-month(data_nona$InvoiceDate, label = FALSE)

SumQuantity<-data_nona%>% 
  group_by(InvoiceMonth)%>% 
  summarise(sum_Quantity=sum(Quantity),
            sum_transaction=length(unique(InvoiceNo)))

barplot(
  t(SumQuantity[c('sum_Quantity')]),
  beside=T,
  names.arg=SumQuantity$InvoiceMonth,
  xlab='Month',
  ylab='Total Quantity Sold',
  legend.text=FALSE,col="darkblue"
)
barplot(
  t(SumQuantity[c('sum_transaction')]),
  beside=T,
  names.arg=SumQuantity$InvoiceMonth,
  xlab='Month',
  ylab='Total Transactions',
  legend.text=FALSE,col="darkblue"
)


################################################
###transfer the data to Customer Level Data#####
################################################

library(tidyverse)

#get customer id / total transactions / total quantity
customer<-data_nona%>% 
  group_by(CustomerID)%>% 
  summarise(TotalTransaction=unique(n()),
            UnitPurchase=sum(Quantity),
            TotalRevenue=sum(Revenue),
            Recency=as.Date(as.POSIXct('2011-12-10'))-as.Date(max(InvoiceDate)))

# delete customer with unit purchase<0
customer<-customer%>% 
  filter(UnitPurchase>0)
#get total return
return<-data_nona%>% 
  filter(Quantity<0)%>% 
  group_by(CustomerID)%>% 
  summarise(UnitReturn=abs(sum(Quantity)),
            returnTransaction=unique(n()))

#join return and customer
customer<-left_join(customer,return,by='CustomerID')

# NA value in return means 0 return
customer$UnitReturn[is.na(customer$UnitReturn)]<-0
customer$returnTransaction[is.na(customer$returnTransaction)]<-0

#calculate the return percentage
customer$return_quantity_percent<-round(customer$UnitReturn/customer$UnitPurchase,4)*100
customer$return_transac_percent<-round(customer$returnTransaction/customer$TotalTransaction,4)*100


#check if the return percent is correct
sum(ifelse(customer$return_quantity_percent>100|customer$return_transac_percent>100,1,0))

#delete those customer with return quantity>purchase quantity
customer<-customer%>% 
  filter(return_quantity_percent<100,
         TotalTransaction>1)


#Average interpurchase interval - how many days until come back to make a purchase??? 
interval<-unique(data_nona[,c(7,5)])%>%
  mutate(InvoiceDate =  as.Date(as.POSIXct(InvoiceDate))) %>%
  arrange(InvoiceDate) %>%
  group_by(CustomerID) %>%
  mutate(lagDate=lag(InvoiceDate))

  interval$lagDate[is.na(interval$lagDate)] <- as.character(interval$InvoiceDate[is.na(interval$lagDate)])
  
  interval1<-interval%>%
    group_by(CustomerID) %>%
    mutate(diff_days = InvoiceDate-lagDate)%>%  
   summarise(avgInterval=round(mean(diff_days),2))
  
#join the average interval with the customer table
  customer<-left_join(customer,interval1,by='CustomerID')
  customer$avgTransactionValue<-round(customer$TotalRevenue/customer$TotalTransaction,2)
  
  write.csv(customer, 'Customer_Alex.csv')
  
  #################################################
  ######visualization using customer data##########
  #################################################
  
  #get the numeric value characteristics for all the customers
  customerdf<-data.frame(customer[,2:10])
  customerdf$Recency<-as.numeric(customerdf$Recency)
  customerdf$avgInterval<-as.numeric(customerdf$avgInterval)
  
  #corplor 
  library(corrplot)
  corrplot(cor(customerdf), type="upper", method="number", t1.cex=0.9)
  
  library(RColorBrewer)
  
  #create flags for quartile for Total Revenue 
  customer$quartile_revenue <- ntile(customer$TotalRevenue, 4)
  customer$quartile_revenue <- factor(customer$quartile_revenue)
  
  
  #Plot Average Interval and Revenue of customers
  summary(customer)
  ggplot(customer) +
    geom_histogram(aes(x = customer$avgInterval,fill=customer$quartile_revenue),
                   binwidth = 50, col="black")+
    scale_fill_brewer(palette = "PuBu")+
    theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
    labs(title="Average Interval and Revenue Rank of Customers", 
         subtitle="Revenue rank classified by quartile",
         x="Average Interval",
         y="Count of Customer",
         fill = "Revenue Quartile")
  
  #Create flags for customers that have returned vs not returned 
  customer$returns <- ifelse(customer$UnitReturn>0,"yes","no")
  customer$returns <- factor(customer$returns)
  
  
  #boxplot to see recency and returns 
  customer %>% 
    filter(is.na(returns) == FALSE) %>% 
    ggplot(aes(x=returns, y=Recency))+geom_boxplot()+
    labs(title="Recency of Customers that have Return Purchases")
  
  
##################################
#########cluster##################
##################################

  
  library(factoextra)
  library(cluster)

####################################################
########cluster using different variables######
####################################################

  #Unit purchase+Recency+TotalTransaction+avgInterval+return_quantity_percent
  
  customercluster<-customerdf[,c(1,2,4,7,9)]
  customercluster<-scale(customercluster)
  #Unit purchase+TotalTransaction+avgInterval+return_quantity_percent
  customercluster2<-customerdf[,c(1,2,7,9)]
  customercluster2<-scale(customercluster2)
  #TotalRevenue, UnitReturn, avgInterval, UnitPurchase
  customercluster3 <- customerdf %>%
    select(TotalRevenue, UnitReturn, avgInterval, UnitPurchase)
  customercluster3<-scale(customercluster3)
######################3
########optimal k######
######################

  MyKmeansFUN <- function(x,k) cluster=kmeans(x, k, iter.max=50)
  
  #silhouette
  k11<-fviz_nbclust(customercluster, FUNcluster = MyKmeansFUN, method = "silhouette")
  plot(k11)
  k12<-fviz_nbclust(customercluster2, FUNcluster = MyKmeansFUN, method = "silhouette")
  plot(k12)
  k13<-fviz_nbclust(customercluster3, FUNcluster = MyKmeansFUN, method = "silhouette")
  plot(k13)
  #wss keep decreasing
  k21<-fviz_nbclust(customercluster, FUNcluster = MyKmeansFUN, method ="wss")
  plot(k21)
  k22<-fviz_nbclust(customercluster2, FUNcluster = MyKmeansFUN, method ="wss")
  plot(k22)
  k23<-fviz_nbclust(customercluster3, FUNcluster = MyKmeansFUN, method ="wss")
  plot(k23)
  #gap_stat
  # k3<-fviz_nbclust(customercluster, FUNcluster = MyKmeansFUN, method = "gap_stat")
  # plot(k3)
  
  ######################
  ########cluster#######
  ######################
  
  set.seed(123)
  cluster<-kmeans(customercluster2,centers = 6,nstart=20)
  print(cluster)
  fviz_cluster(cluster, data=customercluster)

  clusterresult6<-cbind(customerdf,cluster$cluster)
  clusterresult3<-cbind(customerdf,cluster$cluster)
#   clusterresult3<-cbind(clusterresult3,customer[,1])
# write.csv(clusterresult,'6_purchase+TotalTransaction+avgInterval+return_quantity_percent.csv') 
# write.csv(clusterresult3,'3_purchase+TotalTransaction+avgInterval+return_quantity_percent.csv') 

#unitpurchase+TotalTransaction+avgInterval+return_quantity_percent
#clusterresult6
customerclassify<-clusterresult6[,c(1,2,3,4,7,9,10)]
colnames(customerclassify)[7] <- "label"
#delete the first 2 cluster for classification
customerclassify <- customerclassify[ which(customerclassify$label!=1
                         & customerclassify$label!=2), ]

write.csv(customerclassify,'customerclassify.csv')


customerclassify<-read.csv('customerclassify.csv')
library(tidyverse)
library(caret)
library(nnet)
install.packages('e1071')
library(e1071)

## 75% of the sample size
smp_size <- floor(0.75 * nrow(customerclassify))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(customerclassify)), size = smp_size)

train <- customerclassify[train_ind, ]
test <- customerclassify[-train_ind, ]

train$X <- NULL
test$X <- NULL
# Fit the model - Multinomial Logistic regression
# putting variables that were used to predict customer clusters while profiling 
model <- nnet::multinom(factor(label)~Recency+avgInterval+TotalRevenue, data = train)
# Summarize the model
summary(model)
# Make predictions
predicted.classes <- model %>% predict(test)
head(predicted.classes)
# Model accuracy - 95% correct classification rate
mean(predicted.classes == factor(test$label))
# Lets see where the 5% misclassfication happens
table(test$label, predicted.classes)
prop.table(table(test$label, predicted.classes),1)

# High-flyers and regular shoppers are getting misclassified 

# training random forest for multinomial classification 
install.packages("randomForest")
library(randomForest)
model.rf = randomForest(factor(label)~Recency+avgInterval+TotalRevenue, data=train, ntree=1000, mtry=3, importance=TRUE)

# random forest accuracy - 94% correct classfication
model.rf$pred.label.rf = predict(model.rf, test, type="response")
mean(model.rf$pred.label.rf == factor(test$label))

# Lets see where the 5% misclassfication happens
prop.table(table(test$label, model.rf$pred.label.rf),1)

# High-flyers and regular shoppers are getting misclassified 


#pie chart for different label customers
revenuedistribute<-customerclassify%>%
  group_by(label)%>%
  summarise(count=n(),revenue=sum(TotalRevenue))%>%
  mutate(countshare=count/sum(count)*100.0,reveshare=revenue/sum(revenue)*100.0)

revenuedistribute$name<-c('Need to win back','Sporadic visitors','High-flyers','Run-of-the-mill')
  
mycols <- c("#012169","#999999","#F0E442","#56B4E9")
ggplot(revenuedistribute, aes(x = "", y = count, fill =factor(name))) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+scale_fill_manual(name = "Name of Clusters",values = mycols)+theme_void()+
  ggtitle("Customer count per cluster")+
  theme(plot.title = element_text(hjust = 0.5))+ geom_text(aes(label = paste0(round(countshare,2), "%")), 
                                                           position = position_stack(vjust = 0.5),color = '#ffffff')
  ggplot(revenuedistribute, aes(x = "", y = revenue, fill =factor(name))) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+scale_fill_manual(name = "Name of Clusters",values = mycols)+theme_void()+
  ggtitle("Revenue distribution per cluster")+
  theme(plot.title = element_text(hjust = 0.5))+ geom_text(aes(label = paste0(round(reveshare,2), "%")), 
                                                           position = position_stack(vjust = 0.5),color = '#ffffff')

  ### Interpret Clusters - Removing the 2 smallest clusters (3 clusters remaining)
  ggplot(data=customerclassify, aes(x=avgInterval, y=log(TotalRevenue), color=factor(label)))+
    geom_point()+scale_color_discrete(name="Name of Clusters",
                                    labels=c('Need to win back','Sporadic visitors','High-flyers','Run-of-the-mill'))
  