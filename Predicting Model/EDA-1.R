### Telco project - Team 43 LETS DO THIS ###
### loading required 
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(scales)
library(corrplot)

## reading the data
TelcoChurn <- read.csv("raw_data.csv")

## looking at the data
head(TelcoChurn)
summary(TelcoChurn)
## senior citizen is coded as an integer and not a flag - lets fix that
TelcoChurn <- TelcoChurn %>%
  mutate(SeniorCitizen = ifelse(SeniorCitizen == 1, "Yes", "No")) 

## lets do the same for gender as well - new column for gender_male - Yes means male and no means female
TelcoChurn <- TelcoChurn %>%
  mutate(genderMale = ifelse(gender == 'Male', "Yes", "No")) 


##And total charges seem to be the only column with missing values - lets look into this
TelcoChurnMissing <- TelcoChurn[!complete.cases(TelcoChurn),]



## it seems total charges are missing for customers who are new - have 0 as tenure and havent churned out yet obviously 
## lets replace their total charges with their monthly charges
TelcoChurn$TotalCharges[is.na(TelcoChurn$TotalCharges)] <- TelcoChurn$MonthlyCharges[is.na(TelcoChurn$TotalCharges)]
## rechecking summary to confirm
summary(TelcoChurn)
## no missing values for TotalCharges

## custID seems to be the primary key - is custID the primary key?
length(TelcoChurn$customerID) == length(unique(TelcoChurn$customerID)) 
## returned argument is TRUE hence customerID is indeed the primary key


## lets look at what percentage of customers have already churned out
TelcoChurnBaseBar <- TelcoChurn %>% 
  group_by(Churn) %>% 
  summarise(Count = n())%>% 
  mutate(percent = prop.table(Count)*100)%>%
  ggplot(aes(reorder(Churn, percent), percent), fill = Churn)+
  geom_col(fill = c("#B5B5B5" , "#00539B"))+
  geom_text(aes(label = sprintf("%.2f%%", percent)), hjust = 0.01,vjust = -0.5, size =3)+ 
  xlab("Churn") + 
  ylab("Percentage of customers")+
  ggtitle("Distribution of Attrited VS Non Attrited customers")
TelcoChurnBaseBar
## More than 70% of the customer are still with us

## lets see if some of the continous varibles differ by churn percentage or not
## the continous variables are - monthly charges, total charges (should be correlated), and tenure
cor(TelcoChurn$TotalCharges, TelcoChurn$MonthlyCharges)

##hmm, point 0.65.. not too high.. lets park that for now
## lets look at the distribution of monthly charges

g1<-ggplot(TelcoChurn, aes(x=MonthlyCharges)) + 
  geom_histogram(aes(y=..density..), colour="white", fill="#00539B")+xlab('Monthly Charges')+ylab('Percentage')+
  ggtitle('Monthly Charges Distribution')
g1+theme(axis.title=element_text(size=20,face="bold"),
         plot.title = element_text(size=22,face="bold"),
         axis.text.x = element_text(size = 18,face="bold"),
         axis.text.y = element_text(size = 18,face="bold"))


## Most of the customer base is concentrated on the less than $30 mark per month


## it seems most of the customers lie below $30 - lets see what percentage of these are churned VS not churned

g2<-ggplot(TelcoChurn, aes(x=Churn, y=MonthlyCharges, fill=Churn)) +
  geom_boxplot(colour = "black")+scale_fill_manual(values=c("#00539B","#B5B5B5"))+
  ggtitle('Monthly Charges By Churn')
g2+theme(axis.title=element_text(size=20,face="bold"),
         plot.title = element_text(size=22,face="bold"),
         axis.text.x = element_text(size = 18,face="bold"),
         axis.text.y = element_text(size = 18,face="bold"))
## interesting, most of the customers who are still with us pay around $60-65 per month 
## whereas people who have left paid close to $80 per month

## lets look at total charges - might not be helpful as it depends on tenure but still
ggplot(data = TelcoChurn, aes(TotalCharges))+geom_freqpoly(binwidth = 10, size = 0.5)


## looking at another visual for Total Charges VS Churn

g3<-ggplot(TelcoChurn, aes(x=Churn, y=TotalCharges, fill=Churn)) +
  geom_boxplot(colour = "black")+scale_fill_manual(values=c("#00539B","#B5B5B5"))+
  ggtitle('Total Charges By Churn')
g3+theme(axis.title=element_text(size=22,face="bold"),
         plot.title = element_text(size=22,face="bold"),
         axis.text.x = element_text(size = 18,face="bold"),
         axis.text.y = element_text(size = 18,face="bold"))

## and the total charges distribution 
g4<-ggplot(TelcoChurn, aes(x=TotalCharges)) + 
  geom_histogram(aes(y=..density..), colour="white", fill="#00539B")+xlab('Total Charges')+ylab('Percentage')+
  ggtitle('Total Charges Distribution')+scale_x_continuous(breaks=seq(0, 8700, 1000))
g4+theme(axis.title=element_text(size=20,face="bold"),
         plot.title = element_text(size=22,face="bold"),
         axis.text.x = element_text(size = 18,face="bold"),
         axis.text.y = element_text(size = 18,face="bold"))

##let's confirm the correlation between the total charges and the tenure
cor(TelcoChurn$TotalCharges, TelcoChurn$tenure)
## voila - yes they are highly correlated - more than 0.8 - lets keep that in mind going forward
## lets collate all the correlation inferences we have drawn so far 
TelcoChurnCorr <- TelcoChurn[, c(6,19,20)]
TelcoChurnCorr <- cor(TelcoChurnCorr)

corrplot(TelcoChurnCorr, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45,tl.cex=1.5)

## shows that the highest correlation is between Tenure and Total charges followed by Monthly and Total charges
## no correlation exists between monthly charges and tenure 
##test the equation Total Charges =Tenure * Monthly Charges. 
TelcoChurn1<-TelcoChurn%>% mutate(cal_ttlcharges=tenure*MonthlyCharges)
g5<-ggplot(TelcoChurn1, aes(x=cal_ttlcharges, y=TotalCharges,)) +xlab("Tenure * Monthly Charges")+
  geom_point(colour = "black")+ggtitle(' Equation Confirmation')
g5+theme(axis.title=element_text(size=20,face="bold"),
         plot.title = element_text(size=22,face="bold"),
         axis.text.x = element_text(size = 18,face="bold"),
         axis.text.y = element_text(size = 18,face="bold"))

## lets go into tenure 
## Tenure distribution by Churn or not
g6<-ggplot(data = TelcoChurn, aes(tenure, colour = Churn))+
  geom_freqpoly(binwidth = 5, size = 1)+scale_color_manual(values=c("#00539B","#B5B5B5"))+ggtitle('Tenure Distribution')
g6+theme(axis.title=element_text(size=20,face="bold"),
         plot.title = element_text(size=22,face="bold"),
         axis.text.x = element_text(size = 18,face="bold"),
         axis.text.y = element_text(size = 18,face="bold"))

g7<-ggplot(TelcoChurn, aes(x=Churn, y=tenure, fill=Churn)) +
  geom_boxplot(colour = "black")+scale_fill_manual(values=c("#00539B","#B5B5B5"))+ylab("Tenure in Months")
g7+theme(axis.title=element_text(size=20,face="bold"),
         plot.title = element_text(size=22,face="bold"),
         axis.text.x = element_text(size = 18,face="bold"),
         axis.text.y = element_text(size = 18,face="bold"))
## lets look at internet service, contract type and payment method one by one
g13<-ggplot(TelcoChurn)+ 
  geom_bar(aes(x = InternetService, fill = Churn), stat = "count", width = 0.5)+
  theme_minimal()+scale_fill_manual(values=c("#00539B", "#B5B5B5"))+ylab("Count of customers")
g13+theme(axis.title=element_text(size=20,face="bold"),
          plot.title = element_text(size=22,face="bold"),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 18,face="bold"))

## Clearly fiber optic customers are at the most risk of churning out

## lets look at contract type and payment method
g8<-ggplot(TelcoChurn)+ 
  geom_bar(aes(x = PaymentMethod, fill = Churn), stat = "count", width = 0.8)+
  theme_minimal()+scale_fill_manual(values=c("#00539B", "#B5B5B5"))+ylab("Count of customers")
g8+theme(axis.title=element_text(size=20,face="bold"),
         plot.title = element_text(size=22,face="bold"),
         axis.text.x = element_text(size = 15,vjust = 0.5, hjust = 0.3,angle=10),
         axis.text.y = element_text(size = 18,face="bold"))

### Electronic check is the most popular one amongst customers and is also the most risky
g9<-ggplot(TelcoChurn)+ 
  geom_bar(aes(x = Contract, fill = Churn), stat = "count", width = 0.8)+
  theme_minimal()+scale_fill_manual(values=c("#00539B", "#B5B5B5"))+ylab("Count of customers")
g9+theme(axis.title=element_text(size=20,face="bold"),
         plot.title = element_text(size=22,face="bold"),
         axis.text.x = element_text(size = 15,face="bold"),
         axis.text.y = element_text(size = 18,face="bold"))

## do Fiber optics cost more? is that why people are leaving? 
g10<-ggplot(TelcoChurn, aes(x=InternetService, y=MonthlyCharges, fill=InternetService)) +
  geom_boxplot(colour = "black")+scale_fill_manual(values=c("#988675","#B5B5B5","#666666"))
g10+theme(axis.title=element_text(size=20,face="bold"),
          plot.title = element_text(size=22,face="bold"),
          axis.text.x = element_text(size = 15,face="bold"),
          axis.text.y = element_text(size = 18,face="bold"))
## Electronic check customers seem to pay a little more on an average it seems
g11<-ggplot(TelcoChurn, aes(x=PaymentMethod, y=MonthlyCharges, fill=PaymentMethod)) +
  geom_boxplot(colour = "black")+scale_fill_manual(values=c("#988675","#B5B5B5","#666666","#E5E5E5"))
g11+theme(axis.title=element_text(size=20,face="bold"),
          plot.title = element_text(size=22,face="bold"),
          axis.text.x = element_text(size = 15,face="bold",vjust = 0.5, hjust = 0.3,angle=10),
          axis.text.y = element_text(size = 18,face="bold"))

## Does contract type have anything to do with monthly charges? - Nothing here
g12<-ggplot(TelcoChurn, aes(x=Contract, y=MonthlyCharges, fill=Contract)) +
  geom_boxplot(colour = "black")+scale_fill_manual(values=c("#988675","#B5B5B5","#666666"))
g12+theme(axis.title=element_text(size=20,face="bold"),
          plot.title = element_text(size=22,face="bold"),
          axis.text.x = element_text(size = 15,face="bold",),
          axis.text.y = element_text(size = 18,face="bold"))
## taking columns who yes no flags
TelcoChurnPlots2 <- gather(TelcoChurn, columns, value, -PhoneService,-gender,-InternetService,-Contract, -PaymentMethod,-customerID, -TotalCharges, -Churn, -MonthlyCharges, -tenure)

## replace No Internet Service with NO and No Phone service with No

TelcoChurn <- data.frame(lapply(TelcoChurn, function(x) {
  gsub("No internet service", "No", x)}))

TelcoChurn <- data.frame(lapply(TelcoChurn, function(x) {
  gsub("No phone service", "No", x)}))

g14<-ggplot(TelcoChurnPlots2)+ 
  geom_bar(aes(x = value, fill = Churn), stat = "count", width = 0.5)+
  facet_wrap(~ columns) +
  theme_minimal()+coord_flip()+scale_fill_manual(values=c("#00539B", "#B5B5B5"))+
  ylab("Count of customers (normalized)") + 
  xlab("Categorical Variables")+
  ggtitle("Distribution of Attrited VS Non Attrited customers across categorical flags")
g14+theme(axis.title=element_text(size=16),
         plot.title = element_text(size=14),
         axis.text.x = element_text(size = 12,vjust = 0.5, hjust = 0.3,angle=35),
         axis.text.y = element_text(size = 12))
