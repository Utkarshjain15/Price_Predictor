library(knitr)
library(dplyr)
library(corrplot)
library(forecast)
library(ggplot2)
library(gridExtra)
library(grid)
library(kableExtra)
library(extrafont)

Data<- read.csv("D:\\Data mining\\Real_estate.csv")
colnames(Data) <- c('NR', 'Transaction_Date', 'House_Age', 'Dist_MRT_station', 
                    'Num_of_conv_store', 'Latitude', 'Longitude','Price_Unit')


## Data Overview

Data$NR=NULL


## Summary statistics

summary_re<-psych::describe(Data)[,c(3,4,5,8,9,10,11,12)]
colnames(summary_re)<-c("Mean", "Std Dev", "Median", "Min", "Max", "Range", "Skewness", "Kurtosis")
kable(summary_re,booktabs =T)%>%kable_styling(latex_options ="striped")


## Coefficients of variation

cv<-function(data){
  sd1<-apply(data,2,sd,na.rm=TRUE)
  mean1<-apply(data,2,mean,na.rm=TRUE)
  return (coefficient_of_var<-(sd1/mean1)*100)
}
kable(cv(Data),booktabs =T)%>%kable_styling(latex_options ="striped")

## Correlations between variables

cor_matrix<-cor(Data)
corrplot(cor_matrix)


## Missing Data
kable(Data[!complete.cases(Data),],booktabs =T)%>%kable_styling(latex_options ="striped")


## Distribution of the variables

ggplot(Data,aes(Price_Unit)) +
  geom_histogram(binwidth = 3, fill = "white", colour = "darkblue")+
  xlab('')+
  ggtitle('Price per Unit') +
  theme(plot.title = element_text(size = 16,hjust = 0.5),plot.margin = unit(c(1,1,1,1),"lines"))

ggplot(Data,aes(House_Age)) +
  geom_histogram(binwidth = 3, fill = "white", colour = "darkblue")+
  xlab('')+
  ggtitle('House Age') +
  theme(plot.title = element_text(size = 16,hjust = 0.5),plot.margin = unit(c(1,1,1,1),"lines"))

ggplot(Data,aes(Dist_MRT_station)) +
  geom_histogram(binwidth = 50, fill = "white", colour = "darkblue")+
  xlab('')+
  ggtitle('Distance to the nearest MRT station') +
  theme(plot.title = element_text(size = 16,hjust = 0.5),plot.margin = unit(c(1,1,1,1),"lines"))

ggplot(Data,aes(Num_of_conv_store)) +
  geom_histogram(binwidth = 1, fill = "white", colour = "darkblue")+
  xlab('')+
  ggtitle('Number of convenience stores') +
  theme(plot.title = element_text(size = 16,hjust = 0.5),plot.margin = unit(c(1,1,1,1),"lines"))

## How chosen variables affects price per unit of housing area

My_Theme = theme(
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16))

ggplot(Data,aes(y=Price_Unit, x=Dist_MRT_station)) +
  geom_point()+
  geom_smooth(method = 'lm')+
  xlab("Distance to the nearest MRT station")+
  ylab("Price per Unit")+
  My_Theme

ggplot(Data,aes(y=Price_Unit, x=House_Age)) +
  geom_point()+
  geom_smooth(method = 'lm')+
  xlab("House Age")+
  ylab("Price per Unit")+
  My_Theme

ggplot(Data,aes(y=Price_Unit, x=House_Age^2)) +
  geom_point()+
  geom_smooth(method = 'lm')+
  xlab("House Age^2")+
  ylab("Price per Unit")+
  My_Theme

ggplot(Data,aes(y=Price_Unit, x=Num_of_conv_store)) +
  geom_point()+
  geom_smooth(method = 'lm')+
  xlab("Number of convenience stores")+
  ylab("Price per Unit")+
  My_Theme

## Outliers

remove_outliers<-function(percent, data){
  m_dist <- mahalanobis(data, colMeans(data), cov(data))
  data$m_dist <- round(m_dist, 2)
  data$m_dist[m_dist>quantile(m_dist, probs = 1-percent/100)]="Outlier"
  kable(head(data,10),booktabs =T)%>%kable_styling(latex_options ="striped")
  
  data<-data %>% 
    filter(m_dist!="Outlier")
  
  data$m_dist<-NULL
  return (data)
}
Data2<-remove_outliers(5,Data)

## Boxplots before and after removing outliers


b1<-ggplot(Data, aes(y=Price_Unit))+
  geom_boxplot(fill='#ade7ff')+
  scale_x_continuous(breaks=NULL)+
  ylab('')+
  ggtitle('Price per Unit') +
  theme(plot.title = element_text(size = 16,hjust = 0.5),plot.margin = unit(c(1,1,1,1),"lines"))

b2<-ggplot(Data2, aes(y=Price_Unit))+
  geom_boxplot(fill='#ade7ff')+
  scale_x_continuous(breaks=NULL)+
  ylab('')+
  ggtitle('Price per Unit') +
  theme(plot.title = element_text(size = 16,hjust = 0.5),plot.margin = unit(c(1,1,1,1),"lines"))

 grid.arrange(b1, b2, ncol=2)
 
b3<-ggplot(Data, aes(y=House_Age))+
  geom_boxplot(fill='#ade7ff')+
  scale_x_continuous(breaks=NULL)+
  ylab('')+
  ggtitle('House Age') +
  theme(plot.title = element_text(size = 16,hjust = 0.5),plot.margin = unit(c(1,1,1,1),"lines"))

b4<-ggplot(Data2, aes(y=House_Age))+
  geom_boxplot(fill='#ade7ff')+
  scale_x_continuous(breaks=NULL)+
  ylab('')+
  ggtitle('House Age') +
  theme(plot.title = element_text(size = 16,hjust = 0.5),plot.margin = unit(c(1,1,1,1),"lines"))
grid.arrange(b3, b4, ncol=2)

b5<-ggplot(Data, aes(y=Dist_MRT_station))+
  geom_boxplot(fill='#ade7ff')+
  scale_x_continuous(breaks=NULL)+
  ylab('')+
  ggtitle('Distance to the nearest \nMRT station') +
  theme(plot.title = element_text(size = 16,hjust = 0.5),plot.margin = unit(c(1,1,1,1),"lines"))

b6<-ggplot(Data2, aes(y=Dist_MRT_station))+
  geom_boxplot(fill='#ade7ff')+
  scale_x_continuous(breaks=NULL)+
  ylab('')+
  ggtitle('Distance to the nearest \nMRT station') +
  theme(plot.title = element_text(size = 16,hjust = 0.5),plot.margin = unit(c(1,1,1,1),"lines"))

grid.arrange(b5, b6, ncol=2)

b7<-ggplot(Data, aes(y=Num_of_conv_store))+
  geom_boxplot(fill='#ade7ff')+
  scale_x_continuous(breaks=NULL)+
  ylab('')+
  ggtitle('Number of convenience \nstores') +
  theme(plot.title = element_text(size = 16,hjust = 0.5),plot.margin = unit(c(1,1,1,1),"lines"))

b8<-ggplot(Data, aes(y=Num_of_conv_store))+
  geom_boxplot(fill='#ade7ff')+
  scale_x_continuous(breaks=NULL)+
  ylab('')+
  ggtitle('Number of convenience \nstores') +
  theme(plot.title = element_text(size = 16,hjust = 0.5),plot.margin = unit(c(1,1,1,1),"lines"))
grid.arrange(b7, b8, ncol=2)

## Division into training and test set (70/30)

set.seed(130)

test<-sample(1:nrow(Data2), round(nrow(Data2)*0.3,0), replace=FALSE)
test<-sort(test)

x<-seq(1,nrow(Data2),by=1)
x<-x[!(x %in% test)]

test<-Data2[test,]
train<-Data2[x,]

## Logistic Regression Model 

model1<-lm(Price_Unit~.-Longitude, data=train)
summary(model1)

## Building final model only with significant variables


model3<-lm(Price_Unit~Transaction_Date+House_Age+Dist_MRT_station+Num_of_conv_store+Latitude+I(House_Age^2)+I(Dist_MRT_station^2), data=train)
summary(model3)

##Model testing
test$Longitude<-NULL
test$House_Age2<-test$House_Age^2
test$Dist_MRT_station2<-test$Dist_MRT_station^2
test_price<-test$Price_Unit
test$Price_Unit<-NULL
rownames(test)<-NULL
kable(test[1:5,],booktabs =T)%>%kable_styling(latex_options ="striped")

##Pridiction


forec<-predict(model3, test)
real<-test_price
diffs<-forec-real

forecast_table<-data.frame("Real"= real,"Estimated"=forec,"Error"=diffs)
forecast_table<-forecast_table %>% arrange(Error)
rownames(forecast_table)<-NULL
kable(forecast_table[1:10,],booktabs =T)%>%kable_styling(latex_options ="striped")


ggplot(forecast_table,aes(x = Estimated , y = Real)) + 
  geom_point() +
  geom_smooth(method = "lm")



## Summary and Conclusions
summary(model3)





