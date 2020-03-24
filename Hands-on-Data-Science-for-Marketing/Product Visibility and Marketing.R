#Import library
library(readxl)
#Load data
df<-read_excel("Online Retail.xlsx", sheet = "Online Retail")
#Overview of the dataset
summary(df)
str(df)
#Boxplot of Quantity
ggplot(df, aes(x="", y=Quantity))+
  geom_boxplot(outlier.shape = NA)+
  ylim(c(-15,25))+
  ylab("Order quantity")+
  xlab("")+
  ggtitle("Quantity Distribution")+
  theme(plot.title = element_text(hjust=0.5))
#Filter out orders with negative quantity (cancel orders)
df<-df[which(df$Quantity>0),]
#Time series trends
library(lubridate)
timeSeriesNumInvoices<-df%>%
  group_by(InvoiceDate=floor_date(InvoiceDate, "month"))%>%
  summarise(NumOrders=n_distinct(InvoiceNo))
#Visualize time series
ggplot(timeSeriesNumInvoices, aes(x=InvoiceDate, y=NumOrders))+
  geom_line()+
  ylim(c(0, max(timeSeriesNumInvoices$NumOrders)+1000))+
  ylab("number of orders")+
  xlab("date")+
  ggtitle("Number of Orders over Time")+
  theme(plot.title = element_text(hjust=0.5))
summary(df[which(df$InvoiceDate>=as.Date("2011-12-01")),"InvoiceDate"])
df<-df[which(df$InvoiceDate<as.Date("2011-12-01")),]
#Quick look at the monthly revenue data
df$Sales<-df$Quantity*df$UnitPrice
timeSeriesRevenue<-df%>%
  group_by(InvoiceDate=floor_date(InvoiceDate,"month"))%>%
  summarise(Sales=sum(Sales))
#Visualize this data as a line plot
ggplot(timeSeriesRevenue, aes(x=InvoiceDate, y=Sales))+
  geom_line()+
  ylim(c(0, max(timeSeriesRevenue$Sales)+10000))+
  ylab("Sales")+
  xlab("date")+
  ggtitle("Revenue over Time")+
  theme(plot.title = element_text(hjust=0.5))
#Repeat Customers
invoiceCustomerDF<-df%>%
  group_by(InvoiceNo, InvoiceDate)%>%
  summarise(CustomerID=max(CustomerID), Sales=sum(Sales))
timeSeriesCustomerDF<-invoiceCustomerDF%>%
  group_by(InvoiceDate=floor_date(InvoiceDate, "month"), CustomerID)%>%
  summarise(Count=n_distinct(InvoiceNo), Sales=sum(Sales))
repeatCustomers<-na.omit(timeSeriesCustomerDF[which(timeSeriesCustomerDF$Count>1),])
timeSeriesRepeatCustomers<-repeatCustomers%>%
  group_by(InvoiceDate)%>%
  summarise(Count=n_distinct(CustomerID), Sales=sum(Sales))
#Unique Customers
timeSeriesUniqCustomers<-df%>%
  group_by(InvoiceDate=floor_date(InvoiceDate, "month"))%>%
  summarise(Count=n_distinct(CustomerID))
timeSeriesRepeatCustomers$Perc<-timeSeriesRepeatCustomers$Sales/timeSeriesRevenue$Sales*100
timeSeriesRepeatCustomers$Total<-timeSeriesUniqCustomers$Count
#Visualize all of this data in one chart
ggplot(timeSeriesRepeatCustomers)+
  geom_line(aes(x=InvoiceDate, y=Total), stat="identity", color="navy")+
  geom_line(aes(x=InvoiceDate,y=Count), stat="identity", color="orange")+
  geom_bar(aes(x=InvoiceDate, y=Perc*20), stat="identity", fill="gray", alpha=0.5)+
  scale_y_continuous(sec.axis = sec_axis(~./20, name="Percentage(%)"))+
  ggtitle("Number of Unique vs. Repeat & Revenue from Repeat Customers")+
  theme(plot.title = element_text(hjust=0.5))
#Trending items overtime
popularItems<-df%>%
  group_by(InvoiceDate=floor_date(InvoiceDate, "month"), StockCode)%>%
  summarise(Quantity=sum(Quantity))
top5Items<-popularItems[
  which(popularItems$InvoiceDate==as.Date("2011-11-01")),
]%>%
  arrange(desc(Quantity))%>%
  head(5)
timeSeriesTop5<-popularItems[
  which(popularItems$StockCode%in%top5Items$StockCode),
]
#To visualize the time series trends of these five products
ggplot(timeSeriesTop5, aes(x=InvoiceDate, y=Quantity, color=StockCode))+
  geom_line()+
  ylab("number pf purchases")+
  xlab("date")+
  ggtitle("Top 5 Popular Items over Time")+
  theme(plot.title = element_text(hjust=0.5))
