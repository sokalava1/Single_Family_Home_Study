library(tidyverse)
library(dplyr)
library(lubridate)
library(scales)
library(mondate)
library(gganimate)
library(transformr)
library(gifski)
library(ggExtra)
library(RColorBrewer)
#loading file and removing duplicate parcels that have additional buildings on the parcel, sales and number of sales is preserved.
load("~/Documents/R/Sales/Project/data/qs.rda")
levels(as.factor(qs$DOR_CODE))
qs<-qs%>%arrange(BLDG_NUM)%>%distinct(Sale_Date,Parcel, .keep_all = TRUE)
str(qs)
class(qs)
max(qs$Sale_Date)
#Filtering for median price calculations
qs%>%filter(Sales_Price<=285000&`Sales Ratio`>300)%>%mutate(Year=year(Sale_Date))%>%ggplot(aes(Sale_Date, Sales_Price))+geom_point(color="dark green")+labs(title="Sales Price Below $285,000 with Sales Ratio over 300")
qs%>%filter(Sales_Price<=285000&`Sales Ratio`>300)%>%mutate(Year=year(Sale_Date))%>%group_by(Year)%>%summarize(n())
qs%>%filter(Sales_Price<=25000&`Sales Ratio`>300)%>%print()

f<-qs%>%filter(Sales_Price<=25000&`Sales Ratio`>300)%>%group_by(year(Sale_Date))%>%summarize(n(), sum(Sales_Price))
f
f/nrow(qs)*100



# Plot 1: "Median Sales by City 2015 to 2020"
qs%>%filter(Sale_Date<"2020-01-01", !c(Sales_Price<25000&`Sales Ratio`>300))%>%mutate(Year=year(Sale_Date))%>%group_by(Year, Jurisdiction)%>%summarize(Median_Sales=median(Sales_Price))%>%ggplot(aes( Year, Median_Sales, color=Jurisdiction))+geom_line()+geom_point()+labs(title = 'Median Sales Price of Single Family Residential Real Estate', subtitle= 'By Jurisdiction from 2015 to 2019', x = 'Year', y = 'Median Sales Price')+scale_y_continuous(labels=dollar,breaks = scales::pretty_breaks(n = 8))+theme(axis.text.x = element_text(angle=45, hjust=1, size=10),plot.title = element_text(size=16),plot.subtitle = element_text(size=15),legend.title=element_text(size=15),legend.text = element_text(size=14),axis.title = element_text(size=15), axis.text.y = element_text(size=10))
#Q1
qs%>%filter((Sale_Date>="2019-01-01"&Sale_Date<"2019-03-31")|(Sale_Date>="2020-01-01"&Sale_Date<"2020-03-31"), !c(Sales_Price<25000&`Sales Ratio`>300))%>%mutate(Year=year(Sale_Date))%>% group_by(Jurisdiction, Year)%>%summarize(Median_Sales=median(Sales_Price),Number_of_Sales=n(), Volume_of_Sales=sum(Sales_Price))%>%view()
qs%>%filter((Sale_Date>="2019-01-01"&Sale_Date<"2019-03-31")|(Sale_Date>="2020-01-01"&Sale_Date<"2020-03-31"), !c(Sales_Price<25000&`Sales Ratio`>300))%>%mutate(Year=year(Sale_Date))%>%group_by(Year)%>%summarize(Median_Sales=median(Sales_Price),n(), sum(Sales_Price))

#list Median Sales
qs%>%filter(Sale_Date<"2020-01-01",!c(Sales_Price<25000&`Sales Ratio`>300))%>%mutate(Year=year(Sale_Date))%>%group_by(Year, Jurisdiction)%>%summarize(Median_Sales=median(Sales_Price))%>%arrange(Median_Sales)%>%view()

#Zip code data
qs%>%filter(Sale_Date>"2019-01-01", !c(Sales_Price<25000&`Sales Ratio`>300), Sales_Price<7500000,Zip_Code%in%c("32809","32819"))%>%ggplot(aes(Sale_Date, Sales_Price, color=Zip_Code))+geom_point()+scale_y_continuous(labels=dollar,breaks = scales::pretty_breaks(n = 8))+facet_grid(Zip_Code~.)+labs(title = ' Sales Price of Single Family Residential Real Estate', subtitle= 'Zip Code', x = 'Date', y = 'Median Sales Price')
  
scale_y_continuous(labels=dollar,breaks = scales::pretty_breaks(n = 8))+theme(axis.text.x = element_text(angle=45, hjust=1))


#Plot 2: "Median Sales Orange County 2015 to 2020"
qs%>%filter(Sale_Date<"2020-01-01", !c(Sales_Price<25000&`Sales Ratio`>300))%>%group_by(Date=year(Sale_Date))%>%summarize(Median_Sales=median(Sales_Price))%>%
  ggplot(aes(Date, Median_Sales))+geom_line(color="blue")+geom_point(color="green", size=5)+labs(title = 'Median Sales Price of Single Family Residential Real Estate', subtitle= 'Orange County from 2015 to 2019', x = 'Year', y = 'Median Sales Price')+
scale_y_continuous(labels=dollar,breaks = scales::pretty_breaks(n = 8))+theme(axis.text.x = element_text(angle=45, hjust=1, size=13),plot.title = element_text(size=16),plot.subtitle = element_text(size=15),legend.text = element_text(size=14),axis.title = element_text(size=15), axis.text.y = element_text(size=13))

#calculate Counties median!
qs%>%filter(Sale_Date<"2020-01-01",!c(Sales_Price<25000&`Sales Ratio`>300))%>%mutate(Year=year(Sale_Date))%>%group_by(Year)%>%summarize(Median=median(Sales_Price))%>%as.data.frame()%>%view()


# Plot 3: "Median Sales by City 2015 to 2020 by Jurisdiction" GOOD
qs%>%filter(Sale_Date<"2020-01-01", !c(Sales_Price<25000&`Sales Ratio`>300))%>%mutate(Year=year(Sale_Date))%>%group_by(Year, Jurisdiction)%>%summarize(Median_Sales=median(Sales_Price))%>%
  ggplot(aes( Year, Median_Sales, color=Jurisdiction))+geom_line()+geom_point()+facet_wrap(~Jurisdiction)+labs(title = 'Median Sales Price of Single Family Residential Real Estate', subtitle= 'By Jurisdiction from 2015 to 2019', x = 'Year', y = 'Median Sales Price')+
scale_y_continuous(labels=dollar,breaks = scales::pretty_breaks(n = 8))+theme(axis.text.x = element_text(angle=45, hjust=1, size=10),plot.title = element_text(size=16),plot.subtitle = element_text(size=15),legend.text = element_text(size=14),axis.title = element_text(size=15), axis.text.y = element_text(size=10))

# Plot 4: "Median Quarterly Sales Orange County 2015 through 2020 GOOD
qs%>%filter(Sale_Date<"2020-01-01", !c(Sales_Price<25000&`Sales Ratio`>300))%>%mutate(Y=year(Sale_Date),Q=quarter(Sale_Date))%>%group_by(Y, Q)%>%summarize(Median_Sales=median(Sales_Price))%>%ggplot(aes(Y,Median_Sales, color=Q))+geom_line()+geom_point()+facet_wrap(~Q)+
  labs(title = 'Median Quarterly Sales of Single Family Residential Real Estate', subtitle= 'Orange County from 2015 to 2019', x = 'Year', y = 'Median Sales Price')+scale_y_continuous(labels=dollar,breaks = scales::pretty_breaks(n = 8))+theme(axis.text.x = element_text(angle=45, hjust=1, size=10),plot.title = element_text(size=16),plot.subtitle = element_text(size=15),legend.text = element_text(size=14),axis.title = element_text(size=15), axis.text.y = element_text(size=10))
#list median quarter
qs%>%filter( !c(Sales_Price<25000&`Sales Ratio`>300))%>%mutate(Y=year(Sale_Date),Q=quarter(Sale_Date))%>%group_by(Y, Q)%>%summarize(N=n(),Median_Sales=median(Sales_Price))%>%view()
#Plot 5: Number of Sales per Quarter GOOD
qs%>%filter(Sale_Date<"2020-01-01",!c(Sales_Price<25000&`Sales Ratio`>300))%>%mutate(Y=year(Sale_Date),Q=quarter(Sale_Date))%>%ggplot(aes(Q, fill=Y))+geom_bar()+facet_wrap(~Y)+
  labs(title = 'Number of Quarterly Sales of Single Family Residential Real Estate', subtitle= 'Orange County from 2015 to 2019', x = 'Quarter', y = 'Count')+theme(axis.text.x = element_text(angle=45, hjust=1))
#list by quarter
qs%>%filter(!c(Sales_Price<25000&`Sales Ratio`>300))%>%mutate(Y=year(Sale_Date),Q=quarter(Sale_Date))%>%group_by(Y,Q)%>%summarize(n(), sum(Sales_Price))%>%view()

#list by year sales volume by Jurisdiction
qs%>%filter(Sale_Date<"2020-01-01", !c(Sales_Price<25000&`Sales Ratio`>300))%>%mutate(Y=year(Sale_Date))%>%group_by(Y, Jurisdiction)%>%summarise(sum(Sales_Price))%>%view()



#number of sales and volume by jurisdiction 2019
qs%>%filter(Sale_Date<"2020-01-01",Sale_Date>="2019-01-01", !c(Sales_Price<25000&`Sales Ratio`>300))%>%mutate(Y=year(Sale_Date))%>%group_by(Jurisdiction)%>%summarize(Value=sum(Sales_Price), Number=n())%>%view()

#number of sales by jurisdiction 2020 Q1
qs%>%filter((Sale_Date<="2020-03-31"&Sale_Date>="2020-01-01")|(Sale_Date<="2019-03-31"&Sale_Date>="2019-01-01"), !c(Sales_Price<25000&`Sales Ratio`>300))%>%mutate(Y=year(Sale_Date))%>%group_by(Y,Jurisdiction)%>%summarize(Value=sum(Sales_Price), Number=n())%>%view()


#number of sales and volume by year
qs%>%filter(Sale_Date<"2020-01-01", !c(Sales_Price<25000&`Sales Ratio`>300))%>%mutate(Y=year(Sale_Date))%>%group_by(Y)%>%summarize(Value=sum(Sales_Price), Number=n())%>%view()
#in 14
qs14<-read_xlsx("~/Documents/R/Sales/Project/Rawdata/qs14.xlsx")
median(qs14$`Adj Sale Amt`)
qs14%>%filter(!c(`Adj Sale Amt`<25000 &`Sales Ratio`>300))%>%mutate(Y=year(mdy(`Sale Date`)))%>%group_by(Y)%>%summarize(Value=sum(`Adj Sale Amt`), Number=n())%>%view()

#Plot 6: Number of Sales by JURISDICTIONS 2015 to 2019 GOOD


qs%>%filter(Sale_Date<"2020-01-01",!c(Sales_Price<25000&`Sales Ratio`>300))%>%mutate(Y=year(Sale_Date))%>%ggplot(aes(Y, fill=Jurisdiction))+geom_bar()+scale_y_continuous(breaks=scales::pretty_breaks(n=10))+
  labs(title = 'Number of Sales by Jurisdiction of Single Family Residential Real Estate', subtitle= 'Orange County from 2015 to 2019', x = 'Year', y = 'Count')+theme(axis.text.x = element_text(angle=45, hjust=1))


#Plot7: Number of Sales by Zip Code 2015 to 2019 NOT USED

qs%>%filter(Sale_Date<"2020-01-01",!c(Sales_Price<25000&`Sales Ratio`>300))%>%mutate(Y=year(Sale_Date))%>%ggplot(aes(Y, fill=Zip_Code))+geom_bar()+scale_y_continuous(breaks=scales::pretty_breaks(n=10))+
  labs(title = 'Number of Sales by ZipCode of Single Family Residential Real Estate', subtitle= 'Orange County from 2015 to 2019', x = 'Year', y = 'Count')+theme(axis.text.x = element_text(angle=45, hjust=1))


#Plot 8: 2019 Sales distrubution in Orange County by Jurisdiction small labels OK
qs%>%group_by(Jurisdiction, Sales_Price, Sale_Date)%>%filter(Sales_Price<=750000&Sales_Price>20000&Sale_Date>="2019-01-01"&Sale_Date<"2020-01-01", !c(Sales_Price<25000&`Sales Ratio`>300))%>%ggplot(aes(Sales_Price, fill=Jurisdiction))+geom_histogram(binwidth=7000)+scale_colour_brewer(palette=10, aesthetics = "colour")+scale_x_continuous(labels=dollar)+theme(axis.text.x = element_text(angle=45, hjust=1))+
labs(title = '2019 Distribution of Sales Prices of Single Family Residential Real Estate', subtitle= 'By Jurisdiction in Orange County', x = 'Price', y = 'Count')

#Plot 9: Sales distrubution in Orange County 2015 to 2019 GOOD
qs%>%filter(Sales_Price<=750000&Sales_Price>20000&Sale_Date>="2015-01-01", !c(Sales_Price<25000&`Sales Ratio`>300))%>%ggplot(aes(Sales_Price, fill=year(Sale_Date)))+geom_histogram()+scale_colour_brewer(palette=10, aesthetics = "colour")+facet_wrap(~year(Sale_Date))+scale_x_continuous(labels=dollar)+theme(axis.text.x = element_text(angle=45, hjust=1))+
  labs(title = 'Sales Distribution of Single Family Residential Real Estate', subtitle= 'By Year in Orange County', x = 'Price', y = 'Count')+theme(legend.title = element_blank(),axis.text.x = element_text(angle=55, hjust=1, size=10),plot.title = element_text(size=16),plot.subtitle = element_text(size=15),legend.text = element_text(size=14),axis.title = element_text(size=15), axis.text.y = element_text(size=13))

# Plot 10: 2019 Sales Distribution Larger lables GOOD
qs%>%mutate(Sale_Date=round_date(Sale_Date, "month"))%>%filter(Sales_Price<=750000 &Sales_Price>20000 &Sale_Date>="2019-01-01"&Sale_Date<"2020-01-01", !c(Sales_Price<25000&`Sales Ratio`>300))%>%
  ggplot(aes(Sales_Price, fill=Jurisdiction))+geom_histogram(binwidth=7000)+scale_x_continuous(labels=dollar, breaks = scales::pretty_breaks(n = 10))+
  theme(axis.text.x = element_text(angle=45, hjust=1, size=15), plot.title = element_text(size=20),plot.subtitle = element_text(size=18), legend.title = element_text(size=18), legend.text = element_text(size=14),axis.title = element_text(size=18), axis.text.y = element_text(size=15))+
  labs(title = 'Single Family Real Estate Sales Distribution', subtitle= 'Year: 2019', x = 'Sale Price', y = 'Count')

#Plot 11: 2015 to 2020 Sales Distribution with Jurisdiction GOOD
qs%>%mutate(Sale_Date=round_date(Sale_Date, "month"))%>%filter(Sales_Price<=750000, !c(Sales_Price<25000&`Sales Ratio`>300), !c(Sales_Price<25000&`Sales Ratio`>300))%>%
  ggplot(aes(Sales_Price, fill=Jurisdiction))+geom_histogram(binwidth=7000)+scale_x_continuous(labels=dollar, breaks = scales::pretty_breaks(n = 10))+
  theme(axis.text.x = element_text(angle=45, hjust=1, size=8), plot.title = element_text(size=20),plot.subtitle = element_text(size=16), legend.title = element_text(size=18), legend.text = element_text(size=14),axis.title = element_text(size=18), axis.text.y = element_text(size=10))+
  labs(title = 'Single Family Real Estate Sales Distribution', subtitle= 'Year: January 2015 through March 2020', x = 'Sale Price', y = 'Count')+facet_wrap(~year(Sale_Date))
#Plot 12: Sales trendline 2015 to 2019
qs%>%filter(Sale_Date>="2015-01-01", !c(Sales_Price<25000&`Sales Ratio`>300))%>%ggplot()+geom_smooth(aes(Sale_Date,Sales_Price),color="blue", span=0.15, method.args=list(degree=2))+ggtitle("")+scale_y_continuous(labels=dollar)+theme(axis.text.x = element_text(angle=90, hjust=1))+scale_y_continuous(labels=dollar, breaks = scales::pretty_breaks(n = 10))+
  theme(axis.text.x = element_text(angle=45, hjust=1, size=10), plot.title = element_text(size=20),plot.subtitle = element_text(size=16), legend.title = element_text(size=18), legend.text = element_text(size=14),axis.title = element_text(size=18), axis.text.y = element_text(size=10))+
  labs(title = 'Single Family Real Estate Sales Trendline', subtitle= 'Year: January 2015 through March 2020', x = 'Year', y = 'Sales Price')
#Plot 13: Heted area per square feet distribution
str(qs)

qs%>%filter(!c(Sales_Price<25000&`Sales Ratio`>300))%>%mutate(HEATED_AREA=round(HEATED_AREA, digits=-2),Year=year(Sale_Date))%>%filter(Sales_Price<=1000000 &Sales_Price>20000, HEATED_AREA<8500,Year%in%c(2019))%>%
  ggplot(aes(Jurisdiction,HEATED_AREA, fill=Year))+geom_boxplot()+
  theme(legend.position = "none", axis.text.x = element_text(angle=45, hjust=1, size=15), plot.title = element_text(size=20),plot.subtitle = element_text(size=18), legend.title = element_text(size=0), legend.text = element_text(size=0),axis.title = element_text(size=18), axis.text.y = element_text(size=15))+
  labs(title = 'Single Family Real Estate Square Footage Distribution', subtitle= 'Year: 2019', x = 'Jurisdiction', y = 'SF')

