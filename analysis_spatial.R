library(leaflet)
library(tidyverse)
library(dplyr)
library(lubridate)
library(scales)
library(maptools)
library(mondate)
library(transformr)
library(RColorBrewer) ##
library(readxl)
library(shiny)
library(sf)
library(raster)
library(spData)
library(htmlwidgets)
library(htmltools)
library(leaflet.extras)

#https://cfss.uchicago.edu/notes/leaflet/
#load the data file with sales codes
load("~/Documents/R/Sales/Project/data/qs.rda")
qs<-qs%>%arrange(BLDG_NUM)%>%distinct(Sale_Date,Parcel, .keep_all = TRUE)
year(qs$Sale_Date)
head(qs)

#load county boundaries file to pronounce the outline better
county <-subset(getData("GADM", country="usa", level=2), NAME_1=="Florida"&NAME_2=="Orange")
county

library(rgeos)
library(rgdal)
library(ggalt)
class(county)

zipsocpa<-st_read("~/Documents/R/Sales/Project/rawdata/Shapefiles/ZIP_CODES.shp")
summary(zipsocpa)
zipstr<-st_transform(zipsocpa,"+proj=longlat +ellps=WGS84 +datum=WGS84")
zipstr

neighocpa<-st_read("~/Documents/R/Sales/Project/rawdata/Shapefiles/NBHD.shp")
summary(neighocpa)
neighocpatr<-st_transform(neighocpa,"+proj=longlat +ellps=WGS84 +datum=WGS84")
neighocpatr

#this code is just in case you need to pull just the cooordinates
(st_coordinates(zipstr[23,]))

#assign color category to be later used within leaflet
pal<-colorNumeric(palette="Spectral", domain=qs$Sales_Price, reverse=T)
pal1<-colorFactor(palette="Spectral", domain=parceltr$Ratio, reverse=T)
pal2<-colorFactor(palette="Paired", domain=qs$Jurisdiction, reverse=T)


#creating  map with sales 2019 Sales by Jurisdiction GOOD
QSJD<-qs%>%filter(Sale_Date>="2019-01-01"&Sale_Date<"2020-01-01",!c(Sales_Price<25000&`Sales Ratio`>300))
head(QSJD)
labels <- sprintf("<strong>%s</strong><br/>Sale Price: $%g ", QSJD$Jurisdiction, QSJD$Sales_Price) %>% lapply(htmltools::HTML)

mapq1<-QSJD%>%leaflet()%>%addProviderTiles(providers$Esri.WorldGrayCanvas)%>%
addCircles(radius=50, color=pal2(QSJD$Jurisdiction), weight = 3, stroke = FALSE, fillOpacity = 0.8)%>%addCircleMarkers(color=pal2(QSJD$Jurisdiction),clusterOptions = markerClusterOptions(),label=~labels, labelOptions = labelOptions( opacity=0.9, color="grey",textsize = "10px", direction="right"))%>%addPolygons(data=neighocpatr, fillColor="white",fillOpacity = 0,color="lightgrey",  weight=0.1, label=~NBHD_DESC,highlightOptions = highlightOptions(weight=5,color="#666",bringToFront = TRUE))%>%addPolygons(data = county,fillOpacity = 0.1, color = "grey", stroke = TRUE, weight = 5,layerId = county@data$NAME_2)%>%leaflet::addLegend("topright", pal=pal2, values=QSJD$Jurisdiction, title = "2019 Sales by Jurisdiction")
mapq1

#map 2019 sales by sales price under 1 mil GOOD

QS1M<-qs%>%filter(Sale_Date>="2019-01-01"&Sale_Date<"2020-01-01",!c(Sales_Price<25000&`Sales Ratio`>300))
mybins<-c(0,100000,200000,300000,400000,500000,600000,700000,800000,900000,1000000,Inf)
pal3<-colorBin(palette="Spectral",domain=QS1M$Sales_Price, bins=mybins,reverse=T)
mapq2<-QS1M%>%leaflet()%>%addProviderTiles(providers$Esri.WorldGrayCanvas)%>%addCircles(radius=50, color=pal3(QS1M$Sales_Price), weight = 3, stroke = FALSE, fillOpacity = 0.8)%>%addCircleMarkers(color=pal3(QS1M$Sales_Price),clusterOptions = markerClusterOptions())%>%addPolygons(data = county,fillOpacity = 0.1, color = "grey", stroke = TRUE, weight = 5,layerId = county@data$NAME_2)%>%
  addPolygons(data=neighocpatr, fillColor="white",fillOpacity = 0,color="lightgrey",  weight=0.1, label=~NBHD_DESC,highlightOptions = highlightOptions(weight=5,color="#666",bringToFront = TRUE))%>%leaflet::addLegend("topright", pal=pal3, values=QS1M$Sales_Price, title = "2019 Sales by Sale Price")
mapq2

#map 2020 Q1 sales

QS1M<-qs%>%filter(Sale_Date>="2020-01-01"&Sale_Date<="2020-03-31",!c(Sales_Price<25000&`Sales Ratio`>300))
mybins<-c(0,100000,200000,300000,400000,500000,600000,700000,800000,900000,1000000,Inf)
pal3<-colorBin(palette="Spectral",domain=QS1M$Sales_Price, bins=mybins,reverse=T)
mapq2<-QS1M%>%leaflet()%>%addProviderTiles(providers$Esri.WorldGrayCanvas)%>%addCircles(radius=25, color=pal3(QS1M$Sales_Price), weight = 2, stroke = TRUE, fillOpacity = 0.8)%>%addCircleMarkers(color=pal3(QS1M$Sales_Price),radius=2,clusterOptions = markerClusterOptions())%>%addPolygons(data = county,fillOpacity = 0.1, color = "grey", stroke = TRUE, weight = 5,layerId = county@data$NAME_2)%>%
  addPolygons(data=neighocpatr, fillColor="white",fillOpacity = 0,color="lightgrey",  weight=0.1, label=~NBHD_DESC,highlightOptions = highlightOptions(weight=5,color="#666",bringToFront = TRUE))%>%leaflet::addLegend("topright", pal=pal3, values=QS1M$Sales_Price, title = "2020 Q1 Sales by Sale Price")
mapq2




#zip codes
#2019
QS19<-qs%>%filter(Sale_Date>="2019-01-01"&Sale_Date<"2020-01-01",!c(Sales_Price<25000&`Sales Ratio`>300))%>%group_by(Zip_Code)%>%summarize(N= n(),LONGITUDE=median(LONGITUDE),LATITUDE=median(LATITUDE))%>%plyr::rename(c("Zip_Code"= "ZIP_CODE"))

zip19<-zipstr%>%left_join(QS19, by="ZIP_CODE")
zip19<-zip19%>%mutate(N=ifelse(is.na(N), 0, N))
zip19
mybins<-c(0,200,400,600,800,1000,1200,Inf)
pal4<-colorBin(palette="Spectral", domain=QS19$N,bins=mybins, reverse=T)
pal5<-colorBin(palette="Spectral", domain=zip19$N, bins=mybins, reverse=T)
mapq1<-QS19%>%leaflet()%>%addProviderTiles(providers$Esri.WorldGrayCanvas)%>%
  addCircles(radius=~N*2, color="white", weight = 3, stroke = FALSE, fillOpacity = 2,label = ~N,labelOptions = labelOptions(noHide = T, opacity=1,weight=3, direction="center", textOnly=TRUE, style = list("color" = "black","font-style" = "bold", "font-size"="12px")))%>%addPolygons(data = county,fillOpacity = 0.1, color = "grey", stroke = TRUE, weight = 5,  layerId = county@data$NAME_2)%>%
  addPolygons(data=zip19, fillColor=pal5(zip19$N),fillOpacity = 0.4,stroke=TRUE, color="lightgrey", weight=3, label = ~ZIP_CODE, highlightOptions = highlightOptions(weight=5,color="#666",bringToFront = TRUE))%>%leaflet::addLegend("bottomright", pal=pal4, values=QS19$N, title = "2019 Number of Sales by Zip Code")
mapq1
111/(548+531)
#removed%>%setView(lng= -81.265,lat = 28.57, zoom=10)
#2018
QS18<-qs%>%filter(Sale_Date>="2018-01-01"&Sale_Date<"2019-01-01",!c(Sales_Price<25000&`Sales Ratio`>300))%>%group_by(Zip_Code)%>%summarize(N= n(),LONGITUDE=median(LONGITUDE),LATITUDE=median(LATITUDE))%>%plyr::rename(c("Zip_Code"= "ZIP_CODE"))

zip18<-zipstr%>%left_join(QS18, by="ZIP_CODE")
zip18<-zip18%>%mutate(N=ifelse(is.na(N), 0, N))
zip18
mybins<-c(0,200,400,600,800,1000,1200,Inf)
pal4<-colorBin(palette="Spectral", domain=QS18$N,bins=mybins, reverse=T)
pal5<-colorBin(palette="Spectral", domain=zip18$N, bins=mybins, reverse=T)
mapq1<-QS18%>%leaflet()%>%addProviderTiles(providers$Esri.WorldGrayCanvas)%>%
  addCircles(radius=~N*2, color="white", weight = 3, stroke = FALSE, fillOpacity = 2,label = ~N,labelOptions = labelOptions(noHide = T, opacity=1,weight=3, direction="center", textOnly=TRUE, style = list("color" = "black","font-style" = "bold", "font-size"="12px")))%>%addPolygons(data = county,fillOpacity = 0.1, color = "grey", stroke = TRUE, weight = 5,  layerId = county@data$NAME_2)%>%
  addPolygons(data=zip18, fillColor=pal5(zip18$N),fillOpacity = 0.4,stroke=TRUE, color="lightgrey", weight=3, label = ~ZIP_CODE, highlightOptions = highlightOptions(weight=5,color="#666",bringToFront = TRUE))%>%leaflet::addLegend("bottomright", pal=pal4, values=QS18$N, title = "2018 Number of Sales by Zip Code")
mapq1
#2019 zipcodes by average price per sf NOT USED
QS19<-qs%>%filter(Sale_Date>="2019-01-01"&Sale_Date<"2020-01-01",!c(Sales_Price<25000&`Sales Ratio`>300))%>%group_by(Zip_Code)%>%summarize(N=n(),LONGITUDE=median(LONGITUDE),LATITUDE=median(LATITUDE), PPSF=median(`Price Per Sq Ft`))%>%mutate(PPSF=round(PPSF, digits=0))
                                                                                                                                                         
QS19
mybins<-c(100,125,150,175,200,225,250,275, Inf)
pal4<-colorBin(palette="Spectral", domain=QS19$N,bins=mybins, reverse=T)
mapq1<-QS19%>%leaflet()%>%addProviderTiles(providers$Esri.WorldGrayCanvas)%>%
  addCircles(radius=~N*3, color=pal4(QS19$PPSF), weight = 3, stroke = FALSE, fillOpacity = 1,label = ~PPSF,labelOptions = labelOptions(noHide = T, opacity=1,weight=3, direction="center", textOnly=TRUE, style = list("color" = "black","font-style" = "bold", "font-size"="12px")))%>%addPolygons(data = county,fillOpacity = 0.1, color = "grey", stroke = TRUE, weight = 5,  layerId = county@data$NAME_2)%>%addPolygons(data=zipstr, color="grey",fillOpacity = 0.1,stroke=TRUE, weight=3, label = ~ZIP_CODE, labelOptions = labelOptions(noHide = T, textOnly = TRUE, opacity=0.5, color="grey",textsize = "10px"))%>%setView(lng= -81.265,lat = 28.57, zoom=10)%>%leaflet::addLegend("bottomright", pal=pal4, values=QS19$PPSF, title = "2019 PPSF by Zip Code")
mapq1

#2019 zipcodes by median Price per square foot GREAT ONE
QS19PPSF<-qs%>%filter(Sale_Date>="2019-01-01"&Sale_Date<"2020-01-01",!c(Sales_Price<25000&`Sales Ratio`>300))%>%group_by(Zip_Code)%>%summarize(N=n(),LONGITUDE=median(LONGITUDE),LATITUDE=median(LATITUDE), PPSF=median(`Price Per Sq Ft`))%>%mutate(PPSF=round(PPSF, digits=0))%>%data.table::setnames("Zip_Code","ZIP_CODE")
QS19PPSF
mybins<-c(0,100,125,150,175,200,225,250,Inf)
pal4<-colorBin(palette="Spectral", domain=QS19PPSF$PPSF,bins=mybins, reverse=T)
zipstrj<-zipstr%>%left_join(QS19PPSF, by="ZIP_CODE")
zipstrj<-zipstrj%>%mutate(PPSF=ifelse(is.na(PPSF), 0, PPSF))
zipstrj
labels <- sprintf("Zip Code: <strong>%s</strong><br/>Price per Square Foot: $%g<br>Total Sales: %g",zipstrj$ZIP_CODE, zipstrj$PPSF, zipstrj$N) %>% lapply(htmltools::HTML)
mapq1<-QS19PPSF%>%leaflet()%>%addProviderTiles(providers$Esri.WorldGrayCanvas)%>%
  addCircles(radius=10, color="white", weight = 3, stroke = FALSE, fillOpacity = 1,label = ~PPSF,labelOptions = labelOptions(noHide = T, opacity=1,weight=3, direction="center", textOnly=TRUE, style = list("color" = "black","font-style" = "bold", "font-size"="12px")))%>%addPolygons(data = county,fillOpacity = 0.1, color = "grey", stroke = TRUE, weight = 5,  layerId = county@data$NAME_2)%>%
  addPolygons(data=zipstrj, fillColor=~pal4(PPSF),fillOpacity = 0.5,stroke=TRUE,color="white",  weight=2, label = ~labels,labelOptions = labelOptions( opacity=0.9, color="grey",textsize = "10px", direction="right"), highlightOptions = highlightOptions(weight=5,color="#666",bringToFront = TRUE))%>%leaflet::addLegend("bottomright", pal=pal4, values=~PPSF, title = "2019 Price per SF by Zip Code")
mapq1



#table for SF by Year and PPSF  GOOD
QSSF<-qs%>%filter(!c(Sales_Price<25000&`Sales Ratio`>300), !is.na(BEDS), !is.na(BATHS))%>%mutate(Year=year(Sale_Date), AYB=year(mdy(AYB)), EYB=year(mdy(EYB)))%>%group_by(Year, AYB)%>%summarize(N=n(), PPSF=median(`Price Per Sq Ft`), BEDS=median(BEDS), BATHS=median(BATHS), SF=as.numeric(median(HEATED_AREA)), EYB=median(EYB))%>%mutate(SF=round(SF, digits=0))
QSSF
QSSF%>%ggplot()+geom_line(aes(EYB, N,group=Year, color=Year))+facet_grid(Year~.)
geom_line(aes(AYB, N,group=Year, color=Year))
QSREN<-qs%>%filter(!c(Sales_Price<25000&`Sales Ratio`>300), !is.na(BEDS), !is.na(BATHS))%>%mutate(Year=year(Sale_Date), AYB=year(mdy(AYB)), EYB=year(mdy(EYB)))%>%mutate(Renov=(EYB-AYB))%>%filter(Year=="2019", Renov>0)
nrow(QSREN)
MedianSF<-qs%>%filter(!c(Sales_Price<25000&`Sales Ratio`>300), !is.na(BEDS), !is.na(BATHS))%>%mutate(Year=year(Sale_Date), AYB=year(mdy(AYB)), EYB=year(mdy(EYB)))%>%group_by(Year)%>%summarize(SF=median(HEATED_AREA), Age=median(AYB))
 MedianSF%>%view()                                                                                                   
                                                                                                    
                                                                                                    
Test<-qs%>%filter(!c(Sales_Price<25000&`Sales Ratio`>300), !is.na(BEDS), !is.na(BATHS), !is.na(AYB), !is.na(EYB))%>%mutate(Year=year(Sale_Date), AYB=year(mdy(AYB)), EYB=year(mdy(EYB)))%>%mutate(AYB=case_when(.$AYB%in%c(1850:1924)~"<1924",.$AYB%in%c(1925:1949)~"1925-1949", .$AYB%in%c(1950:1974)~"1950-1974", .$AYB%in%c(1975:1999)~"1975-1999", .$AYB%in%c(2000:2014)~"2000-2014", .$AYB%in%c(2015:2020)~"2015<"))%>%
  mutate(EYB=case_when(.$EYB%in%c(1850:1924)~"<1924",.$EYB%in%c(1925:1949)~"1925-1949", .$EYB%in%c(1950:1974)~"1950-1974", .$EYB%in%c(1975:1999)~"1975-1999", .$EYB%in%c(2000:2014)~"2000-2014", .$EYB%in%c(2015:2020)~"2015<"))
Test%>%ggplot()+geom_bar(aes(AYB), fill="darkgreen")+facet_grid(.~Year)
Test%>%group_by(Year, AYB)%>%filter(Year=="2018")%>%summarize(N=n())%>%view()
Test%>%group_by(Year, EYB)%>%filter(Year=="2018")%>%summarize(N=n())%>%view()
tx<-Test%>%filter(Year=="2019")
sum(tx$AYB==tx$EYB)/nrow(tx)
Test%>%group_by(Year, EYB)%>%summarize(N=n())
geom_bar(aes(EYB), fill="blue")+facet_grid()

QSSF<-qs%>%filter(!c(Sales_Price<25000&`Sales Ratio`>300), !is.na(BEDS), !is.na(BATHS))%>%mutate(Year=year(Sale_Date), AYB=year(mdy(AYB)))%>%group_by(Year, AYB)%>%ggplot(aes(AYB))+geom_histogram()

#overall SF all years, median beds and baths GREAT
QS19<-qs%>%filter(Sale_Date>="2019-01-01"&Sale_Date<"2020-01-01",!c(Sales_Price<25000&`Sales Ratio`>300))%>%group_by(Zip_Code)%>%summarize(N=n(),LONGITUDE=median(LONGITUDE),LATITUDE=median(LATITUDE), SF=median(HEATED_AREA), Sales_Price=median(Sales_Price))%>%mutate(SF=round(SF, digits=0))%>%data.table::setnames("Zip_Code","ZIP_CODE")%>%mutate(ZIP_CODE=as.character(ZIP_CODE))
QS19
mybins<-c(0,1000,1250,1500,1750,2000,2250,2500, 2750,3000,3250)
pal4<-colorBin(palette="Spectral", domain=QS19$SF,bins=mybins, reverse=T)
zipstr<-zipstr%>%left_join(QS19, by="ZIP_CODE")
zipstr<-zipstr%>%mutate(SF=ifelse(is.na(SF), 0, SF))
zipstr
labels <- sprintf("Zip Code: <strong>%s</strong><br/>Median Sales Price: $%g<br>Median Size: %g sf<br>Total Sales: %g",zipstr$ZIP_CODE, zipstr$Sales_Price, zipstr$SF, zipstr$N) %>% lapply(htmltools::HTML)
mapq1<-QS19%>%leaflet()%>%addProviderTiles(providers$Esri.WorldGrayCanvas)%>%
  addCircles(radius=10, color="white", weight = 3, stroke = FALSE, fillOpacity = 1,label = ~SF,labelOptions = labelOptions(noHide = T, opacity=1,weight=3, direction="center", textOnly=TRUE, style = list("color" = "black","font-style" = "bold", "font-size"="12px")))%>%addPolygons(data = county,fillOpacity = 0.1, color = "grey", stroke = TRUE, weight = 5,  layerId = county@data$NAME_2)%>%
  addPolygons(data=zipstr, fillColor=~pal4(SF),fillOpacity = 0.5,stroke=TRUE,color="white",  weight=2, label = ~labels,labelOptions = labelOptions( opacity=0.9, color="grey",textsize = "10px", direction="right"), highlightOptions = highlightOptions(weight=5,color="#666",bringToFront = TRUE))%>%leaflet::addLegend("bottomright", pal=pal4, values=~SF, title = "2019 Median SF by Zip Code")
mapq1

#2019 top ten neighborhoods by the number of sales SOSO

QS10<-qs%>%filter(Sale_Date>="2019-01-01"&Sale_Date<"2020-01-01",!c(Sales_Price<25000&`Sales Ratio`>300))%>%group_by(NBHD_DESC)%>%summarize(N=n(), LONGITUDE=median(LONGITUDE),LATITUDE=median(LATITUDE), Median=median(Sales_Price), SquareF=median(HEATED_AREA), Beds=median(BEDS), BATHS=median(BATHS), PPF1=median(`Price Per Sq Ft`))%>%arrange(-N)%>%head(10)%>%mutate(Rank=c(1:10))
QS10 
QS10a<-qs%>%filter(Sale_Date>="2019-01-01"&Sale_Date<"2020-01-01",!c(Sales_Price<25000&`Sales Ratio`>300))%>%group_by(NBHD_DESC)%>%filter(NBHD_DESC%in%QS10$NBHD_DESC)
QS10a
mybins<-c(100,150, 200, 250)
pal4<-colorBin(palette="Paired", domain=QS10$N,bins=mybins, reverse=T)
mapq1<-QS10%>%leaflet()%>%addProviderTiles(providers$Esri.WorldGrayCanvas)%>%addCircles(data=QS10a,radius=20, color="green", opacity=0.9, stroke=FALSE)%>%
  addCircles(radius=~N*5, color=pal4(QS10$N), weight = 3, stroke = FALSE, fillOpacity = 0.7,label = ~Rank,labelOptions = labelOptions(noHide = T, opacity=1,weight=3, direction="center", textOnly=TRUE, style = list("color" = "black","font-style" = "bold", "font-size"="20px")))%>%
  addCircles(radius=~N*5, color="grey", weight = 3, stroke = FALSE, fillOpacity = 0.1,label = ~NBHD_DESC,labelOptions = labelOptions(noHide = T, opacity=1,weight=3, style = list("color" = "black","font-style" = "itallic", "font-size"="15px"), direction="top",offset=c(0,-12)))%>%addPolygons(data = county,fillOpacity = 0.1, color = "grey", stroke = TRUE, weight = 5,  layerId = county@data$NAME_2)%>%addPolygons(data=zipstr, color="grey",fillOpacity = 0.1,stroke=TRUE, weight=3, label = ~ZIP_CODE, labelOptions = labelOptions(noHide = T, textOnly = TRUE, opacity=0.5, color="grey",textsize = "10px"))%>%leaflet::addLegend("bottomright", pal=pal4, values=QS10$N, title = "Top 10 Neighborhoods")
mapq1
#2019 neighborhoods __________TEST GREAT ONE
QS19N<-qs%>%filter(Sale_Date>="2019-01-01"&Sale_Date<"2020-01-01",!c(Sales_Price<25000&`Sales Ratio`>300))%>%group_by(NBHD_DESC)%>%summarize(N=n(), LONGITUDE=median(LONGITUDE),LATITUDE=median(LATITUDE), Median=median(Sales_Price), PPSF=median(`Price Per Sq Ft`), Beds=median(BEDS), BATHS=median(BATHS))%>%arrange(-N)%>%mutate(PPSF=round(PPSF, digits=0))
QS19N
QS10<-qs%>%filter(Sale_Date>="2019-01-01"&Sale_Date<"2020-01-01",!c(Sales_Price<25000&`Sales Ratio`>300))%>%group_by(NBHD_DESC)%>%summarize(N=n(), LONGITUDE=median(LONGITUDE),LATITUDE=median(LATITUDE), Median=median(Sales_Price), SquareF=median(HEATED_AREA), Beds=median(BEDS), BATHS=median(BATHS), PPF1=median(`Price Per Sq Ft`))%>%arrange(-N)%>%head(10)%>%mutate(Rank=c(1:10))
QS10%>%view()
mybins<-c(0,5,25,50,75,100,150,200,250)
pal4<-colorBin(palette="Spectral", domain=QS19N$N,bins=mybins, reverse=T)

neighocpatr1<-neighocpatr%>%left_join(QS19N, by="NBHD_DESC")
neighocpatr1<-neighocpatr1%>%mutate(N=ifelse(is.na(N), 0, N))
neighocpatr1
labels <- sprintf("Name: <strong>%s</strong><br>Median Sales Price: $%g <br/>Median Price per SF: $%g<br>Total Sales: %g",neighocpatr1$NBHD_DESC,neighocpatr1$Median, neighocpatr1$PPSF, neighocpatr1$N) %>% lapply(htmltools::HTML)

mapq1<-QS19N%>%leaflet()%>%addProviderTiles(providers$Esri.WorldGrayCanvas)%>%addCircles(data=QS10,radius=~QS10$N*5, color="grey", weight = 3, stroke = FALSE, fillOpacity = 0.7,label = ~QS10$Rank,labelOptions = labelOptions(noHide = T, opacity=1,weight=3, direction="center", textOnly=TRUE, style = list("color" = "black","font-style" = "bold", "font-size"="20px")))%>%addPolygons(data = county,fillOpacity = 0.1, color = "grey", stroke = TRUE, weight = 5,  layerId = county@data$NAME_2)%>%
addPolygons(data=neighocpatr1, fillColor=~pal4(N),fillOpacity = 0.5,stroke=TRUE,color="white",  weight=2, label = ~labels,labelOptions = labelOptions( opacity=0.9, color="grey",textsize = "10px", direction="right"), highlightOptions = highlightOptions(weight=5,color="#666",bringToFront = TRUE))%>%leaflet::addLegend("bottomright", pal=pal4, values=~N, title = "2019 Number of Sales by Neighborhood")
mapq1










#################################EXTRA

#example found to program tha data in the label:
paste("Address:", QS$Situs_Address, "<br>","City:",QS$City, "<br>","Sale Date:", QS$`Sale Date`,"<br>","Sales Price:", QS$`Adj Sale Amt`,"<br>", "Ratio:", QS$`Sales Ratio`)




#test sales File!
sales<-st_read("~/Documents/R/Sales/Project/rawdata/Shapefiles/SV_SALES.shp")
sales<-sales%>%filter(SALE_DATE>="2020-01-01")
sales<-st_transform(sales,"+proj=longlat +ellps=WGS84 +datum=WGS84")
nrow(sales)
nrow(qs$Sale_Date>="2020-01-01")
QSSV<-qs%>%filter(Sale_Date>="2019-01-01"&Sale_Date<"2020-01-01",!c(Sales_Price<25000&`Sales Ratio`>300))
mapq1<-QSJD%>%leaflet()%>%addProviderTiles(providers$Esri.WorldGrayCanvas)%>%
  addCircles(radius=10, color=pal2(QSSV$Jurisdiction), weight = 3, stroke = FALSE, fillOpacity = 0.8)%>%addCircleMarkers(clusterOptions = markerClusterOptions())%>%addPolygons(data = county,fillOpacity = 0.1, color = "grey", stroke = TRUE, weight = 5,layerId = county@data$NAME_2)%>%addCircles(data=sales, color="black",stroke=FALSE)%>%leaflet::addLegend("topright", pal=pal2, values=QSSV$Jurisdiction, title = "2019 Sales by Jurisdiction")
mapq1


#lastly, I would like to add all new houses that were build last year by using an all parcel file filtered by the DOR codes and AYB

###render app



library(shiny)
library(xts)
QS<-qs%>%filter(Sale_Date>="2015-01-01"&Sale_Date<"2020-01-01",!c(Sales_Price<25000&`Sales Ratio`>300), BLDG_NUM==1)%>%mutate(Year=year(Sale_Date))%>%group_by(Zip_Code, Year)%>%summarize(N=n(),LONGITUDE=median(LONGITUDE),LATITUDE=median(LATITUDE))
mybins<-c(0,200,400,600,800,1000,1200,Inf)

pal4<-colorBin(palette="Paired", domain=QS$N,bins=mybins, reverse=T)

ui<-fluidPage(
  sliderInput(inputId = "Year",label="Year", min(QS$Year), max(QS$Year), value=max(QS$Year),step=1, animate=F), leafletOutput("mymap", height=750, width=1153))
save(ui, file="~/Documents/R/Sales/Project/data/app/ui.r", options(encoding = "UTF-8"))
server<-function(input,output, session){
  points<-reactive({QS%>%filter(Year==input$Year)
    })
  output$mymap<-renderLeaflet({
    leaflet()%>%addProviderTiles(providers$Esri.WorldGrayCanvas)%>%
      addCircles(data=points(),radius=~N*2, color=pal4(QS$N), weight = 3, stroke = FALSE, fillOpacity = 1,label = ~N,labelOptions = labelOptions(noHide = T, opacity=1,weight=3, direction="center", textOnly=TRUE, style = list("color" = "black","font-style" = "bold", "font-size"="12px")))%>%addPolygons(data = county,fillOpacity = 0.1, color = "grey", stroke = TRUE, weight = 5,  layerId = county@data$NAME_2)%>%addCircles(color="grey", opacity=0.1,lng= -81.20, lat=28.73, label = "Number of Sales by Zip Code" ,labelOptions = labelOptions(noHide = T, opacity=1,weight=3, direction="center", textOnly=TRUE, style = list("color" = "black","font-style" = "bold", "font-size"="40px")))%>%addPolygons(data=zipstr, color="grey",fillOpacity = 0.1,stroke=TRUE, weight=3, label = ~ZCTA5CE10, labelOptions = labelOptions(noHide = T, textOnly = TRUE, opacity=0.5, color="grey",textsize = "10px"))%>%
      setView(lng= -81.265,lat = 28.57, zoom=11)%>%leaflet::addLegend("bottomright", pal=pal4, values=QS$N, title =" Number of Sales")
  })
}
save(server,file="~/Documents/R/Sales/Project/data/app/server.r")
shinyApp(ui,server)
options(encoding = "UTF-8")
library(rsconnect)
rsconnect::setAccountInfo(name='tsapps', token='0B9816FA277FEC3974F8E75F92DB32B0', secret='fNN+RGxxH1XbtwmbY8XRsVNd5DQXi951DMoFq955')

rsconnect::deployApp("~/Documents/R/Sales/Project/data/app", appName="APPNAME")
Y
