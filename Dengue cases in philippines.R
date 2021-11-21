"Visualization and Time Series Forecasting of Dengue Cases in the Philippines"


#Load libraries
library("ggplot2") 
library("ggfortify") 

#Load in our dataset
denguecases<-read.csv("C:/Users/abc/Downloads/denguecases.csv",h=T)
str(denguecases)
denguecases$Month<-factor(denguecases$Month,levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
str(denguecases)
colnames(denguecases)

head(denguecases,10)

#Checking for missing values
sum(is.na(denguecases))

summary(denguecases)

#Based from above results, we can say that on average there are more than 11 recorded dengue cases per 100,000 population. 
#The highest recorded dengue case is 147.324 per 100,000 population. For that specific case, one in about every 680 people were diagnosed with dengue. 
#Seems like this particular dataset has no missing values therefore no further data cleaning is required.

denguecasesregion<-aggregate(Dengue_Cases~Year+Region,denguecases,sum)
str(denguecasesregion)

ggplot(denguecasesregion,aes(Year,Region,fill=Dengue_Cases))+geom_tile()+
  scale_fill_gradient2(low = "white",mid = "blue",high = "red",midpoint = 500)+
  scale_x_continuous(breaks = c(seq(2008,2016,1)))+ xlab(label="YEAR")+ylab(label="REGION")+
  ggtitle("DENGUE CASES PER YEAR AND REGION")+
  theme(
    axis.title.x = element_text(size=10, face="bold"),
    axis.title.y = element_text(size=10, face="bold"))

#Figure above reveals that the Philippines has suffered a huge chunk of the total number of dengue cases in the past 9 years during 2012 to 2014. 
#Region VII seems to have relatively higher dengue cases compared to other region every year. The worst recorded case happens during 2013 at CAR (Cordillera Administrative Region located at Northern Philippines and has a mountainous terrain) it is very likely that the region suffers from an outbreak during that period, probably due to lack of awareness.
#It is also quite remarkable that ARMM despite being perished with poverty and terrorism has an extremely low rate of dengue cases. This could go two ways 1) The region is fortunate enough to have rare cases of the disease 2) 
#ARMM being the poorest region and one of the less densed region, there is a chance that not all dengue cases
#were properly recorded since ARMM has low number of medical facilities which is mostly located in the urban part of the region while most of its residents resides in its rural parts.


denguecasesmonth<-aggregate(Dengue_Cases~Month+Region,denguecases,sum)
str(denguecasesmonth)

denguecasesmonth$Month<-factor(denguecasesmonth$Month,levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
str(denguecasesmonth)


ggplot(denguecasesmonth,aes(Month,Region,fill=Dengue_Cases))+geom_tile()+
    scale_fill_gradient2(low = "white",mid = "blue",high = "red",midpoint = 200)+
    xlab(label="MONTH")+ylab(label="REGION")+
    ggtitle("MONTHLY DENGUE CASES IN THE PHILIPPINES")+
    theme(
        axis.title.x = element_text(size=10, face="bold"),
        axis.title.y = element_text(size=10, face="bold"))

#Above plot shows some interesting patterns. Dengue cases were relatively lower at the start of every quarter (January, April, July, and October).
#On the other hand, for some odd reasons dengue cases is higher every last month of each quarter (March, June, September, and December). Extreme cases of dengue were recorded during those months in regions VI, VII, and CAR.


#Time Series Plot
#We can investigate trend for the entire Philippine by using a time series plot. We will be aggregating our regional data to extrapolate values for the entire Philippines


#Aggregate regional data to represent the entire country
denguecasesph<-aggregate(Dengue_Cases~Month+Year,denguecases,sum)
str(denguecasesph)
#Convert data into a time series
denguecasesphts<-ts(denguecasesph$Dengue_Cases,c(2008,1),c(2016,12),12)
#Plot time series data
autoplot(denguecasesphts)+xlab(label = "TIME")+
  ylab(label = "DENGUE CASES PER 100,000 POPULATION")+
  ggtitle("DENGUE CASES IN PHILIPPINES")


