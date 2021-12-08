library(tidyverse)
library(lubridate)
library(zoo)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(ggrepel)
library(RColorBrewer)
library(formattable)
library(circlize)
library(data.table)


#3rd Objective

library(ggplot2)

df20 <- read.csv("air.csv")
df2007 <- subset(df20,df20$Year>2006 & df20$Year<2008) #Training data set
df2008 <- subset(df20,df20$Year>2007 & df20$Year<2009) #Test data set


model2 <- lm(df2007$Aircraft_Trips ~ df2007$Month_num,df2007) 
#Aircraft_trips=response variable or target variable   #Month_num=predictor variable
summary(model2)
airTrip2 <- data.frame(df2008$Aircraft_Trips)
result2 <-  predict(model2,airTrip2)
result2 <- as.data.frame(result2)

write.csv(df2008,'y2008.csv')
write.csv(result2,'y2008res.csv')
y08 <- read.csv("y2008.csv")

ggplot(y08,aes(x=Month_num))+geom_line(aes(y=Aircraft_Trips), colour="grey")+
  geom_line(aes(y=result2),colour="Red")+
  scale_x_continuous(
    limits = c(0, 12),
    labels = scales::comma) +
  scale_y_continuous(
    limits = c(0, 1000),
    labels = scales::comma) +
  labs(                # for labelling
    title = "Prediction for 2008", 
    x = "Month number", 
    y = "Aircraft trips")





#2nd objective





read_traffic <- function () {
  read_csv("air.csv")
}



transform_traffic <- function (df) {
  df %>% 
    mutate(
      Month = as.Date(Month, origin="1899-12-30"), # Convert from Excel format
      City1 = as.factor(City1),   #factor is used to encode a vector as a factor 
      City2 = as.factor(City2))
}






by_airport_pairs <- . %>%
  unite(Journey, City1, City2, sep = " - ", remove = TRUE) %>%
  group_by(Journey)



busiest_routes <- function (df, n = 25, year = 2016) { 
  df %>%
    filter(Year == year) %>%
    group_by(City1, City2) %>%
    summarise(
      Trips = Passenger_Trips %>% mean %>% floor) %>%
    arrange(desc(Trips)) %>%
    head(n = n)
}


dom <- read_traffic() %>% transform_traffic()
glimpse(dom)


dom %>%
  busiest_routes(year = 2016, n = 25) %>%
  by_airport_pairs %>%
  ggplot(aes(x = reorder(Journey, Trips), y = Trips)) +
  geom_bar(
   stat = "identity",   #The statistical transformation to use on the data for this layer
    width = 0.5, 
    fill = "#990033") +
  geom_text(
    aes(label = prettyNum(Trips, big.mark = ",")),  #  "prettifying" (possibly formatted like " , ") numbers
    size=2.5, hjust = -0.1,  # hjust is spacing between bar and number
    colour = "#333333") +
  scale_y_continuous(
    limits = c(0, 800000),
    labels = scales::comma) +
  coord_flip() +   # interchanging x and y axis
  labs(                    # for labelling
    title = "Busiest routes in 2016", 
    x = "Journey", 
    y = "Passenger trips") 




#1st objective

dom %>%
  filter(Year == 2000) %>%
  by_airport_pairs() %>%
  summarise(
    Distance = mean(`Distance_GC_(km)`),
    Trips = mean(Passenger_Trips)) %>%
  arrange(desc(Distance)) %>%
  head(n = 10) %>%
  ggplot(aes(x = Distance, y = Trips)) +
  geom_point(size = 3.0, color = "#990033") +
  geom_text_repel(            # the text label repel away so that they don't overlap
    aes(label = Journey), size=2.5, 
    colour = "#333333") +
  scale_x_continuous(
    breaks = seq(0, 5000, 500), 
    limits = c(0, 5000)) +
  scale_y_continuous(
    breaks = seq(0, 200000, 20000), 
    limits = c(0, 200000),
    labels = scales::comma) +
  labs(
    title = "Ten longest flights",
    subtitle = "Plotted against 2000 passenger trips",
    y = "Number of trips", 
    x = "Distance (km)") +
  theme_minimal()



###Trend plot###

df2010 <- subset(df007,df007$Year>2009 & df007$Year<20011) #similarly for other years

df10 <- data.frame(df2010$Aircraft_Trips, df2010$Month_num) #similarly for other years

sum(df10[which(df10[,2]==12),1]) #similarly for other years

df500 <- read.csv("G:/code/trip10.csv")

require(graphics)

with(df500,scatter.smooth(Month_num, Aircraft_Trips, col="red"))
mtext("Graph for 2010")






#Filter city and prediction 1


df20 <- read.csv("air.csv") #Full data


df91to00 <- subset(df20,df20$Year>1990 & df20$Year<2001) #Dataframe from 1991 to 2000 
df01to10 <- subset(df20,df20$Year>2000 & df20$Year<2011) #Dataframe from 2001 to 2010



dfSubset2 <- df91to00[grep("ALBURY", df91to00$City1, "SYDNEY", df91to00$City2), ] #Training data set after filtering city pair
dfSubset3 <- df01to10[grep("ALBURY", df01to10$City1, "SYDNEY", df01to10$City2), ] #Test data set after filtering city pair


model96 <- lm(dfSubset2$Aircraft_Trips ~ dfSubset2$Year,dfSubset2) #linear regression model
#Aircraft_trips=response variable or target variable   #Year=predictor variable
summary(model96)
airTrip96 <- data.frame(dfSubset3$Aircraft_Trips)
result96 <-  predict(model96,airTrip96)
result96 <- as.data.frame(result96)

write.csv(dfSubset3,'sub.csv') #Test data set after filtering city pair to csv
write.csv(result96,'subres.csv') #Result obtained from prediction to csv
y01to10 <- read.csv("sub.csv") 
ggplot(y01to10,aes(x=Year,y=Aircraft_Trips))+geom_line(colour="blue", size=1.2)+
  geom_line(aes(y=result96),colour="Red", size=1.2)+
  scale_x_continuous(
    breaks=seq(2001, 2010,1)) +
  scale_y_continuous(
    limits = c(0, 1000),
    labels = scales::comma) +
  labs(                # for labelling
    title = "Prediction for 2001 to 2010(blue=original values, red=predicted values)", 
    x = "Year", 
    y = "Aircraft trips")



#Predction 2

df20 <- read.csv("air.csv")

df01to05 <- subset(df20,df20$Year>2000 & df20$Year<2006) #Dataframe from 2001 to 2005 
df06to10 <- subset(df20,df20$Year>2005 & df20$Year<2011) #Dataframe from 2006 to 2010

dfSubset4 <- df01to05[grep("ALBURY", df01to05$City1, "MELBOURNE", df01to05$City2), ] #Training data set after filtering city pair
dfSubset5 <- df06to10[grep("ALBURY", df06to10$City1, "MELBOURNE", df06to10$City2), ] #Test data set after filtering city pair


model50 <- lm(dfSubset4$Passenger_Load_Factor ~ dfSubset4$Year,dfSubset4) #linear regression model
#Passenger_Load_Factor=response variable or target variable   #Year=predictor variable
summary(model50)
plf50 <- data.frame(dfSubset5$Passenger_Load_Factor)
result50 <-  predict(model50,plf50)
result50 <- as.data.frame(result50)

write.csv(dfSubset5,'plf.csv') #Test data set after filtering city pair to csv
write.csv(result50,'plfres.csv') #Result obtained from prediction to csv
y06to10 <- read.csv("plf.csv") 
ggplot(y06to10,aes(x=Year,y=Passenger_Load_Factor))+geom_line(colour="blue", size=1.2)+
  geom_line(aes(y=result50),colour="Red", size=1.2)+
  scale_x_continuous(
    breaks=seq(2006, 2010,1)) +
  scale_y_continuous(
    limits = c(0, 100),
    labels = scales::comma) +
  labs(                # for labelling
    title = "Prediction for 2006 to 2010(blue=original values, red=predicted values)", 
    x = "Year", 
    y = "Passenger load factor")