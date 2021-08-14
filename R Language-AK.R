#Ayan Kulov
#TP058560

#installed packages
install.packages("tidyverse")
install.packages("forcats")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("hrbrthemes")
install.packages("ggpubr")
install.packages("scales")

#loading packages
library(tidyverse)
library(forcats)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(ggpubr)
library(scales)

remotes::update_packages("rlang")
install.packages("rlang", type = "source")

#fetch data from file
weather = read.table(file="C:\\Users\\cookie\\Downloads\\weather.csv",header = TRUE, sep = ",")
View(weather)

summary(weather)
names(weather)

##### Data pre-processing #####

##Create new column for months
vec <- rep(c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'), 
           times=c(31,29,31,30,31,30,31,31,30,31,30,31))
weather$Month <-vec

## replace NA with average values
##Sunshine
avg = mean(weather$Sunshine, na.rm=TRUE)
weather$Sunshine = ifelse(is.na(weather$Sunshine),avg, weather$Sunshine)

##WindGustSpeed
avg_wgs = mean(weather$WindGustSpeed, na.rm=TRUE)
weather$WindGustSpeed = ifelse(is.na(weather$WindGustSpeed),avg_wgs, weather$WindGustSpeed)

##WindSpeed9am
avg_ws = mean(weather$WindSpeed9am, na.rm=TRUE)
weather$WindSpeed9am = ifelse(is.na(weather$WindSpeed9am),avg_ws, weather$WindSpeed9am)


#Replace NA Direction with North
weather$WindGustDir = ifelse(is.na(weather$WindGustDir),'N', weather$WindGustDir)
weather$WindDir9am = ifelse(is.na(weather$WindDir9am),'N', weather$WindDir9am)
weather$WindDir3pm = ifelse(is.na(weather$WindDir3pm),'N', weather$WindDir3pm)


##### DATA ANALYSIS #####

#Visualization 1:
##Question: How does seasons and temperature vary?
##Analysis 1:  Finding the overall temperature throughout the year
##Analysis 2: Determining seasonal change with temperature 


ggplot(weather , aes(y=MaxTemp, x=Month, colour="#33FFFF"))+
  geom_line() + geom_line(data = weather,aes(y=MinTemp,x=Month,colour="cyan"))+
  scale_y_continuous(breaks = breaks_width(5)) +
  scale_color_manual(labels = c("Max Temp", "Min Temp"), values = c("#00CCFF", "#FF3333"))+
  labs(titles="Min and Max Temp throughout the Year",y="Temperature")

#Visualization 2:
##Question: What is the relationship between temperature and humidity?
##Analysis 3: Temperature vs humidity 
##Question: Do temperature and humidity affect comfortness?
##Analysis 4: Heat or humidity

weather%>%filter(Month == "Nov")%>%ggplot(mapping=aes(x=Temp9am,y=Humidity9am))+
  geom_point()+ geom_smooth(method = "lm", formula= y~x,se = FALSE)+
  labs(title="Correlation between Temperature and Humidity")

#Visualization 3:
##Question: What are the rainy and non-rainy months?
##Analysis 5:
##Question: Best time to have outdoor activities?
##Analysis 6: 
  
rain = weather%>%group_by(Month)%>%summarise(Rainfall=mean(Rainfall))%>%select(Month,Rainfall)
ggdotchart(rain,x="Month",y="Rainfall",sorting="descending",
           add="segments",color="Month",add.params=list(color="#5b5c5b",size=2),
           dot.size=9,label=round(rain$Rainfall,1),font.label = list(color="white",size=10,vjust=0.4),
           ggtheme=theme_pubr()) + 
        labs(title="Average Rainfall Each Month")+theme_ipsum()

#Visualization 4 & 5:
##Question: How accurate is the prediction of the next day raining?
##Analysis 7: Overall prediction accuracy

ggplot(weather, aes(x=RainToday))+geom_bar(aes(col=RainToday))+
  facet_wrap(~Month)+theme_bw()+
  labs(title="RainToday Count Throughout the Year")

ggplot(weather, aes(x=RainTomorrow))+geom_bar(aes(col=RainTomorrow))+
  facet_wrap(~Month)+theme_bw()+
  labs(title="RainTomorow Prediction Throughout the Year")

#Visualization 6: 
##Question: How high the wind gust speed can reach? Lowest wind gust speed? Normal wind gust speed?
##Analysis 8: 
##Question: How often does the highest wind gust speed happen? and what is the wind gust speed that has the most count?
##Analysis 9: 
  
weather%>%group_by(Month)%>%ggplot(aes(WindGustSpeed))+
  geom_bar(fill="#00CCFF")+
  theme_ipsum()+
  labs(title= "Barchart of Wind Gust Speed")

#Visualization 7:
##Question: What direction does the wind gust speed change to?
##Analysis 10: 
  
weather%>% count(WindGustDir) %>%
  mutate(WindGustDir = fct_reorder(WindGustDir, n)) %>%
  ggplot( aes(x=WindGustDir, y=n)) +
  geom_bar(stat="identity", fill="#52ff5d", alpha=.6, width=.4) +
  coord_flip() +
  xlab("Wind Direction") +ylab("Count") + labs(title="Wind Gust Direction")
  theme_bw()

#Visualization 8 and 9:
##Question: What is the wind direction difference at 9am and 3pm ?
##Analysis 11: 
    
weather%>%count(WindDir9am) %>%
  arrange(WindDir9am) %>%    
  mutate(WindDir9am=factor(WindDir9am, levels=WindDir9am)) %>%   
  ggplot( aes(x=WindDir9am, y=n)) +
  geom_segment( aes(xend=WindDir9am, yend=0)) +
  geom_point( size=4, color="cyan") +
  coord_flip() +
  theme_bw() +
  xlab("Wind Direction") +ylab("Count") + labs(title="Wind Direction at 9am")


weather%>%count(WindDir3pm) %>%
  arrange(WindDir3pm) %>%    
  mutate(WindDir9am=factor(WindDir3pm, levels=WindDir3pm)) %>%   
  ggplot( aes(x=WindDir3pm, y=n)) +
  geom_segment( aes(xend=WindDir3pm, yend=0)) +
  geom_point( size=4, color="purple") +
  coord_flip() +
  theme_bw() +
  xlab("Wind Direction") +ylab("Count") + labs(title="Wind Direction at 3pm")


#Visualization 10:
##Question: What is the relationship between evaporation and sunshine? 
##Analysis 12:
  
ggplot(weather,aes(x=Sunshine,y=Evaporation))+geom_jitter(aes(col=RainToday))+
  stat_smooth(method="lm", formula = y ~ x,color="#57ffb9")+
  labs(title="Correlation between Sunshine and Evaporation")+theme_bw()+ theme(
    panel.background = element_rect(fill = "#2b2b2b"),
    legend.key.width = unit(0.9, "cm"),
    legend.key.height = unit(0.75, "cm"))

#Visualization 11:
##Question: What is the relationship between temperature and sunshine? 
##Analysis 13:
##Question: How evaporation, temperature and sunshine are related to each other?
##Analysis 14:
  
ggplot(weather,aes(x=Sunshine,y=MaxTemp))+geom_jitter(aes(col=RainToday))+
  stat_smooth(method="lm", formula = y ~ x,color="#57ffb9")+
  labs(title="Correlation between Sunshine and Max Temperature")+theme_bw()+ theme(
    panel.background = element_rect(fill = "#2b2b2b"),
    legend.key.width = unit(0.9, "cm"),
    legend.key.height = unit(0.75, "cm"))




