
#Code to analyse the sensor data collected from the network and chemical analysis collected from the SEM
#Make scatter plots of daily, weekly, and monthly averages for both sites.
#Look at seasonal variations
#Make box plots of the data and do Anova on them
#Generate stats for for daily, weekly, and monthly variations (significance, descriptive stats, etc)

################################################################################


#Load in libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(readr)
library(ggpattern)
library(tidyverse)
library(devtools)
library(lubridate)
library(ggpubr)
library("writexl")
library(gclus)

#Select working directory
setwd("C:/Users/Shomari/OneDrive/Desktop/Dissertation/Sensor data/01-11-21_30-06-22")

#Clear environment
rm(list=ls())

#Read in data
#kirk <- read.csv("C:/Users/Shomari/Desktop/Dissertation/Sensor data/01-11-21_30-06-22/Kirkdale station.csv") #excel file of this data
K1 <- read_csv("~/Kirkdale.csv", col_types = cols(Date = col_date(format = "%d/%m/%Y"), 
                                                        Time = col_time(format = "%H:%M:%S")))
L1 <- read_csv("Life sciences building.csv", 
               col_types = cols(Date = col_date(format = "%d/%m/%Y"), 
                                Time = col_time(format = "%H:%M:%S")))
summary(k1)
head(k1)
k1$PM1.0
k1$PM2.5
k1$PM10

#Removes decimal point from column names
names(K1)<-str_replace_all(names(K1), "\\.", "")
names(L1)<-str_replace_all(names(L1), "\\.", "")



####################################################################
#Averaging the data by groups (making data frames)

#Average each day of the week to find daily averages
#Kirkdale
Dailykirk <- data.frame(K1 %>% 
                          mutate(Date = floor_date(Date)) %>%
                          group_by(Date) %>%
                          summarize(PM1 = mean(PM1)),
                        K1 %>% 
                          mutate(Date = floor_date(Date)) %>%
                          group_by(Date) %>%
                          summarize(PM25 = mean(PM25)),
                        K1 %>% 
                          mutate(Date = floor_date(Date)) %>%
                          group_by(Date) %>%
                          summarize(PM10 = mean(PM10)),
                        K1 %>% 
                          mutate(Date = floor_date(Date)) %>%
                          group_by(Date) %>%
                          summarize(Humidity = mean(Humidity)),
                        K1 %>% 
                          mutate(Date = floor_date(Date)) %>%
                          group_by(Date) %>%
                          summarize(Temperature = mean(Temperature)))


#view column names in data sets
names(Dailykirk)

names(Dailykirk)<-str_replace_all(names(Dailykirk), "\\.", "")

#view the class type of each column (type of data)
sapply(Dailykirk, class)

#change columns to class date to allow analysis
Dailykirk[] <- lapply(Dailykirk, function(Date) {
  if (inherits(Date, "POSIXt")) as.Date(Date) else Date
})

getwd() #"C:/Users/Shomari/Desktop/Dissertation/Sensor data/01-11-21_30-06-22"

#write new xls file and save to computer - edit and reload these below
write_xlsx(Dailykirk, "C:/Users/Shomari/Desktop/Dissertation/Sensor data/01-11-21_30-06-22/DailyKirk.xls")

#Load in the new csv files with weekly and monthly averages
Dailykirk <- read_csv("DailyKirk.csv", col_types = cols(Date = col_date(format = "%d/%m/%Y")))
  #read_csv("DailyKirk.csv", col_types = cols(Date = col_date(format = "%d/%m/%Y")))

#Life building
DailyLife <- data.frame(L1 %>% 
                          mutate(Date = floor_date(Date)) %>%
                          group_by(Date) %>%
                          summarize(PM1 = mean(PM1)),
                        L1 %>% 
                          mutate(Date = floor_date(Date)) %>%
                          group_by(Date) %>%
                          summarize(PM25 = mean(PM25)),
                        L1 %>% 
                          mutate(Date = floor_date(Date)) %>%
                          group_by(Date) %>%
                          summarize(PM10 = mean(PM10)),
                        L1 %>% 
                          mutate(Date = floor_date(Date)) %>%
                          group_by(Date) %>%
                          summarize(Humidity = mean(Humidity)),
                        L1 %>% 
                          mutate(Date = floor_date(Date)) %>%
                          group_by(Date) %>%
                          summarize(Temperature = mean(Temperature)))


#view column names in data sets
names(DailyLife)

names(DailyLife)<-str_replace_all(names(DailyLife), "\\.", "")

#view the class type of each column (type of data)
sapply(DailyLife, class)

#change columns to class date to allow analysis
DailyLife[] <- lapply(DailyLife, function(Date) {
  if (inherits(Date, "POSIXt")) as.Date(Date) else Date
})

getwd() #"C:/Users/Shomari/Desktop/Dissertation/Sensor data/01-11-21_30-06-22"

#write new xls file and save to computer - edit and reload these below
write_xlsx(DailyLife, "C:/Users/Shomari/Desktop/Dissertation/Sensor data/01-11-21_30-06-22/DailyLife.xls")

#Load in the new csv files with weekly and monthly averages
DailyLife <- read_csv("DailyLife.csv", col_types = cols(Date = col_date(format = "%d/%m/%Y")))
  #read_csv("DailyLife.csv", col_types = cols(Date = col_date(format = "%d/%m/%Y")))



#average of each hour in the day to see how abundance changes on average throughout the day
#Kirkdale
Hourlykirk <- data.frame(K1 %>% 
                          mutate(Time = hour(Time)) %>%
                          group_by(Time) %>%
                          summarize(PM1 = mean(PM1)),
                        K1 %>% 
                          mutate(Time = hour(Time)) %>%
                          group_by(Time) %>%
                          summarize(PM25 = mean(PM25)),
                        K1 %>% 
                          mutate(Time = hour(Time)) %>%
                          group_by(Time) %>%
                          summarize(PM10 = mean(PM10)),
                        K1 %>% 
                          mutate(Time = hour(Time)) %>%
                          group_by(Time) %>%
                          summarize(Humidity = mean(Humidity)),
                        K1 %>% 
                          mutate(Time = hour(Time)) %>%
                          group_by(Time) %>%
                          summarize(Temperature = mean(Temperature)))

#view column names in datasets
names(Hourlykirk)

#Use this to clean up the columns and delete the "."
names(Hourlykirk)<-str_replace_all(names(Hourlykirk), "\\.", "")

#view the class type of each column (type of data) - these are all numeric or integer so no need to change it 
sapply(Hourlykirk, class)


getwd() #"C:/Users/Shomari/Desktop/Dissertation/Sensor data/01-11-21_30-06-22"

#write new xls file and save to computer - edit and reload these below
write_xlsx(Hourlykirk, "C:/Users/Shomari/Desktop/Dissertation/Sensor data/01-11-21_30-06-22/HourlyKirk.xls")

#Load in the new csv files with weekly and monthly averages
Hourlykirk <- read_csv("HourlyKirk.csv")

#Life sciences building
HourlyLife <- data.frame(L1 %>% 
                           mutate(Time = hour(Time)) %>%
                           group_by(Time) %>%
                           summarize(PM1 = mean(PM1)),
                         L1 %>% 
                           mutate(Time = hour(Time)) %>%
                           group_by(Time) %>%
                           summarize(PM25 = mean(PM25)),
                         L1 %>% 
                           mutate(Time = hour(Time)) %>%
                           group_by(Time) %>%
                           summarize(PM10 = mean(PM10)),
                         L1 %>% 
                           mutate(Time = hour(Time)) %>%
                           group_by(Time) %>%
                           summarize(Humidity = mean(Humidity)),
                         L1 %>% 
                           mutate(Time = hour(Time)) %>%
                           group_by(Time) %>%
                           summarize(Temperature = mean(Temperature)))

#view column names in datasets
names(HourlyLife)

#Use this to clean up the columns and delete the "."
names(HourlyLife)<-str_replace_all(names(HourlyLife), "\\.", "")

#view the class type of each column (type of data) - these are all numeric or integer so no need to change it 
sapply(HourlyLife, class)


getwd() #"C:/Users/Shomari/Desktop/Dissertation/Sensor data/01-11-21_30-06-22"

#write new xls file and save to computer - edit and reload these below
write_xlsx(HourlyLife, "C:/Users/Shomari/Desktop/Dissertation/Sensor data/01-11-21_30-06-22/HourlyLife.xls")

#Load in the new csv files with weekly and monthly averages
HourlyLife <- read_csv("HourlyLife.csv")

#############################################################################################################################





##############################################################################################################################
#Making scatter plots of daily and weekly trends in data 

#scatter plot of daily averaged concentrations across the entire sample period
#Kirkdale station
plot2 <-    ggplot(Dailykirk, aes()) +
  geom_line(aes(x=Date, y=PM10, colour='PM10'))+
  geom_line(aes(x=Date, y=PM25, colour='PM25'))+
  geom_line(aes(x=Date, y=PM1, colour='PM1'))+
  xlab("Month")+
  ylab(mean~PM~concentration~(mu * g / m^3))+
  ylim(0, 200)+
  theme(legend.position = 'bottom', panel.background = element_rect(fill=NA, colour = "black"),
        panel.grid.minor = element_blank(), panel.grid.major=element_line(colour="white"))+
  annotate("text", x=(c("2022-06-31")), y = 180, label = "A)", size=5)

#Life sciences building
plot1 <- ggplot(DailyLife, aes()) +
  geom_line(aes(x=Date, y=PM10, colour='PM10'))+
  geom_line(aes(x=Date, y=PM25, colour='PM25'))+
  geom_line(aes(x=Date, y=PM1, colour='PM1'))+
  xlab("Month")+
  ylab(mean~PM~concentration~(mu * g / m^3))+
  ylim(0, 175)+
  theme(legend.position = 'bottom', panel.background = element_rect(fill=NA, colour = "black"),
        panel.grid.minor = element_blank(), panel.grid.major=element_line(colour="white"))+
 annotate("text", x=(c("2022-06-31")), y = 200, label = "A)", size=5)

require(gridExtra)
grid.arrange(plot1, plot2, ncol=2) #Puts both plots side by side

#scatter plot of average daily particle concentrations (hours of the day)
#Kirkdale station
kirk <- ggplot(Hourlykirk, aes())+
  geom_line(aes(x=Time, y=PM10, colour='PM10'))+
  geom_line(aes(x=Time, y=PM25, colour='PM25'))+
  geom_line(aes(x=Time, y=PM1, colour='PM1'))+
  xlab("Hour of the day")+
  ylab(mean~PM~concentration~(mu * g / m^3))+
  theme(legend.position = 'bottom', panel.background = element_rect(fill=NA, colour = "black"),
        panel.grid.minor = element_blank(), panel.grid.major=element_line(colour="white"))

#Life sciences building
life <- ggplot(HourlyLife, aes())+
  geom_line(aes(x=Time, y=PM10, colour='PM10'))+
  geom_line(aes(x=Time, y=PM25, colour='PM25'))+
  geom_line(aes(x=Time, y=PM1, colour='PM1'))+
  xlab("Hour of the day")+
  ylab(mean~PM~concentration~(mu * g / m^3))+
  theme(legend.position = 'bottom', panel.background = element_rect(fill=NA, colour = "black"),
        panel.grid.minor = element_blank(), panel.grid.major=element_line(colour="white"))
#annotate("text", x=25, y = 150, label = "A)", size=5)


require(gridExtra)
grid.arrange(life, kirk, ncol=2)


####################################################################
#Making scatter plots of weekly trends in data

#Scatter plot of weekly trends: days of the week
#Kirkdale
kirkw <- ggplot(Dailykirk, aes()) +
  geom_line(aes(x=Weekday, y=WPM10, colour='PM10'))+
  geom_line(aes(x=Weekday, y=WP25, colour='PM25'))+
  geom_line(aes(x=Weekday, y=WPM1, colour='PM1'))+
  xlab("Days of the week")+
  ylab(mean~PM~concentration~(mu * g / m^3))+
  theme(legend.position = 'bottom', panel.background = element_rect(fill=NA, colour = "black"),
        panel.grid.minor = element_blank(), panel.grid.major=element_line(colour="white"))
#annotate("text", x=(c("2022-06-31")), y = 200, label = "A)", size=5)

names(Dailykirk)

#Life sciences building
lifew <- ggplot(DailyLife, aes()) +
  geom_line(aes(x=Weekday, y=WPM10, colour='PM10'))+
  geom_line(aes(x=Weekday, y=WP25, colour='PM25'))+
  geom_line(aes(x=Weekday, y=WPM1, colour='PM1'))+
  xlab("Days of the week")+
  ylim(0, 100)+
  ylab(mean~PM~concentration~(mu * g / m^3))+
  theme(legend.position = 'bottom', panel.background = element_rect(fill=NA, colour = "black"),
        panel.grid.minor = element_blank(), panel.grid.major=element_line(colour="white"))
#annotate("text", x=(c("2022-06-31")), y = 200, label = "A)", size=5)

names(Dailykirk)

require(gridExtra)
grid.arrange(lifew, kirkw, ncol=2)

####################################################################
#Making scatter plots of monthly trends in data

#Converting data to a factor
Dailykirk$Month <- as.factor(Dailykirk$Month)
DailyLife$Month <- as.factor(DailyLife$Month)

#scatter plot of average monthly particle concentrations
#Kirkdale
#ggplot(Dailykirk, aes())+
  #geom_line(aes(x=factor(Month1, level = c('November', 'December', 'January', 'February', 'March', 'April', 'May', 'June')), y=MPM10, colour='PM10'))+
  #geom_line(aes(x=factor(Month1, level = c('November', 'December', 'January', 'February', 'March', 'April', 'May', 'June')), y=MP25, colour='PM25'))+
  #geom_line(aes(x=factor(Month1, level = c('November', 'December', 'January', 'February', 'March', 'April', 'May', 'June')), y=MPM1, colour='PM1'))+
  #xlab("Months")+
  #ylim(0, 75)+
  #ylab(mean~PM~concentration~(mu * g / m^3))+
  #theme(legend.position = 'bottom', panel.background = element_rect(fill=NA, colour = "black"),
  #      panel.grid.minor = element_blank(), panel.grid.major=element_line(colour="white"))

#names(Dailykirk)
#names(Dailykirk)<-str_replace_all(names(Dailykirk), "\\.", "")

#Kirkdale
ggplot(Dailykirk, aes()) +
  geom_line(aes(x=Month, y=MPM10, colour='PM10'))+
  geom_line(aes(x=Month, y=MP25, colour='PM25'))+
  geom_line(aes(x=Month, y=MPM1, colour='PM1'))+
  xlab("Months of the year")+
  ylim(0, 75)+
  ylab(mean~PM~concentration~(mu * g / m^3))+
  theme(legend.position = 'bottom', panel.background = element_rect(fill=NA, colour = "black"),
        panel.grid.minor = element_blank(), panel.grid.major=element_line(colour="white"))


#Life sciences building
#ggplot(DailyLife, aes())+
  #geom_line(aes(x=factor(Month, level = c('11', '12', '1', '2', '3', '4', '5', '6')), y=MPM10, colour='PM10'))+
  #geom_line(aes(x=factor(Month, level = c('11', '12', '1', '2', '3', '4', '5', '6')), y=MP25, colour='PM25'))+
  #geom_line(aes(x=factor(Month, level = c('11', '12', '1', '2', '3', '4', '5', '6')), y=MPM1, colour='PM1'))+
  #xlab("Months")+
  #ylab(mean~PM~concentration~(mu * g / m^3))+
  #theme(legend.position = 'bottom', panel.background = element_rect(fill=NA, colour = "black"),
  #     panel.grid.minor = element_blank(), panel.grid.major=element_line(colour="white"))

#Life sciences building
ggplot(DailyLife, aes()) +
  geom_line(aes(x=Month, y=MPM10, colour='PM10'))+
  geom_line(aes(x=Month, y=MP25, colour='PM25'))+
  geom_line(aes(x=Month, y=MPM1, colour='PM1'))+
  xlab("Months of the year")+
  ylim(0, 150)+
  ylab(mean~PM~concentration~(mu * g / m^3))+
  theme(legend.position = 'bottom', panel.background = element_rect(fill=NA, colour = "black"),
        panel.grid.minor = element_blank(), panel.grid.major=element_line(colour="white"))

names(DailyRoxby)
names(DailyRoxby)<-str_replace_all(names(DailyRoxby), "\\.", "")


#################################################################################################
#making scatter plots of seasonal data (seasonal data made lower down in boxplot section)

#Kirkdale
#Spring
kirks <- ggplot(spring, aes()) +
  geom_line(aes(x=Date, y=PM10, colour='PM10'))+
  geom_line(aes(x=Date, y=PM25, colour='PM25'))+
  geom_line(aes(x=Date, y=PM1, colour='PM1'))+
  xlab("Spring months")+
  ylab(mean~PM~concentration~(mu * g / m^3))+
  theme(legend.position = 'bottom', panel.background = element_rect(fill=NA, colour = "black"),
        panel.grid.minor = element_blank(), panel.grid.major=element_line(colour="white"))

#Winter
kirkwt <- ggplot(winter, aes()) +
  geom_line(aes(x=Date, y=PM10, colour='PM10'))+
  geom_line(aes(x=Date, y=PM25, colour='PM25'))+
  geom_line(aes(x=Date, y=PM1, colour='PM1'))+
  xlab("Winter months")+
  ylim(0, 150)+  # removed extreme value (~400)
  ylab(mean~PM~concentration~(mu * g / m^3))+
  theme(legend.position = 'bottom', panel.background = element_rect(fill=NA, colour = "black"),
        panel.grid.minor = element_blank(), panel.grid.major=element_line(colour="white"))

require(gridExtra)
grid.arrange(kirkwt, kirks, ncol=2)

#Life sciences building
#Spring
lifes <- ggplot(Spring1, aes()) +
  geom_line(aes(x=Date, y=PM10, colour='PM10'))+
  geom_line(aes(x=Date, y=PM25, colour='PM25'))+
  geom_line(aes(x=Date, y=PM1, colour='PM1'))+
  xlab("Spring months")+
  ylab(mean~PM~concentration~(mu * g / m^3))+
  theme(legend.position = 'bottom', panel.background = element_rect(fill=NA, colour = "black"),
        panel.grid.minor = element_blank(), panel.grid.major=element_line(colour="white"))

#Winter
lifewt <- ggplot(Winter1, aes()) +
  geom_line(aes(x=Date, y=PM10, colour='PM10'))+
  geom_line(aes(x=Date, y=PM25, colour='PM25'))+
  geom_line(aes(x=Date, y=PM1, colour='PM1'))+
  xlab("Winter months")+
  ylim(0, 150)+  # removed extreme value (~400)
  ylab(mean~PM~concentration~(mu * g / m^3))+
  theme(legend.position = 'bottom', panel.background = element_rect(fill=NA, colour = "black"),
        panel.grid.minor = element_blank(), panel.grid.major=element_line(colour="white"))

require(gridExtra)
grid.arrange(lifewt, lifes, ncol=2)

#winters and springs on one graph

require(gridExtra)
grid.arrange(lifewt, kirkwt, ncol=2) #winter months

require(gridExtra)
grid.arrange(lifes, kirks, ncol=2) #Spring months


####################################################################
#Making box plots of seasonal trends in data

#Winter months
#converting data to numeric
#Dailykirk$Month <- as.numeric(Dailykirk$Month)
#Winter <- filter(Dailykirk, Month <3)
#Winter1 <- filter(Dailykirk, Month >6)

#Merging two dataframes
#W <- rbind(Winter, Winter1)

#filter the data instead of making and merging data frames
#Kirkdale
#winter months
Dailykirk$Month <- as.numeric(Dailykirk$Month)
winter <- filter(Dailykirk, Month %in% c("11", "12", "1","2"))

#Spring months
#converting data to numeric
Dailykirk$Month <- as.numeric(Dailykirk$Month)
spring <- filter(Dailykirk, Month %in% c("3", "4", "5","6"))

#Life sciences building
#winter months
DailyLife$Month <- as.numeric(DailyLife$Month)
Winter1 <- filter(DailyLife, Month %in% c("11", "12", "1","2"))

#Spring months
#converting data to numeric
DailyLife$Month <- as.numeric(DailyLife$Month)
Spring1 <- filter(DailyLife, Month %in% c("3", "4", "5","6"))

#Make box plot
#Kirkdale
Boxk <- ggplot(Dailykirk, aes(x=factor(Season, level=c('Winter', 'Spring')), y=MPM10, rm.na=TRUE))+
  geom_boxplot()+
  xlab("Season")+
  ylab(mean~PM~concentration~(mu * g / m^3))+
  ylim(20, 100)+
  theme(legend.position = 'bottom', panel.background = element_rect(fill=NA, colour = "black"),
        panel.grid.minor = element_blank(), panel.grid.major=element_line(colour="white"))
mean(winter$PM10) #48.76
mean(spring$PM10) #33.15
max(winter$PM10) #557.84
max(spring$PM10) #181.41
min(winter$PM10) #8.90
min(spring$PM10) #10.12
median(winter$PM10) #41.75
median(spring$PM10) #27.42
sd(winter$PM10) #50.54
sd(spring$PM10) #20.96
quantile(winter$PM10) #0%:8.903492 25%: 29.908065 50%: 41.753033 75%: 57.421952 100%:557.842321 
quantile(spring$PM10) #0%:10.11826 25%: 21.36846  50%: 27.41855 75%: 38.36842 100%:181.41404
getmode(winter$PM10)#47.70
getmode(spring$PM10)#44.46

mean(winter$PM25) #17.66
mean(spring$PM25) #13.00

mean(winter$PM1) #7.27
mean(spring$PM1) #6.43

#Life sciences building
Boxl <- ggplot(DailyLife, aes(x=factor(Season, level=c('Winter', 'Spring')), y=MPM10, rm.na=TRUE))+
  geom_boxplot()+
  xlab("Season")+
  ylab(mean~PM~concentration~(mu * g / m^3))+
  theme(legend.position = 'bottom', panel.background = element_rect(fill=NA, colour = "black"),
        panel.grid.minor = element_blank(), panel.grid.major=element_line(colour="white"))
mean(Winter1$PM10) #59.62
mean(Spring1$PM10) #30.27
max(Winter1$PM10) #1674.69
max(Spring1$PM10) #152.65
min(Winter1$PM10) #8.16
min(Spring1$PM10) #10.03
median(Winter1$PM10) #39.39
median(Spring1$PM10) #25.08
sd(Winter1$PM10) #157.289
sd(Spring1$PM10) #17.97
quantile(Winter1$PM10) #0%:8.162285  25%: 28.444183  50%: 39.399081 75%: 51.043620 100%:1674.692726 
quantile(Spring1$PM10) #0%:10.02844 25%: 19.61641  50%: 25.08424 75%: 35.03871 100%:152.65073 
getmode(Winter1$PM10)#31.03
getmode(Spring1$PM10)#34.72

mean(Winter1$PM25) #16.97
mean(Spring1$PM25) #12.56

mean(Winter1$PM1) #5.89
mean(Spring1$PM1) #5.78

#do basic stats_ put in table to accompany box plot and scatter plot 

require(gridExtra)
grid.arrange(Boxl, Boxk, ncol=2) #putting plots side by side

##################################################################################
#Box plots: 3 for each site (particle concentrations, comparing deprivation?)
#use dailykirk, dailylife to make boxplots. Put all into one spread sheet with columns labeled location and particulate matter. 
#May be worth doing individual ones for each site and displaying them side by side to show the differences in location. 
#In that case you'd have 6 columns (separated in 2s) labeled rpm and rconcentrartion, kpm, etc. In the code replace season with rpm and the y value with rconcentration

B1 <- read_csv("Concentration.csv")
#Kirkdale
kBox <- ggplot(B1, aes(x=factor(KPM, level=c('1', '25', '10')), y=kconcentration, rm.na=TRUE))+
  geom_boxplot()+
  xlab("Particulate matter")+
  ylab(mean~PM~concentration~(mu * g / m^3))+
  ylim(0, 200)+ #excluded a point that was massive ~600
  theme(legend.position = 'bottom', panel.background = element_rect(fill=NA, colour = "black"),
        panel.grid.minor = element_blank(), panel.grid.major=element_line(colour="white"))+
annotate("text", x=10, y = 150, label = "A)", size=5) #if doesnt work just put a text box on it
sapply(B1, class)

#Life sciences building 
lBox <- ggplot(B1, aes(x=factor(LPM, level=c('1', '25', '10')), y=Lconcentration, rm.na=TRUE))+
  geom_boxplot()+
  xlab("Particulate matter")+
  ylab(mean~PM~concentration~(mu * g / m^3))+
  ylim(0, 200)+ #excluded 2 points that were massive ~1700 and ~600
  theme(legend.position = 'bottom', panel.background = element_rect(fill=NA, colour = "black"),
        panel.grid.minor = element_blank(), panel.grid.major=element_line(colour="white"))+
annotate("text", x=10, y = 150, label = "B)", size=5) #if doesnt work just put a text box on it
sapply(B1, class)
require(gridExtra)
grid.arrange(lBox, kBox, ncol=2) #putting plots side by side
####################################################################
#Basic stats: average, mean, mode, interquartile range, etc

#do on dialykirk
#get values and write them manually in a table

#Create a mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#Stats for Kirkdale
mean(Dailykirk$PM1, na.rm=TRUE) #6.849378
max(Dailykirk$PM1, na.rm=TRUE) #39.289
min(Dailykirk$PM1, na.rm=TRUE) #0.68
median(Dailykirk$PM1, na.rm=TRUE) #4.659537
sd(Dailykirk$PM1, na.rm=TRUE) #6.63
quantile(Dailykirk$PM1, na.rm=TRUE) # 0%:0.6838409 25%: 2.4779451 50%: 4.6595370 75%: 8.5401071 100%:39.2892656 (only take the 25% and 75%)
getmode(Dailykirk$PM1) #2.038632

mean(Dailykirk$PM25, na.rm=TRUE) #15.32448
max(Dailykirk$PM25, na.rm=TRUE) #76.71
min(Dailykirk$PM25, na.rm=TRUE) #2.41
median(Dailykirk$PM25, na.rm=TRUE) #12.78
sd(Dailykirk$PM25, na.rm=TRUE) #10.296
quantile(Dailykirk$PM25, na.rm=TRUE) # 0%:2.406371 25%: 7.982504 50%: 12.775630 75%: 19.190027 100%:76.710862
getmode(Dailykirk$PM25) #12.12128

mean(Dailykirk$PM10, na.rm=TRUE) #40.93
max(Dailykirk$PM10, na.rm=TRUE) #557.84
min(Dailykirk$PM10, na.rm=TRUE) #8.90
median(Dailykirk$PM10, na.rm=TRUE) #35.21
sd(Dailykirk$PM10, na.rm=TRUE) #39.34
quantile(Dailykirk$PM10, na.rm=TRUE) # 0%:8.903492 25%: 23.855948 50%: 35.213703 75%:48.758996 100%:557.842321
getmode(Dailykirk$PM10) #47.70065

#Stats for Life sciences building
mean(DailyLife$PM1, na.rm=TRUE) #5.84
max(DailyLife$PM1, na.rm=TRUE) #46.79
min(DailyLife$PM1, na.rm=TRUE) #0.64
median(DailyLife$PM1, na.rm=TRUE) #3.34
sd(DailyLife$PM1, na.rm=TRUE) #6.94
quantile(DailyLife$PM1, na.rm=TRUE) # 0%:0.6431731 25%: 2.1037581 50%: 3.3419264 75%:  6.0946171 100%:46.7876697 (only take the 25% and 75%)
getmode(DailyLife$PM1) #1.275

mean(DailyLife$PM25, na.rm=TRUE) #14.76
max(DailyLife$PM25, na.rm=TRUE) #155.79
min(DailyLife$PM25, na.rm=TRUE) #2.45
median(DailyLife$PM25, na.rm=TRUE) #11.56
sd(DailyLife$PM25, na.rm=TRUE) #13.48
quantile(DailyLife$PM25, na.rm=TRUE) # 0%:2.447402 25%: 7.499803  50%: 11.559378  75%: 17.439872 100%:155.792697 
getmode(DailyLife$PM25) #7.64

mean(DailyLife$PM10, na.rm=TRUE) #62.05631
max(DailyLife$PM10, na.rm=TRUE) #4742.71
min(DailyLife$PM10, na.rm=TRUE) #6.961893
median(DailyLife$PM10, na.rm=TRUE) #26.17509
sd(DailyLife$PM10, na.rm=TRUE) #320.8599
quantile(DailyLife$PM10, na.rm=TRUE) # 0%: 8.162285 25%: 22.094237 50%: 31.113120 75%:44.939962 100%:1674.692726 
getmode(DailyLife$PM10) #31.03


####################################################################
#QQ plots, F test, anova, histogram, fourier, t-test, regression, and correlations 
#(between average particulate matter between each site, between deprivation classification, between temperature and humidity)

#t-test: compare the sites to each other (pm) (daily and hourly)
#anova: compare the sites to each other (pm) (daily and hourly)
#qq plots to check its normality
#f-test/histogram to compare variance of particles in each site (whether its normal and the skew on it)

#QQ plots to check for normality (do for daily and hourly data)
ggqqplot(Dailykirk$PM1)
ggqqplot(Dailykirk$PM25)
ggqqplot(Dailykirk$PM10)

ggqqplot(Hourlykirk$PM1) #normal
ggqqplot(Hourlykirk$PM25) #normal
ggqqplot(Hourlykirk$PM10) #kinda normal

ggqqplot(DailyLife$PM1)
ggqqplot(DailyLife$PM25)
ggqqplot(DailyLife$PM10)

ggqqplot(HourlyLife$PM1) #normal
ggqqplot(HourlyLife$PM25)#normal
ggqqplot(HourlyLife$PM10)#normal

ggqqplot(Dailykirk$Temperature) #normal
ggqqplot(Dailykirk$Humidity) #normal

ggqqplot(Hourlykirk$Temperature) #normal
ggqqplot(Hourlykirk$Humidity) #normal

ggqqplot(DailyLife$Temperature) #normal
ggqqplot(DailyLife$Humidity) #normal

ggqqplot(HourlyLife$Temperature) #normal
ggqqplot(HourlyLife$Humidity) #normal

##################################################################################################################
#log data
#Kirkdale station

Dailykirk$logPM1 <- log(Dailykirk$PM1)
Dailykirk$logPM25 <- log(Dailykirk$PM25)
Dailykirk$logPM10 <- log(Dailykirk$PM10)

#Life sciences building
DailyLife$logPM1 <- log(DailyLife$PM1)
DailyLife$logPM25 <- log(DailyLife$PM25)
DailyLife$logPM10 <- log(DailyLife$PM10)



###################################################################################################################
#HOURLY DATA WILL SHOW SURING THE DAY
#anova #compare the pm data between the different pm

#comparing the concentration of pm vs days of the week (i.e Monday Tuesday Wednesday, etc)
#Kirkdale station
Kirk.PM1.anova <- aov (WPM1 ~ Weekday, data = Dailykirk) 
summary(Kirk.PM1.anova) #this needs to be the wpm1 data. 
#             Df Sum Sq Mean Sq F value Pr(>F)    
#Weekday       1  116.2   116.2   129.8 <2e-16 ***
#  Residuals   239  213.9     0.9 

Kirk.PM25.anova <- aov (WP25 ~ Weekday, data = Dailykirk) 
summary(Kirk.PM25.anova)
#             Df Sum Sq Mean Sq F value Pr(>F)    
#Weekday       1  225.5  225.49   124.8 <2e-16 ***
#  Residuals   239  431.8    1.81                      

Kirk.PM10.anova <- aov (WPM10 ~ Weekday, data = Dailykirk) 
summary(Kirk.PM10.anova)
#             Df Sum Sq Mean Sq F value  Pr(>F)   
#Weekday       1    169   169.0   11.04 0.00103 **
#  Residuals   239   3658    15.3                

#Life sciences building

Life.PM1.anova <- aov (WPM1 ~ Weekday, data = DailyLife) 
summary(Life.PM1.anova)
#Df Sum Sq Mean Sq F value Pr(>F)    
#Weekday       1  119.3  119.27   178.7 <2e-16 ***
#  Residuals   239  159.5    0.67                   

Life.PM25.anova <- aov (WP25 ~ Weekday, data = DailyLife) 
summary(Life.PM25.anova)
#Df Sum Sq Mean Sq F value  Pr(>F)    
#Weekday       1   81.1   81.11   23.55 2.2e-06 ***
#  Residuals   239  823.2    3.44                    

Life.PM10.anova <- aov (WPM10 ~ Weekday, data = DailyLife) 
summary(Life.PM10.anova)
#Df Sum Sq Mean Sq F value   Pr(>F)    
#Weekday       1   6429    6429   27.86 2.92e-07 ***
#  Residuals   239  55141     231      

#comparing the concentration of pm vs months of the year
#Kirkdale station
MKirk.PM1.anova <- aov (MPM1 ~ Month, data = Dailykirk) 
summary(MKirk.PM1.anova) #this needs to be the wpm1 data. 
#             Df Sum Sq Mean Sq F value Pr(>F)    
#Weekday       1  116.2   116.2   129.8 <2e-16 ***
#  Residuals   239  213.9     0.9 

MKirk.PM25.anova <- aov (MP25 ~ Month, data = Dailykirk) 
summary(MKirk.PM25.anova)
#Df Sum Sq Mean Sq F value Pr(>F)
#Month         1     23   22.78   0.918  0.339
#Residuals   239   5931   24.81                       

MKirk.PM10.anova <- aov (MPM10 ~ Month, data = Dailykirk) 
summary(MKirk.PM10.anova)
#Df Sum Sq Mean Sq F value Pr(>F)  
#Month         1    738   738.0   5.578  0.019 *
#  Residuals   239  31620   132.3  

#Life sciences building
MLife.PM1.anova <- aov (MPM1 ~ Month, data = DailyLife) 
summary(MLife.PM1.anova)
#Df Sum Sq Mean Sq F value Pr(>F)  
#Month         1   33.7   33.73    4.59 0.0332 *
#  Residuals   239 1756.2    7.35                    

MLife.PM25.anova <- aov (MP25 ~ Month, data = DailyLife) 
summary(MLife.PM25.anova)
#Df Sum Sq Mean Sq F value Pr(>F)  
#Month         1    121  120.84   4.832 0.0289 *
#  Residuals   239   5977   25.01                                

MLife.PM10.anova <- aov (MPM10 ~ Month, data = DailyLife) 
summary(MLife.PM10.anova)
#Df Sum Sq Mean Sq F value Pr(>F)    
#Month         1  46707   46707   79.22 <2e-16 ***
#  Residuals   239 140922     590    

#Comparing the two sites to see if they are statistically different (make spreadsheet of the particulate matter data for the two sites on one spread sheet and import it)
#Read in data
Both_sites <- read_csv("Both_sites_all data.csv")

Both.PM10.anova <- aov (LPM10 ~ KPM10, data = Both_sites) 
summary(Both.PM10.anova)  
#Df  Sum Sq Mean Sq F value Pr(>F)    
#KPM10         1 2431128 2431128   962.8 <2e-16 ***
#  Residuals   239  603484    2525

Both.WPM10.anova <- aov (LWPM10 ~ KWPM10, data = Both_sites) 
summary(Both.WPM10.anova)
#Df Sum Sq Mean Sq F value Pr(>F)    
#KWPM10        1  52970   52970    1472 <2e-16 ***
#  Residuals   239   8600      36                        

Both.MPM10.anova <- aov (LMPM10 ~ KMPM10, data = Both_sites) 
summary(Both.MPM10.anova)
#Df Sum Sq Mean Sq F value Pr(>F)    
#KMPM10        1 145839  145839   834.1 <2e-16 ***
#  Residuals   239  41790     175  


#####################################################################################################################
#pairwise t-test
#comparing the concentration of pm vs days of the week (i.e monday tuesday wednesday, etc)
#Kirkdale station
pairwise.t.test(Dailykirk$WPM10, Dailykirk$Weekday, p.adjust.method = 'none') # make a concentration spreadsheet where 1, 2.5, and 10 are in one column and the associated weekday n teh other
#1      2      3      4      5      6     
#2 <2e-16 -      -      -      -      -     
#  3 <2e-16 <2e-16 -      -      -      -     
#  4 <2e-16 <2e-16 <2e-16 -      -      -     
#  5 <2e-16 <2e-16 <2e-16 <2e-16 -      -     
#  6 <2e-16 <2e-16 <2e-16 <2e-16 <2e-16 -     
#  7 <2e-16 <2e-16 <2e-16 <2e-16 <2e-16 <2e-16

#data:  Dailykirk$WPM10 and Dailykirk$Weekday   #This shows that the difference between all the days is significant (Monday being 1, Tuesday being 2, etc)
pairwise.t.test(Dailykirk$MPM10, Dailykirk$Month, p.adjust.method = 'none')
#1      2      3      4      5      6      7     
#2 <2e-16 -      -      -      -      -      -     
#  3 <2e-16 <2e-16 -      -      -      -      -     
#  4 <2e-16 <2e-16 <2e-16 -      -      -      -     
#  5 <2e-16 <2e-16 <2e-16 <2e-16 -      -      -     
#  6 <2e-16 <2e-16 <2e-16 <2e-16 <2e-16 -      -     
#  7 <2e-16 <2e-16 <2e-16 <2e-16 <2e-16 <2e-16 -     
#  8 <2e-16 <2e-16 <2e-16 <2e-16 <2e-16 <2e-16 <2e-16

#Life sciences building
pairwise.t.test(DailyLife$WPM10, DailyLife$Weekday, p.adjust.method = 'none')
#1      2      3      4      5      6     
#2 <2e-16 -      -      -      -      -     
#  3 <2e-16 <2e-16 -      -      -      -     
#  4 <2e-16 <2e-16 <2e-16 -      -      -     
#  5 <2e-16 <2e-16 <2e-16 <2e-16 -      -     
#  6 <2e-16 <2e-16 <2e-16 <2e-16 <2e-16 -     
#  7 <2e-16 <2e-16 <2e-16 <2e-16 <2e-16 <2e-16

pairwise.t.test(DailyLife$MPM10, DailyLife$Month, p.adjust.method = 'none')
#1      2      3      4      5      6      7     
#2 <2e-16 -      -      -      -      -      -     
#  3 <2e-16 <2e-16 -      -      -      -      -     
#  4 <2e-16 <2e-16 <2e-16 -      -      -      -     
#  5 <2e-16 <2e-16 <2e-16 <2e-16 -      -      -     
#  6 <2e-16 <2e-16 <2e-16 <2e-16 <2e-16 -      -     
#  7 <2e-16 <2e-16 <2e-16 <2e-16 <2e-16 <2e-16 -     
#  8 <2e-16 <2e-16 <2e-16 <2e-16 <2e-16 <2e-16 <2e-16

#Comparing the two sites
#During the week
pairwise.t.test(Both_sites$LWPM10, Both_sites$KWPM10, p.adjust.method = 'none')

#Between months
pairwise.t.test(Both_sites$LMPM10, Both_sites$KMPM10, p.adjust.method = 'none')

#Overall
pairwise.t.test(Both_sites$LPM10, Both_sites$KPM10, p.adjust.method = 'none')
#	Pairwise comparisons using t tests with pooled SD 


######################################################################################
#Histogram
#do one for each site
#Converting data to a factor
#Dailykirk$Month <- as.numeric(Dailykirk$Month)
#DailyLife$Month <- as.numeric(DailyLife$Month)

#Kirkdale
#ggplot(Dailykirk, aes()+
  #geom_histogram(aes(x=PM1))+
  #geom_histogram(aes(x=PM25))+
  #geom_histogram(aes(x=PM10))+
  #xlab("pARTICULATE MATTER")+
  #ylab(mean~PM~concentration~(mu * g / m^3))+
  #theme(legend.position = 'bottom', panel.background = element_rect(fill=NA, colour = "black"),
  #      panel.grid.minor = element_blank(), panel.grid.major=element_line(colour="white")))
#sapply(Dailykirk, class)

#Life sciences building
#ggplot(DailyLife, aes()+
         #geom_histogram(aes(x=PM1))+
         #geom_histogram(aes(x=PM25))+
         #geom_histogram(aes(x=PM10))+
         #xlab("pARTICULATE MATTER")+
         #ylab(mean~PM~concentration~(mu * g / m^3))+
         #theme(legend.position = 'bottom', panel.background = element_rect(fill=NA, colour = "black"),
        #panel.grid.minor = element_blank(), panel.grid.major=element_line(colour="white")))
#sapply(Dailykirk, class)


########################################################################################
#Correltaiton 
#cor.test(Dailykirk$Temperature, Dailykirk$PM1, method = 'spearman') # change to pearson if you decide to use that one

#make the spreadsheet of all the info of the both sites to compare them statically using this and anovas

#cor.test(Dailykirk$Temperature, Dailykirk$PM10, method = 'spearman')
#cor.test(Dailykirk$Temperature, Dailykirk$PM25, method = 'spearman')

#cor.test(Dailykirk$Temperature, Dailykirk$PM1, method = 'pearson')

#cor.test(Dailykirk$PM10, Dailykirk$MPM10, method = 'spearman')
######################################################################################
#regression

#stat_regline_equation(label.x=16, label.y=230, position = position_dodge(), aes(x=Temperature, y=PM10))+
#stat_cor(aes(x=Temperature, y=PM10, lbel=..rr..label...), label.x=20, lable.y=230)+  

#put this in scatter plot before xlab and ylab



######################################################################################
#Chemical analysis - SEM data


#Scatter plots of spectra

s1 <- read_csv("C:/Users/Shomari/OneDrive/Desktop/Dissertation/SEM data/Averages.csv")
names(s1)

par(mfrow=c(2,2)) #Plot them in a 2x2 matrix

#Silicates

S <-ggplot(s1, aes()) +
  geom_line(aes(x=SKeV, y=ScpseV, colour='Silicates'))+
  xlab("KeV")+
  ylab("cpseV")+
  theme(legend.position = 'bottom', panel.background = element_rect(fill=NA, colour = "black"),
        panel.grid.minor = element_blank(), panel.grid.major=element_line(colour="white"))+
  annotate("text", x=3, y = 4000, label = "Ag)", size=4)+
  annotate("text", x=1.74, y = 2400, label = "Si)", size=4)+
  annotate("text", x=0.52, y = 2100, label = "O)", size=4)+
  annotate("text", x=0.02, y = 1300, label = "C)", size=4)+
  annotate("text", x=20, y = 4000, label = "A)", size=4)

#Fly ash/coal ash

ggplot(s1, aes()) +
  geom_line(aes(x=FKeV, y=FcpseV, colour='Fly ash/coal ash'))+
  xlab("KeV")+
  ylab("cpseV")+
  theme(legend.position = 'bottom', panel.background = element_rect(fill=NA, colour = "black"),
        panel.grid.minor = element_blank(), panel.grid.major=element_line(colour="white"))+
  annotate("text", x=3, y = 4100, label = "Ag)", size=4)+
  annotate("text", x=1.74, y = 1800, label = "Si)", size=4)+
  annotate("text", x=1.48, y = 750, label = "Al)", size=2.5)+
  annotate("text", x=0.52, y = 2100, label = "O)", size=4)+
  annotate("text", x=1.04, y = 800, label = "Na)", size=2.5)+
  annotate("text", x=0.1, y = 1900, label = "C)", size=4)+
  annotate("text", x=20, y = 4000, label = "c)", size=4)

#Aluminosilicates

AS <-ggplot(s1, aes()) +
  geom_line(aes(x=ASKeV, y=AScpseV, colour='Aluminosilicates'))+
  xlab("KeV")+
  ylab("cpseV")+
  theme(legend.position = 'bottom', panel.background = element_rect(fill=NA, colour = "black"),
        panel.grid.minor = element_blank(), panel.grid.major=element_line(colour="white"))+
  annotate("text", x=3, y = 3000, label = "Ag)", size=4)+
  annotate("text", x=1.74, y = 3000, label = "Si)", size=4)+
  annotate("text", x=1.35, y = 2300, label = "Al)", size=4)+
  annotate("text", x=0.52, y = 4700, label = "O)", size=4)+
  annotate("text", x=0.1, y = 1500, label = "C)", size=4)+
  annotate("text", x=20, y = 5000, label = "B)", size=4)

require(gridExtra)
grid.arrange(S, AS, ncol=2) #Putting the plots side by side

#Calcium Carbonate

ggplot(s1, aes()) +
  geom_line(aes(x=CCKeV, y=CCcpseV, colour='Calcium Carbonate'))+
  xlab("KeV")+
  ylab("cpseV")+
  theme(legend.position = 'bottom', panel.background = element_rect(fill=NA, colour = "black"),
        panel.grid.minor = element_blank(), panel.grid.major=element_line(colour="white"))+
  annotate("text", x=3, y = 3900, label = "Ag)", size=4)+
  annotate("text", x=1.74, y = 800, label = "Si)", size=4)+
  annotate("text", x=4, y = 2600, label = "Ca)", size=4)+
  annotate("text", x=0.4, y = 1200, label = "O)", size=3)+
  annotate("text", x=0.1, y = 1500, label = "C)", size=4)+
  annotate("text", x=0.677, y = 1750, label = "F)", size=4)

#Biogenic

ggplot(s1, aes()) +
  geom_line(aes(x=BKeV, y=BcpseV, colour='Biogenic'))+
  xlab("KeV")+
  ylab("cpseV")+
  theme(legend.position = 'bottom', panel.background = element_rect(fill=NA, colour = "black"),
        panel.grid.minor = element_blank(), panel.grid.major=element_line(colour="white"))+
  annotate("text", x=3, y = 4400, label = "Ag)", size=4)+
  annotate("text", x=0.6, y = 1000, label = "O)", size=3)+
  annotate("text", x=0.01, y = 1000, label = "C)", size=3)+
  annotate("text", x=0.392, y = 4400, label = "N)", size=4)

#Soda

ggplot(s1, aes()) +
  geom_line(aes(x=SOKeV, y=SOcpseV, colour='Soda'))+
  xlab("KeV")+
  ylab("cpseV")+
  theme(legend.position = 'bottom', panel.background = element_rect(fill=NA, colour = "black"),
        panel.grid.minor = element_blank(), panel.grid.major=element_line(colour="white"))+
  annotate("text", x=3, y = 2600, label = "Ag)", size=4)+
  annotate("text", x=1.74, y = 1000, label = "Si)", size=4)+
  annotate("text", x=2.4, y = 1400, label = "S)", size=4)+
  annotate("text", x=0.4, y = 4800, label = "O)", size=4)+
  annotate("text", x=0.01, y = 2100, label = "C)", size=4)+
  annotate("text", x=1.04, y = 2000, label = "Na)", size=4)+
  annotate("text", x=3.59, y = 1100, label = "K)", size=4)+
  annotate("text", x=0.677, y = 2800, label = "F)", size=4)

#Iron Oxides

ggplot(s1, aes()) +
  geom_line(aes(x=IKeV, y=IcpseV, colour='Iron Oxides'))+
  xlab("KeV")+
  ylab("cpseV")+
  theme(legend.position = 'bottom', panel.background = element_rect(fill=NA, colour = "black"),
        panel.grid.minor = element_blank(), panel.grid.major=element_line(colour="white"))+
  annotate("text", x=3, y = 3600, label = "Ag)", size=4)+
  annotate("text", x=0.525, y = 900, label = "O)", size=4)+
  annotate("text", x=0.01, y = 2300, label = "C)", size=4)+
  annotate("text", x=6.4, y = 2800, label = "Fe)", size=4)

#Carbonaceous

ggplot(s1, aes()) +
  geom_line(aes(x=CSKeV, y=CScpseV, colour='Carbonaceous'))+
  xlab("KeV")+
  ylab("cpseV")+
  theme(legend.position = 'bottom', panel.background = element_rect(fill=NA, colour = "black"),
        panel.grid.minor = element_blank(), panel.grid.major=element_line(colour="white"))+
  annotate("text", x=3, y = 4500, label = "Ag)", size=4)+
  annotate("text", x=0.525, y = 7200, label = "O)", size=4)+
  annotate("text", x=0.01, y = 1800, label = "C)", size=4)


#Carbonate

ggplot(s1, aes()) +
  geom_line(aes(x=CBKeV, y=CBcpseV, colour='Carbonate'))+
  xlab("KeV")+
  ylab("cpseV")+
  theme(legend.position = 'bottom', panel.background = element_rect(fill=NA, colour = "black"),
        panel.grid.minor = element_blank(), panel.grid.major=element_line(colour="white"))+
  annotate("text", x=3, y = 3900, label = "Ag)", size=4)+
  annotate("text", x=0.525, y = 2800, label = "O)", size=4)+
  annotate("text", x=0.01, y = 1800, label = "C)", size=4)+
  annotate("text", x=3.7, y = 2500, label = "Ca)", size=4)

#Mineral dust/crustal

ggplot(s1, aes()) +
  geom_line(aes(x=MKeV, y=McpseV, colour='Mineral dust/crustal'))+
  xlab("KeV")+
  ylab("cpseV")+
  theme(legend.position = 'bottom', panel.background = element_rect(fill=NA, colour = "black"),
        panel.grid.minor = element_blank(), panel.grid.major=element_line(colour="white"))+
  annotate("text", x=3, y = 2300, label = "Ag)", size=4)+
  annotate("text", x=1.74, y = 5500, label = "Si)", size=4)+
  annotate("text", x=0.525, y = 3500, label = "O)", size=4)+
  annotate("text", x=0.01, y = 1500, label = "C)", size=4)+
  annotate("text", x=1.04, y = 1800, label = "Na)", size=4)+
  annotate("text", x=4, y = 1400, label = "Ca)", size=4)+
  annotate("text", x=0.677, y = 3000, label = "F)", size=4)

#Secondary inorganic particle

ggplot(s1, aes()) +
  geom_line(aes(x=SEKeV, y=SEcpseV, colour='Secondary inorganic particles'))+
  xlab("KeV")+
  ylab("cpseV")+
  theme(legend.position = 'bottom', panel.background = element_rect(fill=NA, colour = "black"),
        panel.grid.minor = element_blank(), panel.grid.major=element_line(colour="white"))+
  annotate("text", x=3, y = 4800, label = "Ag)", size=4)+
  annotate("text", x=0.3, y = 900, label = "N)", size=3)+
  annotate("text", x=0.525, y = 1900, label = "O)", size=4)+
  annotate("text", x=0.01, y = 1600, label = "C)", size=4)+
  annotate("text", x=1.2, y = 1500, label = "Na)", size=4)

#Silicon carbonate

ggplot(s1, aes()) +
  geom_line(aes(x=SCKeV, y=SCcpseV, colour='Silicon carbonate'))+
  xlab("KeV")+
  ylab("cpseV")+
  theme(legend.position = 'bottom', panel.background = element_rect(fill=NA, colour = "black"),
        panel.grid.minor = element_blank(), panel.grid.major=element_line(colour="white"))+
  annotate("text", x=3, y = 4100, label = "Ag)", size=4)+
  annotate("text", x=1.74, y = 1900, label = "Si)", size=4)+
  annotate("text", x=0.1, y = 2200, label = "O)", size=4)+
  annotate("text", x=0, y = 1700, label = "C)", size=4)+
  annotate("text", x=0.9, y = 2200, label = "Fe)", size=4)


par(mfrow = c(1, 1)) #Puts plotting parameters back to its original state

#annotate("text", x=(c("2022-06-31")), y = 180, label = "A)", size=5)


#####################################################################################
#Bar chart of particle frequency

Cfrequency <- read_csv("C:/Users/Shomari/Desktop/Test.csv")

#make it a data frame
#results <- data.frame(Cfrequency)
#names(results)
#ggplot(data = results, aes(x = Type.of.particle, y = Invisible.wind.factory)) +
  #geom_bar(stat = "identity", position = position_dodge(), alpha = 0.75, fill = "blue")  +
  #ylim(0,30) +
  #geom_text(aes(label = Roxby.building), fontface = "bold", vjust = 1.5,
            #position = position_dodge(.9), size = 4,) +
  #labs(x = "Particle", y = "Frequency\n", title = "Particle frequency at each site") +
  #theme(plot.title = element_text(hjust = 0.5), 
        #axis.title.x = element_text(face="bold", colour="red", size = 12),
        #axis.title.y = element_text(face="bold", colour="red", size = 12),
        #legend.title = element_text(face="bold", size = 10),
        #axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        #legend.position = 'bottom', panel.background = element_rect(fill=NA, colour = "black"),
        #panel.grid.minor = element_blank(), panel.grid.major=element_line(colour="white"))


particle <- read_csv("C:/Users/Shomari/OneDrive/Desktop/Dissertation/SEM data/parts.csv")
  #read_csv("C:/Users/Shomari/Desktop/parts.csv")

particle %>%
  filter(Particle == 'Aluminosilicate' |
          Particle == 'Biogenic' |
           Particle == 'Calcium carbonate' |
           Particle == 'Carbonaceaous' |
           Particle == 'Carbonate' |
           Particle == 'Fly ash/coal ash' |
           Particle == 'Iron oxide' |
           Particle == 'Mineral dust/crustal' |
           Particle == 'Secondary inorganic particle' |
           Particle == 'Silicate' |
           Particle == 'Silicon carbonate' |
           Particle == 'Soda') %>%
  drop_na(Building) %>%
  ggplot(aes(Particle, fill = Building))+
  geom_bar(position = "dodge",
           alpha = 0.5,
           )+
  scale_fill_manual(
    values = c("red", "light blue")
  )+
  labs(x = "Particle", y = "Frequency\n", title = "Particle frequency at each site") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="black", size = 12),
        axis.title.y = element_text(face="bold", colour="black", size = 12),
        legend.title = element_text(face="bold", size = 10),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = 'bottom', panel.background = element_rect(fill=NA, colour = "black"),
        panel.grid.minor = element_blank(), panel.grid.major=element_line(colour="white"))



##################################################################################
#Pie chart of mineral dust classification

#mineral <- read_csv("C:/Users/Shomari/Desktop/Minerals.csv")

#ggplot(particle, aes(x=factor(1), fill=Percentage))+
  #geom_bar(width = 1)+
  #coord_polar("y")+


#bp<- ggplot(mineral, aes(x="", y=Percentage, fill= Particle))+ # make bar plot to visualise data
  #geom_bar(width = 1, stat = "identity")
#bp #shows plot

#pie <- bp + coord_polar("y", start=0) #make pie chart
#pie #shows plot

#pie + scale_fill_brewer(palette="Blues")+ #changes color of pie chart using color palette
  theme_minimal()

###################################################################################
#Bar chart of particle sizes

#Roxby building

size <- read_csv("C:/Users/Shomari/OneDrive/Desktop/Dissertation/SEM data/sizes.csv")

#converting to a factor
size$Size <- as.factor(size$Size)

size %>%
  filter(Particle == 'Aluminosillicate' |
           Particle == 'Biogenic' |
           Particle == 'Calcium carbonate' |
           Particle == 'Carbonaceaous' |
           Particle == 'Carbonate' |
           Particle == 'Fly ash/coal ash' |
           Particle == 'Iron oxide' |
           Particle == 'Mineral dust/crustal' |
           Particle == 'Secondary inorganic particle' |
           Particle == 'Silicate' |
           Particle == 'Silicon carbonate' |
           Particle == 'Soda') %>%
  drop_na(Size) %>%
  ggplot(aes(Particle, fill = Size))+
  geom_bar(position = "dodge",
           alpha = 0.5,
  )+
  scale_fill_manual(
    values = c("red", "light blue")
  )+
  labs(x = "Particle", y = "Frequency\n", title = "Particle size at each site") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="black", size = 12),
        axis.title.y = element_text(face="bold", colour="black", size = 12),
        legend.title = element_text(face="bold", size = 10),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = 'bottom', panel.background = element_rect(fill=NA, colour = "black"),
        panel.grid.minor = element_blank(), panel.grid.major=element_line(colour="white"))
names(size)

#Life Sciences building
size <- read_csv("C:/Users/Shomari/OneDrive/Desktop/Dissertation/SEM data/sizes.csv")

#converting to a factor
size$Size1 <- as.factor(size$Size1)

size %>%
  filter(Particle1 == 'Aluminosillicate' |
           Particle1 == 'Biogenic' |
           Particle1 == 'Calcium carbonate' |
           Particle1 == 'Carbonaceaous' |
           Particle1 == 'Carbonate' |
           Particle1 == 'Fly ash/coal ash' |
           Particle1 == 'Iron oxide' |
           Particle1 == 'Mineral dust/crustal' |
           Particle1 == 'Secondary inorganic particle' |
           Particle1 == 'Silicate' |
           Particle1 == 'Silicon carbonate' |
           Particle1 == 'Soda') %>%
  drop_na(Size1) %>%
  ggplot(aes(Particle1, fill = Size1))+
  geom_bar(position = "dodge",
           alpha = 0.5,
  )+
  scale_fill_manual(
    values = c("red", "light blue")
  )+
  labs(x = "Particle", y = "Frequency\n", title = "Particle size at each site") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="black", size = 12),
        axis.title.y = element_text(face="bold", colour="black", size = 12),
        legend.title = element_text(face="bold", size = 10),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = 'bottom', panel.background = element_rect(fill=NA, colour = "black"),
        panel.grid.minor = element_blank(), panel.grid.major=element_line(colour="white"))
names(size)

#############################################################################################














































