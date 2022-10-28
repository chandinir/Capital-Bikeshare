library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(patchwork)
library(stargazer)

setwd("~/Documents/Documents/MSBA/Stats")
bikeshareData = read.csv("Capital Bike Sharing data by hour.csv")
  
#Question 1 What is the trend in overall bike demand over the months of the year? 
bikeshareData=bikeshareData %>%
  mutate(ShareDate = as.Date(dteday, "%Y-%m-%d"))

str(bikeshareData)
anyNA(bikeshareData)
hist(bikeshareData$cnt)

bikeshareQ1 = bikeshareData %>%
  mutate(month = month(ShareDate), year = year(ShareDate)) %>%
  group_by(month, year) %>%
  summarise(total = sum(cnt)) %>%
  arrange(year, month)

bikeshareQ1  %>%
  ggplot(aes(x = month,
             y = total, color = factor(year))) +
  geom_point() + 
  scale_x_continuous(n.breaks = 12) + 
  labs(x="Month",
       y="Total Bike Rentals",
       fill="", 
       title = "Total Bike Rentals per Month",
       subtitle = "for 2011 and 2012") +
  scale_y_continuous(labels = scales::comma) + 
  scale_color_discrete(name="Year",
                      breaks=c(2011, 2012),
                      labels=c("2011", "2012"))



#Question 2 The data science group at Capital bike share hypothesize that a. 
#There must be high demand during office timings. Early morning and late 
#evening can have different trends (cyclist) and low demand from 10:00pm to 
#4:00am. Do you agree? b. Registered users demand more bike on weekdays compared
#to the weekend or holiday. Do you agree?

#a
bikeshareQ2a = bikeshareData %>%
  group_by(hr, workingday) %>%
  summarise(total = sum(cnt)) 

bikeshareQ2a$WorkingDayText = ifelse(bikeshareQ2a$workingday==1, "Working Day", "Weekend")

bikeshareQ2a  %>%
  ggplot(aes(x = hr,
             y = total, color=WorkingDayText)) +
  geom_point() + 
  facet_wrap(~WorkingDayText,
             scales="free_x") +
  labs(x="Hour of Day",
       y="Total Bike Rentals",
       fill="", 
       title = "Total Bike Rentals per Hour",
       subtitle = "for Weekends and Work Days") +
  scale_y_continuous(labels = scales::comma) +
  scale_color_discrete(name="Type of Day")

#b
bikeshareQ2b = bikeshareData %>%
  group_by(workingday) %>%
  summarise(avgReg = mean(registered)) 

bikeshareQ2b$WorkingDayText = ifelse(bikeshareQ2b$workingday==1, "Working Day", "Weekend")

bikeshareQ2b  %>%
  ggplot(aes(x = as.factor(WorkingDayText),
             y = avgReg)) +
  geom_bar(stat='identity') +
  labs(x="Working Day",
       y="Total Bike Rentals",
       fill="", 
       title = "Total Bike Rentals",
       subtitle = "Working Days vs Weekends (where 1=working day and 0=weekend)")+
  scale_y_continuous(labels = scales::comma)

#Question 3Is there any relationship between season and bike rental? 
#Create a visualization displaying the relationship. 

bikeshareQ3 = bikeshareData %>%
  group_by(season) %>%
  summarise(avg = mean(cnt)) 

bikeshareQ3$SeasonName[1]="Winter"
bikeshareQ3$SeasonName[2]="Spring"
bikeshareQ3$SeasonName[3]="Summer"
bikeshareQ3$SeasonName[4]="Fall"

bikeshareQ3  %>%
  ggplot(aes(x = SeasonName,
             y = avg, fill=SeasonName)) +
  geom_bar(stat='identity') +
  labs(x="Season Name",
       y="Average Bike Rentals",
       fill="", 
       title = "Average Bike Rentals",
       subtitle = "per season") +
  scale_y_continuous(labels = scales::comma)+
  theme(legend.position="none")


#Question 4 What type of relationship do you see between weather and bike rental?
#Is the relationship the same for registered vs. casual users?

#Temp vs Total
bikeshareData %>%
  group_by(temp) %>%
  summarise(avg= mean(cnt)) %>%
  ggplot(aes(x = temp,
             y = avg)) +
  geom_point() +
  labs(x="Normalized Temperature",
       y="Average Bike Rentals",
       fill="", 
       title = "Average Bike Rentals",
       subtitle = "by Normalized Temperature") +
  scale_y_continuous(labels = scales::comma) +
  geom_abline(slope=bikeShareTempModel$coefficients[2],
              intercept=bikeShareTempModel$coefficients[1],
              color="blue",
              size=1)

#Feeling Temp vs Total
bikeshareData %>%
  group_by(atemp) %>%
  summarise(avg = mean(cnt)) %>%
  ggplot(aes(x = atemp,
             y = avg)) +
  geom_point() +
  labs(x="Normalized Feeling Temperature",
       y="Average Bike Rentals",
       fill="", 
       title = "Average Bike Rentals",
       subtitle = "by Normalized Feeling Temperature") +
  #theme_clean() +
  scale_y_continuous(labels = scales::comma) +
  geom_abline(slope=bikeShareAtempModel$coefficients[2],
              intercept=bikeShareAtempModel$coefficients[1],
              color="blue",
              size=1)

#casual users
p1 = bikeshareData %>%
  group_by(temp) %>%
  summarise(avg  = mean(casual)) %>%
    ggplot(aes(x = temp,
               y = avg)) +
      geom_point(color="#F8766D") +
  labs(x="Normalized Temperature",
       y="Avg Bike Rentals",
       fill="", 
       title = "Temp vs Avg Bike Rentals",
       subtitle = "for Casual Users") +
  scale_y_continuous(labels = scales::comma)

#registered users
p2 = bikeshareData %>%
  group_by(temp) %>%
  summarise(avg = mean(registered)) %>%
  ggplot(aes(x = temp,
             y = avg)) +
  geom_point(color="#619CFF") +
  labs(x="Normalized Temperature",
       y="",
       fill="", 
       title = "Temp vs Avg Bike Rentals",
       subtitle = " for Registered Users") +
  scale_y_continuous(labels = scales::comma)

p1 + p2

#Question 5 Fit a linear model predicting the total bike rental demand from daily
#temperature. What kind of insights can you generate? (make sure to write the 
#linear model and interpret it in the context of the data)

bikeshareModel = bikeshareData %>%
  group_by(temp) %>%
  summarise(avg = mean(cnt))

bikeShareTempModel = lm(avg~temp, data = bikeshareModel)
bikeShareTempModel
summary(bikeShareTempModel) 
str(bikeShareTempModel)

plot(bikeShareTempModel)

#Question 6 Fit another linear model predicting total daily bike rentals from 
#daily feeling temperature. Write the linear model, interpret the slope, etc. 
#Is the temperature or feeling temperature a better predictor of bike rentals? 

bikeshareModel2 = bikeshareData %>%
  group_by(atemp) %>%
  summarise(avg = mean(cnt))

bikeShareAtempModel = lm(avg~atemp, data = bikeshareModel2)
bikeShareAtempModel
summary(bikeShareAtempModel) 
str(bikeShareAtempModel)

stargazer(bikeShareTempModel, bikeShareAtempModel, type="text",
          dep.var.caption = "", dep.var.labels.include = F,
          report = "c*", df = F, model.numbers = F,
          keep.stat = c("ser","rsq","adj.rsq"),
          column.labels = c("Normalized Temp vs Avg","Feeling Temp vs Avg"))
