library(dplyr)
library(lubridate)
library(ggplot2)
library(zoo)

# Import data
getwd()
setwd("C:/Documents/School/6 Spring 2023/CMPT 318/G1/CMPT318A1")
df <- read.table("Group_Assignment_2_Dataset.txt", header = TRUE, sep = ",")
df <- na.omit(df)

# Format Date column as Date object
df$Date <- as.Date(df$Date, format = "%d/%m/%Y")

# Group by week
df$week_num <- strtoi(strftime(df$Date, format = "%V"))
df <- group_by(df, week_num)

# Smoothing Global intensity with a moving average with a window of 7

df$Global_intensity_smoothed <- 
    rollmean(df$Global_intensity, k = 7, na.pad = TRUE)

ggplot(df, aes(x = Date, y = Global_intensity_smoothed) ) +
  geom_point()


# Average for each smoothed week

average_smoothed_week <- summarise(df, Global_intensity_mean = mean(Global_intensity_smoothed))

ggplot(average_smoothed_week, aes(x = week_num, y = Global_intensity_mean) ) +
  geom_point()



