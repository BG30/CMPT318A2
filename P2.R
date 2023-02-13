library(dplyr)
library(lubridate)
library(ggplot2)
library(zoo)

# Import data
getwd()
setwd("C:/Study/CMPT_318/A2/CMPT318A2")
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

# ggplot(df, aes(x = Date, y = Global_intensity_smoothed) ) + 
#   geom_point()


# Average for each smoothed week
average_smoothed_week <- summarise(df, Global_intensity_mean = mean(Global_intensity_smoothed))

ggplot(average_smoothed_week, aes(x = week_num, y = Global_intensity_mean) ) +
  geom_point()

# merge average_smoothed_week and df, df will contain the column of Global_intensity_mean
df <-
  merge(df, average_smoothed_week, by = "week_num")
df <- na.omit(df)

# get mean absolute deviation(MAD) from average_smoothed_week and save the score
df$MAD_score <-
   abs(df$Global_intensity_smoothed - df$Global_intensity_mean)

# rank all weeks by MAD score
ranked_weeks <-
  df[order(df$MAD_score, decreasing = TRUE), ]

# get the most and least anomalous week
most_anomalous_week <- ranked_weeks[1, "week_num"]
least_anomalous_week <- ranked_weeks[nrow(ranked_weeks), "week_num"]

# save date of average, least anomalous and most anomalous week
least_anomalous_week_data = subset(df, week_num == most_anomalous_week)
most_anomalous_week_data = subset(df, week_num == least_anomalous_week)
average_smoothed_week_data = subset(df, week_num == 6)

# create new data to put 3 of them (average, least anomalous and most anomalous week) together
three_week_data <- data.frame(most_anomalous_week_Gim = least_anomalous_week_data$Global_intensity_mean[0:10078], 
                              least_anomalous_week_Gim = most_anomalous_week_data$Global_intensity_mean[0:10078],
                              average_smoothed_week_Gim = average_smoothed_week_data$Global_intensity_mean[0:10078])
three_week_data$ID <- seq.int(nrow(three_week_data))

# three_week_data$m_smoothed <- 
#   rollmean(three_week_data$most_anomalous_week_Gim, k = 50, na.pad = TRUE)
# three_week_data$l_smoothed <- 
#   rollmean(three_week_data$least_anomalous_week_Gim, k = 50, na.pad = TRUE)
# three_week_data$a_smoothed <- 
#   rollmean(three_week_data$average_smoothed_week_Gim, k = 50, na.pad = TRUE)

# plot average smoothened week, least anomalous week, most anomalous week all together
ggplot(three_week_data, aes(x = ID)) +
  geom_line(aes(y = average_smoothed_week_Gim), color = "black") +
  geom_line(aes(y = least_anomalous_week_Gim), color = "blue") +
  geom_line(aes(y = most_anomalous_week_Gim), color = "red") +
  scale_y_continuous(limits = c(4, 6.5)) +
  scale_x_continuous(breaks = seq(0, 10000, 500))
  # geom_smooth(data = subset(three_week_data, week_num == most_anomalous_week), se = FALSE, color = "red") +
  # geom_smooth(data = subset(three_week_data, week_num == least_anomalous_week), se = FALSE, color = "blue") +
  # geom_smooth(data = subset(three_week_data, week_num == 2), se = FALSE, color = "green")

  # geom_smooth(data = subset(smooth_scores, week == which.min(score)), se = FALSE, color = "red") +
  # geom_smooth(data = subset(smooth_scores, week == which.max(score)), se = FALSE, color = "blue") +
  # geom_smooth(data = smooth_scores, se = FALSE, color = "black")
