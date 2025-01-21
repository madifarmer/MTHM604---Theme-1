setwd("~/Masters/MTHM604/Theme_01/SensorData/Humi")

humid1 <- read.csv("06_humi_1643819.csv")
humid2 <- read.csv("09_humi_1643806s1642598.csv")
humid3 <- read.csv("11_humi_1643103s1642673.csv")
humid4 <- read.csv("13_humi_1643812.csv")
humid5 <- read.csv("16_humi_1643146.csv")
humid6 <- read.csv("28_humi_1643163s1643171.csv")
humid7 <- read.csv("29_humi_1643816.csv")
humid8 <- read.csv("34_humi_1643810.csv")
humid9 <- read.csv("58_humi_1643178.csv")
humid10 <- read.csv("85_humi_1643809.csv")


library(tidyverse)
library(lubridate)

humid_combined <- list(humid1, humid2, humid3, humid4, humid5, humid6, humid7, humid8, humid9, humid10) %>%
  bind_rows(.id = "house_id")

humid_clean <- humid_combined %>%
  mutate(
    house_id = paste0("house_", house_id),  
    datetime = as.POSIXct(datetime),       
    date = as.Date(datetime),              
    time = format(datetime, "%H:%M:%S"),  
    value = as.numeric(value)              
  ) %>%
  filter(complete.cases(.))


monthly_means <- humid_clean %>%
  mutate(month = floor_date(datetime, "month")) %>% 
  group_by(house_id, month) %>%
  summarize(mean_value = mean(value, na.rm = TRUE), .groups = "drop")  

combined_means <- monthly_means %>%
  group_by(month) %>%
  summarize(mean_value = mean(mean_value, na.rm = TRUE))  

ggplot() +
  geom_line(data = monthly_means, aes(x = month, y = mean_value, color = house_id), linewidth = 0.7) +
  geom_line(data = combined_means, aes(x = month, y = mean_value), color = "black", linetype = "dashed", linewidth = 0.8) +
  labs(
    x = "Month",
    y = "Mean Humidity",
    color = "House ID"
  ) +
  theme_minimal()


#9 starts at different time - find start date
start_date_house_9 <- humid_clean %>%
  filter(house_id == "house_9") %>%  
  summarize(start_date = min(datetime)) 

start_date_house_9
