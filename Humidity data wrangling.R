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
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop")  

combined_means <- monthly_means %>%
  group_by(month) %>%
  summarise(mean_value = mean(mean_value, na.rm = TRUE))  

ggplot() +
  geom_line(data = monthly_means, aes(x = month, y = mean_value, colour = house_id), linewidth = 0.7) +
  geom_line(data = combined_means, aes(x = month, y = mean_value), colour = "black", linetype = "dashed", linewidth = 0.8) +
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





h1_day_means <- humid1 %>%
  mutate(
    datetime = as.POSIXct(datetime),       
    day = as.Date(floor_date(datetime, "day"))  
  ) %>%
  group_by(day) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop")

h1_day_means_complete <- h1_day_means %>%
  complete(day = seq.Date(min(day), max(day), by = "day"),
           fill = list(mean_value = NA))

h1_day_means_complete[rowSums(is.na(h1_day_means_complete)) > 0,]

h1_plot <- ggplot() +
  geom_line(data = h1_day_means_complete, aes(x = day, y = mean_value))

h1_plot


h2_day_means <- humid2 %>%
  mutate(
    datetime = as.POSIXct(datetime),       
    day = as.Date(floor_date(datetime, "day"))  
  ) %>%
  group_by(day) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop")

h2_day_means_complete <- h2_day_means %>%
  complete(day = seq.Date(min(day), max(day), by = "day"),
           fill = list(mean_value = NA))

h2_day_means_complete[rowSums(is.na(h2_day_means_complete)) > 0,]

h2_plot <- ggplot() +
  geom_line(data = h2_day_means_complete, aes(x = day, y = mean_value))

h2_plot 
##CHECK DATA!


h3_day_means <- humid3 %>%
  mutate(
    datetime = as.POSIXct(datetime),       
    day = as.Date(floor_date(datetime, "day"))  
  ) %>%
  group_by(day) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop")

h3_day_means_complete <- h3_day_means %>%
  complete(day = seq.Date(min(day), max(day), by = "day"),
           fill = list(mean_value = NA))

h3_day_means_complete[rowSums(is.na(h3_day_means_complete)) > 0,]

h3_plot <- ggplot() +
  geom_line(data = h3_day_means_complete, aes(x = day, y = mean_value))

h3_plot


h4_day_means <- humid4 %>%
  mutate(
    datetime = as.POSIXct(datetime),       
    day = as.Date(floor_date(datetime, "day"))  
  ) %>%
  group_by(day) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop")

h4_day_means_complete <- h4_day_means %>%
  complete(day = seq.Date(min(day), max(day), by = "day"),
           fill = list(mean_value = NA))

h4_day_means_complete[rowSums(is.na(h4_day_means_complete)) > 0,]

h4_plot <- ggplot() +
  geom_line(data = h4_day_means_complete, aes(x = day, y = mean_value))

h4_plot


h5_day_means <- humid5 %>%
  mutate(
    datetime = as.POSIXct(datetime),       
    day = as.Date(floor_date(datetime, "day"))  
  ) %>%
  group_by(day) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop")

h5_day_means_complete <- h5_day_means %>%
  complete(day = seq.Date(min(day), max(day), by = "day"),
           fill = list(mean_value = NA))

h5_day_means_complete[rowSums(is.na(h5_day_means_complete)) > 0,]

h5_plot <- ggplot() +
  geom_line(data = h5_day_means_complete, aes(x = day, y = mean_value))

h5_plot

h6_day_means <- humid6 %>%
  mutate(
    datetime = as.POSIXct(datetime),       
    day = as.Date(floor_date(datetime, "day"))  
  ) %>%
  group_by(day) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop")

h6_day_means_complete <- h6_day_means %>%
  complete(day = seq.Date(min(day), max(day), by = "day"),
           fill = list(mean_value = NA))

h6_day_means_complete[rowSums(is.na(h6_day_means_complete)) > 0,]

h6_plot <- ggplot() +
  geom_line(data = h6_day_means_complete, aes(x = day, y = mean_value))

h6_plot


h7_day_means <- humid7 %>%
  mutate(
    datetime = as.POSIXct(datetime),       
    day = as.Date(floor_date(datetime, "day"))  
  ) %>%
  group_by(day) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop")

h7_day_means_complete <- h7_day_means %>%
  complete(day = seq.Date(min(day), max(day), by = "day"),
           fill = list(mean_value = NA))

h7_day_means_complete[rowSums(is.na(h7_day_means_complete)) > 0,]

h7_plot <- ggplot() +
  geom_line(data = h7_day_means_complete, aes(x = day, y = mean_value))

h7_plot


h8_day_means <- humid8 %>%
  mutate(
    datetime = as.POSIXct(datetime),       
    day = as.Date(floor_date(datetime, "day"))  
  ) %>%
  group_by(day) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop")

h8_day_means_complete <- h8_day_means %>%
  complete(day = seq.Date(min(day), max(day), by = "day"),
           fill = list(mean_value = NA))

h8_day_means_complete[rowSums(is.na(h8_day_means_complete)) > 0,]

h8_plot <- ggplot() +
  geom_line(data = h8_day_means_complete, aes(x = day, y = mean_value))

h8_plot

h9_day_means <- humid9 %>%
  mutate(
    datetime = as.POSIXct(datetime),       
    day = as.Date(floor_date(datetime, "day"))  
  ) %>%
  group_by(day) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop")

h9_day_means_complete <- h9_day_means %>%
  complete(day = seq.Date(min(day), max(day), by = "day"),
           fill = list(mean_value = NA))

h9_day_means_complete[rowSums(is.na(h9_day_means_complete)) > 0,]

h9_plot <- ggplot() +
  geom_line(data = h9_day_means_complete, aes(x = day, y = mean_value))

h9_plot


h10_day_means <- humid10 %>%
  mutate(
    datetime = as.POSIXct(datetime),       
    day = as.Date(floor_date(datetime, "day"))  
  ) %>%
  group_by(day) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop")

h10_day_means_complete <- h10_day_means %>%
  complete(day = seq.Date(min(day), max(day), by = "day"),
           fill = list(mean_value = NA))

h10_day_means_complete[rowSums(is.na(h10_day_means_complete)) > 0,]

h10_plot <- ggplot() +
  geom_line(data = h10_day_means_complete, aes(x = day, y = mean_value))

h10_plot
