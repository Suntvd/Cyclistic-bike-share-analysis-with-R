# Loading Packages

library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)

# Importing datasets

# Get a list of CSV file paths
file_paths <- list.files("./dataset", full.names = TRUE)

# Read each CSV file and save to dataframes list
dataframes <- lapply(file_paths, function(file_path) {
  read_csv(file_path)
})

# Combine dataframes into a single dataframe
cyclistic_data <- do.call(rbind, dataframes)

head(cyclistic_data)

str(cyclistic_data)

# Check for missing values
# Select the necessary data columns
cyclistic_data <-cyclistic_data %>% 
  select(ride_id, rideable_type, started_at, ended_at, start_station_name, end_station_name, member_casual)

colSums(is.na(cyclistic_data))

cyclistic_data %>% 
  filter(is.na(end_station_name))

# Check for duplicate values

any(duplicated(cyclistic_data$ride_id))

cyclistic_data <- cyclistic_data %>%
  mutate(ride_length = round(as.numeric(difftime(ended_at, started_at, units = "mins"))),
         month = format(as.Date(started_at), "%B"),
         day_of_week = format(as.Date(started_at), "%A"))

unique(cyclistic_data$rideable_type)

# Replacing values

cyclistic_data <- cyclistic_data %>%
  mutate(rideable_type = case_when(
    rideable_type == "docked_bike" ~ "classic_bike",
    TRUE ~ rideable_type
  ))

summary(cyclistic_data)

# Remove "bad" data

cyclistic_clean_data<- cyclistic_data %>% 
  filter(between(ride_length, 1, 1440))

summary(cyclistic_clean_data$ride_length)
dim(cyclistic_clean_data)

# Analyze

cyclistic_clean_data %>%
  summarize(average_trip_duration = mean(ride_length),
            total_trips_taken = n(),
            average_daily_trip = as.integer(total_trips_taken/365))

# Distribution of trips by member type
colors <- c("#eb4f1b","#1bb7eb")

total_trips_by_member_casual <- cyclistic_clean_data %>% 
  group_by(member_casual) %>% 
  summarize(total_trips_taken = n())

ggplot(total_trips_by_member_casual, aes(x = "", y = total_trips_taken, fill = member_casual)) +
  geom_col(width = 1, color = "black") +
  geom_text(aes(label = round(total_trips_taken, 0)),  
            color = "black",
            position = position_stack(vjust = 0.5), 
            hjust = 0.5,  
            size = 3.5) + 
  coord_polar(theta = "y") +
  scale_fill_manual(values = colors) +
  guides(fill = guide_legend(title = "Rider Type")) +
  theme_void()

# Bike usage by member type

rideable_type_and_member_casual <- cyclistic_clean_data %>% 
  group_by(member_casual, rideable_type) %>% 
  summarize(total_trips_taken = n()) %>% 
  arrange(total_trips_taken)

ggplot(rideable_type_and_member_casual, aes(x = member_casual, y = total_trips_taken, fill = rideable_type)) +
  geom_bar(stat = "identity", position = 'dodge', color = "black", size = 0.25) +
  geom_text(aes(label = total_trips_taken),
            color = "black",
            position = position_dodge(width = 0.9),
            vjust = -0.5, 
            size = 3) +
  scale_fill_manual(values = colors) +
  labs(x = "Member Type", y = "Total Trips Taken") +
  guides(fill = guide_legend(title = "Bike Type")) +
  theme_minimal()

# Monthly breakdown of trips by membership type

total_trips_taken_by_month <- cyclistic_clean_data %>% 
  group_by(member_casual, month) %>% 
  summarize(total_trips_taken = n()) %>% 
  arrange(member_casual,desc(total_trips_taken)) %>% 
  print(n=24)

total_trips_taken_by_month$month <- factor(total_trips_taken_by_month$month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

ggplot(total_trips_taken_by_month, aes(x = month, y = total_trips_taken, color = member_casual, group = member_casual)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  geom_text(aes(label = total_trips_taken), 
            color = "black", 
            position = position_dodge(width = 0.9),
            vjust = -0.5, 
            size = 4) +
  labs(x = "Month", y = "Total Trips Taken", color = "Member Type") +
  scale_color_manual(values = c("member" = "#eb4f1b", "casual" = "#1bb7eb")) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)


# Daily breakdown of trips by membership type

total_trips_taken_by_day_of_week <- cyclistic_clean_data %>% 
  group_by(member_casual, day_of_week) %>% 
  summarize(total_trips_taken = n()) %>% 
  arrange(member_casual,desc(total_trips_taken))

total_trips_taken_by_day_of_week$day_of_week <- factor (total_trips_taken_by_day_of_week$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))


ggplot(total_trips_taken_by_day_of_week, aes(x = day_of_week, y = total_trips_taken, color = member_casual, group = member_casual)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  geom_text(aes(label = total_trips_taken), 
            color = "black", 
            position = position_dodge(width = 0.9),
            vjust = -0.5, 
            size = 4) +
  labs(x = "Weekday", y = "Total Trips Taken", color = "Member Type") +
  scale_color_manual(values = c("member" = "#eb4f1b", "casual" = "#1bb7eb")) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)


# Which hour has the most trips taken?

total_trips_taken_by_time_of_day <- cyclistic_clean_data %>% 
  mutate(hour = hour(started_at),
         time_of_day = case_when(
           hour %in% 0:3 ~ "00:00 - 03:59",
           hour %in% 4:7 ~ "04:00 - 07:59",
           hour %in% 8:11 ~ "08:00 - 11:59",
           hour %in% 12:15 ~ "12:00 - 15:59",
           hour %in% 16:19 ~ "16:00 - 19:59",
           hour %in% 20:23 ~ "20:00 - 23:59"
         )) %>% 
  group_by(member_casual, time_of_day) %>% 
  summarize(total_trips_taken = n()) %>% 
  arrange(member_casual, desc(total_trips_taken))

total_trips_taken_by_time_of_day$time_of_day <- factor(total_trips_taken_by_time_of_day$time_of_day, levels = c("00:00 - 03:59", "04:00 - 07:59","08:00 - 11:59", "12:00 - 15:59", "16:00 - 19:59", "20:00 - 23:59"))

ggplot(total_trips_taken_by_time_of_day, aes(x = time_of_day, y = total_trips_taken, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = total_trips_taken),
            color = "black",
            position = position_dodge(width = 0.7),
            vjust = -0.5, 
            size = 3) +
  labs(x = "Time of Day", y = "Total Trips Taken") +
  scale_fill_manual(values = colors) +
  guides(fill = guide_legend(title = "Member Type")) +
  theme_minimal()


# Which start station is the most popular?

install.packages("treemapify")
install.packages("randomcoloR")
library(treemapify)
library(randomcoloR)
n <- 10
palette <- distinctColorPalette(n)

popular_start_station_by_casual_member<- cyclistic_clean_data %>% 
  group_by( start_station_name) %>% 
  filter(start_station_name != "") %>% 
  summarize(total_trips_taken = n()) %>% 
  slice_max(total_trips_taken, n = 10)

ggplot(popular_start_station_by_casual_member, aes(fill = start_station_name, area = total_trips_taken, label = paste0(start_station_name,"\n", total_trips_taken))) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "centre") +
  labs(title = "POPULAR STARTING STATIONS") +
  theme(legend.position = "none") +
  scale_fill_manual(values = palette)


# Which end station is the most popular?

popular_end_station_by_casual_member<- cyclistic_clean_data %>% 
  group_by(end_station_name) %>% 
  filter(end_station_name != "") %>% 
  summarize(total_trips_taken = n()) %>% 
  slice_max(total_trips_taken, n = 10)

ggplot(popular_end_station_by_casual_member, aes(fill = end_station_name, area = total_trips_taken, label = paste0(end_station_name,"\n", total_trips_taken))) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "centre") +
  labs(title = "POPULAR ENDING STATIONS") +
  theme(legend.position = "none") +
  scale_fill_manual(values = palette)
