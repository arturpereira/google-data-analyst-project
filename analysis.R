#### Case Study - Cyclist Bike-Share ####

### Analysis

## Initial Setup

# Importing Libraries
library(dplyr)
library(readr)

# Importing Cleaned Dataframe
df_main <- read_csv("data/clean_df.csv", col_types = list(
                      ride_id = col_character(),
                      rideable_type = col_character(),
                      started_at = col_datetime(format = ""),
                      ended_at = col_datetime(format = ""),
                      start_station_name = col_character(),
                      start_station_id = col_character(),
                      end_station_name = col_character(),
                      end_station_id = col_character(),
                      start_lat = col_double(),
                      start_lng = col_double(),
                      end_lat = col_double(),
                      end_lng = col_double(),
                      member_casual = col_character()
                      )
                    )

df_main$X1 <- NULL

# Creating a column for day of the week of the trip.
# Current R version in portuguese, so the resulting day is in portuguese too.
df_main$day_of_week = weekdays(df_main$started_at)

### General descriptive statistics
summary(df_main$ride_time_length)

# There seems to be at least one great outlier for ride time length (3524302 seconds), although the mean ride length is 1506,
# as well as ride times that are very short (1 second, for example).
# These outliers will distort any visual representations, therefore further investigation is made for them.
df_main = arrange(df_main, ride_time_length)

# In order to remove these outliers, a minimum and maximum ride times are considered (between 60 seconds and 2 hours)
# These values were chosen considering the mean travel time of around 25 minutes.

df_clean <- df_main %>%
  filter(ride_time_length > 60 & ride_time_length < 7200)

length(df_main$ride_id) - length(df_clean$ride_id)

# A total of 247,245 observations were removed.
247245/8090879 # About 3%

summary(df_main$ride_time_length)
summary(df_clean$ride_time_length)
1145/60

## Average ride length per day with group by
aggregate(df_clean$ride_time_length, by=list(df_clean$day_of_week, df_clean$member_casual), FUN = mean)
aggregate(df_clean$ride_time_length, by=list(df_clean$member_casual), FUN = mean)

## Initial Visualizations
# Importing ggplot
library(ggplot2)
library(scales)

## Getting a visual representation of ride time distribution
ggplot(df_clean, aes(x=ride_time_length)) + 
  geom_histogram(binwidth=20,fill="#4094a5") +
  labs(title = "Ride Time Length Distribution", x = "Ride Length (seconds)", y = "Count") +
  annotate(geom="text", x=4000, y=100000, label="Mean: 1145 seconds (19.1 min)") +
  scale_y_continuous(labels = comma)

# Ride time distribution by member type
ggplot(df_clean, aes(x=ride_time_length, fill=member_casual)) + 
  geom_histogram(binwidth=20) +
  labs(title = "Ride Length by Membership", x = "Ride Length (seconds)", y = "Count", fill="Usage") +
  annotate(geom="text", x=4000, y=100000, label="Member Mean: 854 seconds (14.2 min)") +
  annotate(geom="text", x=4000, y=80000, label="Casual Mean: 1506 seconds (25.1 min)") +
  scale_y_continuous(labels = comma)

# Ride time distrubution by vehicle type
ggplot(df_clean, aes(x=ride_time_length, fill=rideable_type)) + 
  geom_histogram(binwidth=20)

# Ride time distrubution by week day
ggplot(df_clean, aes(x=ride_time_length, fill=day_of_week)) + 
  geom_histogram(binwidth=20)

## Week Day ride frequency
ggplot(df_clean, aes(x=reorder(day_of_week,day_of_week,length), fill=member_casual)) + 
  geom_bar() +
  scale_x_discrete("",labels=c("segunda-feira"="Monday", "terça-feira"="Tuesday", "quarta-feira"="Wednesday", 
                               "quinta-feira"="Thursday", "sexta-feira"="Friday", "sábado"="Saturday", 
                               "domingo"="Sunday")) +
  labs(title = "Weekday Usage by Membership", x = "", y = "Count", fill="Usage") +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  scale_y_continuous(labels = comma)

## Vehicle type frequency
ggplot(df_clean, aes(x=rideable_type, fill=member_casual)) + geom_bar()

## Member type
ggplot(df_clean, aes(x=member_casual)) + geom_bar()

### Specific member type statistics
df_members <- df_clean %>%
  filter(member_casual == "member")

df_casual <- df_clean %>%
  filter(member_casual == "casual")

summary(df_members$ride_time_length)
854/60
summary(df_casual$ride_time_length)
1506/60

(1506-854)/854

df_members %>% 
  count(day_of_week)

df_casual %>% 
  count(day_of_week)

# Ratios:
556109/659642
656391/385407
639969/403390
651497/791158
567463/382422
643696/512380
625492/368618

Ratios = c(1.483866,	1.696857,	1.703111,	1.586477,	1.256286,	0.8234727,	0.8430467)
Days = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
level_order <- factor(Days, level = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

df_ratios = data.frame(Ratios,Days)

ggplot(df_ratios, aes(x=level_order, y=Ratios)) + 
  geom_bar(stat = "identity", fill="#4094a5") +
  labs(title = "Weekday Usage Ratio", x = "", y = "Ratio", subtitle = "Usage ratio per day (Member/Casual)")

# As expected the mean ride length for casual riders is much higher than members.
# Casual users are much more common on weekends, a targeted ad campaign for weekend users couold yield good results.
# In addition to that, a new product could be potentially imagined for weekend specific users in order to give them annual
# membership options that fit their weekend-usage profile.