#### Case Study - Cyclist Bike-Share ####

### Data Process - Cleaning and Data Manipulation

## Data Aggregation - Joining all monthly .csv's into one single dataframe

# Importing Libraries
library(dplyr)
library(readr)

# Merging .csv's into single dataframe

## Note: Not all files had the same column types.
## A pre-defined col_types argument was used in order to address it.

df_main <- list.files(path="data/trip_data", full.names = TRUE) %>% 
  lapply(read_csv, 
         col_types = list(
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
        ) %>% 
  bind_rows

# Maintaining an original copy of the dataframe
df_og <- data.frame(df_main)

# Including column for ride_id character count to check consistency
df_main$ride_id_length <- nchar(df_main$ride_id)

# Summary of dataframe's internal structure
str(df_main)
unique(df_main[c("rideable_type")]) # docked_bike; electric_bike; classic_bike 
unique(df_main[c("member_casual")]) # member; casual
unique(df_main[c("ride_id_length")]) # 16

# Creating column of trip time in seconds
df_main$ride_time_length <- df_main$ended_at - df_main$started_at

### Cleaning the Dataframe
# Checking for negative travel times
any(df_main$ride_time_length < 0) # TRYUE, which implies incorrect trip information

## Given there are negative trip times, I remove these entries.

# Data frame of the trips that had negative time lengths. Total of 11,462 observations.
negative_trips <- df_main[df_main$ride_time_length <= 0, ]

# Clean dataframe from these values
df_clean <- df_main[df_main$ride_time_length > 0, ]

# Checking for duplicated ride_id's (should be unique)
any(duplicated(df_clean$ride_id)) # FALSE, which implies there are no duplicated id's

# Addressing duplicated ride_id's
n_occur <- data.frame(table(df_clean$start_station_id))

## There seems to be some different variations of Station ID's that could be an indication of error, although 
## there is no indication on the other variables that these stations are not new, as the most recent available
## data for station information is April 2021. Station names could have been added or edited in the more recent times.
## More information should be obtained in order to validate these Station IDs.

# Saving clean data frame for further analysis.
write.csv(df_clean, "data/clean_df.csv")
