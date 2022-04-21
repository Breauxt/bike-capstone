
#Install tidyverse package
install.packages("tidyverse")
install.packages("ggplot2")
##loads the librarys
library(tidyverse)
library(data.table)
library(lubridate)
library(dplyr)
library(ggplot2)

#####2022
#note: before running this code, ensure that date format is POSIXct, and trip duration is formatted to an integer value (number no decimal)
# pull subject year of files togther into one folder
# set working directory to folder. 
setwd("~/Google Data analytics cert/Course 8/Bike Ride share/Divvy-Trip data/trips/2022")

## names the list.file directory 
file_list <- list.files()
## this lists all files within directory
file_list 

## binds all tables in directory and names file as "trip_data",  This will take some time
#eliminates unused columns
trip_data_2022 <- rbindlist(lapply(file_list,fread), fill=TRUE,) %>%  # Creates table and combines all data from sources
  drop_na(ride_id, started_at, member_casual) #drops any row with NA in the columns within ()

trip_data_2022 <- trip_data_2022 %>% 
  mutate(trip_duration = as.double(difftime(ended_at, started_at, units = "secs")), #adds a column that calculates the duration of each ride
         year = "2022",
         month = month.name[month(trip_data_2022$started_at)]) # adds a column for month of each trip using "started_at" column

trip_data_2022$weekday <- wday(trip_data_2022$started_at, label=TRUE, abbr=FALSE) # adds a column for the weekday of each trip
trip_data_2022 <- trip_data_2022[!(trip_data_2022$trip_duration<60),] %>% #eliminates any trip duration less than 60 seconds
  select(ride_id, 
         started_at, 
         ended_at, 
         start_station_name,
         end_station_name,
         member_casual,
         trip_duration,
         year,
         month,
         weekday,
         rideable_type,) # Selects above columns needed for data processing


# builds a table that shows total trip count and average trip duration by month, weekday, member type, and bike type
weekday_member_bike_2022 <- trip_data_2022 %>%
  group_by(year, month, weekday, start_station_name, member_casual, rideable_type) %>% 
  summarize (trip_count = n(),
             ride_time=mean(trip_duration, na.rm=TRUE)) %>% 
  mutate(avg_trip = round(ride_time)) %>% 
  select(-ride_time) %>% 
  arrange(month, weekday, start_station_name, member_casual, rideable_type, desc(trip_count))


head(weekday_member_bike_2022) # returns top 6 results in console

#####2021
#note: before running this code, ensure that date format is POSIXct, and trip duration is formatted to an integer value (number no decimal)
# pull subject year of files togther into one folder
# set working directory to folder. 
setwd("~/Google Data analytics cert/Course 8/Bike Ride share/Divvy-Trip data/trips/2021")

## names the list.file directory 
file_list <- list.files()
## this lists all files within directory
file_list 

## binds all tables in directory and names file as "trip_data",  This will take some time
#eliminates unused columns
trip_data_2021 <- rbindlist(lapply(file_list,fread), fill=TRUE,) %>%  # Creates table and combines all data from sources
  drop_na(ride_id, started_at, member_casual) #drops any row with NA in the columns within ()

trip_data_2021 <- trip_data_2021 %>% 
  mutate(trip_duration = as.double(difftime(ended_at, started_at, units = "secs")), #adds a column that calculates the duration of each ride
         year = "2021",
         month = month.name[month(trip_data_2021$started_at)]) # adds a column for month of each trip using "started_at" column

trip_data_2021$weekday <- wday(trip_data_2021$started_at, label=TRUE, abbr=FALSE) # adds a column for the weekday of each trip
trip_data_2021 <- trip_data_2021[!(trip_data_2021$trip_duration<60),] %>% #eliminates any trip duration less than 60 seconds
  select(ride_id, 
         started_at, 
         ended_at, 
         start_station_name,
         end_station_name,
         member_casual,
         trip_duration,
         year,
         month,
         weekday,
         rideable_type,) # Selects above columns needed for data processing


# builds a table that shows total trip count and average trip duration by month, weekday, member type, and bike type
weekday_member_bike_2021 <- trip_data_2021 %>%
  group_by(year, month, weekday, start_station_name, member_casual, rideable_type) %>% 
  summarize (trip_count = n(),
             ride_time=mean(trip_duration, na.rm=TRUE)) %>% 
  mutate(avg_trip = round(ride_time)) %>% 
  select(-ride_time) %>% 
  arrange(month, weekday, start_station_name, member_casual, rideable_type, desc(trip_count))

#####2020
#note: before running this code, ensure that date format is POSIXct, and trip duration is formatted to an integer value (number no decimal)
# pull subject year of files togther into one folder
# set working directory to folder. 
setwd("~/Google Data analytics cert/Course 8/Bike Ride share/Divvy-Trip data/trips/2020")

## names the list.file directory 
file_list <- list.files()
## this lists all files within directory
file_list 

## binds all tables in directory and names file as "trip_data",  This will take some time
#eliminates unused columns
trip_data_2020 <- rbindlist(lapply(file_list,fread), fill=TRUE,) %>%  # Creates table and combines all data from sources
  drop_na(ride_id, started_at, member_casual) #drops any row with NA in the columns within ()

trip_data_2020 <- trip_data_2020 %>% 
  mutate(trip_duration = as.double(difftime(ended_at, started_at, units = "secs")), #adds a column that calculates the duration of each ride
         year = "2020",
         month = month.name[month(trip_data_2020$started_at)]) # adds a column for month of each trip using "started_at" column
         
trip_data_2020$weekday <- wday(trip_data_2020$started_at, label=TRUE, abbr=FALSE) # adds a column for the weekday of each trip
trip_data_2020 <- trip_data_2020[!(trip_data_2020$trip_duration<60),] %>% #eliminates any trip duration less than 60 seconds
  select(ride_id, 
         started_at, 
         ended_at, 
         start_station_name,
         end_station_name,
         member_casual,
         trip_duration,
         year,
         month,
         weekday,
         rideable_type,) # Selects above columns needed for data processing


# builds a table that shows total trip count and average trip duration by month, weekday, member type, and bike type
weekday_member_bike_2020 <- trip_data_2020 %>%
  group_by(year, month, weekday, start_station_name, member_casual, rideable_type) %>% 
  summarize (trip_count = n(),
             ride_time=mean(trip_duration, na.rm=TRUE)) %>% 
  mutate(avg_trip = round(ride_time)) %>% 
  select(-ride_time) %>% 
  arrange(month, weekday, start_station_name, member_casual, rideable_type, desc(trip_count))

#####2019
#note: before running this code, ensure that date format is POSIXct, and trip duration is formatted to an integer value (number no decimal)
# pull subject year of files togther into one folder
# set working directory to folder. 
setwd("~/Google Data analytics cert/Course 8/Bike Ride share/Divvy-Trip data/trips/2019") #sets the working directory to location where 2019 files exist


## names the list.file directory 
file_list <- list.files() # builds a table with the file list
## this lists all files within directory
file_list #prints the file list to view in console

## binds all tables in directory and names file as "trip_data",  This will take some time
#eliminates unused columns
trip_data_2019 <- rbindlist(lapply(file_list,fread), use.names=FALSE, fill=FALSE,) #binds all items in the folder   
colnames(trip_data_2019)[1:12] <- c("ride_id",
                                    "started_at",
                                    "ended_at",
                                    "bikeid",
                                    "trip_duration",
                                    "start_station_id",
                                    "start_station_name",
                                    "end_station_id",
                                    "end_station_name",
                                    "member_casual",
                                    "gender", 
                                    "birth_year") #  renames all columns above

trip_data_2019$member_casual[trip_data_2019$member_casual == "Subscriber"] <- "member" #changes "Subscriber" to "member"
trip_data_2019$member_casual[trip_data_2019$member_casual == "Customer"] <- "casual" #changes "Customer" to "casual"
trip_data_2019 <- trip_data_2019[!trip_data_2019$member_casual=="Dependent", ] #eliminates "Dependent
trip_data_2019$trip_duration <- as.numeric(gsub("\\..*|,", "", trip_data_2019$trip_duration,)) #formats trip duration to a numeric format 
trip_data_2019 <- trip_data_2019[!(trip_data_2019$trip_duration<60),] # eliminates rows with trip duration of less than 60 seconds
trip_data_2019$month <- month(trip_data_2019$started_at, label=TRUE, abbr=FALSE) # adds a column for the month of each trip
trip_data_2019$weekday <- wday(trip_data_2019$started_at, label=TRUE, abbr=FALSE) # adds a column for the weekday of each trip

trip_data_2019 <- trip_data_2019 %>% 
  mutate(year = "2019") %>% #Creates a year column
  select(ride_id, 
         started_at, 
         ended_at,
         start_station_name,
         end_station_name,
         member_casual,
         trip_duration,
         year,
         month, 
         weekday) %>% # Selects all of the above columns
   drop_na(ride_id, member_casual, month) #eliminates any NA's in columns specified


# builds a table that shows total trip count and average trip duration by month, weekday, member type, and bike type
weekday_member_bike_2019 <- trip_data_2019 %>%
  group_by(year, month, weekday, start_station_name, member_casual) %>% 
  summarize (trip_count = n(),
             ride_time=mean(trip_duration, na.rm=TRUE)) %>% 
  mutate(avg_trip = round(ride_time)) %>% 
  select(-ride_time) %>% 
  arrange(month, weekday, start_station_name, member_casual, desc(trip_count))

#####2018
#note: before running this code, ensure that date format is POSIXct, and trip duration is formatted to an integer value (number no decimal)
# pull subject year of files togther into one folder
# set working directory to folder. 
setwd("~/Google Data analytics cert/Course 8/Bike Ride share/Divvy-Trip data/trips/2018") #sets the working directory to location where 2018 files exist


## names the list.file directory 
file_list <- list.files() # builds a table with the file list
## this lists all files within directory
file_list #prints the file list to view in console

## binds all tables in directory and names file as "trip_data",  This will take some time
#eliminates unused columns
trip_data_2018 <- rbindlist(lapply(file_list,fread), use.names=FALSE, fill=FALSE,) #binds all items in the folder   
colnames(trip_data_2018)[1:12] <- c("ride_id",
                                    "started_at",
                                    "ended_at",
                                    "bikeid",
                                    "trip_duration",
                                    "start_station_id",
                                    "start_station_name",
                                    "end_station_id",
                                    "end_station_name",
                                    "member_casual",
                                    "gender", 
                                    "birth_year") #  renames all columns above

trip_data_2018$member_casual[trip_data_2018$member_casual == "Subscriber"] <- "member" #changes "Subscriber" to "member"
trip_data_2018$member_casual[trip_data_2018$member_casual == "Customer"] <- "casual" #changes "Customer" to "casual"
trip_data_2018 <- trip_data_2018[!trip_data_2018$member_casual=="Dependent", ] #eliminates "Dependent
trip_data_2018$trip_duration <- as.numeric(gsub("\\..*|,", "", trip_data_2018$trip_duration,)) #formats trip duration to a numeric format 
trip_data_2018 <- trip_data_2018[!(trip_data_2018$trip_duration<60),] # eliminates rows with trip duration of less than 60 seconds
trip_data_2018$month <- month(trip_data_2018$started_at, label=TRUE, abbr=FALSE) # adds a column for the month of each trip
trip_data_2018$weekday <- wday(trip_data_2018$started_at, label=TRUE, abbr=FALSE) # adds a column for the weekday of each trip

trip_data_2018 <- trip_data_2018 %>% 
  mutate(year = "2018") %>% #Creates a year column
  select(ride_id, 
         started_at, 
         ended_at,
         start_station_name,
         end_station_name,
         member_casual,
         trip_duration,
         year,
         month, 
         weekday) %>% # Selects all of the above columns
  drop_na(ride_id, member_casual, month) #eliminates any NA's in columns specified


# builds a table that shows total trip count and average trip duration by month, weekday, member type, and bike type
weekday_member_bike_2018 <- trip_data_2018 %>%
  group_by(year, month, weekday, start_station_name, member_casual) %>% 
  summarize (trip_count = n(),
             ride_time=mean(trip_duration, na.rm=TRUE)) %>% 
  mutate(avg_trip = round(ride_time)) %>% 
  select(-ride_time) %>% 
  arrange(month, weekday, start_station_name, member_casual, desc(trip_count))


#####2017
#note: before running this code, ensure that date format is POSIXct, and trip duration is formatted to an integer value (number no decimal)
# pull subject year of files togther into one folder
# set working directory to folder. 
setwd("~/Google Data analytics cert/Course 8/Bike Ride share/Divvy-Trip data/trips/2017")


## names the list.file directory 
file_list <- list.files()
## this lists all files within directory
file_list 

## binds all tables in directory and names file as "trip_data",  This will take some time
#eliminates unused columns
trip_data_2017 <- rbindlist(lapply(file_list,fread), use.names=TRUE, fill=TRUE,)   
colnames(trip_data_2017)[1:12] <- c("ride_id",
                    "started_at",
                    "ended_at",
                    "bikeid",
                    "trip_duration",
                    "start_station_id",
                    "start_station_name",
                    "end_station_id",
                    "end_station_name",
                    "member_casual",
                    "gender", 
                    "birth_year") 
trip_data_2017$dates <- as.Date(trip_data_2017$started_at, "%m/%d/%Y")
trip_data_2017$weekday <- weekdays(trip_data_2017$dates) # adds a column for the weekday of each trip
trip_data_2017$member_casual[trip_data_2017$member_casual == "Subscriber"] <- "member" 
trip_data_2017$member_casual[trip_data_2017$member_casual == "Customer"] <- "casual"
trip_data_2017 <- trip_data_2017[!trip_data_2017$member_casual=="Dependent", ] 
trip_data_2017 <- trip_data_2017[!(trip_data_2017$trip_duration<60),] 
trip_data_2017$month <- as.numeric(gsub("\\/.*", "", trip_data_2017$started_at))
trip_data_2017 <- trip_data_2017 %>% 
  mutate(month = month.name[(trip_data_2017$month)], 
         year = "2017") %>% 
  drop_na(ride_id, member_casual, month) %>% 
  distinct(ride_id, .keep_all = TRUE) %>% 
  select(-bikeid,
        -start_station_id,
        -end_station_id,
        -gender,
        -birth_year,
        -dates) 

# builds a table that shows total trip count and average trip duration by month, weekday, member type, and bike type
weekday_member_bike_2017 <- trip_data_2017 %>%
  group_by(year, month, weekday, start_station_name, member_casual) %>% 
  summarize (trip_count = n(),
             ride_time=mean(trip_duration, na.rm=TRUE)) %>% 
  mutate(avg_trip = round(ride_time)) %>% 
  select(-ride_time) %>% 
  arrange(month, weekday, start_station_name, member_casual, desc(trip_count))

#####2016
#note: before running this code, ensure that date format is POSIXct, and trip duration is formatted to an integer value (number no decimal)
# pull subject year of files togther into one folder
# set working directory to folder. 
setwd("~/Google Data analytics cert/Course 8/Bike Ride share/Divvy-Trip data/trips/2016")


## names the list.file directory 
file_list <- list.files()
## this lists all files within directory
file_list 

## binds all tables in directory and names file as "trip_data",  This will take some time
#eliminates unused columns
trip_data_2016 <- rbindlist(lapply(file_list,fread), use.names=TRUE, fill=TRUE,)   
colnames(trip_data_2016)[1:12] <- c("ride_id",
                                    "started_at",
                                    "ended_at",
                                    "bikeid",
                                    "trip_duration",
                                    "start_station_id",
                                    "start_station_name",
                                    "end_station_id",
                                    "end_station_name",
                                    "member_casual",
                                    "gender", 
                                    "birth_year") 
trip_data_2016$dates <- as.Date(trip_data_2016$started_at, "%m/%d/%Y")
trip_data_2016$weekday <- weekdays(trip_data_2016$dates) # adds a column for the weekday of each trip
trip_data_2016$member_casual[trip_data_2016$member_casual == "Subscriber"] <- "member" 
trip_data_2016$member_casual[trip_data_2016$member_casual == "Customer"] <- "casual"
trip_data_2016 <- trip_data_2016[!trip_data_2016$member_casual=="Dependent", ] 
trip_data_2016 <- trip_data_2016[!(trip_data_2016$trip_duration<60),] 
trip_data_2016$month <- as.numeric(gsub("\\/.*", "", trip_data_2016$started_at))
trip_data_2016 <- trip_data_2016 %>% 
  mutate(month = month.name[(trip_data_2016$month)], 
         year = "2016") %>% 
  drop_na(ride_id, member_casual, month) %>% 
  distinct(ride_id, .keep_all = TRUE) %>% 
  select(-bikeid,
         -start_station_id,
         -end_station_id,
         -gender,
         -birth_year,
         -dates) 

# builds a table that shows total trip count and average trip duration by month, weekday, member type, and bike type
weekday_member_bike_2016 <- trip_data_2016 %>%
  group_by(year, month, weekday, start_station_name, member_casual) %>% 
  summarize (trip_count = n(),
             ride_time=mean(trip_duration, na.rm=TRUE)) %>% 
  mutate(avg_trip = round(ride_time)) %>% 
  select(-ride_time) %>% 
  arrange(month, weekday, start_station_name, member_casual, desc(trip_count))


#####2015
#note: before running this code, ensure that date format is POSIXct, and trip duration is formatted to an integer value (number no decimal)
# pull subject year of files togther into one folder
# set working directory to folder. 
setwd("~/Google Data analytics cert/Course 8/Bike Ride share/Divvy-Trip data/trips/2015")


## names the list.file directory 
file_list <- list.files()
## this lists all files within directory
file_list 

## binds all tables in directory and names file as "trip_data",  This will take some time
#eliminates unused columns
trip_data_2015 <- rbindlist(lapply(file_list,fread), use.names=TRUE, fill=TRUE,)   
colnames(trip_data_2015)[1:12] <- c("ride_id",
                                    "started_at",
                                    "ended_at",
                                    "bikeid",
                                    "trip_duration",
                                    "start_station_id",
                                    "start_station_name",
                                    "end_station_id",
                                    "end_station_name",
                                    "member_casual",
                                    "gender", 
                                    "birth_year") 
trip_data_2015$dates <- as.Date(trip_data_2015$started_at, "%m/%d/%Y")
trip_data_2015$weekday <- weekdays(trip_data_2015$dates) # adds a column for the weekday of each trip
trip_data_2015$member_casual[trip_data_2015$member_casual == "Subscriber"] <- "member" 
trip_data_2015$member_casual[trip_data_2015$member_casual == "Customer"] <- "casual"
trip_data_2015 <- trip_data_2015[!trip_data_2015$member_casual=="Dependent", ] 
trip_data_2015 <- trip_data_2015[!(trip_data_2015$trip_duration<60),] 
trip_data_2015$month <- as.numeric(gsub("\\/.*", "", trip_data_2015$started_at))
trip_data_2015 <- trip_data_2015 %>% 
  mutate(month = month.name[(trip_data_2015$month)], 
         year = "2015") %>% 
  drop_na(ride_id, member_casual, month) %>% 
  distinct(ride_id, .keep_all = TRUE) %>% 
  select(-bikeid,
         -start_station_id,
         -end_station_id,
         -gender,
         -birth_year,
         -dates) 

# builds a table that shows total trip count and average trip duration by month, weekday, member type, and bike type
weekday_member_bike_2015 <- trip_data_2015 %>%
  group_by(year, month, weekday, start_station_name, member_casual) %>% 
  summarize (trip_count = n(),
             ride_time=mean(trip_duration, na.rm=TRUE)) %>% 
  mutate(avg_trip = round(ride_time)) %>% 
  select(-ride_time) %>% 
  arrange(month, weekday, start_station_name, member_casual, desc(trip_count))

#####2014
#note: before running this code, ensure that date format is POSIXct, and trip duration is formatted to an integer value (number no decimal)
# pull subject year of files togther into one folder
# set working directory to folder. 
setwd("~/Google Data analytics cert/Course 8/Bike Ride share/Divvy-Trip data/trips/2014")


## names the list.file directory 
file_list <- list.files()
## this lists all files within directory
file_list 

## binds all tables in directory and names file as "trip_data",  This will take some time
#eliminates unused columns
trip_data_2014 <- rbindlist(lapply(file_list,fread), use.names=TRUE, fill=TRUE,)   
colnames(trip_data_2014)[1:12] <- c("ride_id",
                                    "started_at",
                                    "ended_at",
                                    "bikeid",
                                    "trip_duration",
                                    "start_station_id",
                                    "start_station_name",
                                    "end_station_id",
                                    "end_station_name",
                                    "member_casual",
                                    "gender", 
                                    "birth_year") 
trip_data_2014$dates <- as.Date(trip_data_2014$started_at, "%m/%d/%Y")
trip_data_2014$weekday <- weekdays(trip_data_2014$dates) # adds a column for the weekday of each trip
trip_data_2014$member_casual[trip_data_2014$member_casual == "Subscriber"] <- "member" 
trip_data_2014$member_casual[trip_data_2014$member_casual == "Customer"] <- "casual"
trip_data_2014 <- trip_data_2014[!trip_data_2014$member_casual=="Dependent", ] 
trip_data_2014 <- trip_data_2014[!(trip_data_2014$trip_duration<60),] 
trip_data_2014$month <- as.numeric(gsub("\\/.*", "", trip_data_2014$started_at))
trip_data_2014 <- trip_data_2014 %>% 
  mutate(month = month.name[(trip_data_2014$month)], 
         year = "2014") %>% 
  drop_na(ride_id, member_casual, month) %>% 
  distinct(ride_id, .keep_all = TRUE) %>% 
  select(-bikeid,
         -start_station_id,
         -end_station_id,
         -gender,
         -birth_year,
         -dates) 

# builds a table that shows total trip count and average trip duration by month, weekday, member type, and bike type
weekday_member_bike_2014 <- trip_data_2014 %>%
  group_by(year, month, weekday, start_station_name, member_casual) %>% 
  summarize (trip_count = n(),
             ride_time=mean(trip_duration, na.rm=TRUE)) %>% 
  mutate(avg_trip = round(ride_time)) %>% 
  select(-ride_time) %>% 
  arrange(month, weekday, start_station_name, member_casual, desc(trip_count))

#####2013
#note: before running this code, ensure that date format is POSIXct, and trip duration is formatted to an integer value (number no decimal)
# pull subject year of files togther into one folder
# set working directory to folder. 
setwd("~/Google Data analytics cert/Course 8/Bike Ride share/Divvy-Trip data/trips/2013") #sets the working directory to location where 2013 files exist


## names the list.file directory 
file_list <- list.files() # builds a table with the file list
## this lists all files within directory
file_list #prints the file list to view in console

## binds all tables in directory and names file as "trip_data",  This will take some time
#eliminates unused columns
trip_data_2013 <- rbindlist(lapply(file_list,fread), use.names=FALSE, fill=FALSE,) #binds all items in the folder   
colnames(trip_data_2013)[1:12] <- c("ride_id",
                                    "started_at",
                                    "ended_at",
                                    "bikeid",
                                    "trip_duration",
                                    "start_station_id",
                                    "start_station_name",
                                    "end_station_id",
                                    "end_station_name",
                                    "member_casual",
                                    "gender", 
                                    "birth_year") #  renames all columns above

trip_data_2013$member_casual[trip_data_2013$member_casual == "Subscriber"] <- "member" #changes "Subscriber" to "member"
trip_data_2013$member_casual[trip_data_2013$member_casual == "Customer"] <- "casual" #changes "Customer" to "casual"
trip_data_2013 <- trip_data_2013[!trip_data_2013$member_casual=="Dependent", ] #eliminates "Dependent
trip_data_2013$trip_duration <- as.numeric(gsub("\\..*|,", "", trip_data_2013$trip_duration,)) #formats trip duration to a numeric format 
trip_data_2013 <- trip_data_2013[!(trip_data_2013$trip_duration<60),] # eliminates rows with trip duration of less than 60 seconds
trip_data_2013$month <- month(trip_data_2013$started_at, label=TRUE, abbr=FALSE) # adds a column for the month of each trip
trip_data_2013$weekday <- wday(trip_data_2013$started_at, label=TRUE, abbr=FALSE) # adds a column for the weekday of each trip

trip_data_2013 <- trip_data_2013 %>% 
  mutate(year = "2013") %>% #Creates a year column
  select(ride_id, 
         started_at, 
         ended_at,
         start_station_name,
         end_station_name,
         member_casual,
         trip_duration,
         year,
         month, 
         weekday) %>% # Selects all of the above columns
  drop_na(ride_id, member_casual, month) #eliminates any NA's in columns specified


# builds a table that shows total trip count and average trip duration by month, weekday, member type, and bike type
weekday_member_bike_2013 <- trip_data_2013 %>%
  group_by(year, month, weekday, start_station_name, member_casual) %>% 
  summarize (trip_count = n(),
             ride_time=mean(trip_duration, na.rm=TRUE)) %>% 
  mutate(avg_trip = round(ride_time)) %>% 
  select(-ride_time) %>% 
  arrange(month, weekday, start_station_name, member_casual, desc(trip_count))


#########################################################################################################################################

file_list = list(weekday_member_bike_2013,
            weekday_member_bike_2014,
            weekday_member_bike_2015,
            weekday_member_bike_2016,
            weekday_member_bike_2017,
            weekday_member_bike_2018,
            weekday_member_bike_2019,
            weekday_member_bike_2020,
            weekday_member_bike_2021,
            weekday_member_bike_2022)
weekday_member_bike_all<-rbindlist(file_list, use.names=TRUE, fill=TRUE)
month_table <- data.table(month=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
                                month_num=c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), 
                                month_abv=c("Jan", "Feb", "Mar","Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
weekday_member_bike_all1 <- merge(weekday_member_bike_all, month_table, by="month", all.x=TRUE, all.y = TRUE)
weekday_member_bike_all <-weekday_member_bike_all1 %>%  
  unite(year_month, year,month_num, remove=FALSE ) 
rm(weekday_member_bike_all1)
  
# trip totals 2013-2022 month by month
Monthly_member_data <- weekday_member_bike_all %>% 
  group_by(year, month_abv, member_casual) %>%  
  summarize(trip_count = sum(trip_count))

require(scales)
require(ggplot2)
year_month_member_rides_plot <- ggplot(Monthly_member_data, aes(x = month_abv, y = trip_count, colour = member_casual, group=member_casual))+
  geom_line(size=1.5)+
  geom_point(size =2, shape = 21, fill = "white")+
  facet_grid(.~ year)+
  theme(axis.text.x = element_text(size = 6, angle = 90, hjust = 1))+
  scale_x_discrete(limits=c("Jan", "Feb", "Mar","Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))+
  scale_y_continuous(labels= comma)+
  xlab("Year and Month") +
  ylab("Trip Count")


# useage in time

useage_data <- weekday_member_bike_all %>% 
  group_by(year, month_abv, member_casual) %>%  
  summarize(avg_trip = sum(avg_trip*trip_count)/sum(trip_count)) %>% 
  mutate(avg_trip= round(avg_trip))

require(scales)
require(ggplot2)
year_month_ride_times_plot <- ggplot(useage_data, aes(x = month_abv, y = avg_trip , colour = member_casual, group=member_casual)) +
  geom_line(size=1.5)+
  geom_point(size =2, shape = 21, fill = "white")+
  facet_grid(.~ year)+
  theme(axis.text.x = element_text(size = 6, angle = 90, hjust = 1))+
  scale_x_discrete(limits=c("Jan", "Feb", "Mar","Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))+
  scale_y_continuous(labels= comma)+
  xlab("Year and Month") +
  ylab("Average trip useage in seconds")




#################################################################################################################################################3


#########

# builds a table that provides data on weekdays by member type
weekday_all <- weekday_member_bike_all %>% 
  group_by(weekday, member_casual) %>% 
  summarize(trip_count = sum(trip_count))

# plots trip totals by weekday, and member type
require(scales) #tells R to ensure that scales package is installed and library is called
require(ggplot2) #tells R to ensure that ggplot2 package is installed and library is called
weekday_member_type_plot <- ggplot(weekday_all, aes(x =weekday, y = trip_count, color=member_casual, group= member_casual))+ # tells to output a plot with aestetics as specified.  provides colors for ride type, and groups by ride type
  geom_line(size=1.5)+ # outputs data in a line graph
  geom_point(size =2, shape = 21, fill = "white")+ #adds white point for each row to plot on the graph
  theme(axis.text.x = element_text(size = 6, angle = 90, hjust = 1))+ # sets size and angle of months in x axis
  scale_x_discrete(limits=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))+ #orders months chronologically vs alphabetically
  scale_y_continuous(labels= comma)+ #shows y axis numbers with comma (100,000).  Eliminates exponent (1e+05).  
  labs(title = "Weekday Rides by Member Type")+
  xlab("Weekdays by month)") + # labels x axis
  ylab("Trip Count") # labels y axis

#########

# builds a table that provides monthly data on weekdays by member type
month_weekday_all <- weekday_member_bike_all %>% 
  group_by(month_abv, weekday, member_casual) %>% 
  summarize(trip_count = sum(trip_count))
month_weekday_all$month_abv = factor(month_weekday_all$month_abv, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# plots trip totals by month, weekday, and rideable type
require(scales) #tells R to ensure that scales package is installed and library is called
require(ggplot2) #tells R to ensure that ggplot2 package is installed and library is called
month_weekday_member_type_plot <- ggplot(month_weekday_all, aes(x =weekday, y = trip_count, color=member_casual, group= member_casual))+ # tells to output a plot with aestetics as specified.  provides colors for ride type, and groups by ride type
  geom_line(size=1.5)+ # outputs data in a line graph
  geom_point(size =2, shape = 21, fill = "white")+ #adds white point for each row to plot on the graph
  facet_grid(.~ month_abv)+ # combines data into multiple charts by abbreviated month
  theme(axis.text.x = element_text(size = 6, angle = 90, hjust = 1))+ # sets size and angle of months in x axis
  scale_x_discrete(limits=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))+ #orders months chronologically vs alphabetically
  scale_y_continuous(labels= comma)+ #shows y axis numbers with comma (100,000).  Eliminates exponent (1e+05).  
  labs(title = "Rides by Member Type 2013-2022")+
  xlab("Weekdays by month)") + # labels x axis
  ylab("Trip Count") # labels y axis

#########

# builds a table that provides data on weekdays for member subscribers
weekday_member_all <- weekday_member_bike_all %>% 
  group_by(year, month_abv, weekday) %>% 
  filter(member_casual == "member") %>% 
  summarize(trip_count = sum(trip_count))
weekday_member_all$month_abv = factor(weekday_member_all$month_abv, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# plots trip totals by month, weekday, and rideable type for subscribed members
require(scales) #tells R to ensure that scales package is installed and library is called
require(ggplot2) #tells R to ensure that ggplot2 package is installed and library is called
subscribed_member_rides_by_year_plot <- ggplot(weekday_member_all, aes(x =weekday, y = trip_count, color = year, group= year))+ # tells to output a plot with aestetics as specified.  provides colors for ride type, and groups by ride type
  geom_line(size=1.5)+ # outputs data in a line graph
  geom_point(size =2, shape = 21, fill = "white")+ #adds white point for each row to plot on the graph
  facet_grid(.~ month_abv)+ # combines data into multiple charts by abbreviated month
  theme(axis.text.x = element_text(size = 6, angle = 90, hjust = 1))+ # sets size and angle of months in x axis
  scale_x_discrete(limits=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))+ #orders months chronologically vs alphabetically
  scale_y_continuous(labels= comma)+ #shows y axis numbers with comma (100,000).  Eliminates exponent (1e+05).  
  labs(title = "Subscribed Member Rides 2013-2022", tag = "A")+
  xlab("Weekdays by month)") + # labels x axis
  ylab("Trip Count") # labels y axis

# builds a table that provides data on weekdays for casual users
weekday_casual_all <- weekday_member_bike_all %>% 
  group_by(year, month_abv, weekday) %>% 
  filter(member_casual == "casual") %>% 
summarize(trip_count = sum(trip_count))
weekday_casual_all$month_abv = factor(weekday_member_all$month_abv, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# plots trip totals by month, weekday, and rideable type for casual users
require(scales) #tells R to ensure that scales package is installed and library is called
require(ggplot2) #tells R to ensure that ggplot2 package is installed and library is called
Casual_rides_by_year_plot <- ggplot(weekday_casual_all, aes(x =weekday, y = trip_count, color = year, group= year))+ # tells to output a plot with aestetics as specified.  provides colors for ride type, and groups by ride type
  geom_line(size=1.5)+ # outputs data in a line graph
  geom_point(size =2, shape = 21, fill = "white")+ #adds white point for each row to plot on the graph
  facet_grid(.~ month_abv)+ # combines data into multiple charts by abbreviated month
  theme(axis.text.x = element_text(size = 6, angle = 90, hjust = 1))+ # sets size and angle of months in x axis
  scale_x_discrete(limits=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))+ #orders months chronologically vs alphabetically
  scale_y_continuous(labels= comma)+ #shows y axis numbers with comma (100,000).  Eliminates exponent (1e+05).  
  labs(title = "Casual Rides 2013-2022", tag = "B")+
  xlab("Weekdays by month (2013-2022)") + # labels x axis
  ylab("Trip Count") # labels y axis

#########

#  builds a table that provides data on weekdays by user and bike type.  Eliminates docked bikes and any N/A's
weekday_bike_type_all <- weekday_member_bike_all %>% 
  group_by(month_abv, weekday, rideable_type, member_casual) %>% 
  filter(rideable_type != "docked_bike") %>% 
  summarize(trip_count = sum(trip_count))
  weekday_bike_type_all$member_bike <- paste(weekday_bike_type_all$member_casual, weekday_bike_type_all$rideable_type)
weekday_bike_type_all$month_abv = factor(weekday_bike_type_all$month_abv, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# plots trip totals by month, weekday, and rideable type
require(scales) #tells R to ensure that scales package is installed and library is called
require(ggplot2) #tells R to ensure that ggplot2 package is installed and library is called
month_weekday_rides_by_member_bike_type_plot <- ggplot(weekday_bike_type_all, aes(x =weekday, y = trip_count, color = member_bike, group= member_bike))+ # tells to output a plot with aestetics as specified.  provides colors for ride type, and groups by ride type
  geom_line(size=1.5)+ # outputs data in a line graph
  geom_point(size =2, shape = 21, fill = "white")+ #adds white point for each row to plot on the graph
  facet_grid(.~ month_abv)+ # combines data into multiple charts by abbreviated month
  theme(axis.text.x = element_text(size = 6, angle = 90, hjust = 1))+ # sets size and angle of months in x axis
  scale_x_discrete(limits=c("Monday", "Tuesday", "Wednesday","Thursday", "Friday", "Saturday", "Sunday"))+ #orders months chronologically vs alphabetically
  scale_y_continuous(labels= comma)+ #shows y axis numbers with comma (100,000).  Eliminates exponent (1e+05).  
  labs(title = "Rides by Member Type and Bike Type 2020-2022")+
  xlab("Weekdays by month (2020-2022)") + # labels x axis
  ylab("Trip Count") # labels y axis


