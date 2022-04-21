

#Install tidyverse package
install.packages("tidyverse")
##loads the librarys
library(tidyverse)
library(data.table)
library(lubridate)


# pull last 12 months of files togther into one folder (202103:202202)
# set working directory to folder. 
setwd("~/Google Data analytics cert/Course 8/Bike Ride share/Divvy-Trip data/trips/last_12_months")

## names the list.file directory 
file_list <- list.files() # builds file list directory
file_list ## prints list of all files within directory into console

## binds all tables in directory and names file as "trip_data",  This will take some time
trip_data <- rbindlist(lapply(file_list,fread), fill=TRUE,) %>% 
  select(-start_station_id, -end_station_id) #eliminates unused columns

#adds a column that calculates the duration of each ride. and adds a column for start_month name of each trip
trip_data_12_mo <- trip_data %>% #builds trip_data_12_mo directory
  mutate(ride_time = difftime(ended_at, started_at, units = "secs"), 
         month_name = month.name[month(trip_data$started_at)])


#When data is already loaded and saved, do this to load data files insead of above: data location varies based on directory
load("~/Google Data analytics cert/Course 8/Bike Ride share/Google capstone/Google capstone.RData")

#Note: all columns must match, and formatting must be the same. 

#total trip data group info
trip_avgs <- trip_data_12_mo %>% 
    summarize(start_lat_avg=mean(start_lat, na.rm=TRUE), 
          start_long_avg=mean(start_lng, na.rm=TRUE), 
          end_lat_avg=mean(end_lat, na.rm=TRUE), 
          end_long_avg=mean(end_lng, na.rm=TRUE), 
          trip_duration=mean(ride_time, na.rm=TRUE), 
          trip_count = n()) %>% 
    mutate(avg_trip = round(trip_duration)) %>%  
    select(-trip_duration)
trip_avgs

# member type data group info
mbr_trip_avgs <- trip_data_12_mo %>% 
  group_by(member_casual) %>%  
  summarize(trip_count = n(), 
            start_lat_avg=mean(start_lat, na.rm=TRUE), 
            start_long_avg=mean(start_lng, na.rm=TRUE), 
            end_lat_avg=mean(end_lat, na.rm=TRUE), 
            end_long_avg=mean(end_lng, na.rm=TRUE), 
            trip_duration=mean(ride_time, na.rm=TRUE)) %>% 
  mutate(avg_trip = round(trip_duration)) %>%  
  select(-trip_duration)
mbr_trip_avgs 

# member and bicycle type data group info. 
mbr_type_trip_avgs <- trip_data_12_mo %>% 
    group_by(member_casual, rideable_type) %>%  
    summarize(trip_count = n(), 
          start_lat_avg=mean(start_lat, na.rm=TRUE), 
          start_long_avg=mean(start_lng, na.rm=TRUE), 
          end_lat_avg=mean(end_lat, na.rm=TRUE), 
          end_long_avg=mean(end_lng, na.rm=TRUE), 
          trip_duration=mean(ride_time, na.rm=TRUE)) %>% 
    mutate(avg_trip = round(trip_duration)) %>%  
    select(-trip_duration)
mbr_type_trip_avgs 

monthly_trips_member_type <- trip_data_12_mo %>%
    group_by(month_name, member_casual, rideable_type) %>% 
    summarize (trip_count = n(),
          start_lat_avg=mean(start_lat, na.rm=TRUE), 
          start_long_avg=mean(start_lng, na.rm=TRUE), 
          end_lat_avg=mean(end_lat, na.rm=TRUE), 
          end_long_avg=mean(end_lng, na.rm=TRUE), 
          trip_duration=mean(ride_time, na.rm=TRUE)) %>%  
    arrange(member_casual, desc(trip_duration))
head(monthly_trips_member_type)

monthly_trips_member_by_duration <- trip_data_12_mo %>%
  group_by(month_name, member_casual) %>% 
  summarize (trip_count = n(),
             start_lat_avg=mean(start_lat, na.rm=TRUE), 
             start_long_avg=mean(start_lng, na.rm=TRUE), 
             end_lat_avg=mean(end_lat, na.rm=TRUE), 
             end_long_avg=mean(end_lng, na.rm=TRUE), 
             trip_duration=mean(ride_time, na.rm=TRUE)) %>%  
  arrange(member_casual, desc(trip_duration))
head(monthly_trips_member_by_duration)

monthly_trips_member_by_count <- trip_data_12_mo %>%
  group_by(month_name, member_casual) %>% 
  summarize (trip_count = n(),
             start_lat_avg=mean(start_lat, na.rm=TRUE), 
             start_long_avg=mean(start_lng, na.rm=TRUE), 
             end_lat_avg=mean(end_lat, na.rm=TRUE), 
             end_long_avg=mean(end_lng, na.rm=TRUE), 
             trip_duration=mean(ride_time, na.rm=TRUE)) %>%  
  arrange(member_casual, desc(trip_count))
head(monthly_trips_member_by_count)

monthly_trips_wide<-dcast(setDT(monthly_trips_member_type), month_name ~ member_casual + rideable_type, value.var="trip_count")
          

#return number of members vs casual riders at each location not null
membership_by_location <- trip_data_12_mo %>%
    group_by(start_station_name) %>%
    count(start_station_name, member_casual) %>% 
    rename(rides=n)
head(membership_by_location)

# Returns distinct locations and averages the start lat and long for each location.  
locations1 <- distinct(trip_data_12_mo, ride_id, start_lat, member_casual, .keep_all = TRUE) %>% 
group_by(start_station_name, member_casual) %>% 
  count(start_station_name) %>% 
  filter(start_station_name != "") %>% 
  rename(rides=n) 
locations1 <- spread(locations1, member_casual, rides)

locations2 <- distinct(trip_data_12_mo, ride_id, start_lat, member_casual, .keep_all = TRUE) %>% 
  group_by(start_station_name) %>% 
  filter(start_station_name != "") %>% 
summarize(start_lat_avg=mean(start_lat), start_long_avg=mean(start_lng))

locations <- right_join(locations1, locations2, by=NULL, copy=FALSE)
locations$member_greater <- locations$casual <= locations$member
locations <- locations %>% 
  rename(lat=start_lat_avg, lng=start_long_avg)

remove(locations1)
remove(locations2)
######################################################################
# Merges locations with member_by_location to provide casual vs members for each location and count of rides for each.
member_locations <- right_join(locations, membership_by_location, by=NULL, copy=FALSE)
member_locations_rides <- select(member_locations, name=start_station_name, start_lat=start_lat_avg, start_lng=start_long_avg, member_casual, rides)

#widen data for each station to inlude members & casual numbers
member_location_rides_wide <- dcast(setDT(member_locations_rides), name + start_lat + start_lng ~ member_casual, value.var="rides")
member_location_rides_wide
#difference for each station in members vs casual


######################################################################
install.packages('devtools')
devtools::install_github('rstudio/leaflet')
require(leaflet)
require(tidyverse)
  locations$member_greater[locations$member_greater=="TRUE"] <- "green"
  locations$member_greater[locations$member_greater=="FALSE"] <- "red"

map <- leaflet(locations)%>% 
  setView(lng = -87.7169, lat = 41.8855, zoom = 10) %>% 
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(~lng, ~lat, popup = paste("Station name", "-", locations$start_station_name, "<br>",
                                       "Member trips", "-", locations$member, "<br>",
                                       "Casual trips", "-", locations$casual), 
             label = ~ as.character(start_station_name),
             clusterOptions = markerClusterOptions())
map # Print the map





