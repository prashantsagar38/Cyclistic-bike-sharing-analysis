# Cyclistic-bike-sharing-analysis
Case Study 1: How Does a Bike-Share Navigate Speedy Success?

#### Load dataset CSV files (12 months of Cyclistic trip data for FY 2020)

```{r loading data}
tripdata_Q1_2020 <- read.csv("C:/Users/PSagar/Downloads/Divvy_trip_data_FY2020/Divvy_Trips_2020_Q1.csv")
tripdata_202004 <- read.csv("C:/Users/PSagar/Downloads/Divvy_trip_data_FY2020/202004-divvy-tripdata.csv")
tripdata_202005 <- read.csv("C:/Users/PSagar/Downloads/Divvy_trip_data_FY2020/202005-divvy-tripdata.csv")
tripdata_202006 <- read.csv("C:/Users/PSagar/Downloads/Divvy_trip_data_FY2020/202006-divvy-tripdata.csv")
tripdata_202007 <- read.csv("C:/Users/PSagar/Downloads/Divvy_trip_data_FY2020/202007-divvy-tripdata.csv")
tripdata_202008 <- read.csv("C:/Users/PSagar/Downloads/Divvy_trip_data_FY2020/202008-divvy-tripdata.csv")
tripdata_202009 <- read.csv("C:/Users/PSagar/Downloads/Divvy_trip_data_FY2020/202009-divvy-tripdata.csv")
tripdata_202010 <- read.csv("C:/Users/PSagar/Downloads/Divvy_trip_data_FY2020/202010-divvy-tripdata.csv")
tripdata_202011 <- read.csv("C:/Users/PSagar/Downloads/Divvy_trip_data_FY2020/202011-divvy-tripdata.csv")
tripdata_202012 <- read.csv("C:/Users/PSagar/Downloads/Divvy_trip_data_FY2020/202012-divvy-tripdata.csv")
```

# Data wrangling and combine into single dataframe
```{r}
colnames(tripdata_Q1_2020)
colnames(tripdata_202004)
colnames(tripdata_202005)
colnames(tripdata_202006)
colnames(tripdata_202007)
colnames(tripdata_202008)
colnames(tripdata_202009)
colnames(tripdata_202010)
colnames(tripdata_202011)
colnames(tripdata_202012)
```

```{r structure of df}
str(tripdata_202012)
```
```{r convert datatype}
tripdata_202012 <- mutate(tripdata_202012, start_station_id = as.integer(start_station_id), end_station_id = as.integer(end_station_id))
```

```{r add all df into single df}
all_trips <- bind_rows(tripdata_Q1_2020, tripdata_202004, tripdata_202005, tripdata_202006, tripdata_202007, tripdata_202008, tripdata_202009, tripdata_202010, tripdata_202011, tripdata_202012)
```

#rename colnames
```{r rename columns}
all_trips <- all_trips %>%
  rename(trip_id = ride_id
         ,  bike_id = rideable_type
         , start_time = started_at
         , end_time = ended_at
         , from_station_name = start_station_name
         , from_station_id = start_station_id
         , to_station_name = end_station_name
         , to_station_id = end_station_id
         , usertype = member_casual
  )
```


```{r insepct structure}
str(all_trips)
```

#remove not required columns
```{r}
all_trips <- all_trips %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))
```

#Perform data cleaning and prepare data for analysis

```{r list of colnames}
colnames(all_trips)
```
```{r number of rows in dataframe}
nrow(all_trips)
```
```{r dimension of dataframe}
dim(all_trips)
```
```{r see first 6 rows of df}
head(all_trips)
```
```{r see structure}
str(all_trips)
```
```{r summarize data}
summary(all_trips)
```
```{r display entries in usertype}
table(all_trips$usertype)
```

#create calculated columns to aggregate data

```{r create date column}
all_trips$date <- as.Date(all_trips$start_time)
```

```{r create day | month | year | day_of_week}
all_trips$day <- format(as.Date(all_trips$date),"%d")
all_trips$month <- format(as.Date(all_trips$date),"%m")
all_trips$year <- format(as.Date(all_trips$date),"%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date),"%A")
```

```{r add column ride_length to calcuate duration of trips}
all_trips$ride_duration <- difftime(all_trips$end_time,all_trips$start_time)
```

```{r inspect the structure}
str(all_trips)
```
```{r factor}
is.factor(all_trips$ride_duration)
```
```{r convert ride_duration datatype}
all_trips$ride_duration <- as.numeric(as.character(all_trips$ride_duration))
```

```{r numeric}
is.numeric(all_trips$ride_duration)
```
```{r remove bad data}
all_trips_v2 <- all_trips[!(all_trips$from_station_name == "HQ QR" | all_trips$ride_duration<0),]
```

#Conduct Descriptive Analysis

```{r mean of ride duration}
mean(all_trips_v2$ride_duration)
```
```{r midpoint number}
median(all_trips_v2$ride_duration)
```
```{r longest ride}
max(all_trips_v2$ride_duration)
```
```{r shortest ride}
min(all_trips_v2$ride_duration)
```
```{r}
summary(all_trips_v2$ride_duration)
```
#Comparing member and casual users

```{r}
aggregate(all_trips_v2$ride_duration ~ all_trips_v2$usertype, FUN = mean)
aggregate(all_trips_v2$ride_duration ~ all_trips_v2$usertype, FUN = median)
aggregate(all_trips_v2$ride_duration ~ all_trips_v2$usertype, FUN = max)
aggregate(all_trips_v2$ride_duration ~ all_trips_v2$usertype, FUN = min)
```
#see the average ride duration by each day for members vs casual users
```{r ride_duration by each day}
aggregate(all_trips_v2$ride_duration ~ all_trips_v2$usertype + all_trips_v2$day_of_week, FUN = mean)
```
# Notice that the days of the week are out of order

```{r order day_of_week}
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday","Monday","Tuesday","Wednesday","Thrusday","Friday","Saturday"))
```

# Re-run average ride by each day for members vs casual users

```{r average ride_duration per user per day}
aggregate(all_trips_v2$ride_duration ~ all_trips_v2$usertype + all_trips_v2$day_of_week, FUN = mean)
```
# Analyze ridership data by type and weekday
```{r}
all_trips_v2 %>%
  mutate(weekday = wday(start_time,label = TRUE)) %>% #creates weekday field using wday func
  group_by(usertype,weekday) %>% #groups by usertype and weekday
  summarise(number_of_rides = n(),average_duration = mean(ride_duration)) %>% #calculates the number of rides and average duration 
  arrange(usertype,weekday) #sorts usertype and weekday
```
# Visualize the number of rides by rider type

```{r}
all_trips_v2 %>%
  mutate(weekday = wday(start_time, label = TRUE)) %>%
  group_by(usertype, weekday) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_duration)) %>%
  arrange(usertype, weekday) %>%
  ggplot(aes(x=weekday,y=number_of_rides,fill=usertype)) + geom_col(position = "dodge")
```

#Create visualization for average duration

```{r}
all_trips_v2 %>%
  mutate(weekday = wday(start_time, label = TRUE)) %>%
  group_by(usertype, weekday) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_duration)) %>%
  arrange(usertype, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration, fill = usertype)) + geom_col(position = "dodge")
```

