q1_2019 <- read_csv("Divvy_Trips_2019_Q1.csv")
q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")
colnames(q1_2019)
colnames(q1_2020)
(q1_2019 <- rename(q1_2019
,ride_id = trip_id
,rideable_type = bikeid
,started_at = start_time
,ended_at = end_time
,start_station_name = from_station_name
,start_station_id = from_station_id
,end_station_name = to_station_name
,end_station_id = to_station_id
,member_casual = usertype
))

str(q1_2019)
str(q1_2020)


q1_2019 <-  mutate(q1_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 


all_trips<-bind_rows(q1_2019,q1_2020)


all_trips <- all_trips %>%
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, "tripduration"))


nrow(all_trips)
colnames(all_trips)
dim(all_trips)
head(all_trips)
str(all_trips)
summary(all_trips)


table(all_trips$member_casual)

all_trips <-  all_trips %>% 
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))



table(all_trips$member_casual)


all_trips$date<-as.Date(all_trips$started_at)
all_trips$year<-format(all_trips$started_at, "%y")
all_trips$month<-format(all_trips$started_at, "%m")
all_trips$day<-format(all_trips$started_at, "%d")
all_trips$day_of_the_week<-format(all_trips$started_at, "%A")

colnames(all_trips)


all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

str(all_trips)

is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)



all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]


mean(all_trips_v2$ride_length)
median(all_trips_v2$ride_length)
max(all_trips_v2$ride_length)
max(all_trips_v2$ride_length)
summary(all_trips_v2$ride_length)


aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)


aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_the_week,
FUN = mean)

all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_the_week, levels=c("Sunday", "Monday",
"Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))


aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week,
FUN = mean)


all_trips_v2 %>%
mutate(weekday = wday(started_at, label = TRUE)) %>%
group_by(member_casual, weekday) %>%
summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>%
arrange(member_casual, weekday)


aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week,
FUN = mean)


all_trips_v2$z_score <- scale(all_trips_v2$ride_length)



library(ggplot2)

ggplot(all_trips_v2, aes(x = member_casual, y = ride_length, fill = member_casual)) +
  geom_boxplot(fill = c("Annual Member" = "red", "Casual" = "blue"), color = "black") +
  geom_point(data = outliers, aes(x = member_casual, y = ride_length), color = "purple", size = 3) +
  scale_fill_manual(values = c("Annual Member" = "red", "Casual" = "blue"), guide = "legend") +
  labs(title = "Boxplot of Ride Length by Member Type with Outliers",
       x = "Member Type",
       y = "Ride Length (seconds)")



all_trips<-all_trips_v2 %>%
  mutate(month = format(started_at, "%B")) %>%
  group_by(member_casual, year) %>%
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length)) %>%
  arrange(member_casual, year)
 %>% ggplot(aes(x = year, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Number of Rides per Year",
       x = "Year",
       y = "Number of Rides",
       fill = "Member Type") +
  scale_fill_manual(values = c("member" = "purple", "casual" = "blue")) +
  scale_y_continuous(labels = scales::comma)



all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Number of Rides by Weekday",
       x = "Weekday",
       y = "Number of Rides",
       fill = "Member Type") +
  scale_fill_manual(values = c("member" = "purple", "casual" = "blue")) +
  scale_y_continuous(labels = scales::comma) +  # Format y-axis labels
  theme_minimal() +
  theme(legend.position = "top")


counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual +
all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = 'avg_ride_length.csv')

colnames(all_trips_v2$ride_length)