library(dplyr)

flights <- readRDS("data/flights.RData")

x1 <- filter(flights, arr_delay >= 120)
x2 <- filter(flights, dest=="IAH" | dest=='HOU')
x3 <- filter(flights, carrier=='UA' | carrier=='AA' | carrier=='DL')
x4 <- filter(flights, month == 8 | month == 7 | month == 6)
x5 <- filter(flights, arr_delay > 120 & dep_delay == 0)
x6 <- filter(flights, dep_delay>=60 & dep_delay > arr_delay+31)
x7 <- filter(flights, between(flights$dep_time, 0000, 0600))

nrow(filter(flights, is.na(dep_time)))

sum(is.na(flights$dep_time))
sum(is.na(flights$year))
sum(is.na(flights$month))
sum(is.na(flights$day))
sum(is.na(flights$dep_time))
sum(is.na(flights$sched_dep_time))
sum(is.na(flights$dep_delay))
sum(is.na(flights$arr_time))
sum(is.na(flights$sched_arr_time))
sum(is.na(flights$arr_delay))
sum(is.na(flights$carrier))
sum(is.na(flights$flight))
sum(is.na(flights$tailnum))
sum(is.na(flights$origin))
sum(is.na(flights$dest))
sum(is.na(flights$air_time))
sum(is.na(flights$distance))
sum(is.na(flights$hour))
sum(is.na(flights$minute))
sum(is.na(flights$time_hour))

b1<-arrange(flights, !is.na(arr_delay), flights$arr_delay) 
b2<-arrange(flights, flights$arr_delay) 
b3<-arrange(flights, -flights$arr_delay) 
flights$b4 <- flights$sched_dep_time - flights$dep_time 
b5<-arrange(flights,flights$sched_dep_time > flights$b4) 
b6<- arrange(flights, flights$air_time) 

c1 <- select(flights, dep_time, dep_delay, arr_time) 
c2 <- select(flights, dep_time:dep_delay) 
c3 <- select(flights, -dep_time) 
c4 <- select(flights, time_hour, air_time, everything()) 
c5 <- select(flights, dep_time, arr_time)