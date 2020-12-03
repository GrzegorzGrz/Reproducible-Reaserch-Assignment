#read date
library(lubridate)
unzip(zipfile = "repdata_data_activity.zip")
db <- read.csv("activity.csv", header = TRUE, na.strings = "NA")
db$date <- ymd(db$date)

#step1 What is mean total number of steps taken per day?
total_steps_day <- aggregate(steps ~ date, data=db, sum)
hist(total_steps_day$steps, breaks = 10, xlab = "number of steps per day", main= NULL)

#Mean and median number of steps taken each day

mean(total_steps_day$steps)
median(total_steps_day$steps)


#avarege number of steps per interval
av_steps_interval <- aggregate(steps ~ interval, data=db, mean)

#plot above pattern


plot(av_steps_interval)
mx <- max(av_steps_interval$steps)

#ggplot alternatives
#library(ggplot2)
#ggplot(av_steps_interval, aes(interval, steps)) + geom_point(alpha = 1/2)
#qplot(interval, steps, data=av_steps_interval, xlab="interval", ylab="steps")

#max steps
mx <- max(av_steps_interval$steps)
#which interval is highest
mx_interval <- av_steps_interval[av_steps_interval$steps == mx, 1]

abline(v = mx_interval, col = "red")
abline(h = mx, col = "red")


#============== second scenario fixing NA============
#checking # of na
na_in_steps <- sum(is.na(db$steps))
na_in_interval <- sum(is.na(db$interval))
na_in_date <- sum(is.na(db$date))

#replacing na with av value for a given interval
db_no_na <- db

for (i in c(1:length(db_no_na$steps))) {
    if (is.na(db_no_na[i,1]) == TRUE) {
      db_no_na[i,1] <- av_steps_interval[i,2]
      } 
}


#step1 What is mean total number of steps taken per day? -- NA FIXED
total_steps_day_NA_fixed <- aggregate(steps ~ date, data=db_no_na, sum)
par(mfrow = c(1,2))
hist(total_steps_day_NA_fixed$steps, breaks = 10, xlab = "number of steps per day, NA fixed", main= NULL)
hist(total_steps_day$steps, breaks = 10, xlab = "number of steps per day", main= NULL)
#Mean and median number of steps taken each day

mean(total_steps_day_NA_fixed$steps)
median(total_steps_day_NA_fixed$steps)

#============weekdays/weekends

weekday <- weekdays(db_no_na$date)
db_no_na_w<- cbind(db_no_na, weekday)
av_steps_interval_na_fixed_weekend <- aggregate(steps ~ interval, data=subset(db_no_na_w, db_no_na_w$weekday == "Saturday" | db_no_na_w$weekday == "Sunday" ), mean)
av_steps_interval_na_fixed_week_day <- aggregate(steps ~ interval, data=subset(db_no_na_w, db_no_na_w$weekday != "Saturday" | db_no_na_w$weekday != "Sunday" ), mean)
par(mfrow = c(1,2))
plot(av_steps_interval_na_fixed_weekend, main = "Weekend")
plot(av_steps_interval_na_fixed_week_day, main = "Week day")

##
