
####################################################
##  load  packages in your current session #########
####################################################

library(data.table)
library(ggplot2)
library(tidyverse)
library(chron)
library(dplyr)

#####################################################
##                  Import Data             #########
#####################################################


# Import downloaded data 
##green <- fread("https://s3.amazonaws.com/nyc-tlc/trip+data/green_tripdata_2015-09.csv")
green <- fread("green_tripdata_2015-09.csv")

# Check dimensions
dim_green <- dim(green)
dim_green

# there are 1494926 observations and 21 variables

# column names of table
column <- colnames(green)
column

# View the head and tail of the data

#green_head <- head(green, 10)
#green_tail <- tail(green,10)

#View(green_head)
#View(green_tail)

# Check Data Type of each variable
var_type <- sapply(green, class)
var_type

# Check Unique records
unique_rec <- unique(green)
dim(unique_rec)

#No duplicate records in the data set

#table(green$Trip_distance)


# Check missing values

green_missing <- colSums(is.na(green))
green_missing

# Summary of green table
summary(green)

####################
#temp_1 <- green[green$Payment_type == 2, ]
#temp_2 <- green[green$Payment_type == 3, ]
#tail(temp_1,25)
#table(temp_2$Tip_amount)
#temp_2
#####################


##sum(colSums(is.na(green)))

####################################################
##                  Data Cleaning          #########
####################################################

# drop Ehail_fee as the whole column has missing values. 
# drop Store_and_fwd_flag as I will not use it for this coding challenge
# drop Trip_type as it will not be required for my analysis
#green[which(green$Total_amount == min(green$Total_amount)), ]
#green[green$Total_amount < 0, ]
# create temporary green table green_temp which will have cleaned values

green_temp <- green %>% 
             select(-c( Ehail_fee, Store_and_fwd_flag, Trip_type, VendorID)) %>%
             filter((Fare_amount >= 0) & 
                      (Extra >= 0) & 
                      (MTA_tax >= 0) &
                      (Tip_amount >= 0) & 
                      (Tolls_amount >= 0) &  
                      (improvement_surcharge >= 0) &
                      (Total_amount >= 2.5))


dim(green_temp)
str(green_temp)
summary(green_temp)
sum(is.na(green_temp))

# histogram 
##hist(green_temp$Trip_distance, xlim=c(0,30), breaks = 1000000, main = "Histogram of Trip Distance", xlab = "Trip Distance")
ggplot(green_temp, aes(x= Trip_distance))+
  geom_histogram(binwidth = 0.3, aes(fill = ..count..)) +
  scale_fill_gradient( name = "Frequency",
    low = "green",
    high = "red", labels = c("0", "40K", "80k","120k","160k")) +
  coord_cartesian(xlim = c(0,10)) +
  ggtitle("Histogram for Trip Distance")+
  xlab("Trip Distance(miles)") + 
  ylab("Frequency of Trip Distance") 
  
  
##################################################
# Calculate average speed for a trip using pick ## 
# up time, drop time and trip distance          ##      
##################################################

## install and load chron package to extract hour value from datatime data type
#install.packages("chron")
#library(chron)
## Append column hour_time 

green_temp$hour_time <- format(as.POSIXct(green_temp$lpep_pickup_datetime, 
                                          format = "%Y-%m-%d %H:%M"), 
                               format = "%H")
dim(green_temp)
sum(is.na(green_temp))

## Hourly mean Trip Distance and hourly median trip distance

mean_trip_distance <- green_temp %>% select(Trip_distance, hour_time) %>%
                      group_by(hour_time) %>% 
                      summarise(mean_trip_distance = mean(Trip_distance))
head(mean_trip_distance,5)
median_trip_distance <- green_temp %>% 
                        select(Trip_distance, hour_time) %>%  
                        group_by(hour_time) %>% 
                        summarise(median_trip_distance = median(Trip_distance))
head(median_trip_distance,5)


############################################
########### Calculating Average Speed ######
############################################

green_temp$trip_duration <- as.POSIXct(green_temp$Lpep_dropoff_datetime) - 
            as.POSIXct(green_temp$lpep_pickup_datetime)
units(green_temp$trip_duration) <- "hours"

#average speed for each trip
green_temp$avg_speed <- green_temp$Trip_distance/as.numeric(green_temp$trip_duration)

green_avg_speed <- green_temp %>% filter(Trip_distance > 0 & trip_duration > 0) %>%
                  mutate(avg_speed = Trip_distance/as.numeric(trip_duration)) %>%
                  select(lpep_pickup_datetime, hour_time, 
                         Trip_distance, trip_duration, avg_speed) 

colSums(is.na(green_avg_speed))

# fetch week_number 
green_avg_speed$week_number <- sapply(strsplit(
               as.character(green_avg_speed$lpep_pickup_datetime), " "),"[", 1) %>%
                strftime(format = "%V") %>%
                as.numeric()
unique(green_avg_speed$week_number)

hourly_average_speed <-  green_avg_speed %>% 
                     group_by(hour_time) %>%
                    summarise(avg_speed_hourly = mean(avg_speed, na.rm = TRUE))
                     
ggplot(hourly_average_speed, aes(x = hour_time, y = avg_speed_hourly, fill = hour_time)) +
  geom_bar(stat = "identity")

weekly_average_speed <- green_avg_speed %>% 
                         group_by(week_number) %>%
                         summarise(avg_speed_weekly = mean(avg_speed, na.rm = TRUE))

ggplot(weekly_average_speed, aes(x = week_number , y = avg_speed_weekly)) +
   geom_bar( stat = "identity", aes(fill = factor(week_number))) +
  labs(title =" Weekly Average Speed in September 2015", 
         x = "Week Number of the Year 2015",
         y = "Weekly Average Speed") +
             scale_fill_discrete(name = "Week number")


#####################################################
################ Hypothesis Testing #################
#####################################################

hypo_weekly_speed <- aov(week_number ~ avg_speed_weekly, weekly_average_speed)
summary(hypo_weekly_speed)
  
#################################################################
#### Linear Regression ##########################################
### Check for linear relation between Tip Amount and Total Fare##
#################################################################
 
quantile(green_temp$Fare_amount, probs = 0.99)
quantile(green_temp$Fare_amount, probs = 0.01)
quantile(green_temp$Tip_amount, probs = 0.99)
quantile(green_temp$Tip_amount, probs = 0.01)

green_sample  <-  green_temp[sample(1:nrow(green_temp), 160000,
                               replace = FALSE),]

tip_relation <- green_sample %>%
                filter(Payment_type == 1 & Fare_amount >= 3 &
                                        Fare_amount <=  50) %>%
                select(-c(1,2,3,4,5,6,7,20)) %>%
                mutate(total_fare = Fare_amount + Extra + Tolls_amount +
                                    improvement_surcharge + MTA_tax) 
              

ggplot(tip_relation, aes(x = total_fare, y = Tip_amount)) +
  geom_point(alpha = 0.3) +
  geom_jitter() +
  geom_smooth() +
  coord_cartesian(xlim = c(0,60), ylim = c(0,15)) +
  ggtitle("Relation between Tip amount and Total fare")+
  xlab("Total Fare") + 
  ylab("Tip Amount")

linear_model <- lm(Tip_amount ~ total_fare, data = tip_relation)
summary(linear_model)
plot(linear_model)











