#import necessary libraries: dplyr, ggplot2, and lubridate
library(dplyr)
library(ggplot2)
library(lubridate)

#define the data types of the colums
col_types <- c('CRASH_DATE' = 'character', 
               'PEDESTRIAN' = 'logical', 
               'BICYCLIST_' = 'logical', 
               'MOTORCYCLE' = 'logical', 
               'IMPROPER_R' = 'logical', 
               'UNRESTRAIN' = 'logical', 
               'DUI' = 'logical', 
               'INTERSECTI' = 'logical', 
               'WILD_ANIMA' = 'logical', 
               'DOMESTIC_A' = 'logical', 
               'OVERTURN_R' = 'logical', 
               'COMMERCIAL' = 'logical', 
               'TEENAGE_DR' = 'logical', 
               'OLDER_DRIV' = 'logical', 
               'NIGHT_DARK' = 'logical', 
               'SINGLE_VEH' = 'logical', 
               'DISTRACTED' = 'logical', 
               'DROWSY_DRI' = 'logical', 
               'ROADWAY_DE' = 'logical')
#import the csv using col_types as the columnClasses
crash <- read.csv("State_of_Utah_Crash_Data_2015-2019.csv", stringsAsFactors = TRUE, colClasses=col_types)
SRSS_times <- read.csv("SLC_sun_schedule.csv")

#convert CRASH_DATE column to datetime using format mm/dd/yyyy H:M:S AM/PM
crash$CRASH_DATE <- as.POSIXct(crash$CRASH_DATE, tz = "America/Denver", format = "%m/%d/%Y %I:%M:%S %p")

#create columns that contain the hour, clock time, and calendar day (w/o year) of the crash
crash <- crash %>%
    mutate(crash_hour = hour(CRASH_DATE)) %>%
    mutate(crash_time = strftime(CRASH_DATE, format="%H:%M:%S")) %>%
    mutate(crash_day = strftime(CRASH_DATE, format="%m/%d")) %>%
    mutate(crash_time_15 = crash_hour + ((floor(minute(CRASH_DATE)/15))/4)) %>%
    mutate(crash_time_30 = crash_hour + ((floor(minute(CRASH_DATE)/30))/2))

#Use only columns 2 through 11 of the SRSS_times df
SRSS_times <- SRSS_times[c(2:11)]

#inner join the two dfs so SRSS data is attached to each crash
crash_srss <- crash %>%
    inner_join(SRSS_times, by = c("crash_day" = "Date2"))

#define a function that returns which other modes (omode) were involved in the crash
bikeped <- function(bike, ped) {
    if (bike == TRUE & ped == FALSE){
        omode <- "BIKE"
    }else if (bike == FALSE & ped == TRUE){
        omode <- "PEDESTRIAN"
    }else if (bike == TRUE & ped == TRUE){
        omode <- "BIKE & PEDESTRIAN"
    }else {
        omode <- "CAR ONLY"
    }
    return (omode)
}

#create a new column in df using bikeped fxn
crash_srss <- crash_srss %>% rowwise %>%
    mutate(modes = bikeped(BICYCLIST_, PEDESTRIAN))

#graph generic crash data (all crashes) for 2 hrs before and after sunrise and sunset (2 graphs)
#create separate dfs for morning (crash_sr) and evening (crash_ss)
crash_sr <- crash_srss %>%
    filter(crash_hour <= 11)
crash_ss <- crash_srss %>%
    filter(crash_hour >= 12)

#create a new column in both crash_sr and crash_ss dfs to indicate time difference between the crash
#and sunrise or sunset
crash_sr <- crash_sr %>%
    mutate(sr_diff = strptime(Sunrise, format = "%H:%M:%S") - strptime(crash_time, format = "%H:%M:%S"))

crash_ss <- crash_ss %>%
    mutate(ss_diff = strptime(Sunset, format = "%H:%M:%S") - strptime(crash_time, format = "%H:%M:%S"))

#for both dfs, convert drtn type to numeric, in units of minutes
#filter for events that are between -60 and 60

crash_sr$sr_diff <- as.numeric(crash_sr$sr_diff, units = "mins")

crash_sr_narrow <- crash_sr %>%
    filter(sr_diff >= -60 & sr_diff <= 59)

crash_ss$ss_diff <- as.numeric(crash_ss$ss_diff, units = "mins")

crash_ss_narrow <- crash_ss %>%
    filter(ss_diff >= -60 & ss_diff <= 59)

#graph the density plot of crashes around sunrise and sunset
ggplot(crash_sr_narrow, aes(x = sr_diff))+
    geom_histogram(binwidth = 2)+
    scale_x_continuous(breaks = seq(-60, 60, by = 15))+
    ylim(0, 700)

ggplot(crash_ss_narrow, aes(x = ss_diff))+
    geom_histogram(binwidth = 2)+
    scale_x_continuous(breaks = seq(-60, 60, by = 15))+
    ylim(0, 700)


