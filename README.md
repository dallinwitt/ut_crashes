# Utah Car Crash Type and Time Distribution 
## Data Analysis and Visualization

#### Motivation
As an avid cyclist - as well as a driver -  living in Salt Lake City, my safety is always at the front of my mind. I wanted to analyze crash statistics in Utah and Salt Lake City specifically, to see what trends emerged, and how I could structure my own life in a way that minimized undue risk.

I came into this task with two primary questions:
1. What impact does time of day have on the likelihood of a crash involving a cyclist or pedestrian?
2. What effect does sunrise or sun set have on the likelihood of an accident in general?

To answer these questions, I used data from the [Utah open data catalog](https://opendata.utah.gov/Public-Safety/State-of-Utah-Crash-Data-2015-2019/7ihm-46s4), as well as sunrise and sunset data from [Time and Date](https://www.timeanddate.com/sun/usa/salt-lake-city).

#### Methods
I analyzed this dataset using the R tidyverse, primarily lubridate, dplyr and ggplot2. I plotted the absolute and individual distributions of crashes of three types (car only, car and pedestrian, and car and cyclist) for crashes that took place in the city of Salt Lake. 

To analyze the impact of sunrise and sunset, I assigned each crash a minute values based on how long before or after sunrise or sunset it took place. I plotted all of the crashes that took place within one hour of one of these events in a histogram, and examined for meaningful trends.

#### Outcomes
Surprisingly, even though car crashes involving a cyclist are the most common around 6 PM, the car crashes that occur between 9 and 10 PM are the most likely to involve a cyclist.

In the sunrise/sunset analysis, the two events showed radically different profiles. While crashes consistently declined over the two hours around sunrise, crashes actually took a dip right around sunset, and were higher before and after.
