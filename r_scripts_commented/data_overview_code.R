
#Set up ----
library(readr)
library(ggfortify)
library(arrow)
library(skimr)
library(tidymodels)
library(lubridate)
library(patchwork)
library(stringr)

#set seed for reproducibility
set.seed(123)

#data
youtube_data <- read_parquet("data/youtube_metrics.parquet")

#Inspection ---- 

#number of rows and columns
nrow(youtube_data) #92275
ncol(youtube_data) #20

#skimming the data 
skim_without_charts(youtube_data) #description and duration_seconds have missing observations 

#Manipulation ----

#getting rid of missing observations in duration
youtube_data <- youtube_data %>%
  filter(!is.na(duration_seconds))

#skimming data 
skim_without_charts(youtube_data)

#creating derived variables 
youtube_data <- youtube_data %>%
  mutate(released_date = as_date(publishedAt), #date when video was published
         #hour when video was published
         hour_released = hour(publishedAt), 
         #day of the week when video was published
         day_week_released = wday(publishedAt), 
         #number od days from release to becoming trending 
         time_release_trending = as.numeric(trending_date - released_date)) 

#converting day of the week to strings 
youtube_data <- youtube_data %>%
  mutate(day_week_released = if_else(day_week_released == 1, 
                                     "Sunday", 
                                     if_else(day_week_released == 2, 
                                             "Monday", 
                                             if_else(day_week_released == 3, 
                                                     "Tuesday", 
                                                     if_else(day_week_released == 4, 
                                                             "Wednesday", 
                                                             if_else(day_week_released == 5, 
                                                                     "Thursday", 
                                                                     if_else(day_week_released == 6, 
                                                                             "Friday", 
                                                                             if_else(day_week_released == 7, 
                                                                                     "Saturday",
                                                                                     "None"))))))))
#checking final youtube_data 
skim_without_charts(youtube_data)

#save final youtube_data 
save(youtube_data, file = "data/youtube_data.rda")

#FINISH SECTION 

