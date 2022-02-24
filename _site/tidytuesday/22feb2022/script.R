################################################################################
# Title: NSC-R TT. Wrangling dates-time data.                                   #
# Date: 22/02/2022                                                             #
# Author: Alex Trinidad                                                        #
# Source:                                                                      #  
citation("tidyverse")                                                          #
citation("lubridate")                                                          #
citation("tidytuesdayR")                                                       #
################################################################################
# Install TT package (if necessary)
# install.packages("tidytuesdayR")
# install.packages("tidyverse")

# Load packages
library(tidyverse)
library(lubridate)
library(tidytuesdayR)


# Download data. 
mydatalist <- tidytuesdayR::tt_load("2021-06-29")

# Data as tbl
mydata <- mydatalist$animal_rescues

# Explore the data
glimpse(mydata)

# Do we have missing data? 
summary(mydata)

# Create a unique ID 
mydata <- mydata %>% 
  arrange(cal_year) %>% 
  mutate(uid = paste0(seq(1:n()), LETTERS, letters))

# Any duplicated?
table(duplicated(mydata$uid))

# Select variables of interest
mydataselection <- mydata %>% 
  select(uid, date_time_of_call, type_of_incident, animal_group_parent, borough_code)

# Frequency of type of animal
myfreq <- mydataselection %>% 
  group_by(animal_group_parent) %>% 
  summarise(freq = n()) %>% 
  arrange(-freq)

# Remove unkonwn type of animals
mydataselection <- mydataselection %>% 
  filter(!grepl("Unknown", animal_group_parent))

myfreq <- mydataselection %>% 
  group_by(animal_group_parent) %>% 
  summarise(freq = n()) %>% 
  arrange(-freq)

# Merging cat counts
mydataselection$animal_group_parent <- recode(mydataselection$animal_group_parent,
                                              "cat" = "Cat")

# Nick van Doormaal suggestion
mydataselection$animal_group_parent <- tolower(mydataselection$animal_group_parent)

# Working with Date-Time Data -------------------------------------------------------
# We want to separate the date in year, month, day, hour....
# What variable type is the date in our data set?
glimpse(mydataselection)

# If not "date" format, transform ir 
mydatadate <- mydataselection %>% 
  mutate(datetime = lubridate::as_datetime(date_time_of_call, 
                                   format = "%d/%m/%Y %H:%M"))
# # Non-lubridate Alternative
# mydatadate <- mydataselection %>% 
#   mutate(datetime = strptime(date_time_of_call,
#                          format ="%d/%m/%Y %H:%M", 
#                          tz = "Europe/London"))
# OlsonNames() # function for for the tz

# Create separate 
mydatadate <- mydataselection %>% 
  mutate(datetime = as_datetime(date_time_of_call,
                            format ="%d/%m/%Y %H:%M"),
         day = day(datetime),
         month = month(datetime),
         year = year(datetime),
         hour = hour(datetime),
         minute = minute(datetime),
         date = as_date(datetime))

head(mydatadate[, 6:12])

# How many cases per day?

caseperday <- mydatadate %>% 
  group_by(date) %>% 
  summarise(resc_counts = n())

# Plot trends of cases
ggplot(data = caseperday, 
       aes(
         x = date,
         y = resc_counts
       )) +
  geom_line()

# How many cases per year?

mydatadate %>% 
  group_by(year) %>% 
  summarise(resc_counts = n()) %>% 
  ggplot() +
  aes(
    x = year,
    y = resc_counts
  ) + 
  geom_line()

# Is a rescue every day?
perday <- mydatadate %>% 
          group_by(date) %>% 
          summarise(resc_counts = n())

# How many days are (more or less) in those years?
length(unique(mydatadate$year)) * 365

# How can I know the days that are missing? 
# Create a data set with all the days 
compdates <- data.frame(date = c(seq(ymd('2009-01-01'), 
                                     ymd('2021-12-31'), by = '1 day')))

# Save missing dates
missingdates <- anti_join(compdates, perday)

# Add missing dates to our data set

fulldates <- rbind(perday, missingdates) #Error because we need the same arguments

# We need the same arguments

missingdates <- missingdates %>% 
  mutate(resc_counts = vector(mode = "numeric", length = length(.)))

# Add missing dates to our data set
fulldates <- rbind(perday, missingdates)

# Any date duplicated?
table(duplicated(fulldates$date))

# Wim Bernasco suggestion instead of using anti_join() and rbind(), use lef_join
fulldates <- left_join(compdates, perday, by = "date") %>% 
  replace(is.na(.), 0)

# Separate the date ymd
fulldates <- fulldates %>% 
  mutate(year = year(date),
         month = month(date),
         day = day(date))

# What week of the year did it happen?
fulldates <- fulldates %>% 
  mutate(week = week(date))


# Day of the week 
fulldates <- fulldates %>% 
  mutate(weekday = wday(date, label = TRUE))


# Plot by week
byweek <- fulldates %>% 
  group_by(year, week) %>% 
  summarise(resc_counts = sum(resc_counts))

ggplot(data = byweek) +
  geom_line(aes(x = week, y = resc_counts), size = 1) +
  facet_wrap(vars(year),scales = "free_x") 


# Plot Trends by Type of Animal -------------------------------------------
# Accounting the type of animals 


cat <- mydatadate %>% 
  filter(animal_group_parent == "cat") %>%  
  group_by(date, animal_group_parent) %>% 
  summarise(resc_counts = n())

mdatecat <- anti_join(compdates, cat)

fullcat <- rbind(cat, mdatecat) %>% 
  mutate(animal_group_parent = "cat") %>% 
  replace(is.na(.),0)

dog <- mydatadate %>% 
  filter(animal_group_parent == "dog") %>%  
  group_by(date, animal_group_parent) %>% 
  summarise(resc_counts = n())

mdatedog <- anti_join(compdates, dog)

fulldog <- rbind(dog, mdatedog) %>% 
  mutate(animal_group_parent = "dog") %>% 
  replace(is.na(.),0)

bird <- mydatadate %>% 
  filter(animal_group_parent == "bird") %>%  
  group_by(date, animal_group_parent) %>% 
  summarise(resc_counts = n())

mdatebird <- anti_join(compdates, bird)

fullbird <- rbind(bird, mdatebird) %>% 
  mutate(animal_group_parent = "bird") %>% 
  replace(is.na(.),0)

myfulldata <- rbind(fullcat,fulldog, fullbird) 

# dates by components
myfulldata <- myfulldata %>% 
  mutate(day = day(date),
         month = month(date, label = TRUE),
         year = year(date),
         week = week(date),
         weekday = wday(date, label = TRUE))

# By day of the week
bywday <- myfulldata %>% 
  group_by(year, weekday, animal_group_parent) %>% 
  summarise(resc_counts = sum(resc_counts))

# What levels are in weekday
levels(bywday$weekday)

# Order the levels
levelorder <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

ggplot(data = bywday) +
  geom_line(aes(x = factor(weekday, level = levelorder), 
                y = resc_counts, 
                group = animal_group_parent, 
                color = animal_group_parent), size = 1) +
  facet_wrap(vars(year), scales = "free_x")+
  labs(
    title = "Animal Rescue per Weekday",
    x = "Weekday",
    y = "Count of Rescues",
    color = "Animal"
  )

# By week of the year
myfulldata <- myfulldata %>% 
  mutate(weekyear = paste0(week,month,day))

byweek <- myfulldata %>% 
  group_by(year, weekyear, animal_group_parent) %>% 
  summarise(resc_counts = sum(resc_counts))

ggplot(data = byweek) +
  geom_line(aes(x = weekyear, 
                y = resc_counts, 
                group = animal_group_parent, 
                color = animal_group_parent), size = 1) +
  facet_wrap(vars(year), scales = "free_x") +
  labs(
    title = "Animal Rescue per Week",
    x = "Week",
    y = "Count of Rescues",
    color = "Animal"
  )


# By month of the year

bymonth <- myfulldata %>% 
  group_by(year, month, animal_group_parent) %>% 
  summarise(resc_counts = sum(resc_counts))

ggplot(data = bymonth) +
  geom_line(aes(x = month, 
                y = resc_counts, 
                group = animal_group_parent, 
                color = animal_group_parent), size = 1) +
  facet_wrap(vars(year), scales = "free_x") +
  labs(
    title = "Animal Rescue per Month",
    x = "Month",
    y = "Count of Rescues",
    color = "Animal"
  )

# By year

byyear <- myfulldata %>% 
  group_by(year,animal_group_parent) %>% 
  summarise(resc_counts = sum(resc_counts))

ggplot(data = byyear) +
  geom_line(aes(x = year, 
                y = resc_counts, 
                group = animal_group_parent, 
                color = animal_group_parent), size = 1) +
  labs(
    title = "Animal Rescue per Year",
    x = "Year",
    y = "Count of Rescues",
    color = "Animal"
  )


