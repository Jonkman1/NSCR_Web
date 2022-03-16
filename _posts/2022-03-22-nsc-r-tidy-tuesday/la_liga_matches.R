# NSC-R Tidy Tuesday March 22, 2022

# Install packages
install.packages('tidyverse')
install.packages('lubridate')
install.packages('devtools')
library(devtools)
install_github("jalapic/engsoccerdata")

# Load libraries
library(tidyverse)
library(lubridate)
library(engsoccerdata)

# What is in engsoccerdata?
data(package="engsoccerdata")    # lists datasets currently available

# What is in 'spain'?
spain %>% glimpse()

