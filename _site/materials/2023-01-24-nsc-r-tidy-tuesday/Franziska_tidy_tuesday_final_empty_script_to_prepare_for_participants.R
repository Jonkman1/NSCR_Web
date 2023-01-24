###
#  title: "Franziska's tidy tuesday"
#  date: '2023-01-16'
# Thank you to Wim Bernasco who provided detailed feedback on the script and supervision on preparing this session
###

#### Structure ####
    # this script consists of subsection per question. Of you enjoy having a nice structured overview you can click 
    # on the arrows on the left. This opens and closes the subsections. You can also use the following short-cuts
    # collapsed (with Alt-o) and expanded (with Alt-Shift-o)
    # for with individual sections (Alt-l and Alt-Shift-l)


#### Remove environment ####
rm(list = ls())

#### Install needed libraries ####
    #install.packages("tidyverse")
    #install.packages("priceR")

#### Load needed libraries ####
library(tidyverse)   # general data wrangling functions, e.g. 'readr'
library(priceR)      # handling price and currency data

#### Load data ####
      # Salary survey data
survey <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-18/survey.csv')

#### Question 1: What kind of variables are in the dataset ####


#### Question 2: How many people participated in the survey? ####


#### Question 3:  what was the distribution of the ages of the people that participated in the survey? ####



#### Question 4:  make a dataframe with the variables age, gender, annual salary, currency, other_monetary_comp ####


#### Question 5:  What is the distribution of gender in percentages ####



#### Question 6: Get the min, max, mean, sd, median, IQR and amount of missings for the variable annual salary ####




#### Question 7: Plot the distribution of annual salary ####




#### Question 8: Can we get a salary independent of currency? ####

    
#### Question 9: Compare salary across genders? ####

  
#### Bonus question: Create the overall mean score: p####
      # The happy_1,2 and 3 describe a participants happiness score. Add a column that shows the mean of happiness per person.
      # For people that have >50% missing on the happiness scale make the mean NA

additional <- data.frame(id = c("p1", "p2", "p3", "p4", "p5"),
                         happy_1 = c(1,2,3,4,NA),
                         happy_2 = c(1,2,3,NA,5),
                         happy_3 = c(1,2,3,NA,5))

