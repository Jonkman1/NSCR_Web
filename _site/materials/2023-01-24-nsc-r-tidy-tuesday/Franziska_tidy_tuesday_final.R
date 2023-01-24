###
#  title: "Franziska's tidy tuesday"
#  date: '2023-01-16'
# Thank you to Wim Bernasco who provided detailed feedback on the script and supervision on preparing this session
###


#### Introduction ####
# Feedback
# Questions
# Own tidy tuesday



#### Structure ####
    # collapsed (with Alt-o) and expanded (with Alt-Shift-o)
    # for with individual sections (Alt-l and Alt-Shift-l)
    # want to comment or uncomment major sections of your script use CTRL + SHIFT + C
    # 
    # 
    #


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
      # '|>' is equal to '%>%', the latter is included in tidyverse
survey |> names() 
survey |> dplyr::glimpse()

      # What would you look at?
      # 'annual_salary' and 'other_monetary_comp' all variables are text ('strings' in SPSS speak)

#### Question 2: How many people participated in the survey? ####
survey |> nrow() # since 1 row = 1 participant, but using glimpse already indicated that there are 26.232 rows

#### Question 3:  what was the distribution of the ages of the people that participated in the survey? ####
survey |> count(how_old_are_you, sort = TRUE) # The under 18 variable is on the bottom, so not ordered correctly, can we fix this?
survey |> count(how_old_are_you)


  ## Order the categories in this variable
survey <-
  survey |>
  mutate(age_category = factor(how_old_are_you)) # First construct a factor version of 'how_old_are_you'

survey |>
  select(how_old_are_you, age_category) |>
  head() # Show that they are different types (character vs. factor) but contain the same information

  ## Reorder the factor variable in a generic way (without a function call)
survey <-
  survey |>
  mutate(age_category = relevel(age_category,
                                             "under 18",
                                             "18-24",
                                             "25-34",
                                             "35-44",
                                             "45-54",
                                             "55-64",
                                             "65 or over"))
survey <-
  survey |>
  mutate(age_category = relevel(age_category,
                                "under 18"))


survey|>
  count(age_category) # Check whether it looks as desired

  ## Write this to Excel
age <- survey |> # save it in a variable
  count(age_category)

write_csv(age,"C:/Users/FY125/Desktop/temp/Own tidy tuesday/age.csv") 
      # the info in quotation marks has to be changed to a folder that fits your set-up
      # Illustrate in Excel: data -> text to columns

write_delim(age,"C:/Users/FY125/Desktop/temp/Own tidy tuesday/age_delim.csv", delim = ";") # then you automatically get a nice csv
# depends on windows installation which of the two options work

# Use APA packages to write tables for articles ! 

      # So far so good - any questions?

  ## plotting would also be a good idea
survey |>
  ggplot() +
  geom_bar(aes(y = age_category),   # age_category variable on the y axis
           color="black", fill = "white")

      # figuring out syntax without google
survey |>
  ggplot() +
  geom_bar(aes(y = age_category),   # age_category variable on the y axis
           color="blue", fill = "red")

  ## Add title, axis-titles
survey |>
  ggplot() +
  geom_bar(aes(y = age_category),   # age_category variable on the y axis
           color="black", fill = "white") +
  labs(title="Participants age",
       x ="Number of participants", y = "Age category", fill = "Age category")


#### Demonstrate thinking steps - optional: if time left  ####
demo <- survey |> # this is a function I found online and it does what I want, but....
  mutate(age_category = # New variable
           fct_reorder(how_old_are_you, parse_number(how_old_are_you))) # This part reorders the factor according to
                                                                        # the first number in the how_old_are_you_variable
demo |> count(age_category) # Check: not yet what I want....

parse_number(demo$how_old_are_you)   # we could have been lucky that this would be sufficient
                                     # but we need an additional step namely bringing under 18 to the front

demo <- demo|>
  mutate(age_category = 
           fct_relevel(fct_reorder(how_old_are_you, parse_number(how_old_are_you)), "under 18")) # brings 18 to the front
demo |> count(age_category) # Check: looks good

demo <- demo |>
  mutate(age_category = #New variable
           fct_relevel(as.factor(how_old_are_you), "under 18")) #simpler and also works, my preferred version
demo |> count(age_category) # This is what we want


#### Question 4:  make a dataframe with the variables age, gender, annual salary, currency, other_monetary_comp ####
df <- survey |> dplyr::select(age_category, gender, annual_salary, currency, other_monetary_comp) #Check environment

  ## Age
    # We already looked at age, so we know the distribution. Mean and median not so relevant with this categorical variable
    # But are there any missings? 
    # Eyeballing through df

df |>  count(age_category) # Checking using syntax
df |>  count(is.na(age_category))

  ## Other alternatives: not discussed in the session
df |> dplyr::select(age_category) |> # option 1
 mutate(missing_age = is.na(age_category)) |> 
count(missing_age) 

df |> dplyr::select(age_category) |> is.na() |> table() # option 2: untidy way

  ## Gender
df  |> count(gender) # this time there are NA's (148)

  ## Currency
df  |> count(currency)

  ## Annual salary
df|> count(annual_salary) # not ideal in this case 

df |> dplyr::select(annual_salary) |> 
  mutate(missing_annual = is.na(annual_salary)) |> # explain is.na
  count(missing_annual) 



#### Question 5:  What is the distribution of gender in percentages ####
df |>
  count(gender) |> 
  mutate(percent = n/sum(n)*100) |> 
  dplyr::select(-n) # not yet rounded

df |>
  count(gender) |> 
  mutate(percent = round((n/sum(n)*100), digits = 3)) |> 
  dplyr::select(-n) # rounded

df |>
  count(gender) |> 
  mutate(percent = (n/sum(n))*100,
         percent = round(percent,3)) # of course there is always a better way: over-writing and reusing

    # Remember how we wrote this kind of table to Excel: try at home! 

#### Question 6: Get the min, max, mean, sd, median, IQR and amount of missings for the variable annual salary ####
df|> 
  summarise(mean = mean(annual_salary),
            median = median(annual_salary)) # this is one way to do it if you have only 1 variable


      # A different way that can be useful for multiple varibales
pv <- df |> dplyr::select(annual_salary) |> 
  pivot_longer(cols = annual_salary, names_to = "variable_name", values_to = "value")

descriptive_stat <- pv |> 
  group_by(variable_name) |> 
  summarise(min = min(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE),
            mean = mean(value, na.rm = TRUE),
            sd   = sd(value, na.rm = TRUE),
            median = median(value, na.rm = TRUE),
            iqr = IQR(value, na.rm = TRUE),
            miss   = sum(is.na(value)))

      # Why do it like this? Because then you can use it for multiple variables.
      # Now we can:
pv_2 <- df |> dplyr::select(annual_salary, other_monetary_comp) |>
  pivot_longer(cols = c(annual_salary, other_monetary_comp), names_to = "variable_name", values_to = "value")

descriptive_stat_2 <- pv_2 |> 
  group_by(variable_name) |> 
  summarise(min = min(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE),
            mean = mean(value, na.rm = TRUE),
            sd   = sd(value, na.rm = TRUE),
            median = median(value, na.rm = TRUE),
            iqr = IQR(value, na.rm = TRUE),
            miss   = sum(is.na(value))) 

      # 100 variables or even more variables at once: no problem!
      # try to write it to a csv! 


#### Not discussed during the session : Writing a function advanced option ####

 descriptives_stat_3 <- function(dataframe, variable) {
   dataframe |>
     summarize(mean   = mean({{variable}}, na.rm=TRUE),
            median = median({{variable}}, na.rm=TRUE),
            sd     = sd({{variable}}, na.rm=TRUE),
            min    = min({{variable}}, na.rm=TRUE),
            max    = max({{variable}}, na.rm=TRUE),
            iqr    = IQR({{variable}}, na.rm=TRUE),
            miss   = sum(is.na({{variable}}))
            )
 }
 df |> descriptives_stat_3 (annual_salary)
 df |> descriptives_stat_3 (other_monetary_comp)

#### Question 7: Plot the distribution of annual salary ####
df |> pull(annual_salary) |> hist() # simple, but not yet very informative
df |> dplyr::select(annual_salary) |> hist() # select would not work because it is a tibble not a vector
df  |> filter(annual_salary < 1000000) |> pull(annual_salary) |> hist() # filter to remove outliers above 1000000
df  |> filter(annual_salary < 300000) |> pull(annual_salary) |> hist() 

df |>
  filter(annual_salary < 300000) |> # if you prefer using ggplot
  ggplot() +
  geom_histogram(aes(x = annual_salary), color = "black", fill="white", binwidth=5000)



#### Question 8: Can we get a salary independent of currency? ####
converted<- df |> filter(currency == "USD" | currency == "CAD") |>
  mutate(
  USD = convert_currencies(
    price_start = annual_salary,
    from = currency,
    to = "USD",
    date = as.Date("2019-12-14"))
  )


converted_all<- df |> filter(!currency %in% c("AUD/NZD", "Other")) |>
  mutate(
    USD = convert_currencies(
      price_start = annual_salary,
      from = currency,
      to = "USD",
      date = lubridate::today())
  )

    
#### Question 9: Compare salary across genders? ####
converted_all |> group_by(gender) |> filter(USD < 300000) |>
  summarise(mean = mean(USD)) 

converted_all |> filter(USD < 300000) |> group_by(gender) |> 
  ggplot(aes(x = as.factor(gender), y = USD)) +
  geom_boxplot()
  
#### Bonus question: Create the overall mean score: ####
      # The happy_1,2 and 3 describe a participants happiness score. Add a column that shows the mean of happiness per person.
      # For people that have >50% missing on the happiness scale make the mean NA

additional <- data.frame(id = c("p1", "p2", "p3", "p4", "p5"),
                         happy_1 = c(1,2,3,4,NA),
                         happy_2 = c(1,2,3,NA,5),
                         happy_3 = c(1,2,3,NA,5))

      # Imagine now this is a questionnaire and the item happy 1-3 are items that measure overall happiness. 
      # Create the overall happiness mean score per person
my_way <- additional |>
  rowwise() |>  # to do row wise operations
  mutate(missings = sum(is.na(across(c(happy_1, happy_2, happy_3)))), #remember from above is.na give you a column with True or False, see below
         exclude = if_else(missings > 1, "yes", "no"), # make columns where if missings >1, yes else no, can be left out
         m = mean(c(happy_1, happy_2, happy_3),na.rm=TRUE), # calculate the mean
         final_mean = if_else(missings > 1, as.numeric(NA_integer_), m)) # finally column with the mean only if not more than one missing

### in between demonstration
quick_demo <- additional |>
  rowwise() |>  # to do row wise operations
  mutate(missings = is.na(across(c(happy_1, happy_2, happy_3)))) # and try to add sum around it
###

my_way_short <- additional |>
  rowwise() |>  # to do row wise operations
  mutate(missings = sum(is.na(across(c(happy_1, happy_2, happy_3)))), #if I only do is.na I get three extra columns where see below
         final_mean = if_else(missings > 1, as.numeric(NA_integer_), mean(c(happy_1, happy_2, happy_3),na.rm=TRUE))) 
    
      # if the variables are next to each other                       
my_way_short <- additional |>
  rowwise() |>  # to do row wise operations
  mutate(missings = sum(is.na(across(happy_1:happy_3))), #if I only do is.na I get three extra columns where see below
         final_mean = if_else(missings > 1, as.numeric(NA_integer_), mean(c(happy_1, happy_2, happy_3),na.rm=TRUE))) 


    # Or like this but then the last step namely changing the mean to NA where more than 1 NA is still missing
Wims_way <- additional |> 
  mutate(missings = rowSums(across(c(happy_1, happy_2, happy_3), is.na)), 
         major_mis= if_else(missings > 1, "yes", "no"),
         mean     = rowMeans(across(c(happy_1, happy_2, happy_3)), na.rm=TRUE))
