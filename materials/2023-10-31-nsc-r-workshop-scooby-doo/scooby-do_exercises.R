#==============================================================================#
#                                Scooby-Doo                                    #
#==============================================================================#

# The data used this week is collected on ScoobyPedia, an encyclopedia on the 
# hit cartoon series Scooby-Doo. This is a quote from their website:
#
# The show follows the iconic mystery solving detectives, know as Mystery Inc., 
# as they set out to solve crime and unmask criminals, bent on revenge or 
# committing criminal acts for their own personal gain.
#
# Titular character, Scooby, is followed by his best pal Shaggy as both vie for 
# Scooby Snacks on their adventures! Velma brings her extra intellect and 
# initiative to them, setting out plans to catch criminals. Fred is the team's 
# leader while Daphne is bold and full of personality.


# The data we will be using has been coded for research purposes and 
# demonstrated by the Tidy Tuesday Initiative. The data is described here:

# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-07-13/readme.md

# Package loading --------------------------------------------------------------
library(ggplot2)  # For plotting
library(readr)    # For loading data
library(dplyr)    # For tidy wrangling
library(stringr)  # For string operations


# Data loading -----------------------------------------------------------------

# We can load the data directly from Github.
scooby_doo <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv")

# Glimpse the data structure:
scooby_doo %>% 
  glimpse()

# Q1: What can we learn about the data from glimpsing it? What is the unit of 
# observation? Are all variables coded properly? How can we fix it?



# Practice tidy data wrangling: filtering and selecting ------------------------
# Say we only want to use data from the original series "Scooby Doo, Where Are 
# You!". We can subset the data by using filter().

# Exercise A: create a new dataset called scooby_doo_orig by filtering the original dataset.

# Q2: How many episode did the original series have?

# For a later exercise, we wish to subset the dataset of the original series to 
# include each episode's index, title, and monster data. 

# Exercise B: Create this dataset and call it scooby_doo_orig_monster. Check 
# that you have 9 columns in the new dataset.




# Average IMDB rating per season, per series -----------------------------------
# For this, we will use the full dataset again. 

# Exercise C: inspect the number of episodes in each season per series (hint: try 
# grouping the appropriate variables and counting the resulting number of cases)

# Inspecting the results a little tells us that there are some special and 
# crossover episodes from series that are not Scooby Doo-centered. There are 
# also a few movies. Say we are only interested in regular Scooby Doo episodes.

# Exercise D: Create a new dataset called scooby_doo_ep that only include Scooby 
# Doo TV Series and segmented TV series.

# Exercise E: Summarize the average IMDB rating per season of each 
# series.


# Q3: What percentage of the real culprits were unsuspected? -----------------------
# We can find this out by summing up the number of "TRUE" in the column 
# non_suspect. We can then compare this number to the number of culprits who 
# were suspected (marked by "FALSE").

# Exercise F: Find out the percentage of real culprits that were unsuspected.




# Q4: Who caught the most culprits? ------------------------------------------------
# To answer this question, we need to count the number of "TRUE" in each column 
# containing the term "caught".

# Step 1: Select columns containing the term "caught":
caught <- scooby_doo_ep %>%
  select(starts_with("caught"))

glimpse(caught)

# We have a dataset containing one column per character, with a logical value 
# for each episode (caught/not). By summing up each column, we can find out the 
# number of times "TRUE" is coded for each character.

# Step 2: Sum up each column to find the number of "TRUE" incidences
caught_total <- caught %>%
  colSums(na.rm = TRUE) %>% # Results in a named vector
  enframe()                 # Nicely transforms the named vector to a dataframe.

caught_total

# Step 3: Sort caught_total in a descending order to easily find the best monster
# catcher.
caught_total %>% 
  arrange(value %>% desc())

# It looks like Scooby is our champion!


# Q5: Who unmasked the most monsters? ----------------------------------------------
# Exercise G: Repeat the steps above with the appropriate variables to find out.



# Q6: Who was captured the most times? ---------------------------------------------
# Exercise H: Find out!




# Q7: Can we break a myth? -----------------------------------------------------
# Scooby-Doo is known for unmasking many monsters to be fake. Of the mythical
# monsters, how many were real?
  
# Let's first inspect the different categories under the variable monster_type:
scooby_doo_ep %>%
  group_by(monster_type) %>%
  count()

# We quickly notice that some cells are coded with multiple monster_type values.
# That is because the dataset is structured in a wide format, with one episode 
# per row. If we wish to analyze the different mosters, we need to pivot the
# dataset to a longer format, analyzing one monster per row. 
# Note: For simplicity, we will only focus on monster_type, although more 
# variables are structured the same way.

scooby_doo_ep_longtype <- scooby_doo_ep %>%
  separate_longer_delim(cols = monster_type, delim = ",")

scooby_doo_ep_longtype %>%
  View()

# Exercise I: filter the long dataset to select "real"monsters only and count 
# the number of incidences for each monster type left.

# Data visualization -----------------------------------------------------------
# For GGPlot, we always have to specify an aesthetic argument, where we specify 
# what should be treated as the x, y, or group variable. After we specify that, 
# we can add new arguments by using the "+" sign and adding more and more layers 
# to the plot. 

# Plot 1: IMDB Rating per date, for different formats  =========================
# For example, if we want to produce a point-plot, we can do so with:
scooby_doo %>%
  ggplot(aes(x = date_aired,
             y = imdb,
             color = format)
  ) + 
  geom_point()


# Plot 2: Suspected vs unsuspected culprits in different formats ===============
# For a bar chart, we only need to specify an x or a y variable:
scooby_doo %>%
  ggplot(aes(x = non_suspect)) + 
  geom_bar()


# Plot 3: Who caught the most culprits? ========================================
# We can plot a summary table like the one we created for "caught_total" with a 
# geom_col() command.

# Let's remember first how the summary table looked like
head(caught_total)

# And plot the summary table
caught_total %>%
  ggplot(aes(x = name, y = value)) +
  geom_col() 



# Plot 4: Per monster type, how many real vs unreal monsters were uncovered? =======================================
  
# Earlier, we saw how we can create a bar chart with ggplot. But what if we want 
# to add another layer to the data, for example different categories? We can do 
# so by adding an aesthetic "fill".

# Note: Fill refers to the color of the chart, while "color" refers to its 
# border. Therefore, for some chart types (e.g. point, line), we should specify 
# a "color" aesthetic instead of "fill" for illustrating different categories. 
# Try both on the code below!
scooby_doo_ep_longtype %>%
  ggplot(aes(y = monster_type, 
             fill = monster_real)
  ) +
  geom_bar(position = "dodge") # Try removing the position argument
