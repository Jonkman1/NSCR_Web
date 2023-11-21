#==============================================================================#
#                                 Scooby-Doo                                   #
#                               Visualization                                  #
#==============================================================================#

# 
# The data used this week is the same dataset used on October 31st, about Scooby-Doo episodes. This time, we will focus on visualization techniques using ggplot.
# 
# The data is described here:
#   
#   https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-07-13/readme.md

# Package loading -------
library(conflicted) # To solve conflicting packages
library(ggplot2)    # For plotting
library(tidyverse)  # For tidy work

conflict_prefer(name = "summarize", 
                winner = "dplyr")


# Data loading ------
scooby_doo <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv",
                       na = "NULL")

# Inspect the data structure:
 
scooby_doo %>% 
  glimpse()

# Note: One thing we would like to do later treats column imdb as a double 
# variable. When testing, we found that for some versions of R, the column is 
# read as a character. If this is the case for you, run the following line:

scooby_doo <- scooby_doo %>%
  mutate(imdb = as.numeric(imdb))

# Data visualization -----

# Using GGPlot, there is some basic syntax we need to take into account before 
# we plot anything:
  
# 1. Asking for a plot using a data frame.
# 
# 2. Calling for a ggplot using the ggplot() function.
# 
# 3. Specifying the columns that define the aesthetic our plot is based on with 
#    an aes() function.
# 
# 4. Adding "layers" to define what the ggplot should show, starting with the 
#    kind of plot.
# 
# 5. Bonus: addig other layers to the plot like the kind of statistics that may 
#    need to be calculated, the axis labels and ticks, text arguments, etc. Those 
#    arguments are added up on top of the basic ggplot function with a "+".

# Point plot -----

# Plot 1: IMDB Rating per date, for different formats ------

# With a point plot, we can visualize a scatterplot directly from the data, by 
# providing GGplot with columns as an X and a Y argument.
# 
# Say we are interested to look at the trends of IMDB rating over time.
# 
# For example, if we want to produce a point-plot, we can do so with:
  
# 1. Specifying your dataset (this is given first with tidy work)
scooby_doo %>%
  # 2. Asking for a ggplot 
  ggplot(
    # 3.Specifying the columns our plot is based on
    aes(x = date_aired,
        y = imdb)
  ) + 
  # 4. Adding layers
  geom_point()


# Note: the warning about missing values is there because the IMDB rating is 
# missing for 15 episodes.

# Plot 1.1: IMDB rating over time, per format -----

# Say we are interested in IMDB rating trends over time, but we wish to 
# distinguish between different groups. In this case, different formats of 
# Scooby-Doo content. We can do this by adding a grouping variable in the 
# aesthetic argument. Depending on the type of plot we wish to use, it will 
# either be a "color" or a "fill" argument.

# RULE OF THUMB: Use "color" for a small defined shape (like point or line) 
# or the border of a large shape (like a bar chart)
# 
# Use "fill" to fill up a shape.

scooby_doo %>%
  ggplot(
    aes(x = date_aired,
        y = imdb,
        color = format)
  ) + 
  geom_point(na.rm = TRUE)

# Bar plots -----

# Bar plots can also be used to visualize information from the data. They are 
# useful, for example, to visualize count data. 

# Plot 2: Who caught the most culprits? -----
  
# Last time we used the Scooby Doo data, we used "select" and "colSums" to count 
# the number of "TRUE" incidences in all the columns describing whether a 
# character has caught a culprit in each episode. As a reminder:
  
caught <- scooby_doo %>%
  select(starts_with("caught")) %>%
  colSums(na.rm = TRUE) %>% # Results in a named vector
  enframe() 

caught

# One example of a bar plot is called with geom_col, which can be created with 
# an X and a Y argument:
  
caught %>%
  ggplot(aes(x = name, y = value)) +
  geom_col() 

# Plot 3: Suspected vs unsuspected culprits in different formats -----
# Another example of a bar chart can be requested with geom_bar().
# 
# Say we want to see how many culprits were suspected versus unsuspected in 
# different media formats.
# 
# For a bar chart, we only need to specify an x or a y argument.
# 
# For grouping the plot by format, we need to add a "fill" argument.

scooby_doo %>%   
  ggplot(aes(x = non_suspect,              
             fill = format)) +    
  geom_bar()   
# geom_bar(position = "dodge")

# Try it yourself:
#   
# * see what happens if you change the "x" argument to a "y" argument.
# 
# * See what happens when changing the "fill" argument to a "color" argument.
# 
# What happens if you replace "geom_bar()" with 'geom_bar(position = "dodge")'? 
# You can test this by removing the # to uncomment a line, and placing a # to 
# comment a line.


# Boxplot -----

# Up til now, we plotted data directly without any statistical process in the 
# background. It is also possible to create plots based on some background 
# statistical process, like in the example of boxplots.

# Plot 4: Boxplot of IMDB rating per format -----

# Let's plot the average IMDB rating for different formats displaying Scooby-Doo.

scooby_doo %>%
  ggplot(aes(x = format,
             y = imdb)) +
  geom_boxplot(na.rm = TRUE)

# Summary statistics ------

# In the same spirit, there are two ways to plot summary statistics

# 1. Plotting summary statistics directly, by asking for the statistic as a 
#    layer.
# 
# 2. Calculating the summary statistics yourself and ask GGplot to visualize the 
#    resulting table.

# Plot 5: What are the mean and sd IMDB rating at each format? ----

# For example, let's plot the average IMDB rating for different formats 
# displaying Scooby-Doo. We can do this by directly asking GGPlot to calculate 
# the statistic for us:
  
# First, make sure you have the package "Hmisc" for plotting standard deviation:
if (! require("Hmisc")){
  install.packages("Hmisc")
}

scooby_doo %>%
  ggplot(aes(x = format,
             y = imdb,
             fill = format)) +
  stat_summary(geom = "col",
               fun = mean,
               na.rm = TRUE) +
  stat_summary(fun.data = mean_sdl, 
               geom = "errorbar",
               na.rm = TRUE)


# Another way in which we can do this is by first asking for a summary table, 
# and building a plot on top of that new dataframe. If we were to ask for a 
# summary table, it would look like this:
  
scooby_doo %>%
  group_by(format) %>%
  summarize(mean_imdb = mean(imdb, na.rm = TRUE),
            sd_imdb = sd(imdb, na.rm = TRUE))

# To plot this, we can use the same code as the data frame we base our GGPlot 
# on:
  
p <- scooby_doo %>%
  group_by(format) %>%
  summarize(mean_imdb = mean(imdb, na.rm = TRUE),
            sd_imdb = sd(imdb, na.rm = TRUE)) %>%
  ggplot(aes(x = format,
             y = mean_imdb,
             fill = format)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_imdb - sd_imdb,
                    ymax = mean_imdb + sd_imdb))

p

# NOTE: Since we use both x and y in this plot, this is a geom_col rather than a 
# geom_bar!
# 
# NOTE 2: I saved the last plot as an object. To see what the plot looks like, 
# we need to ask R to print the object "p". After saving the plot as an object, 
# we can also change or add new layers to improve the visualization. Let's do that next.

# Visualization layers -----

# Plot 5.1 ----

# The plot we created last is informative, but hard to read. We will do the 
# following to improve how it looks:

# 1. Sort the bars to appear from tallest to shortest
# 
# 2. Change the labels to be more readable.
# 
# 3. Get the legend out of the way
# 
# 4. Fix the tick labels on the X axis
# 
# 5. Change the color scheme
# 
# 6. Add the average IMDB rating at the top of each column

p +
  aes(x = fct_reorder(format, -mean_imdb)) +
  labs(x = "Format",
       y = "Mean IMDB Rating",
       title = "Scooby dooby doo!") +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 25, 
                                   hjust = 1)
        ) +
  scale_fill_brewer(palette = "Dark2") +
  geom_text(aes(label = round(mean_imdb, 1)),
            nudge_y = rel(1.7))

# Bonus: Plot 3.1 ----

# Let's improve the appearance of the plot describing who caught the most 
# suspects:
  
# * Focus on main characters
# 
# * Sort data by number of catches.
# 
# * Fix labels.
#  
# * Color by character
# 
# * Add number of catches at the top of each bar
# 
# * Add a picture of each character as the label
# 
# * Image credit: https://static.wikia.nocookie.net/
  
# To load the images of the characters, we can directly load them from the web:
images <- data.frame(
  name = c("scooby", "fred", "daphnie", "velma", "shaggy"),
  image = c(
    "https://static.wikia.nocookie.net/great-characters/images/6/6e/Scoobert.png/revision/latest?cb=20181102135349",
    "https://static.wikia.nocookie.net/p__/images/4/4e/Fred_Jones_render.png/revision/latest?cb=20201231191620&path-prefix=protagonist",
    "https://static.wikia.nocookie.net/p__/images/0/0c/Daphne_Blake_render.png/revision/latest?cb=20201229221052&path-prefix=protagonist",
    "https://static.wikia.nocookie.net/p__/images/c/c8/Velma_1.png/revision/latest?cb=20201110000519&path-prefix=protagonist",
    "https://static.wikia.nocookie.net/p__/images/2/23/Shaggy_11.png/revision/latest?cb=20190319022400&path-prefix=protagonist"
  )
)

images <- images %>%
  mutate(image_src = str_c("<img src='{", image, "}' width='{50}'/>"))

images

# The next step is to remove the unnecessary rows from the summary table and 
# add the image sources. This will happen by default when we use merge(), which 
# merges two dataframes together and by default, removes rows that are not 
# represented in both dataframes. Merge() requires a column with the same name 
# in two dataframes, with the same (at least partially) values. For this, we can 
# remove the "caught_" from the name in the dataframe "caught":
  
caught <- caught %>%
  mutate(name = str_remove(name, "caught_")) %>%
  merge(images)

# It's time to plot!

# By running this, you can ensure to have installed the necessary package
if (!require("ggimage")){
  install.packages("ggimage")
}

library(ggimage)

caught %>%
  ggplot(aes(
    x = fct_reorder(name,-value),
    y = value,
    fill = name
  )) +
  geom_col() +
  # Replace the x axis, which otherwise covers the images
  geom_hline(aes(yintercept = 0)) +
  geom_image(aes(x = fct_reorder(name, -value),
                 image = image),
             size = .15, # To reduce size to 15%
             y = 0, # Where to locate images on Y axis
             nudge_y = -1, # To move images down
             ) +
  coord_cartesian(clip = "off", expand = FALSE) + # So image doesn't crop at bottom
# Add number of catches
geom_text(aes(label = value),
          nudge_y = 7) +
  labs(x = "",
       y = "Total catches",
       title = "Scooby-Doo team catches") +
  theme_classic() +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.line = element_blank(),
        plot.margin = margin(0, 0, 2, 0, "cm")) 


