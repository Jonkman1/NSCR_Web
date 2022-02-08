# Load packages
library(dplyr)
library(ggplot2)

# So today's data is actually a relational database, which means there are
# multiple datasets that can be linked through some identifier or `id` variable

# Import data
ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/ratings.csv")
details <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/details.csv")

# So let's start by talking a quick look at the data

# Explore data
glimpse(ratings)
glimpse(details)

# The number of observations does not match! ----

# Explore whether both datasets have identical `id`
identical(details$id, ratings$id)
# Count how many `id` are missing
table(ratings$id %in% details$id)
# Find which `id` are present in both datasets
intersect(details$id, ratings$id)
# Find which `id` are absent in the dataset with the least observations
length(setdiff(ratings$id, details$id)) == (nrow(ratings) - nrow(details))

# Merge data
df <- inner_join(ratings, details, by = "id")
# Explore data
glimpse(df)

# Looks like there are some duplicated variables! ----

# Not really
identical(df$year, df$yearpublished)
# What is the difference?
which(df$year != df$yearpublished)
# Ah, I see!
df$year[21623]
df$yearpublished[21623]

# Nope. Very different. Can these be row numbers (e.g. spreadsheet)?
identical(df$num.x, df$num.y)

# Almost, but nope.
identical(df$name, df$primary)
which(df$name != df$primary)
# Looks like there is some sort of syntax issue in `primary`
df$name[19925]
df$primary[19925]

# Bingo!
identical(df$playingtime, df$maxplaytime)

# Let's drop the duplicated variable
df <- df %>% 
  select(
    - playingtime,
    - primary
  )

# Looks like we are good now! Imagine the following:

# We want to buy a board game for a friend, but we don't know which one to get.
# We know our friend likes to play alone with his girlfriend sometimes, but some
# other times he likes to invite friends over to play.
# What are the top 3 best-selling board games by game type? 
# How do board game sales relate to online user ratings?

# What are the top 3 best-selling board games by game type? ----
df <- df %>% 
  # Create new variable to define type of game based on number of players
  # mutate(minplayers_fct = if_else(
  #   condition = minplayers < 2,
  #   true = "single player",
  #   false = if_else(
  #     condition = minplayers == 2,
  #     true = "two players",
  #     false = "multiplayer"
  #   )
  # )) %>% 
  mutate(minplayers_fct = case_when(
    minplayers < 2 ~ "single player",
    minplayers == 2 ~ "two players",
    minplayers > 2 ~ "multiplayer"
  ))

df %>% 
  # Select only relevant variables
  select(
    name,
    minplayers_fct,
    owned
  ) %>% 
  group_by(minplayers_fct) %>% 
  slice_max(owned, n = 3) %>% 
  # tidyr::pivot_wider(
  #   names_from = minplayers_fct,
  #   values_from = name
  # ) %>% 
  knitr::kable()

# We have identified the best-sellers for each category, but best-sellers might
# be a product of marketing rather than an indicator of good quality. It might
# be the case that players buy the product because of good marketing but then
# find the game boring, too complicated, etc.

# How do board game sales relate to online user ratings? ----

# Plot data
# qplot(df$owned, df$average, alpha = .5)
df %>% 
  ggplot(mapping = aes(
    x = average,
    y = owned,
    # Examine if there are clear patterns by number of players
    color = minplayers_fct
  )) +
  geom_point(alpha = .5) +
  scale_color_viridis_d() +
  labs(color = "Category") +
  theme_classic()

# There seems to be three outliers that are best-sellers and also have high 
# ratings
df %>% 
  select(
    name,
    minplayers_fct,
    owned
    ) %>% 
  filter(owned > 150000)