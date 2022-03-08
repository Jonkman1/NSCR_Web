library(tidyr)
library(dplyr)
library(readr)

relig_income

?pivot_longer

pivot_longer(relig_income, !religion)

df <- relig_income %>%
  pivot_longer(!religion, names_to = 'income', values_to = 'count')

billboard


billboard %>%
  pivot_longer(
    cols = starts_with('wk'),
    values_drop_na = TRUE,
    names_to = "week",
    values_to = 'rank',
    names_prefix = "wk",
    names_transform = list(week = as.integer)
    )

install.packages("remotes")
remotes::install_github("davidsjoberg/ggsankey")
library(ggsankey)
library(ggplot2)

?mtcars

df <- mtcars %>%
  make_long(cyl, vs, am, gear, carb)

ggplot(df, aes(x = x,
               node = node,
               next_x = next_x,
               next_node = next_node,
               fill = factor(node),
               label = node)) +
  geom_sankey() +
  geom_sankey_label() 

df1 <- mtcars %>%
  select(cyl, vs, am, gear, carb) %>%
  pivot_longer(everything()) %>%
  mutate(next_x = lead(.data$name),
         next_node = lead(.data$value)
  )


  

