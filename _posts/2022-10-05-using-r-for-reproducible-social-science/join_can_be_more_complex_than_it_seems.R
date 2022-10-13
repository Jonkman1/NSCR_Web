library(tidyverse)

# -- A simple dataframe --------------------------------------------------------
a <- tibble(
  x1 = c("A", "B", "C"),
  x2 = c( 1 ,  2 ,  3 )
)
a
# -- Another simple dataframe --------------------------------------------------
b <- tibble(
  x1 = c("A", "B", "D"),
  x3 = c(TRUE, FALSE, TRUE)
)
b

# -- left_join -----------------------------------------------------------------
left_join(a,b, by="x1")

# -- right_join ----------------------------------------------------------------
right_join(a,b, by="x1")

# -- inner_join ----------------------------------------------------------------
inner_join(a,b, by="x1")

# -- full_join -----------------------------------------------------------------
full_join(a,b, by="x1")

# -- A slightly different dataframe a2 -------------------------
a2 <- tibble(
  x1 = c("A", "B", "C", "A"),
  x2 = c( 1 ,  2 ,  3 ,  4)
)
a2
# -- What do you think will happen if we left_join(a2, b, by="x1")?
left_join(a2,b, by="x1")

# -- A slightly different dataframe b2 -------------------------
b2 <- tibble(
  x1 = c("A", "B", "D", "A"),
  x3 = c(TRUE, FALSE, TRUE, NA)
)
b2

# -- What do you think will happen if we left_join(a2,b2, by="x1")?
# --   new version of b?
left_join(a2,b2, by="x1")

# So what does 'left_join' do?
# 1. For each row in the first (left) table:
#    a. copy the row to a new table
#    b. find the value of the key (here it is x1)
#    c. for each row in the second (right) table:
#       I. find te value of the key
#       II. if the values of the key equals the value of the key in the first table,
#           append this row to the right side of the row copied from the first table   


