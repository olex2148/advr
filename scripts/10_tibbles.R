library(tidyverse)

as_tibble(iris)

#1 

mtcars

as_tibble(mtcars)

is_tibble(mtcars)

is_tibble(as_tibble(mtcars))

#2

df <- data.frame(abc = 1, xyz = "a")

# partial matching
df$x
# all rows from col "xyz"
df[, "xyz"]
df[, c("abc", "xyz")]

tbl <- as_tibble(df)
tbl$x

tbl[, "xyz"]

tbl[, c("abc", "xyz")]

#3

#You can use the double bracket, like df[[var]]. 
#You cannot use $, because df$var would look for a column named var.

df <- mtcars

var <- "mpg"

df[[var]]

df$var

#4

annoying <- tibble(
  `1` = 1:10,
  `2` = `1` * 2 + rnorm(length(`1`))
)

#.1

annoying[["1"]]


#.2

ggplot(annoying, aes(x = `1`, y = `2`)) +
  geom_point()

#.3

annoying <- mutate(annoying, `3` = `2` / `1`)

#.4

annoying <- rename(annoying, one = `1`, two = `2`, three = `3`)
annoying

#5

#Converts named vectors into a dataframe:

enframe(c(a = 1, b = 2, c = 3))


#6

#Try ?print.tbl  It is n_extra


?print.tbl

#max_extra_cols was previously n_extra






