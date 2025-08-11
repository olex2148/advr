# Tidy data, run first

tidy4a <- table4a %>% 
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "cases")
tidy4b <- table4b %>% 
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "population")


# 12.3

#1

stocks <- tibble(
  year   = c(2015, 2015, 2016, 2016),
  half  = c(   1,    2,     1,    2),
  return = c(1.88, 0.59, 0.92, 0.17)
)
stocks %>% 
  pivot_wider(names_from = year, values_from = return) %>% 
  pivot_longer(`2015`:`2016`, names_to = "year", values_to = "return")


# They are not perfectly symmetrical because when pivot_wider() turns variable values
# into column names, those names are always stored as character strings.
# When pivot_longer() reverses the process, it has no way of knowing the original type (e.g. numeric)
# so you must explicitly convert it back using names_transform.

#2

# It fails because the numbers in the vector are seen as element number. We need to put it into quotations:

table4a %>% 
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "cases")

#3

people <- tribble(
  ~name, ~key, ~value,
  #-----------------|--------|------
  "Phillip Woods",  "age", 45,
  "Phillip Woods", "height", 186,
  "Phillip Woods", "age", 50,
  "Jessica Cordero", "age", 37,
  "Jessica Cordero", "height", 156
)
glimpse(people)

# If you widen this table, pivot_wider() will produce list-columns 
# because name and key do not uniquely identify rows (e.g. Phillip Woods has two “age” values).

people2 <- people %>%
  group_by(name, key) %>%
  mutate(obs = row_number())

# This obs column ensures that each (name, key, obs) combination is unique
# so pivot_wider() can create a regular table.

pivot_wider(people2, names_from="name", values_from = "value")

#4

preg <- tribble(
  ~pregnant, ~male, ~female,
  "yes", NA, 10,
  "no", 20, 12
)

# We need to pivot longer to get the variables: sex, pregnant and count:

preg_tidy <- preg %>%
  pivot_longer(c(male, female), names_to = "sex", values_to = "count")
preg_tidy

# If we think about our data, we could drop the instances that are impossible:

preg_tidy2 <- preg %>%
  pivot_longer(c(male, female), names_to = "sex", values_to = "count", values_drop_na = TRUE)
preg_tidy2

