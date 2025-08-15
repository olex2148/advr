library(nycflights13)
library(tidyverse)

nycflights13::flights
View(flights)

# 5.2

# 1 
filter(flights, arr_delay >= 120)

# 2 

filter(flights, dest == "IAH" | dest == "HOU")

# %in% scales better

filter(flights, dest %in% c("IAH", "HOU"))


# 3

airlines

filter(flights, carrier %in% c("AA", "DL", "UA"))

# 4

filter(flights, month == 7 | month == 8 | month == 9)

filter(flights, month %in% 7:9)

# 5

filter(flights, arr_delay > 120, dep_delay <= 0)

# 6

filter(flights, dep_delay >= 60, dep_delay - arr_delay > 30)

# 7

summary(flights$dep_time)
# Midnight is represented 2400 not 0

filter(flights, dep_time <= 600 | dep_time == 2400)

#Modulus operation would also be good, we will see this later:
filter(flights, dep_time %% 2400 <= 600)


# 2

#between(x, left, right) x is larger or = to left and equal or smaller or equal to than right.

filter(flights, between(month, 7, 9))

# 3

filter(flights, is.na(dep_time))
# arr_time also missing, flights cancelled

# 4

# Anything ^0 = 1
# Na|TRUE is unknown or TRUE, so true. 
# FALSE & Na is FALSE or Na so FALSE
#  & TRUE is Na, Na could be FALSE

# x * 0 = 0 for finite numbers, but Inf and -Inf are NaN, not a number

Inf * 0

# 5.3

# 1 

#Nas sorted last:
arrange(flights, dep_time) %>%
  tail()
# desc() does not change this:
arrange(flights, desc(dep_time))

# The flights will first be sorted by desc(is.na(dep_time)). 
# desc(is.na(dep_time)) evaluates to TRUE when dep_time is missing, or FALSE, when it is not, 
# the rows with missing values of dep_time will come first, since TRUE > FALSE
arrange(flights, desc(is.na(dep_time)), dep_time)

# 2 

# Most delayed are the highest dep_delay:
arrange(flights, desc(dep_delay))

# Earliest is the other end
arrange(flights, dep_delay)

# 3
#Least time in the air
head(arrange(flights, air_time))
# highest speed: 
head(arrange(flights, desc(distance / air_time)))


# 4

# shortest
arrange(flights, desc(distance))
# longest
arrange(flights, distance)


# 5.4

# 1 

select(flights, dep_time, dep_delay, arr_time, arr_delay)

# Col names are strings, remember for later
select(flights, "dep_time", "dep_delay", "arr_time", "arr_delay")

select(flights, 4, 6, 7, 9)

select(flights, starts_with("dep_"), starts_with("arr_"))

# However, this will include unwanted cols, beware
select(flights, ends_with("arr_time"), ends_with("dep_time"))

# 2

# Ignored
select(flights, year, month, day, year, year)

# 3

# one_of() function selects variables with a character vector rather than unquoted variable name arguments. 

vars <- c("year", "month", "day", "dep_delay", "arr_delay")
select(flights, one_of(vars))

# 4

select(flights, contains("TIME"))
# Not case sensitive but we can make it if we want:

select(flights, contains("TIME", ignore.case = FALSE))

# 5.5

# 1

# Hours since midnight: integer division %/% discards remainder
1504 %/% 100

# Modulo gives us the minutes instead
1504 %% 100

# To get minutes after midnight we multiply the hours with 60 and add the remainder minutes: 
1504 %/% 100 * 60 + 1504 %% 100

# Remember midnight is represented as 2400, which is 1440 min. We can set it to 0 with %% 1440 
# since everything under will stay the same. 1440 %% 1440 = 0

flights_times <- mutate(flights,
                        dep_time_mins = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440,
                        sched_dep_time_mins = (sched_dep_time %/% 100 * 60 +
                                                 sched_dep_time %% 100) %% 1440
)

#Relevant cols: 
select(
  flights_times, dep_time, dep_time_mins, sched_dep_time,
  sched_dep_time_mins
)

# 2

# we expect air_time = arr_time - dep_time

flights_airtime <-
  mutate(flights,
         dep_time = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440,
         arr_time = (arr_time %/% 100 * 60 + arr_time %% 100) %% 1440,
         air_time_diff = air_time - arr_time + dep_time
  )

# in diff there should now be no flights:

nrow(filter(flights_airtime, air_time_diff != 0))

# Either flight passed midnight or crossed time zones

# 3

# We expect dep_delay = dep_time - sched_dep_time

flights_deptime <-
  mutate(flights,
         dep_time_min = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440,
         sched_dep_time_min = (sched_dep_time %/% 100 * 60 +
                                 sched_dep_time %% 100) %% 1440,
         dep_delay_diff = dep_delay - dep_time_min + sched_dep_time_min
  )

# dep_delay_diff, is the difference between the column, dep_delay, and departure delay
# calculated directly from the scheduled and actual departure times

filter(flights_deptime, dep_delay_diff != 0)

# Since it is just departure, timezones do not account for this but delay past midnight is.

# 4

flights %>%
  mutate(rank = min_rank(desc(arr_delay))) %>%
  filter(rank <= 10) %>%
  arrange(rank)

# This code ranks flights by arrival delay in descending order, selects the five most delayed flights 
# (including ties) and arranges them by their rank.
# min_rank() does the same as sports rankings, in that it assigns the same rank 
# to tied values and skips subsequent ranks accordingly

# 5

1:3 + 1:10
#recyles shorter to fit longer

# 6
# ?Trig read, 
# base::pi 

# 5.6

# 1
# The cost function is what matters, the arrival is the only delay we care about
# and the consistency as well. Don't need to find ways to measure this, just reflect on it

# 2

not_cancelled <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))

# n() 
not_cancelled %>%
  group_by(dest) %>%
  summarise(n = n())

# wt = distance is the same as saying sum(distance)
not_cancelled %>%
  group_by(tailnum) %>%
  summarise(n = sum(distance))

# 3

# If a flight never departs, then it won’t arrive. A flight could also depart and not arrive 
# if it is redirected and lands in an airport other than its intended destination. 
# So the most important column is arr_delay. 

# Some flights depart but don't arrive where they should:
filter(flights, !is.na(dep_delay), is.na(arr_delay)) %>%
  select(dep_time, arr_time, sched_arr_time, dep_delay, arr_delay)


# 4

# We can make an indicator for if the flight is cancelled and make a grouped 
# summary of cancelled_num = the number of flights that were cancelled on that day
# and flights_num = the total number of flights scheduled for that day

cancelled_per_day <- 
  flights %>%
  mutate(cancelled = (is.na(arr_delay) | is.na(dep_delay))) %>%
  group_by(year, month, day) %>%
  summarise(
    cancelled_num = sum(cancelled),
    flights_num = n(),
  )

ggplot(cancelled_per_day) +
  geom_point(aes(x = flights_num, y = cancelled_num))

# Congestion could cause cancellations, we can try to plot delay vs proportion of cancellations
# Remember mean() of a Boolean is a proportion

cancelled_and_delays <- 
  flights %>%
  mutate(cancelled = (is.na(arr_delay) | is.na(dep_delay))) %>%
  group_by(year, month, day) %>%
  summarise(
    cancelled_prop = mean(cancelled),
    avg_dep_delay = mean(dep_delay, na.rm = TRUE),
    avg_arr_delay = mean(arr_delay, na.rm = TRUE)
  )

ggplot(cancelled_and_delays) +
  geom_point(aes(x = avg_dep_delay, y = cancelled_prop))

ggplot(cancelled_and_delays) +
  geom_point(aes(x = avg_arr_delay, y = cancelled_prop))


# 5

# Worst delays by carrier

flights %>%
  group_by(carrier) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(desc(arr_delay))


filter(airlines, carrier == "F9")


# skip this,  way too long
# Can you disentangle the effects of bad airports vs. bad carriers?
# Comparing the delay of carrier within each route

flights %>%
  filter(!is.na(arr_delay)) %>%
  # Total delay by carrier within each origin + dest
  group_by(origin, dest, carrier) %>%
  summarise(
    arr_delay = sum(arr_delay),
    flights = n()
  ) %>%
  # Total delay within each origin dest
  group_by(origin, dest) %>%
  mutate(
    arr_delay_total = sum(arr_delay),
    flights_total = sum(flights)
  ) %>%
  # average delay of each carrier - average delay of other carriers
  ungroup() %>%
  mutate(
    arr_delay_others = (arr_delay_total - arr_delay) /
      (flights_total - flights),
    arr_delay_mean = arr_delay / flights,
    arr_delay_diff = arr_delay_mean - arr_delay_others
  ) %>%
  # remove NaN values (when there is only one carrier)
  filter(is.finite(arr_delay_diff)) %>%
  # average over all airports it flies to
  group_by(carrier) %>%
  summarise(arr_delay_diff = mean(arr_delay_diff)) %>%
  arrange(desc(arr_delay_diff))

# This code compares each carrier’s average arrival delay on a given route to the average delay of all other carriers
# on the same route, then averages those differences across all routes for each carrier 
# to see who performs better or worse than their competition

# 6

# The sort argument to count() sorts the results in order of n. 
# You could use this anytime you would run count() followed by arrange()

flights %>%
  count(dest, sort = TRUE)



# 5.7

# 1:

tibble(x = 1:9,
       group = rep(c("a", "b", "c"), each = 3)) %>%
  mutate(x_mean = mean(x)) %>%
  group_by(group) %>%
  mutate(x_mean_2 = mean(x))

# + - * / not affected by group_by:

tibble(x = 1:9,
       group = rep(c("a", "b", "c"), each = 3)) %>%
  mutate(y = x + 2) %>%
  group_by(group) %>%
  mutate(z = x + 2)

# modular arithmetic operators like %/% and %% are not either

tibble(x = 1:9,
       group = rep(c("a", "b", "c"), each = 3)) %>%
  mutate(y = x %% 2) %>%
  group_by(group) %>%
  mutate(z = x %% 2)

# Same for log()

tibble(x = 1:9,
       group = rep(c("a", "b", "c"), each = 3)) %>%
  mutate(y = log(x)) %>%
  group_by(group) %>%
  mutate(z = log(x))

# Cumulative and rolling aggregates cumsum(), cumprod(), cummin() and cummax are, gives by group:

tibble(x = 1:9,
       group = rep(c("a", "b", "c"), each = 3)) %>%
  mutate(x_cumsum = cumsum(x)) %>%
  group_by(group) %>%
  mutate(x_cumsum_2 = cumsum(x))

# Logical comparisons, <, <=, >, >=, !=, and == are not affected by group_by()

tibble(x = 1:9,
       y = 9:1,
       group = rep(c("a", "b", "c"), each = 3)) %>%
  mutate(x_lte_y = x <= y) %>%
  group_by(group) %>%
  mutate(x_lte_y_2 = x <= y)

# arrange() ignores groups when sorting: 

tibble(x = runif(9),
       group = rep(c("a", "b", "c"), each = 3)) %>%
  group_by(group) %>%
  arrange(x)
 
# Ranking functions like min_rank() work within each group when used with group_by()


tibble(x = 1:9,
       group = rep(c("a", "b", "c"), each = 3)) %>%
  mutate(rnk = min_rank(x)) %>%
  group_by(group) %>%
  mutate(rnk2 = min_rank(x))


# 2


# Worst on-time record can be either:
#1 proportion of flights not delayed or cancelled, to get proper sample size n >= 20 
#2 and mean arrival delay.


flights %>%
  filter(!is.na(tailnum), !is.na(arr_delay)) %>%
  mutate(on_time = !is.na(arr_time) & (arr_delay <= 0)) %>%
  group_by(tailnum) %>%
  summarise(on_time = mean(on_time), n = n()) %>%
  filter(n >= 20) %>%
  filter(min_rank(on_time) == 1)


flights %>%
  filter(!is.na(arr_delay)) %>%
  group_by(tailnum) %>%
  summarise(arr_delay = mean(arr_delay), n = n()) %>%
  filter(n >= 20) %>%
  filter(min_rank(desc(arr_delay)) == 1)

# 3

flights %>%
  group_by(hour) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(arr_delay)

# 4

flights %>%
  filter(arr_delay > 0) %>%
  group_by(dest) %>%
  mutate(
    arr_delay_total = sum(arr_delay),
    arr_delay_prop = arr_delay / arr_delay_total
  ) %>%
  select(dest, month, day, dep_time, carrier, flight,
         arr_delay, arr_delay_prop) %>%
  arrange(dest, desc(arr_delay_prop))


# Skip this and forward, too long












