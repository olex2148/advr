library("tidyverse")
library("nycflights13")
library("viridis")
library("Lahman")


# # Primary key: cols that uniquely identifies each row in its own table. It is unique and never NULL.
# 
# # Foreign key: cols in one table that refers to the primary key in another table
# # creating the link between the two.
# 
# #1
# 
# # we need longitude, latitude, dest, origin. We can join flights and airports for this. 
# # Origin and destination links them
# 
# flights_latlon <- flights %>%
#   inner_join(select(airports, origin = faa, origin_lat = lat, origin_lon = lon),
#              by = "origin"
#   ) %>%
#   inner_join(select(airports, dest = faa, dest_lat = lat, dest_lon = lon),
#              by = "dest"
#   )
# 
# # we can plot them like this: 
# 
# flights_latlon %>%
#   slice(1:20) %>%
#   ggplot(aes(
#     x = origin_lon, xend = dest_lon,
#     y = origin_lat, yend = dest_lat
#   )) +
#   borders("state") +
#   geom_segment(arrow = arrow(length = unit(0.1, "cm"))) +
#   coord_quickmap() +
#   labs(y = "Latitude", x = "Longitude")
# 
# 
# #2 
# 
# #https://jrnold.github.io/r4ds-exercise-solutions/diagrams/nycflights.png
# 
# #The column airports$faa is a foreign key of weather$origin since it refers to a primary key: weather$origin 
# 
# #3
# 
# # If the weather was included for all airports in the US, 
# # then it would provide the weather for the destination of each flight.
# # The weather data frame columns (year, month, day, hour, origin) are a foreign key 
# # for the flights data frame columns (year, month, day, hour, dest). 
# # This would provide information about the weather at the destination airport at the time of the flight take off
# 
# #4 
# 
# special_days <- tribble(
#   ~year, ~month, ~day, ~holiday,
#   2013, 01, 01, "New Years Day",
#   2013, 07, 04, "Independence Day",
#   2013, 11, 29, "Thanksgiving Day",
#   2013, 12, 25, "Christmas Day"
# )
# 
# # The primary key of the table would be the (year, month, day) columns. 
# # The (year, month, day) columns could be used to join special_days with other tables.
# 
# 
# #13.3
# 
# #1
# 
# flights %>%
#   arrange(year, month, day, sched_dep_time, carrier, flight) %>%
#   mutate(flight_id = row_number()) %>%
#   glimpse()
# 
# #2
# 
# #The primary key for Lahman::Batting is (playerID, yearID, stint)
# #The columns (playerID, yearID) are not a primary key because players can play on different teams within the same year.
# 
# Lahman::Batting %>%
#   count(playerID, yearID, stint) %>%
#   filter(n > 1) %>%
#   nrow()
# 
# #3
# 
# nasaweather::atmos %>%
#   count(lat, long, year, month) %>%
#   filter(n > 1) %>%
#   nrow()
# 
# #4
# 
# fueleconomy::vehicles %>%
#   count(id) %>%
#   filter(n > 1) %>%
#   nrow()
# 
# #5
# 
# ggplot2::diamonds %>%
#   distinct() %>%
#   nrow()
# 
# 
# nrow(ggplot2::diamonds)
# 
# #no primary key
# 
# #3
# 
# Lahman::Batting
# Lahman::Master
# Lahman::Salaries
# 
# # playerID is complete and in all tables
# 
# https://jrnold.github.io/r4ds-exercise-solutions/diagrams/Lahman1.png
# 
# # SKIP the rest
# # 
# # Lahman::Master
# # Lahman::Managers
# # Lahman::AwardsManagers
# # 
# # https://jrnold.github.io/r4ds-exercise-solutions/diagrams/Lahman2.png
# 
# 
# https://jrnold.github.io/r4ds-exercise-solutions/diagrams/Lahman3.png
# 
# # the combination (playerID, yearID, stint) are primary keys. 
# # Fielding has POS, Pitching does not, so the right one is Pitching

#13.4

#1 


avg_dest_delays <-
  flights %>%
  group_by(dest) %>%
  # arrival delay NA's are cancelled flights
  summarise(delay = mean(arr_delay, na.rm = TRUE)) %>%
  inner_join(airports, by = c(dest = "faa"))


avg_dest_delays %>%
  ggplot(aes(lon, lat, colour = delay)) +
  borders("state") +
  geom_point() +
  coord_quickmap()

#2

airport_locations <- airports %>%
  select(faa, lat, lon)

# We join by faa twice and dplyr will add .x and .y. Here .x will be the first i.e. origin and .y is dest. 

flights %>%
  select(year:day, hour, origin, dest) %>%
  left_join(
    airport_locations,
    by = c("origin" = "faa")
  ) %>%
  left_join(
    airport_locations,
    by = c("dest" = "faa")
  )


#3

# Join flights and planes by "tailnum" to get information on age, rename year to plane_year

plane_cohorts <- inner_join(flights,
                            select(planes, tailnum, plane_year = year),
                            by = "tailnum"
) %>%
  mutate(age = year - plane_year) %>%
  filter(!is.na(age)) %>%
  # Cut off older than 25 and leave others as is
  mutate(age = if_else(age > 25, 25L, age)) %>%
  group_by(age) %>%
  summarise(
    dep_delay_mean = mean(dep_delay, na.rm = TRUE),
    arr_delay_mean = mean(arr_delay, na.rm = TRUE),
    # Count non-missing values of the delays
    n_arr_delay = sum(!is.na(arr_delay)),
    n_dep_delay = sum(!is.na(dep_delay))
  )

# Departure delay
ggplot(plane_cohorts, aes(x = age, y = dep_delay_mean)) +
  geom_point() +
  scale_x_continuous("Age of plane (years)", breaks = seq(0, 30, by = 10)) +
  scale_y_continuous("Mean Departure Delay (minutes)")

# Arrival delay
ggplot(plane_cohorts, aes(x = age, y = arr_delay_mean)) +
  geom_point() +
  scale_x_continuous("Age of Plane (years)", breaks = seq(0, 30, by = 10)) +
  scale_y_continuous("Mean Arrival Delay (minutes)")


#4

#Precipitation and visibility are likely causing delays  

flight_weather <-
  flights %>%
  inner_join(weather, by = c(
    "origin" = "origin",
    "year" = "year",
    "month" = "month",
    "day" = "day",
    "hour" = "hour"
  ))

flight_weather %>%
  group_by(precip) %>%
  summarise(delay = mean(dep_delay, na.rm = TRUE)) %>%
  
  ggplot(aes(x = precip, y = delay)) +
  geom_line() + geom_point()


flight_weather %>%
  # Visibility into 10 categories of equal sizes
  mutate(visib_cat = cut_interval(visib, n = 10)) %>%
  group_by(visib_cat) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  
  ggplot(aes(x = visib_cat, y = dep_delay)) +
  geom_point()

#5

flights %>%
  filter(year == 2013, month == 6, day == 13) %>%
  group_by(dest) %>%
  summarise(delay = mean(arr_delay, na.rm = TRUE)) %>%
  inner_join(airports, by = c("dest" = "faa")) %>%
  
  ggplot(aes(y = lat, x = lon, size = delay, colour = delay)) +
  borders("state") +
  geom_point() +
  coord_quickmap() +
  scale_colour_viridis()

#https://en.wikipedia.org/wiki/June_12%E2%80%9313,_2013_derecho_series


# 
# #13.5
# 
# #1
# 
# flights %>%
#   filter(is.na(tailnum), !is.na(arr_time)) %>%
#   nrow()
# 
# # Flights that have a missing tailnum all have missing values of arr_time, meaning that the flight was canceled.
# 
# #2
# 
# # First find the planes that have flown 100 or more flights
# 
# planes_gte100 <- flights %>%
#   filter(!is.na(tailnum)) %>%
#   group_by(tailnum) %>%
#   count() %>%
#   filter(n >= 100)
# 
# # Then semi join the data frame of planes that have flown at least 100 flights to 
# # the data frame of flights to select the flights by those planes.
# 
# flights %>%
#   semi_join(planes_gte100, by = "tailnum")
# 
# 
# #3
# 
# fueleconomy::vehicles %>%
#   semi_join(fueleconomy::common, by = c("make", "model"))
# 
# # We have to join by make and model both since there are model names share between makers and vice versa.
# 
# fueleconomy::vehicles %>%
#   distinct(model, make) %>%
#   group_by(model) %>%
#   filter(n() > 1) %>%
#   arrange(model)
# 
# 
# 
# fueleconomy::common %>%
#   distinct(model, make) %>%
#   group_by(model) %>%
#   filter(n() > 1) %>%
#   arrange(model)
# 
# 
# #4
# 
# # We need to find the 48 hours with the worst delays. 
# # We group flights by hour of scheduled departure time and calculate the average delay. 
# # Then I select the 48 observations (hours) with the highest average delay
# 
# worst_hours <- flights %>%
#   mutate(hour = sched_dep_time %/% 100) %>%
#   group_by(origin, year, month, day, hour) %>%
#   summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
#   ungroup() %>%
#   arrange(desc(dep_delay)) %>%
#   slice(1:48)
# 
# # We can use semi_join to get those hours from weather
# 
# weather_most_delayed <- semi_join(weather, worst_hours, 
#                                   by = c("origin", "year",
#                                          "month", "day", "hour"))
# 
# # We can then get the 48 non-contiguous hours with the worst delay and weather patterns:
# 
# select(weather_most_delayed, temp, wind_speed, precip) %>%
#   print(n = 48)
# 
# ggplot(weather_most_delayed, aes(x = precip, y = wind_speed, color = temp)) +
#   geom_point()
# 
# #5
# 
# anti_join(flights, airports, by = c("dest" = "faa"))
# 
# # Returns flights that did not go to a FAA destination, which are domestic airports, thus a foreign flight
# # there are 4 such destination in the dataset:
# 
# anti_join(flights, airports, by = c("dest" = "faa")) %>% 
#   distinct(dest)
# 
# anti_join(airports, flights, by = c("faa" = "dest"))
# 
# 
# # Returns airports that do not have a direct flight from NYC in that year.
# 
# anti_join(airports, flights, by = c("faa" = "dest"))
# 
# 
# #6
# 
# # Planes might be sold among airlines. 
# 
# # All distinct airline, plane combinations
# planes_carriers <-
#   flights %>%
#   filter(!is.na(tailnum)) %>%
#   distinct(tailnum, carrier)
# 
# # The number of planes that have flown for more than one airline 
# # are those tailnum that appear more than once in the planes_carriers data
# 
# planes_carriers %>%
#   count(tailnum) %>%
#   filter(n > 1) %>%
#   nrow()
# 
# # The names of airlines are easier to understand than the two-letter carrier codes. 
# # The airlines data frame contains the names of the airlines
# 
# carrier_transfer_tbl <- planes_carriers %>%
#   # keep only planes which have flown for more than one airline
#   group_by(tailnum) %>%
#   filter(n() > 1) %>%
#   # join with airlines to get airline names
#   left_join(airlines, by = "carrier") %>%
#   arrange(tailnum, carrier) %>% 
#   print()

