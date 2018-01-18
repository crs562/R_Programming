# The "Extended Best Tracks" for the North Atlantic are hurricane tracks that include both the best estimate of the
# central location of each storm and also gives estimates of how far winds of certain speeds extended from the
# storm's center in four quadrants of the storm (northeast, northwest, southeast, southwest) at each measurement point.

library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(gridExtra)
library(ggmap)

# Web Based File
ext_tracks_file <- "http://rammb.cira.colostate.edu/research/tropical_cyclones/tc_extended_best_track_dataset/data/ebtrk_atlc_1988_2015.txt"

# Create a vector of the width of each column
ext_tracks_widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
                       4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)

# Create a vector of column names, based on the online documentation for this data
ext_tracks_colnames <- c("storm_id", "storm_name", "month", "day",
                         "hour", "year", "latitude", "longitude",
                         "max_wind", "min_pressure", "rad_max_wind",
                         "eye_diameter", "pressure_1", "pressure_2",
                         paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                         "storm_type", "distance_to_land", "final")

# Read the file in from its url
ext_tracks <- read_fwf(ext_tracks_file, 
                       fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                       na = "-99")

# Prints a sample of four rows of data from Hurricane Katrina, with, for each row, the date and time, maximum wind
# speed, minimum pressure, and the radius of maximum winds of the storm for that observation.
ext_tracks %>%
  filter(storm_name == "KATRINA") %>%
  select(month, day, hour, max_wind, min_pressure, rad_max_wind) %>%
  sample_n(4)

# without piping, see time, date, and maximum winds for Katrina from the first three rows of the ext_tracks hurricane data.
katrina <- filter(ext_tracks, storm_name == "KATRINA")
katrina_reduced <- select(katrina, month, day, hour, max_wind)
head(katrina_reduced, 3)

# data for Hurricane Katrina without Piping one function inside another
head(select(filter(ext_tracks, storm_name == "KATRINA"), month, day, hour, max_wind), 3)

# summary of the number of observations in the ext_tracks hurricane dataset, as well as the highest measured maximum
# windspeed (given by the column max_wind in the dataset) in any of the storms, and the lowest minimum pressure
# (min_pressure)
ext_tracks %>%
  summarize(n_obs = n(),
            worst_wind = max(max_wind),
            worst_pressure = min(min_pressure))

# In Summary max_wind is in knots converting to mph
knots_to_mph <- function(knots)
{
  mph <- 1.152 * knots
}

# Summarizing data in mph
ext_tracks %>%
  summarize(n_obs = n(), 
            worst_wind = knots_to_mph(max(max_wind)), 
            worst_pressure = min(min_pressure))

# Group_by example
ext_tracks %>%
  group_by(storm_name, year) %>%
  head()

# group_by with summarize
ext_tracks %>%
  group_by(storm_name, year) %>%
  summarize(n_obs = n(),
            worst_wind = max(max_wind),
            worst_pressure = min(min_pressure))

# plot a histogram of maximum wind speed observed for each storm.
ext_tracks %>%
  group_by(storm_name) %>%
  summarize(worst_wind = max(max_wind)) %>%
  ggplot(aes(x = worst_wind)) + geom_histogram()

# Selecting and Filtering Data

# Select the storm name, date, time, latitude, longitude, and maximum wind speed from the ext_tracks dataset
ext_tracks %>%
  select(storm_name, month, day, hour, year, latitude, longitude, max_wind)

# Select with starts_with
ext_tracks %>%
  select(storm_name, latitude, longitude, starts_with("radius_34"))

# select with ends_with
select(ext_tracks, ends_with("ne"))

# select with contains
select(ext_tracks, contains("34"))

# select with matches
select(ext_tracks, matches("_[0-9][0-9]_"))

# filters
filter(ext_tracks, storm_name == "KATRINA")
filter(ext_tracks, min_pressure != 0)
filter(ext_tracks, latitude > 25)
filter(ext_tracks, max_wind >= 160)
filter(ext_tracks, min_pressure < 900)
filter(ext_tracks, distance_to_land <= 0)
filter(ext_tracks, storm_name %in% c("KATRINA", "ANDREW"))
filter(ext_tracks, is.na(radius_34_ne))
filter(ext_tracks, !(storm_name %in% c("KATRINA", "ANDREW")))
filter(ext_tracks, !(is.na(radius_34_ne)))

head(ext_tracks$hour)
head(ext_tracks$hour == "00")

ext_tracks %>%
  select(storm_name, hour, max_wind) %>%
  head(9)

ext_tracks %>%
  select(storm_name, hour, max_wind) %>%
  filter(hour == "00") %>%
  head(3)

# determine which storms had maximum wind speed equal to or above 160 knots.
ext_tracks %>%
  group_by(storm_name, year) %>%
  summarize(worst_wind = max(max_wind)) %>%
  filter(worst_wind >= 160)

# pull out observations for Hurricane Andrew when it was at or above Category 5 strength (137 knots or higher)
ext_tracks %>%
  select(storm_name, month, day, hour, latitude, longitude, max_wind) %>%
  filter(storm_name == "ANDREW" & max_wind >= 137)

andrew_tracks <- ext_tracks %>%
  filter(storm_name == "ANDREW" & year == "1992") %>%
  select(year, month, day, hour, max_wind, min_pressure) %>%
  unite(datetime, year, month, day, hour) %>%
  mutate(datetime = ymd_h(datetime))

head(andrew_tracks, 3)

class(andrew_tracks$datetime)

# plot a time series using datetime, ggplot2 can recognize that this object is a date-time and will make sensible
# axis labels.
andrew_tracks %>%
  gather(measure, value, -datetime) %>%
  ggplot(aes(x = datetime, y = value)) +
  geom_point() + geom_line() +
  facet_wrap(~ measure, ncol = 1, scales = "free_y")

andrew_tracks %>%
  select(datetime) %>%
  mutate(year = year(datetime),
         month = months(datetime),
         weekday = weekdays(datetime),
         yday = yday(datetime),
         hour = hour(datetime)) %>%
  slice(1:3)

# average value of max_wind storm observations by day of the week and by month
check_tracks <- ext_tracks %>%
  select(month, day, hour, year, max_wind) %>%
  unite(datetime, year, month, day, hour) %>%
  mutate(datetime = ymd_h(datetime),
         weekday = weekdays(datetime),
         weekday = factor(weekday, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
         month = months(datetime),
         month = factor(month, levels = c("January", "February", "March", "April", "May", "June", "July", "August",
                                          "September", "October", "November", "December")))

check_weekdays <- check_tracks %>%
  group_by(weekday) %>%
  summarize(ave_max_wind = mean(max_wind)) %>%
  rename(grouping = weekday)

check_months <- check_tracks %>%
  group_by(month) %>%
  summarize(ave_max_wind = mean(max_wind)) %>%
  rename(grouping = month)

a <- ggplot(check_weekdays, aes(x = grouping, y = ave_max_wind)) + geom_bar(stat = "identity") + xlab("")

b <- a %+% check_months

grid.arrange(a, b, ncol = 1)

# Location of Hurricane Andrew by date as it neared and crossed the United States, based on date-time observation in UTC
andrew_tracks <- ext_tracks %>%
  filter(storm_name == "ANDREW") %>%
  select(year, month, day, hour, latitude, longitude) %>%
  unite(datetime, year, month, day, hour) %>%
  mutate(datetime = ymd_h(datetime), date = format(datetime, "%b %d"))

miami <- get_map("miami", zoom = 5)

ggmap(miami) + geom_path(data = andrew_tracks, aes(x = -longitude, y = latitude), color = "gray", size = 1.1) +
  geom_point(data = andrew_tracks, aes(x = -longitude, y = latitude, color = date), size = 2)

andrew_tracks <- andrew_tracks %>%
  mutate(datetime = with_tz(datetime, tzone = "America/New_York"),
         date = format(datetime, "%b %d"))

ggmap(miami) + geom_path(data = andrew_tracks, aes(x = -longitude, y = latitude), color = "gray", size = 1.1) +
  geom_point(data = andrew_tracks, aes(x = -longitude, y = latitude, color = date), size = 2)
