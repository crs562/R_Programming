# This file contain data clining from web base data about the "Extended Best Tracks" for North Atlantic are hurricane tracks that
# include both the best estimate of the central location of each storm and also gives estimates of how far winds of certain speeds
# extended from the storm's center in four quadrants of the storm (northeast, northwest, southeast, southwest) at each measurement
# point.

library(readr)
library(dplyr)
library(ggplot2)

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

# data for Hurricane Katrina with Piping
ext_tracks %>%
  filter(storm_name == "KATRINA") %>%
  select(month, day, hour, max_wind, min_pressure, rad_max_wind) %>%
  sample_n(4)

# data for Hurricane Katrina without Piping creating new object at each step
katrina <- filter(ext_tracks, storm_name == "KATRINA")
katrina_reduced <- select(katrina, month, day, hour, max_wind)
head(katrina_reduced, 3)

# data for Hurricane Katrina without Piping one function inside another
head(select(filter(ext_tracks, storm_name == "KATRINA"), month, day, hour, max_wind), 3)

# Summarizing data
ext_tracks %>%
  summarize(n_obs = n(), worst_wind = max(max_wind), worst_pressure = min(min_pressure))

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

# create plot for above summery
ext_tracks %>%
  group_by(storm_name) %>%
  summarize(worst_wind = max(max_wind)) %>%
  ggplot(aes(x = worst_wind)) + geom_histogram()

# Selecting and Filtering Data

# Select Function
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

# Filtering with summarization
ext_tracks %>%
  group_by(storm_name, year) %>%
  summarize(worst_wind = max(max_wind)) %>%
  filter(worst_wind >= 160)

ext_tracks %>%
  select(storm_name, month, day, hour, latitude, longitude, max_wind) %>%
  filter(storm_name == "ANDREW" & max_wind >= 137)
