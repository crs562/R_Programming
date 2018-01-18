# pull weather data from airports around the world directly from the Iowa Environmental Mesonet.

library(httr)
library(tidyverse)
library(riem)

# To get wind speed in miles per hour (data = "sped") for Denver, CO, (station = "DEN") for the month of June 2016
# (year1 = "2016", month1 = "6", etc.) in Denver’s local time zone (tz = "America/Denver") and in a comma-separated file
# (format = "comma").

meso_url <- "https://mesonet.agron.iastate.edu/cgi-bin/request/asos.py/"
denver <- GET(url = meso_url, query = list(station = "DEN", data = "sped", year1 = "2016", month1 = "6", day1 = "1", 
                                           year2 = "2016", month2 = "6", day2 = "30", tz = "America/Denver", 
                                           format = "comma")) %>%
  content() %>%
  read_csv(skip = 5, na = "M")

denver %>%
  slice(1:3)

# Get Available networks
riem_networks()

# Get available stations for one network
riem_stations(network = "IN_ASOS")

# Get measures for one station
measures <- riem_measures(station = "VOHY", date_start = "2000-01-01", date_end = "2016-04-22")
knitr::kable(head(measures))
