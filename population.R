# Population mean county centers for Colorado counties, from the US Census

library(readr)

population_file <- "https://www2.census.gov/geo/docs/reference/cenpop2010/county/CenPop2010_Mean_CO08.txt"

# Create a vector of the width of each column
population_widths <- c(3, 4, 15, 15, 15, 15, 15)

# Create a vector of column names, based on the online documentation for this data
population_colnames <- c("STATEFP", "COUNTYFP", "COUNAME", "STNAME", "POPULATION", "LATITUDE", "LONGITUDE")

# read the file in from its url
population <- read_csv(population_file, fwf_widths(population_widths, population_colnames))
