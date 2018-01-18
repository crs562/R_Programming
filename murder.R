# Load package and data
library(dslabs)
data("murders")

# provide class of a data
class(murders)

# provide the structure of data frames
str(murders)

# provide the first six row of the data frames
head(murders)

# provide the data for column population
pop <- murders$population
pop1 <- murders[["population"]]
a <- murders$abb
a1 <- murders[["abb"]]

# provide the names of the columns
names(murders)

# length of a vector
length(pop)

# provide a class of state column
class(murders$state)
class(a)
class(a1)

# provide a class of region column
class(murders$region)

# provide the names of a region column factors
levels(murders$region)

# Determine the number of regions included in the region
length(levels(murders$region))

# Show the number of states per region
table(murders$region)

# sorting murders
sort(murders$total)

