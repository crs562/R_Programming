# Data illustrating death rates in Virginia in 1940 (not a tidy data)

library(tidyr)
library(dplyr)
data("VADeaths")
head(VADeaths)

# Create a tidy data, the variables are age category, gender, and urban-ness.
# Finally, the death rate itself, which is the fourth variable, is presented inside the table.
VADeaths %>%
  tbl_df() %>%
  mutate(age = row.names(VADeaths)) %>%
  gather(key, death_rate, -age) %>%
  separate(key, c("urban", "gender"), sep = " ") %>%
  mutate(age = factor(age), urban = factor(urban), gender = factor(gender))

# Move age from row names into a column
VADeaths <- VADeaths %>%
  tbl_df() %>%
  mutate(age = row.names(VADeaths))

# Gather everything but age to tidy data
VADeaths %>%
  gather(key = key, value = death_rate, -age)
