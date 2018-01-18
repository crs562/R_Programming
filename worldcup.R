# Here is the worldcup dataset from the package faraway, which statistics from the 2010 World Cup.
# This dataset has observations by player, including the player's team, position, amount of time played in this World Cup,
# and number of shots, passes, tackles, and saves.

library(faraway)
library(tidyr)
library(ggplot2)
library(knitr)
data(worldcup)

# provide row name to its own column
world_cup <- worldcup %>%
  mutate(player_name = rownames(worldcup))

world_cup %>%
  slice(1:3)

# Add a column with the average number of shots for a player's position added as a new column.
world_cup %>%
  group_by(Position) %>%
  mutate(ave_shots = mean(Shots)) %>%
  ungroup() %>%
  slice(1:3)

world_cup %>%
  group_by(Position) %>%
  summarize(ave_shots = mean(Shots)) %>%
  ungroup() %>%
  slice(1:3)

# renaming
world_cup %>%
  rename(Name = player_name) %>%
  slice(1:3)

# Plot the relationship between the time a player played in the World Cup and his number of saves, tackles, and shots,
# with a separate graph for each position.
world_cup %>%
  select(Position, Time, Shots, Tackles, Saves) %>%
  gather(Type, Number, -Position, -Time) %>%
  ggplot(aes(x = Time, y = Number)) +
  geom_point() +
  facet_grid(Type ~ Position)

# Print a table of the average number and range of passes by position for the top four teams in this World Cup
# (Spain, Netherlands, Uruguay, and Germany)
# Summarize the data to create the summary statistics
wc_table <- world_cup %>%
  filter(Team %in% c("Spain", "Netherlands", "Uruguay", "Germany")) %>%
  select(Team, Position, Passes) %>%
  group_by(Team, Position) %>%
  summarize(ave_passes = mean(Passes),
            min_passes = min(Passes),
            max_passes = max(Passes),
            pass_summary = paste0(round(ave_passes), " (", min_passes, ", ", max_passes, ")")) %>%
  select(Team, Position, pass_summary)

# What the data looks like before using spread
wc_table

# Use spread to create a prettier format for a table
wc_table %>%
  spread(Position, pass_summary) %>%
  kable()
