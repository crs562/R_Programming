# Read CSV file

library(readr)

teams <- read_csv("data/team_standings.csv")
teams

teams <- read_csv("data/team_standings.csv", col_types = "cc")
teams

# merge two dataset
left_join(world_cup, teams, by = "Team")

# create a table of the top 5 players by shots on goal, as well as the final standing for each of these player's teams,
# using the worldcup and team_standings data.
data("worldcup")
worldcup %>%
  mutate(Name = rownames(worldcup),
         team = as.character(Team)) %>%
  select(Name, Position, Shots, Team) %>%
  arrange(desc(Shots)) %>%
  slice(1:5) %>%
  left_join(teams, by = "Team") %>% # Merge in team standings
  rename("Team Standing" = Standing) %>%
  kable()
