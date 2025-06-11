#Code
library(tidyverse)
ufa_throws <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/ufa_throws.csv")

ufa_throws <- read_csv("https://raw.githubusercontent.com/BradenEberhard/Expected-Throwing-Value/refs/heads/main/data/all_games_1024.csv")


library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

colnames(ufa_throws)

cor(ufa_throws$throw_distance, ufa_throws$turnover, method = 'pearson')


#########Turnover Rate by Throw Distance#########

ufa_throws %>% 
  group_by(throw_distance_bin = cut(throw_distance, breaks = seq(0, max(throw_distance, na.rm=TRUE), by=10))) %>%
  filter(!is.na(throw_distance_bin)) %>%
  summarise(turnover_rate = mean(turnover, na.rm = TRUE)) %>%
  ggplot(aes(x = throw_distance_bin, y = turnover_rate, group = 1)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "lm", linewidth=2) +
  labs(title = "Turnover Rate by Throw Distance",
       x = "Throw Distance (binned)",
       y = "Turnover Rate") +
  theme_minimal()



cor(ufa_throws$throw_distance, ufa_throws$turnover, method = 'pearson')


########Heat Map#########
ufa_throws %>% filter(score_diff%in% -4:4) %>% filter(game_quarter%in% 1:4) %>% ggplot(aes(x=game_quarter,y=score_diff, fill = throw_distance))+
  geom_tile() +scale_fill_gradient(low="white", high="blue", name= "Throw Distance")
######



game_data <- ufa_throws %>% group_by(gameID) %>% mutate(Year= year(gameID)) %>% 
  summarize(total_plays = n(),
            total_turnover = sum(turnover, na.rm = TRUE),
            total_throw_distance = sum(throw_distance, na.rm = TRUE)) %>% select(everything())




# Reshape data to long format
df_long <- pivot_longer(year_data, cols = -Year, names_to = "Metric", values_to = "Value")


#########THROW DISTANCE by Year#####
# Plot
ggplot(df_long, aes(x = factor(Year), y = Value)) +
  geom_bar(stat = "identity") +
  labs(title = "Ultimate Frisbee Total Throw Distance by Year",
       x = "Year", y = "Throw Distance") +
  scale_fill_brewer(palette ="Set3" ) +
  theme_minimal()

install.packages("RColorBrewer")
library(RColorBrewer)
display.brewer.all()


# Separate gameid into columns
game_data_clean <- ufa_throws %>%
  separate(gameID, into = c("year", "month", "day", "team1", "team2"), sep = "-") %>%
  mutate(date = as.Date(paste(year, month, day, sep = "-"))) %>%
  select(date, team1, team2, everything(), -year, -month, -day)

# View result
print(game_data_clean)



###########    Throw distance by Team     ################

team_year_data <- game_data_clean %>%
  mutate(year = format(date, "%Y")) %>%
  pivot_longer(cols = c(team1, team2), names_to = "team_role", values_to = "team") %>%
  filter(!team %in% c("allstar", "game")) %>%  # <- filter out unwanted team names
  group_by(year, team) %>%
  summarize(t_total_throw_distance = sum(throw_distance, na.rm = TRUE),
            t_total_turnovers = sum(turnover, na.rm = TRUE),.groups = "drop")

# Plot
ggplot(team_year_data, aes(x = reorder(team, -t_total_throw_distance), y = t_total_throw_distance, fill = team)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ year, scales = "free_x") +
  labs(title = "Total Throw Distance by Team (2021–2024)",
       x = "Team", y = "Throw Distance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")


########total_turnover#########
team_turnover_data <- game_data_clean %>%
  mutate(year = format(date, "%Y")) %>%
  pivot_longer(cols = c(team1, team2), names_to = "team_role", values_to = "team") %>%
  filter(!team %in% c("allstar", "game")) %>%
  group_by(year, team) %>%
  summarize(total_turnovers = sum(turnover, na.rm = TRUE), .groups = "drop")

# Plot
ggplot(team_turnover_data, aes(x = reorder(team, -total_turnovers), y = total_turnovers, fill = team)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ year, scales = "free_x") +
  labs(title = "Total Turnovers by Team (2021–2024)",
       x = "Team", y = "Turnovers") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")


#############.  Tean Wins per Year #############

wins<- ufa_throws %>%
  distinct(gameID, home_teamID, away_teamID, home_team_win) %>%          # 1 row per game
  mutate(
    year = str_sub(gameID, 1, 4),                                         # Extract year
    winner = if_else(home_team_win == 1, home_teamID, away_teamID)       # Determine winner
  ) %>% filter(!home_teamID %in% c("allstars1", "allstars2")) %>% 
  count(year, winner, name = "wins") %>%                                  # Count wins per team per year
  rename(teamID = winner)


ggplot(wins, aes(x = reorder(teamID, -wins), y = wins, fill = year)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Team Wins per Year", x = "Team", y = "Wins") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#########       Horizontal plot, color by team. #############
ggplot(wins, aes(x = wins, y = reorder(teamID, wins), fill = teamID)) +
  geom_bar(stat = "identity") +
  facet_wrap(~year, scales = "free_y") +  # One panel per year
  labs(title = "Team Wins per Year", x = "Wins", y = "Team") +
  theme_minimal() +
  theme(legend.position = "none")


#12 games in the regular season

#install.packages("tidytext")
#library(tidytext)

ggplot(team_wins, aes(x = wins, y = reorder_within(teamID, wins, year), fill = teamID)) +
  geom_col() +
  facet_wrap(~year, scales = "free_y") +
  scale_y_reordered() +
  labs(title = "Team Wins per Year",
       x = "Wins", y = "Team") +
  theme_minimal() +
  theme(legend.position = "none")

#outlaws, legion and cannons disbanded/ceased operations

#havoc 2023
#mechanix 2024
#shred, nitro & summitt 2022
wins<- ufa_throws %>%
  distinct(gameID, home_teamID, away_teamID, home_team_win) %>%          # 1 row per game
  mutate( winner = if_else(home_team_win == 1, home_teamID, away_teamID)       # Determine winner
  ) %>% filter(!home_teamID %in% c("allstars1", "allstars2")) %>% 
  count(winner, name = "wins") %>%                                  # Count wins per team per year
  rename(teamID = winner)



team_year_data <- game_data_clean %>%
  mutate(year = format(date, "%Y"), winner = if_else(home_team_win == 1, home_teamID, away_teamID) %>%
  pivot_longer(cols = c(team1, team2), names_to = "team_role", values_to = "team") %>%
  filter(!team %in% c("allstar", "game")) %>%  # <- filter out unwanted team names
  count(year, winner, name = "wins") %>% 
  group_by(year, team) %>%
  summarize(t_total_throw_distance = sum(throw_distance, na.rm = TRUE),
            t_total_turnovers = sum(turnover, na.rm = TRUE),.groups = "drop"
            )