#Code
library(tidyverse)
ufa_throws <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/ufa_throws.csv")

ufa_throws <- read_csv("https://raw.githubusercontent.com/BradenEberhard/Expected-Throwing-Value/refs/heads/main/data/all_games_1024.csv")




colnames(ufa_throws)
library(ggplot2)


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
  geom_tile() +scale_fill_gradient(low="white", high="darkblue", name= "Throw Distance")
######



ufa_throws <- ufa_throws %>% mutate(ufa_angle= throw_angle * (180/pi))

ufa_throws %>% ggplot(aes(x = ufa_angle)) +
  geom_histogram(binwidth = 1, fill = "blue", alpha = 0.9) +
  coord_polar(start = 0) +
  theme_minimal() +
  labs(title = "Throw Angle")



game_data <- ufa_throws %>% group_by(gameID) %>% mutate(Year= year(gameID)) %>% 
  summarize(total_plays = n(),
            total_turnover = sum(turnover, na.rm = TRUE),
            passing_plays = sum(throw_distance, na.rm = TRUE))





# Reshape data to long format
df_long <- pivot_longer(year_data, cols = -Year, names_to = "Metric", values_to = "Value")


#########THROW DISTANCE by Year#####
# Plot
ggplot(df_long, aes(x = factor(Year), y = Value)) +
  geom_bar(stat = "identity") +
  labs(title = "Ultimate Frisbee Total Throw Distance by Year",
       x = "Year", y = "Throw Distance") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()


library(dplyr)
library(tidyr)

# Separate gameid into columns
game_data_clean <- game_data %>%
  separate(gameID, into = c("year", "month", "day", "team1", "team2"), sep = "-") %>%
  mutate(date = as.Date(paste(year, month, day, sep = "-"))) %>%
  select(date, team1, team2, everything(), -year, -month, -day)

# View result
print(game_data_clean)



# Convert to long format (one row per team)
team_long <- team_data_2022 %>%
  pivot_longer(cols = c(team1, team2), names_to = "team_role", values_to = "team") %>%
  group_by(team) %>%
  summarize(total_throw_distance = sum(throw_distance, na.rm = TRUE),.groups = "drop")


library(dplyr)
###########Throw distance by Team################

team_year_data <- game_data_clean %>%
  mutate(year = format(date, "%Y")) %>%
  pivot_longer(cols = c(team1, team2), names_to = "team_role", values_to = "team") %>%
  filter(!team %in% c("allstar", "game")) %>%  # <- filter out unwanted team names
  group_by(year, team) %>%
  summarize(total_throw_distance = sum(throw_distance, na.rm = TRUE)
            , .groups = "drop")

# Plot
ggplot(team_year_data, aes(x = reorder(team, -total_throw_distance), y = total_throw_distance, fill = team)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ year, scales = "free_x") +
  labs(title = "Total Throw Distance by Team (2021–2024)",
       x = "Team", y = "Throw Distance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")


library(dplyr)

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








