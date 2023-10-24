library(arrow)
library(nflfastR)
library(nflreadr)
library(tidyverse)
library(ggrepel)
library(ggplot2)
library(magrittr)
library(ggimage)
library(dplyr)
library(stringr)

# Insert the CSV data
pass_rush_data <- read.csv('pass_rush_summary.csv')
run_defense_data <- read.csv('run_defense_summary.csv')

# Select the columns you are actually trying to pull together
pass_rush_data %>%
  select(
    player,
    team_name,
    position,
    total_pressures
    ) -> pressures_NFL
# Did it for Pressures above, now do it again for the run stops
run_defense_data %>%
  select(
    player,
    team_name,
    position,
    stops
  ) -> stops_NFL
# Now join the two datasets together to make Defense_NFL
defense_NFL <- pressures_NFL %>%
  left_join(stops_NFL, by = c("player", "team_name"))
# Here we want to add every Pressure and Stop, and regroup them by each specific NFL team (should be 32)
defense_NFL <- defense_NFL %>%
  group_by(team_name) %>%
  summarize(Total_Pressures = sum(total_pressures),
            Total_Stops = sum(stops))
# PFF's CSV team abbr are different than Team_Color_Logo. So there's a few we need to manually edit
defense_NFL <- defense_NFL %>%
  mutate(team_name = recode(team_name, "CLV" = "CLE", "ARZ" = "ARI", "BLT" = "BAL", "HST" = "HOU"))

# Some additions left columns with a N/A value. So I'm just going to manually edit the columns with the correct number of stops
defense_NFL <- defense_NFL %>%
  mutate(Total_Stops = case_when(
    team_name == "DEN" ~ 85,
    team_name == "LAC" ~ 48,
    team_name == "MIN" ~ 90,
    team_name == "NE" ~ 103,
    team_name == "TEN" ~ 92,
    team_name == "WAS" ~ 92,
    TRUE ~ Total_Stops
  ))
# Enable to get logos
View(teams_colors_logos)
# Combine original dataset with team logos
defense_NFL <- defense_NFL %>%
  left_join(teams_colors_logos, by = c('team_name' = 'team_abbr'))
# Now create plot 
defense_NFL %>%
  ggplot(aes(x = Total_Stops, y = Total_Pressures)) +
  geom_hline(yintercept = mean(defense_NFL$Total_Pressures), color = "red", linetype = "dashed", alpha = 0.8) +
  geom_vline(xintercept = mean(defense_NFL$Total_Stops, na.rm = TRUE), color = "red", linetype = "dashed", alpha = 0.8) +
  geom_image(aes(image = team_logo_espn), size = 0.06, asp = 16/9) +
  theme_bw() +
  labs(x = "Total Stops",
       y = "Total Pressures",
       title = "NFL Defenses: Pressures and Stops (Thru Week 6)",
       caption = "By. Joey Rodriguez | @0x676f64") +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  xlim(c(min(defense_NFL$Total_Stops, na.rm = TRUE) - 5, max(defense_NFL$Total_Stops) + 5)) +
  ylim(c(min(defense_NFL$Total_Pressures, na.rm = TRUE) - 5, max(defense_NFL$Total_Pressures) + 5))
