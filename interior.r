# Insert all needed libraries
library(arrow)
library(tidyverse)
library(ggrepel)
library(nflfastR)
library(nflreadr)
library(ggplot2)
library(magrittr)
library(ggimage)
library(dplyr)
library(stringr)
options(mc.scores = parallel::detectCores())

# Base data set that will be used for all future analysis
pass_rush_data <- read.csv('pass_rush_summary.csv')
run_defense_data <- read.csv('run_defense_summary.csv')

# Grab the columns you want to use for Pass Rushing 
pass_rush_data %>%
  select(
    player,
    team_name,
    position,
    total_pressures
  ) -> passer_data2

# Filter out specific values within the Position column
edge_rushers <- subset(passer_data2, position == "ED")
interior_rushers <- subset(passer_data2, position == "DI")

# Now grab the columns for the Run Defense side
run_defense_data %>%
  select(
    player, 
    team_name, 
    position, 
    stops
  ) -> run_stops

# Again, filter out specific values within the Position column
edge_stoppers <- subset(run_stops, position == "ED")
interior_stoppers <- subset(run_stops, position == "DI") 

# Now combine the Total Pressure datasets with the Total Stops 
total_edge <- edge_rushers %>%
  left_join(edge_stoppers, by = c("player", "team_name"))
# Do it again, but his time for all Defensive Interior players  
total_interior <- interior_rushers %>%
  left_join(interior_stoppers, by = c("player", "team_name"))

# Replace these values below with their new abbr, so we can get logos on our scatter plots
total_edge <- total_edge %>%
  mutate(team_name = recode(team_name, "CLV" = "CLE", "ARZ" = "ARI", "BLT" = "BAL", "HST" = "HOU"))

# Insert team logos
# View(teams_colors_logos)
# Connect, in this case  "Team_Name" based on CSV data to "Team_Abbr"
#total_edge <- total_edge %>%
#  left_join(teams_colors_logos, by = c("team_name" = "team_abbr"))

# There's way too many players in this dataset. Let's do top 48
total_edge <- total_edge %>%
  top_n(48, total_pressures) %>%
  top_n(48, stops)

total_edge %>%
  left_join(teams_colors_logos, by = c('team_name' = 'team_abbr')) -> total_edge

# Now let's create our graph
total_edge %>%
  ggplot(aes(x = stops, y = total_pressures, label = player)) +
  geom_hline(yintercept = mean(total_edge$total_pressures), color = "red", linetype = "dashed", alpha = 0.8) +
  geom_vline(xintercept = mean(total_edge$stops), color = "red", linetype = "dashed", alpha = 0.8) +
  # geom_image(aes(image = team_logo_espn), size = 0.02, asp = 16/9) +
  geom_point(color = total_edge$team_color) + # We'll use points instead of logos. And we'll use the colors from team_colors
  geom_text_repel(
    max.overlaps = 30,
    box.padding = 0.5,
    point.padding = 0.5,
    size = 3.25
  ) +
  theme_bw() +
  labs(x = "Total Stops",
       y = "Total Pressures",
       title = "Top Edge Rushers 2023 (Thru Week 6)",
       caption = "By Joey Rodriguez | @0x676f64") +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  xlim(c(min(total_edge$stops) - 3.75, max(total_edge$stops) + 3.75)) +
  ylim(c(min(total_edge$total_pressures) - 3.75, max(total_edge$total_pressures) + 3.75))