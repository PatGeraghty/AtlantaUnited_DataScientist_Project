setwd("~/Lab Dataset")

# import libraries 
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)
library(fmsb)
library(ggrepel)
library(SBpitch)

### Project 1: xG + xA Predictions ### 

# import data
schedule <- read.csv('ATL_MatchSchedule.csv')
raw_player_data <- read.csv('ATL_MatchStats.csv')

# join datasets on match_id
data <- raw_player_data %>% inner_join(schedule, by = "match_id")

# add rows for later prediction service 
# filter for this weekends matches
this_weekend_matches <- schedule %>%
  filter(match_date %in% c("2025-03-22", "2025-03-23"))

# select players with over 100 mins so far this season
eligible_players <- data %>%
  filter(season_id.x == 315) %>%
  group_by(player_id, player_name, team_name, team_id) %>%
  summarise(total_minutes = sum(player_match_minutes, na.rm = TRUE), .groups = "drop") %>%
  filter(total_minutes > 120)

# match players to their upcoming weekend match
this_weekend_players <- eligible_players %>%
  left_join(this_weekend_matches %>%
              rename(match_team_id = home_team_id), 
            by = c("team_id" = "match_team_id")) %>%
  bind_rows(eligible_players %>%
              left_join(this_weekend_matches %>%
                          rename(match_team_id = away_team_id), 
                        by = c("team_id" = "match_team_id"))) %>%
  filter(!is.na(match_id)) %>%  
  select(player_id, team_id, match_id, match_date, home_team_id, away_team_id, player_name, team_name)

# fill in missing home_team_id and away_team_id using match_id from this_weekend_matches
this_weekend_players <- this_weekend_players %>%
  left_join(this_weekend_matches %>% select(match_id, home_team_id, away_team_id),
            by = "match_id") %>%
  mutate(home_team_id = coalesce(home_team_id.x, home_team_id.y),
         away_team_id = coalesce(away_team_id.x, away_team_id.y)) %>%
  select(-home_team_id.x, -home_team_id.y, -away_team_id.x, -away_team_id.y)

# create new rows with necessary columns
new_rows <- this_weekend_players %>% mutate(match_date = as.Date(match_date),  
                                            season_id = 315,
                                            player_match_minutes = NA,  
                                            np_xg_op_xa = NA) %>%
  select(match_id, match_date, player_id, player_name, team_id, team_name, 
         season_id, player_match_minutes, np_xg_op_xa, home_team_id, away_team_id)

# ensure both match_date are date 
data <- data %>% mutate(match_date = as.Date(match_date))
new_rows <- new_rows %>% mutate(match_date = as.Date(match_date))

# add the new rows to the original dataset
data <- bind_rows(data, new_rows)

# create non pen xg + open play xa metric
data$np_xg_op_xa <- data$player_match_np_xg + data$player_match_op_xa

# do home teams score more? will i want a feature for this in the final model?
mean(data$home_score, na.rm = TRUE)
mean(data$away_score, na.rm = TRUE)
# home and away does make a difference

# ensure match_date is data again
data <- data %>% mutate(match_date = as.Date(match_date, format = "%Y-%m-%d"))

# create opposition_team_id and opposition_goals_allowed
data <- data %>% mutate(opposition_team_id = ifelse(team_id == home_team_id, away_team_id, home_team_id))
data <- data %>% mutate(opposition_goals_allowed = ifelse(opposition_team_id == home_team_id, away_score, home_score))

# create is_player_home variable
data <- data %>% mutate(is_player_home = ifelse(team_id == home_team_id, 1, 0))



# create rolling average for player xg + xa 
data <- data %>% arrange(player_id, match_date) %>%
  group_by(player_id) %>%
  mutate(rolling_avg_np_xg_op_xa = ifelse(row_number() > 1, 
  rollapply(data = lag(np_xg_op_xa),  
  width = pmax(1, pmin(row_number() - 1, 25)),  
  FUN = mean,
  align = "right", 
  fill = NA), NA)) %>%ungroup()
# tried mean basic average rather than linear for FUN function
# FUN = function(x) sum(x * seq_along(x)) / sum(seq_along(x)),

# create rolling average for opposition def strength (goals allowed)
# match level data
match_level_data <- data %>% 
  select(opposition_team_id, match_id, match_date, opposition_goals_allowed) %>%
  distinct() %>%
  arrange(opposition_team_id, match_date)

# compute rolling average at the match level
match_level_data <- match_level_data %>% group_by(opposition_team_id) %>%
    mutate(rolling_avg_opp_goals_allowed = rollapply(
    data = lag(opposition_goals_allowed), 
    width = pmax(1, pmin(row_number() - 1, 15)), 
    FUN = function(x) sum(x * seq_along(x)) / sum(seq_along(x)),
    align = "right", 
    fill = NA,
    partial = TRUE)) %>% ungroup()

# merge back to main dataset
data <- data %>%
  left_join(match_level_data %>% select(opposition_team_id, match_id, rolling_avg_opp_goals_allowed), 
            by = c("opposition_team_id", "match_id"))




# create rolling average for opposition def strenth v2 (xg allowed)
# create match-level np_xg conceded (i.e., np_xg allowed by opposition)
match_np_xg_allowed <- data %>% group_by(match_id, opposition_team_id) %>%
  summarise(opposition_np_xg_allowed = sum(player_match_np_xg, na.rm = TRUE), .groups = "drop")

# join the np_xg allowed back to main dataset
data <- data %>% left_join(match_np_xg_allowed, by = c("match_id", "opposition_team_id"))

# create match-level data for rolling average
match_level_data <- data %>%
  select(opposition_team_id, match_id, match_date, opposition_np_xg_allowed) %>%
  distinct() %>% arrange(opposition_team_id, match_date)

# compute rolling average at match level 
match_level_data <- match_level_data %>%
  group_by(opposition_team_id) %>%
  mutate(rolling_avg_opp_np_xg_allowed = rollapply(
    data = lag(opposition_np_xg_allowed),
    width = pmax(1, pmin(row_number() - 1, 15)),  
    FUN = function(x) sum(x * seq_along(x)) / sum(seq_along(x)),
    align = "right",
    fill = NA,
    partial = TRUE)) %>% ungroup()

# join rolling avg np_xg allowed back to main data
data <- data %>% left_join(match_level_data %>%
              select(opposition_team_id, match_id, rolling_avg_opp_np_xg_allowed),
              by = c("opposition_team_id", "match_id"))


# create rolling average for team offensive strength (xg)
match_team_np_xg <- data %>% group_by(match_id, team_id) %>%
  summarise(team_np_xg = sum(player_match_np_xg, na.rm = TRUE), .groups = "drop")

data <- data %>% left_join(match_team_np_xg, by = c("match_id", "team_id"))

match_level_team_data <- data %>%
  select(team_id, match_id, match_date, team_np_xg) %>%
  distinct() %>% arrange(team_id, match_date)

match_level_team_data <- match_level_team_data %>%
  group_by(team_id) %>%
  mutate(rolling_avg_team_np_xg = rollapply(
    data = lag(team_np_xg),
    width = pmax(1, pmin(row_number() - 1, 15)),
    FUN = function(x) sum(x * seq_along(x)) / sum(seq_along(x)),
    align = "right",
    fill = NA,
    partial = TRUE)) %>% ungroup()

data <- data %>% left_join(match_level_team_data %>%
              select(team_id, match_id, rolling_avg_team_np_xg),
              by = c("team_id", "match_id"))


# create rolling weighted average of player_match_minutes for each player
data <- data %>% arrange(player_id, match_date) %>%
      group_by(player_id) %>%
      mutate(rolling_avg_mins_played = rollapply(
      data = lag(player_match_minutes), 
      width = pmax(1, pmin(row_number() - 1, 15)), 
      FUN = function(x) sum(x * seq_along(x)) / sum(seq_along(x)), 
      align = "right", 
      fill = NA)) %>% ungroup()


# create rolling weighted average of player_match_obv
data <- data %>% arrange(player_id, match_date) %>%
  group_by(player_id) %>%
  mutate(rolling_avg_obv = rollapply(
    data = lag(player_match_obv), 
    width = pmax(1, pmin(row_number() - 1, 20)), 
    FUN = function(x) sum(x * seq_along(x)) / sum(seq_along(x)), 
    align = "right", 
    fill = NA)) %>% ungroup()
# this predictor did not end up being stat significant with the mix of predictors


# normalize predictor variables 
data <- data %>%
    mutate(rolling_avg_np_xg_op_xa = scale(rolling_avg_np_xg_op_xa, center = TRUE, scale = TRUE)[,1],
    rolling_avg_opp_np_xg_allowed = scale(rolling_avg_opp_np_xg_allowed, center = TRUE, scale = TRUE)[,1],
    rolling_avg_team_np_xg = scale(rolling_avg_team_np_xg, center = TRUE, scale = TRUE)[,1],
    rolling_avg_obv = scale(rolling_avg_obv, center = TRUE, scale = TRUE)[,1],
    rolling_avg_mins_played = scale(rolling_avg_mins_played, center = TRUE, scale = TRUE)[,1])

# split data into train and test (test will be this weekend matches)
test_data <- data %>% filter(match_date %in% c("2025-03-22", "2025-03-23"))
train_data <- data %>% filter(!match_date %in% as.Date(c("2025-03-22", "2025-03-23")))

# distribution of non pen xg and open play xa (very skewed)
ggplot(data, aes(x = np_xg_op_xa)) +
  geom_histogram(binwidth = 0.5, fill = "#69b3a2", color = "black") +
  labs(title = "Distribution of Value", x = "Value", y = "Count") +
  theme_minimal()

# linear regression model #1
lrm1 <- lm(np_xg_op_xa ~ is_player_home + rolling_avg_np_xg_op_xa + rolling_avg_team_np_xg +
             rolling_avg_opp_np_xg_allowed + rolling_avg_mins_played, data = train_data)
summary(lrm1)

# make predictions on the test set
test_data <- test_data %>% mutate(predicted_np_xg_op_xa = predict(lrm1, newdata = test_data))

# final predictions
predictions <- test_data %>% select(player_name, team_name, predicted_np_xg_op_xa)









### Project 2: Recruitment Analysis - Top Attackers ### 

# import data
event_data <- readRDS("ATL_eventdata.rds")
schedule <- read.csv('ATL_schedule.csv')
player_mins_apps <- read.csv('ATL_player_mins_apps.csv')

# check event types 
event_types <- unique(event_data$type.name)
event_types

# column names
col_names <- colnames(event_data)
col_names

# for attackers, we will focus on the following metrics: 
#                non pen goals for 'goal scoring'
#                non pen xG for 'goal scoring'
#                xA for 'chance creation'
#                passes into/in the box for 'passing'
#                passes into/in the final third for 'passing'  
#                successful dribbles for 'dribbling'
#                receptions in the box for 'off ball movement'
#                retention (succ. actions / total actions) for 'retention'
#                pressures for 'defensive activity' 

# first, get each player's most common position
# since there are so few minutes, we cannot filter by position on the event level
most_common_pos <- event_data %>%
  group_by(player.id, position.name) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(player.id) %>%
  slice_max(n, with_ties = FALSE) %>%
  select(player.id, most_com_pos = position.name)

# create xG metric 
np_xG <- event_data %>%
  filter(type.name == "Shot") %>% filter(shot.type.name!="Penalty" | is.na(shot.type.name)) %>% 
  group_by(player.id) %>% summarise(np_xG = sum(shot.statsbomb_xg, na.rm = TRUE))

# create non pen goals metric 
np_goals <- event_data %>% 
  filter(type.name == "Shot") %>% filter(shot.outcome.name == "Goal") %>% 
  group_by(player.id) %>% summarise(np_goals = n(), .groups = "drop")

# create xA metric 
xA <- event_data %>%
  filter(type.name=="Shot") %>% select(shot.key_pass_id, xA = shot.statsbomb_xg) 
  
shot_assists <- left_join(event_data, xA, by = c("id" = "shot.key_pass_id")) %>% 
  select(player.id, type.name, pass.shot_assist, pass.goal_assist, xA) %>% 
  filter(pass.shot_assist==TRUE | pass.goal_assist==TRUE)

xA <- shot_assists %>% group_by(player.id) %>% summarise(xA = sum(xA, na.rm = TRUE))

# create passes in/into the box metric 
box_passes <- event_data %>%
  filter(type.name == "Pass" & is.na(pass.outcome.name)) %>%
  filter(pass.end_location.x >= 102 & pass.end_location.y <= 62 & 
           pass.end_location.y >= 18) %>%
  group_by(player.id) %>%
  summarise(passes_completed_box = n(), .groups = "drop")

# create passes in/into the final third 
f3_passes <- event_data %>%
  filter(type.name == "Pass" & is.na(pass.outcome.name)) %>%
  filter(pass.end_location.x >= 80) %>%
  group_by(player.id) %>%
  summarise(passes_completed_f3 = n(), .groups = "drop")

# create successful dribbles/take ons metric 
succ_drib <- event_data %>% 
  filter(type.name == "Dribble") %>% filter(dribble.outcome.name == "Complete") %>% 
  group_by(player.id) %>% summarise(succ_drib = n(), .groups = "drop")

# create box receptions metric
box_rec <- event_data %>% 
  filter(type.name == "Ball Receipt*" & is.na(ball_receipt.outcome.name)) %>% 
  filter(location.x >= 102 & location.y <= 62 & location.y >= 18) %>%
  group_by(player.id) %>% summarise(box_rec = n(), .groups = "drop")

# create retention metric (already have succ drib)
total_passes <- event_data %>% filter(type.name == "Pass") %>% 
  group_by(player.id) %>% summarise(total_passes = n(), .groups = "drop")

total_drib <- event_data %>% filter(type.name == "Dribble") %>% 
  group_by(player.id) %>% summarise(total_drib = n(), .groups = "drop")

succ_passes <- event_data %>% filter(type.name == "Pass" & is.na(pass.outcome.name)) %>% 
  group_by(player.id) %>% summarise(succ_passes = n(), .groups = "drop")

retention <- total_passes %>% full_join(total_drib, by = "player.id") %>%
  full_join(succ_passes, by = "player.id") %>% full_join(succ_drib, by = "player.id")

retention <- retention %>% mutate(across(everything(), ~replace_na(.x, 0)))

retention$retention <- (retention$succ_passes+retention$succ_drib)/(retention$total_passes+retention$total_drib)

retention <- retention %>% select(player.id, retention)

# create pressures metric 
pressures <- event_data %>% filter(type.name == "Pressure") %>% 
  group_by(player.id) %>% summarise(pressures = n(), .groups = "drop")


# now join all these metric df's into one
player_data <- np_xG %>% full_join(np_goals, by = "player.id") %>%
  full_join(xA, by = "player.id") %>% full_join(box_passes, by = "player.id") %>%
  full_join(f3_passes, by = "player.id") %>% full_join(succ_drib, by = "player.id") %>%
  full_join(box_rec, by = "player.id") %>% full_join(retention, by = "player.id") %>%
  full_join(pressures, by = "player.id") %>% full_join(most_common_pos, by = "player.id")

player_data <- player_data %>% rename(player_id = player.id)

player_data <- player_data %>%
  left_join(player_mins_apps %>% select(player_id, player_season_minutes), by = "player_id")

# now we need to make our metrics per 90 mins (but for retention)
player_data <- player_data %>% mutate(across(-most_com_pos, ~replace_na(.x, 0)))
player_data$nineties_played <- player_data$player_season_minutes/90
player_data$np_xG <- player_data$np_xG/player_data$nineties_played
player_data$np_goals <- player_data$np_goals/player_data$nineties_played
player_data$xA <- player_data$xA/player_data$nineties_played
player_data$passes_completed_box <- player_data$passes_completed_box/player_data$nineties_played
player_data$passes_completed_f3 <- player_data$passes_completed_f3/player_data$nineties_played
player_data$succ_drib <- player_data$succ_drib/player_data$nineties_played
player_data$box_rec <- player_data$box_rec/player_data$nineties_played
player_data$pressures <- player_data$pressures/player_data$nineties_played
player_data <- player_data %>% mutate(across(everything(), ~replace(.x, is.na(.x) | is.infinite(.x), 0)))

# remove players with less than 300 minutes
# i would have liked higher minute cutoff, but that hurts the comparison sample too much
player_data <- player_data %>% filter(player_season_minutes >= 300)

positions <- unique(player_data$most_com_pos)
positions

# need to filter for attackers now
player_data <- player_data %>% filter(most_com_pos %in% c("Right Wing", "Left Wing", "Left Attacking Midfield",
                                                 "Right Center Forward", "Center Forward",
                                                 "Left Center Forward", "Center Attacking Midfield"))
# we are left with close to 100 players

# normalize/scale metrics
player_data$np_xG <- scale(player_data$np_xG)
player_data$np_goals <- scale(player_data$np_goals)
player_data$xA <- scale(player_data$xA)
player_data$passes_completed_box <- scale(player_data$passes_completed_box)
player_data$passes_completed_f3 <- scale(player_data$passes_completed_f3)
player_data$succ_drib <- scale(player_data$succ_drib)
player_data$box_rec <- scale(player_data$box_rec)
player_data$retention <- scale(player_data$retention)
player_data$pressures <- scale(player_data$pressures)

# now lets create a quality score for all our attackers 
player_data <- player_data %>%
  mutate(quality_score = 
           (np_xG * 0.3) +
           (xA * 0.3) +
           (passes_completed_box * 0.1) +
           (succ_drib * 0.16) +
           (box_rec * 0.1) +
           (retention * 0.04))


# now turn the perf metrics into percentiles 
player_data <- player_data %>% mutate(np_xG = percent_rank(np_xG))
player_data <- player_data %>% mutate(np_goals = percent_rank(np_goals))
player_data <- player_data %>% mutate(xA = percent_rank(xA))
player_data <- player_data %>% mutate(passes_completed_box = percent_rank(passes_completed_box))
player_data <- player_data %>% mutate(passes_completed_f3 = percent_rank(passes_completed_f3))
player_data <- player_data %>% mutate(succ_drib = percent_rank(succ_drib))
player_data <- player_data %>% mutate(box_rec = percent_rank(box_rec))
player_data <- player_data %>% mutate(retention = percent_rank(retention))
player_data <- player_data %>% mutate(pressures = percent_rank(pressures))


# change variable names for visualizations
player_data <- player_data %>% rename(`Non Pen xG` = np_xG)
player_data <- player_data %>% rename(`Non Pen Goals` = np_goals)
player_data <- player_data %>% rename(`Comp. Box Passes` = passes_completed_box)
player_data <- player_data %>% rename(`Comp. F3 Passes` = passes_completed_f3)
player_data <- player_data %>% rename(`Succ. Dribbles` = succ_drib)
player_data <- player_data %>% rename(`Box Receptions` = box_rec)
player_data <- player_data %>% rename(`Retention` = retention)
player_data <- player_data %>% rename(`Pressures` = pressures)


# create radars for some players
# just change player id and rest can stay the same 
player_radar <- player_data %>% filter(player_id == 5207) %>% select(2:10)

# RADAR
desired_order <- c("Non Pen xG", "Pressures", "Retention", "Box Receptions", "Succ. Dribbles", 
                   "Comp. F3 Passes", "Comp. Box Passes", "xA", "Non Pen Goals")
player_radar <- player_radar[, desired_order]


# create rows for max and min values
max_row <- player_radar %>% summarise(across(everything(), ~1))
min_row <- player_radar %>% summarise(across(everything(), ~0))

# bind the rows to the top of the player data
player_radar <- bind_rows(max_row, min_row, player_radar)

# radar code
# open a new plot window
par(mar = c(1, 1, 5, 1)) # Adjust margins to make space for the title and subtitle
# create radar chart
radarchart(player_radar, axistype = 4,
           # Customize the radar
           pcol = "darkred", pfcol = "#FF999980", plwd = 4, plty = 1,
           # Customize the grid
           cglcol = "darkgrey", cglty = 1, axislabcol = "darkgrey", cglwd = 0.8,
           # Customize labels
           vlcex = 1)
# add a title 
title(main = "Player 5207 Radar", cex.main = 1.5)
# add a subtitle closer to the main title
mtext("Compared to Attackers | 557 Minutes Played", side = 3, line = 0.5, cex = 1.2)


# MAPS
# Plotting Passes into Box
boxpassvizsetup <- event_data %>%
  filter(type.name == "Pass" & is.na(pass.outcome.name) &
           player.id == 3535) %>% 
  filter(pass.end_location.x >= 102 & pass.end_location.y <= 62 &
           pass.end_location.y >= 18) 

boxpass_viz <- create_Pitch() +
  geom_segment(data = boxpassvizsetup, aes(x = location.x, y = location.y,
                                  xend = pass.end_location.x, yend = pass.end_location.y), 
               lineend = "round", linewidth = 0.5, arrow =
                 arrow(length = unit(0.07, "inches"), ends = "last", type = "open")) + 
  labs(title = "", subtitle = "Player 3535, Completed Passes to the Box") + 
  scale_y_reverse() + 
  coord_fixed(ratio = 105/100) #
boxpass_viz

# Plotting Carries
carries <- event_data %>%
  filter(type.name=="Carry" & player.id == 5207) 

carries <- carries %>%
  mutate(carrylength=sqrt((location.x-carry.end_location.x)^2 + (location.y-carry.end_location.y)^2))

carries <- carries %>% 
  filter(carrylength>=10)

carrie_viz <- create_Pitch() +
  geom_segment(data = carries, aes(x = location.x, y = location.y,
                                   xend = carry.end_location.x, yend = carry.end_location.y), 
               lineend = "round", linewidth = 0.5, colour = "#000000", arrow =
                 arrow(length = unit(0.07, "inches"), ends = "last", type = "open")) + #3 
  labs(title = "", subtitle = "Player 5207 Progressive Carries") + 
  scale_y_reverse() + 
  coord_fixed(ratio = 105/100) #6
carrie_viz

# plot dribbles or receptions
dribviz <- event_data %>% filter(type.name == "Ball Receipt*" & is.na(ball_receipt.outcome.name) & player.id == 5207) 
  
create_Pitch() +
  geom_point(data = dribviz, aes(x = location.x, y = location.y), colour = "#000000") +
  labs(title = "", subtitle = "Player 5207 Receptions") +
  scale_y_reverse() +
  coord_fixed(ratio = 105/100)



# xG_Shot Map
shots <- event_data %>%
  filter(type.name=="Shot" & (shot.type.name!="Penalty" | is.na(shot.type.name)) & player.id == 5207) 

shotmapxgcolors <- c("#192780", "#2a5d9f", "#40a7d0", "#87cdcf", "#e7f8e6", "#f4ef95", "#FDE960", 
                     "#FCDC5F", "#F5B94D", "#F0983E", "#ED8A37", "#E66424", "#D54F1B", "#DC2608", 
                     "#BF0000", "#7F0000", "#5F0000") #2

Shots <- ggplot() +
  annotate("rect",xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) + 
  annotate("rect",xmin = 0, xmax = 60, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) + 
  annotate("rect",xmin = 18, xmax = 0, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) + 
  annotate("rect",xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) + 
  annotate("rect",xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) + 
  annotate("rect",xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) + 
  annotate("rect",xmin = 120, xmax = 120.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) + 
  annotate("rect",xmin = 0, xmax = -0.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) + 
  annotate("segment", x = 60, xend = 60, y = -0.5, yend = 80.5, colour = "black", size = 0.6)+ 
  annotate("segment", x = 0, xend = 0, y = 0, yend = 80, colour = "black", size = 0.6)+ 
  annotate("segment", x = 120, xend = 120, y = 0, yend = 80, colour = "black", size = 0.6)+
  theme(rect = element_blank(),
        line = element_blank()) +
  # add penalty spot right
  annotate("point", x = 108 , y = 40, colour = "black", size = 1.05) + 
  annotate("path", colour = "black", size = 0.6,
           x=60+10*cos(seq(0,2*pi,length.out=2000)),
           y=40+10*sin(seq(0,2*pi,length.out=2000)))+ 
  # add centre spot
  annotate("point", x = 60 , y = 40, colour = "black", size = 1.05) + 
  annotate("path", x=12+10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=40+10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") + 
  annotate("path", x=107.84-10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=40-10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") +
  geom_point(data = shots, aes(x = location.x, y = location.y, fill = shot.statsbomb_xg, colour = shot.outcome.name, shape = shot.body_part.name),
             size = 6, alpha = 0.8) + #3   
  theme(axis.text.x=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.caption=element_text(size=10,family="Calibri", hjust=0.5, vjust=0.5), 
        plot.subtitle = element_text(size = 15, family="Calibri", hjust = 0.5), 
        axis.text.y=element_blank(),
        legend.position = "top",
        legend.title=element_text(size=15,family="Calibri"), 
        legend.text=element_text(size=13,family="Calibri"),
        legend.margin = margin(c(20, 10, -85, 50)),
        legend.key.size = unit(1.5, "cm"),
        plot.title = element_text(margin = margin(r = 10, b = 10), face="bold",size = 18, family="Calibri", colour = "black", hjust = 0.5),
        legend.direction = "horizontal",
        axis.ticks=element_blank(),
        aspect.ratio = c(65/100),
        plot.background = element_rect(fill = "white"),
        strip.text.x = element_text(size=10,family="Calibri")) +
  labs(title = "", subtitle = "Player 5207 Shot Map") + #4
  scale_fill_gradientn(colours = shotmapxgcolors, limit = c(0,.75), oob=scales::squish, name = "Expected Goals Value") + #5
  scale_shape_manual(values = c("Head" = 21, "Right Foot" = 23, "Left Foot" = 24), name ="") +
  scale_colour_manual(values = c("Goal"="red"), name ="") +#6 
  guides(fill = guide_colourbar(title.position = "top"),
         shape = guide_legend(override.aes = list(size = 2, fill = "black"))) + #7
  coord_flip(xlim = c(85, 125)) #8
Shots





