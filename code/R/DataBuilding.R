library(nflverse)
library(tidyverse)
library(nflfastR)
library(ggthemes)
library(ranger)
library(vip)
library(caret)
library(xgboost)
library(randomForest)
library(DescTools)
library(pracma)
library(dplyr)

games <- read.csv("~/Downloads/games.csv")
players <- read.csv("~/Downloads/players.csv")
tackles <- read.csv("~/Downloads/tackles.csv")
tracking_week_6 <- read.csv("~/Downloads/tracking_week_6.csv") |> 
  rename(x=y, y=x)
plays <- read.csv("~/Downloads/plays.csv")




tracking_week_6 = left_join(tracking_week_6, plays)

pbp <- load_pbp(2022)

use_pbp <- pbp |>
  select(play_id, old_game_id, yardline_100, pass_location, air_yards, pass_length, yards_after_catch, yac_epa, xyac_epa, xyac_mean_yardage)
use_pbp$old_game_id = as.numeric(as.character(use_pbp$old_game_id))

ftn_data <- load_ftn_charting(seasons = 2022) |> 
  select(nflverse_game_id, nflverse_play_id, is_screen_pass)
ftn_data <- ftn_data |> 
  mutate(is_screen_pass= ifelse(ftn_data$is_screen_pass == TRUE, 1, 0))

ftn_data = left_join(ftn_data,pbp, join_by(nflverse_game_id == game_id, nflverse_play_id == play_id)) 

ftn_data <- ftn_data |> 
  select(game_id = old_game_id, play_id = nflverse_play_id, is_screen_pass)
ftn_data$game_id = as.numeric(as.character(ftn_data$game_id))

pass_play_tracking <- tracking_week_6|>
  filter(passResult == "C") |> 
  filter(displayName != "football") |> 
  filter(playNullifiedByPenalty == "N") |> 
  select(-playNullifiedByPenalty, -foulName1, -penaltyYards, -foulName2, -foulNFLId1, -foulNFLId2,-prePenaltyPlayResult, -time, -preSnapHomeScore, -preSnapVisitorScore)

pass_play_tracking = left_join(pass_play_tracking, ftn_data, join_by(gameId == game_id, playId == play_id))

pass_play_tracking = left_join(pass_play_tracking, use_pbp, join_by(gameId == old_game_id, playId == play_id))

footballTrackingData <- tracking_week_6 |>
  filter(passResult == "C") |> 
  filter(displayName == "football") |> 
  filter(playNullifiedByPenalty == "N") |> 
  select(gameId, playId, ball_displayName = displayName, frameId, ball_x = x, ball_y = y, ball_s = s, ball_a = a, ball_dis = dis)

pass_play_tracking = left_join(pass_play_tracking, footballTrackingData, join_by(gameId, playId, frameId))

pass_play_tracking <- pass_play_tracking |> 
  filter(is_screen_pass == "0") |> 
  select(-is_screen_pass) |> 
  mutate(h_dist_from_ball = abs(ball_x - x)) |> 
  mutate(v_dist_from_ball = abs(ball_y - y)) |> 
  mutate(total_dist_from_ball = (h_dist_from_ball^2 + v_dist_from_ball^2)^.5) |> 
  mutate(dir = DegToRad(dir)) |> 
  mutate(o = DegToRad(o)) |> 
  mutate(hcomp = cos(dir), vcomp = sin(dir)) |> 
  mutate(h_speed = hcomp*s, v_speed = vcomp*s)


rallyDataForWeek6 <- pass_play_tracking |> 
  filter(frameId == 16) |> 
  filter(club == defensiveTeam | nflId == ballCarrierId)

#write.csv(rallyDataForWeek6, "F16rallyDataWeek6.csv")

ftn_dataZ <- load_ftn_charting(seasons = 2022) |> 
  select(nflverse_game_id, nflverse_play_id, is_screen_pass)
ftn_dataZ <- ftn_dataZ |> 
  mutate(is_screen_pass= ifelse(ftn_data$is_screen_pass == TRUE, 1, 0))

ftn_dataZ = left_join(ftn_dataZ,pbp, join_by(nflverse_game_id == game_id, nflverse_play_id == play_id))|> 
  select(game_id=old_game_id, play_id=nflverse_play_id, is_screen_pass)

ftn_dataZ$game_id = as.numeric(as.character(ftn_dataZ$game_id))

plays = left_join(plays, ftn_dataZ,join_by(gameId == game_id, playId == play_id))

playsWeek6 <- plays |> 
  filter(passResult == "C") |> 
  filter(playNullifiedByPenalty == "N") |> 
  filter(is_screen_pass == 0) |> 
  filter(gameId > 2022101200) |> 
  filter(gameId < 2022101900) |> 
  select(-playNullifiedByPenalty, -foulName1, -penaltyYards, -foulName2, -foulNFLId1, -foulNFLId2,-prePenaltyPlayResult) 

playsWeek6 <- arrange(playsWeek6, gameId, playId)


#write.csv(playsWeek6, "playsWeek6.csv") 


tackle_data_w6 <- tracking_week_6 |>
  filter(event == "tackle"| event == "out_of_bounds" | event == "touchdown" | event == "fumble_defense_recovered" ) |> 
  filter(nflId == ballCarrierId) |> 
  select(gameId, playId, frameId)

playsWeek6 = left_join(playsWeek6, tackle_data_w6, join_by(gameId, playId))|> 
  rename(tackle_frame = frameId)

playsWeek6  = playsWeek6 |> 
  filter(tackle_frame > 13)


#write.csv(playsWeek6, "playsWeek6(tackle14+).csv") 










outputWeek1 <- read.csv("~/Downloads/outputWeek1.csv") |> 
  select(-X, -Unnamed..0) |> 
  filter(preSnapHomeTeamWinProbability < 0.95 & preSnapHomeTeamWinProbability > 0.05) 

outputWeek2 <- read.csv("~/Downloads/outputWeek2.csv") |> 
  select(-X, -Unnamed..0) |> 
  filter(preSnapHomeTeamWinProbability < 0.95 & preSnapHomeTeamWinProbability > 0.05) 

outputWeek3 <- read.csv("~/Downloads/outputWeek3.csv") |> 
  select(-X, -Unnamed..0) |> 
  filter(preSnapHomeTeamWinProbability < 0.95 & preSnapHomeTeamWinProbability > 0.05) 

outputWeek4 <- read.csv("~/Downloads/outputWeek4.csv") |> 
  select(-X, -Unnamed..0) |> 
  filter(preSnapHomeTeamWinProbability < 0.95 & preSnapHomeTeamWinProbability > 0.05) 

outputWeek5 <- read.csv("~/Downloads/outputWeek5.csv") |> 
  select(-X, -Unnamed..0) |> 
  filter(preSnapHomeTeamWinProbability < 0.95 & preSnapHomeTeamWinProbability > 0.05) 

outputWeek6 <- read.csv("~/Downloads/outputWeek6.csv") |> 
  select(-X, -Unnamed..0) |> 
  filter(preSnapHomeTeamWinProbability < 0.95 & preSnapHomeTeamWinProbability > 0.05) 

outputWeek7 <- read.csv("~/Downloads/outputWeek7.csv") |> 
  select(-X, -Unnamed..0) |> 
  filter(preSnapHomeTeamWinProbability < 0.95 & preSnapHomeTeamWinProbability > 0.05) 

outputWeek8 <- read.csv("~/Downloads/outputWeek8.csv") |> 
  select(-X, -Unnamed..0) |> 
  filter(preSnapHomeTeamWinProbability < 0.95 & preSnapHomeTeamWinProbability > 0.05) 

outputWeek9 <- read.csv("~/Downloads/outputWeek9.csv") |> 
  select(-X, -Unnamed..0) |> 
  filter(preSnapHomeTeamWinProbability < 0.95 & preSnapHomeTeamWinProbability > 0.05) 

outputWeek1F14on <- read.csv("~/Downloads/outputWeek1F14on.csv") |> 
  select(-X, -Unnamed..0) |> 
  filter(preSnapHomeTeamWinProbability < 0.95 & preSnapHomeTeamWinProbability > 0.05) 

outputWeek2F14on <- read.csv("~/Downloads/outputWeek2F14on.csv") |> 
  select(-X, -Unnamed..0) |> 
  filter(preSnapHomeTeamWinProbability < 0.95 & preSnapHomeTeamWinProbability > 0.05) 

outputWeek3F14on <- read.csv("~/Downloads/outputWeek3F14on.csv") |> 
  select(-X, -Unnamed..0) |> 
  filter(preSnapHomeTeamWinProbability < 0.95 & preSnapHomeTeamWinProbability > 0.05) 

outputWeek4F14on <- read.csv("~/Downloads/outputWeek4F14on.csv") |> 
  select(-X, -Unnamed..0) |> 
  filter(preSnapHomeTeamWinProbability < 0.95 & preSnapHomeTeamWinProbability > 0.05) 

outputWeek5F14on <- read.csv("~/Downloads/outputWeek5F14on.csv") |> 
  select(-X, -Unnamed..0) |> 
  filter(preSnapHomeTeamWinProbability < 0.95 & preSnapHomeTeamWinProbability > 0.05) 

outputWeek6F14on <- read.csv("~/Downloads/outputWeek6F14on.csv") |> 
  select(-X, -Unnamed..0) |> 
  filter(preSnapHomeTeamWinProbability < 0.95 & preSnapHomeTeamWinProbability > 0.05) 

outputWeek7F14on <- read.csv("~/Downloads/outputWeek7F14on.csv") |> 
  select(-X, -Unnamed..0) |> 
  filter(preSnapHomeTeamWinProbability < 0.95 & preSnapHomeTeamWinProbability > 0.05) 

outputWeek8F14on <- read.csv("~/Downloads/outputWeek8F14on.csv") |> 
  select(-X, -Unnamed..0) |> 
  filter(preSnapHomeTeamWinProbability < 0.95 & preSnapHomeTeamWinProbability > 0.05) 


outputWeek9F14on <- read.csv("~/Downloads/outputWeek9F14on.csv") |> 
  select(-X, -Unnamed..0) |> 
  filter(preSnapHomeTeamWinProbability < 0.95 & preSnapHomeTeamWinProbability > 0.05) 


totalOutputF16 <- rbind(outputWeek1,outputWeek2,outputWeek3,outputWeek4,outputWeek5,outputWeek6,outputWeek7,outputWeek8,outputWeek9)

totalOutputF6 <- left_join(totalOutputF6, use_pbp, join_by(gameId == old_game_id, playId == play_id))

totalOutputF16 <- rbind(outputWeek1F14on,outputWeek2F14on,outputWeek3F14on,outputWeek4F14on,outputWeek5F14on,outputWeek6F14on,outputWeek7F14on,outputWeek8F14on,outputWeek9F14on)

totalOutputF16 <- left_join(totalOutputF16, use_pbp, join_by(gameId == old_game_id, playId == play_id))


tracking_week_9 |> 
  count(event)


which(is.na(totalOutputF16), arr.ind=TRUE)


outputWeek9= outputWeek9 |> 
  select(gameId, playId, rally)

totalOutputF6 = rbind(outputWeek1, outputWeek2, outputWeek3, outputWeek4, outputWeek5,
                      outputWeek6, outputWeek7, outputWeek8, outputWeek9)

#write.csv(totalOutputF6, "totalOutputF6.csv")

totalOutputF16 <- totalOutputF16 |> 
  rename(rally1sec = F16rally)

#write.csv(totalOutputF16, "totalOutputF16.csv")

pbp1018 <- pbp |>
  select(old_game_id, play_id, play_type_nfl, defteam, week, complete_pass, wp, yac_allowed = yards_after_catch, yac_epa_allowed = yac_epa, xyac_epa_allowed = xyac_epa, xyac_mean_yardage_allowed = xyac_mean_yardage) |> 
  filter(10 < week) |> 
  filter(week < 18) |> 
  filter(complete_pass == 1) |> 
  filter(play_type_nfl != "PENALTY") |> 
  filter(wp < .95) |> 
  filter(wp > .05)

pbp1018$old_game_id = as.numeric(as.character(pbp1018$old_game_id))

pbp1018 = left_join(pbp1018,ftn_data, join_by(old_game_id == game_id, play_id == play_id)) 

pbp1018 <- pbp1018 |> 
  filter(is_screen_pass == 0)

write.csv(pbp1018, "pbp1018.csv")
