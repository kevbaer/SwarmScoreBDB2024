#importing libraries  
library(nflverse)  
library(tidyverse)
library(nflfastR)
library(ggthemes)
library(ranger)
library(DescTools)
library(pracma)
library(dplyr)

#importing data, rearranging variables so that y is the vertical direction and x is the horizontal
games <- read.csv("~/Downloads/games.csv")
players <- read.csv("~/Downloads/players.csv")
tackles <- read.csv("~/Downloads/tackles.csv")
tracking_week_6 <- read.csv("~/Downloads/tracking_week_6.csv") |> 
  rename(x=y, y=x)
plays <- read.csv("~/Downloads/plays.csv")


#this is set up for week 6 as that was the last one I built to send to python

tracking_week_6 = left_join(tracking_week_6, plays)

#loading play-by-play courtesy of NflFastR
pbp <- load_pbp(2022)

use_pbp <- pbp |>
  select(play_id, old_game_id, yardline_100, pass_location, air_yards, pass_length, yards_after_catch, yac_epa, xyac_epa, xyac_mean_yardage)
use_pbp$old_game_id = as.numeric(as.character(use_pbp$old_game_id))

#loading ftn's free sample charting data to remove all screen passes
ftn_data <- load_ftn_charting(seasons = 2022) |> 
  select(nflverse_game_id, nflverse_play_id, is_screen_pass)
ftn_data <- ftn_data |> 
  mutate(is_screen_pass= ifelse(ftn_data$is_screen_pass == TRUE, 1, 0))

ftn_data = left_join(ftn_data,pbp, join_by(nflverse_game_id == game_id, nflverse_play_id == play_id)) 

ftn_data <- ftn_data |> 
  select(game_id = old_game_id, play_id = nflverse_play_id, is_screen_pass)
ftn_data$game_id = as.numeric(as.character(ftn_data$game_id))

#this becomes the narrowed down play_by_play with only the information necessary to be plugged into python
pass_play_tracking <- tracking_week_6|>
  filter(passResult == "C") |> 
  filter(displayName != "football") |> 
  filter(playNullifiedByPenalty == "N") |> 
  select(-playNullifiedByPenalty, -foulName1, -penaltyYards, -foulName2, -foulNFLId1, -foulNFLId2,-prePenaltyPlayResult, -time, -preSnapHomeScore, -preSnapVisitorScore)

pass_play_tracking = left_join(pass_play_tracking, ftn_data, join_by(gameId == game_id, playId == play_id))

pass_play_tracking = left_join(pass_play_tracking, use_pbp, join_by(gameId == old_game_id, playId == play_id))

#this holds all the football tracking data separately from the player data to rename and add to every player's individual row
footballTrackingData <- tracking_week_6 |>
  filter(passResult == "C") |> 
  filter(displayName == "football") |> 
  filter(playNullifiedByPenalty == "N") |> 
  select(gameId, playId, ball_displayName = displayName, frameId, ball_x = x, ball_y = y, ball_s = s, ball_a = a, ball_dis = dis)

#adding the football location/data to all rows of pass_play_tracking
pass_play_tracking = left_join(pass_play_tracking, footballTrackingData, join_by(gameId, playId, frameId))

#creating some of the unique variables required to carry out the necessary code
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

#this is set-up for frame 16, 1 second after the ball is caught. All I care about is the 11 defenders and the ball-carrier
rallyDataForWeek6 <- pass_play_tracking |> 
  filter(frameId == 16) |> 
  filter(club == defensiveTeam | nflId == ballCarrierId)

#this is how I wrote csv files to then upload into Google Colab and run the python script
write.csv(rallyDataForWeek6, "F16rallyDataWeek6.csv")

#I used the ftn_dataZ label for the addition to the plays folder
ftn_dataZ <- load_ftn_charting(seasons = 2022) |> 
  select(nflverse_game_id, nflverse_play_id, is_screen_pass)
ftn_dataZ <- ftn_dataZ |> 
  mutate(is_screen_pass= ifelse(ftn_dataZ$is_screen_pass == TRUE, 1, 0))

ftn_dataZ = left_join(ftn_dataZ,pbp, join_by(nflverse_game_id == game_id, nflverse_play_id == play_id))|> 
  select(game_id=old_game_id, play_id=nflverse_play_id, is_screen_pass)

ftn_dataZ$game_id = as.numeric(as.character(ftn_dataZ$game_id))

#creating the final plays dataset
plays = left_join(plays, ftn_dataZ,join_by(gameId == game_id, playId == play_id))

#filtering by gameId and date to ensure that only the games from the correct week are being filter through
playsWeek6 <- plays |> 
  filter(passResult == "C") |> 
  filter(playNullifiedByPenalty == "N") |> 
  filter(is_screen_pass == 0) |> 
  filter(gameId > 2022101200) |> 
  filter(gameId < 2022101900) |> 
  select(-playNullifiedByPenalty, -foulName1, -penaltyYards, -foulName2, -foulNFLId1, -foulNFLId2,-prePenaltyPlayResult) 

playsWeek6 <- arrange(playsWeek6, gameId, playId)

#writing the CSV file for upload to python
write.csv(playsWeek6, "playsWeek6.csv") 

#this data adds the tackle frame information to the plays for use in the 1 sec after catch calculations
#as it doesn't make sense to track Swarm Score if the tackle has already been made, and some plays don't
#even have data that long after
tackle_data_w6 <- tracking_week_6 |>
  filter(event == "tackle"| event == "out_of_bounds" | event == "touchdown" | event == "fumble_defense_recovered" ) |> 
  filter(nflId == ballCarrierId) |> 
  select(gameId, playId, frameId)

playsWeek6 = left_join(playsWeek6, tackle_data_w6, join_by(gameId, playId))|> 
  rename(tackle_frame = frameId)

playsWeek6  = playsWeek6 |> 
  filter(tackle_frame > 13)

#writing the csv for this dataset
write.csv(playsWeek6, "playsWeek6(tackle14+).csv") 




#The following code uploads the output csv's from python to merge them into one total output
#it also removes any play that doesn't meet the win probability requirement between 5 and 95 percent.

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

#I then repeat the same procedure for the Swarm Score calculations 1 sec after catch

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

#now adding all the output weeks together and joining the YAC statistics from nflFastR

totalOutputF6 <- rbind(outputWeek1,outputWeek2,outputWeek3,outputWeek4,outputWeek5,outputWeek6,outputWeek7,outputWeek8,outputWeek9)

totalOutputF6 <- left_join(totalOutputF6, use_pbp, join_by(gameId == old_game_id, playId == play_id))

totalOutputF16 <- rbind(outputWeek1F14on,outputWeek2F14on,outputWeek3F14on,outputWeek4F14on,outputWeek5F14on,outputWeek6F14on,outputWeek7F14on,outputWeek8F14on,outputWeek9F14on)

totalOutputF16 <- left_join(totalOutputF16, use_pbp, join_by(gameId == old_game_id, playId == play_id))

write.csv(totalOutputF6, "totalOutputF6.csv")

totalOutputF16 <- totalOutputF16 |> 
  rename(rally1sec = F16rally)

write.csv(totalOutputF16, "totalOutputF16.csv")

#this is the creation of the pbp file from weeks 10-18 which are used in the "prediction" model of
#future YAC_Allowed success
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

#And then the last csv is written here!
write.csv(pbp1018, "pbp1018.csv")



play2858 <- read.csv("~/Downloads/dataVizExample.csv") |> 
  filter(frameId == 30)

