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

totalOutputF6 <- read.csv('totalOutputF6.csv') |> 
  select(gameId, playId, ballCarrierId, playDescription, defensiveTeam, epa = expectedPointsAdded, swarm = rally, yac_allowed = yards_after_catch,
         yac_epa_allowed = yac_epa, xyac_epa_allowed = xyac_epa, xyac_mean_yardage_allowed = xyac_mean_yardage)
totalOutputF16<- read.csv('totalOutputF16.csv') |> 
  select(gameId, playId, ballCarrierId, playDescription, defensiveTeam, epa = expectedPointsAdded, tackle_frame,
         swarm1sec = rally1sec, yac_allowed = yards_after_catch, yac_epa_allowed = yac_epa, xyac_epa_allowed = xyac_epa, xyac_mean_yardage_allowed = xyac_mean_yardage)
totalOutputF16 = totalOutputF16[-1110, ]

pbp1018 <- read.csv('pbp1018.csv') |> 
  select(-X)




totalOutputF16|>
  group_by(swarm1sec) |> 
  summarize(Z = mean(yards_after_catch_allowed - xyac_allowed_mean_yardage), n())

totalOutputF16|>
  group_by(swarm1sec) |> 
  summarize(Z = mean(yards_after_catch_allowed),n())

totalOutputF16|>
  group_by(swarm1sec) |> 
  summarize(Z = median(yards_after_catch_allowed - xyac_allowed_mean_yardage),n())

totalOutputF16|>
  group_by(swarm1sec) |> 
  summarize(Z = median(yards_after_catch),n())

totalOutputF16|>
  group_by(swarm1sec) |> 
  summarize(Z = mean(yac_epa - xyac_epa),n())

totalOutputF16|>
  group_by(swarm1sec) |> 
  summarize(Z = mean(yac_epa),n())

totalOutputF16|>
  group_by(swarm1sec) |> 
  summarize(Z = median(yac_epa - xyac_epa),n())

totalOutputF16|>
  group_by(swarm1sec) |> 
  summarize(Z = median(yac_epa),n())

A <-totalOutputF16 |> 
  group_by(swarm1sec) |> 
  summarize(Z = mean(tackle_frame), r = n()) |> 
  filter(r > 10)
cor(A$swarm1sec,A$Z)

totalOutputF16 |> 
  group_by(swarm1sec) |> 
  summarize(Z = mean(expectedPointsAdded), n())

totalOutputF6 |> 
  group_by(swarm) |> 
  summarize(Z = mean(expectedPointsAdded), n())

totalOutputF6|>
  group_by(swarm) |> 
  summarize(Z = mean(yards_after_catch - xyac_mean_yardage),n())

totalOutputF6|>
  group_by(swarm) |> 
  summarize(Z = mean(yards_after_catch), n())

totalOutputF6|>
  group_by(swarm) |> 
  summarize(Z = median(yards_after_catch - xyac_mean_yardage), n())

totalOutputF6|>
  group_by(swarm) |> 
  summarize(Z = median(yards_after_catch), n())

totalOutputF6|>
  group_by(swarm) |> 
  summarize(Z = mean(yac_epa - xyac_epa), n())

totalOutputF6|>
  group_by(swarm) |> 
  summarize(Z = mean(yac_epa),n())

totalOutputF6|>
  group_by(swarm) |> 
  summarize(Z = median(yac_epa - xyac_epa),n())

totalOutputF6|>
  group_by(swarm) |> 
  summarize(Z = median(yac_epa),n())


compare_def <- totalOutputF16 |> 
  group_by(defensiveTeam) |> 
  summarize(av_swarm1sec = mean(swarm1sec), av_yac_allowed = mean(yac_allowed)) |> 
  arrange(-av_swarm1sec)

think <- pbp1018 |> 
  group_by(defteam) |> 
  summarize(av_1018yac_allowed = mean(yac_allowed)) |> 
  arrange(av_1018yac_allowed)

compare_def = left_join(compare_def, think, join_by(defensiveTeam == defteam))

cor(compare_def$av_swarm1sec, -compare_def$av_1018yac_allowed)
cor(compare_def$av_yac_allowed, compare_def$av_1018yac_allowed)


yac_allowed_oe_predict <- totalOutputF16 |> 
  group_by(defensiveTeam) |> 
  summarize(av_swarm1sec = mean(swarm1sec), yac_allowed_oe = mean(yac_allowed - xyac_mean_yardage_allowed))

         
yac_allowed_oe_1018 <- pbp1018 |> 
  mutate(av_1018_yac_oe_allowed = mean(yac_allowed - xyac_mean_yardage_allowed)) |> 
  group_by(defteam) |> 
  summarize(n(),av_1018_yac_oe_allowed)


yac_allowed_oe_predict = left_join(yac_allowed_oe_predict, yac_allowed_oe_1018, join_by(defensiveTeam == defteam))

cor(yac_allowed_oe_predict$av_swarm1sec, -yac_allowed_oe_predict$av_1018_yac_oe_allowed)
cor(yac_allowed_oe_predict$yac_allowed_oe, yac_allowed_oe_predict$av_1018_yac_oe_allowed)

