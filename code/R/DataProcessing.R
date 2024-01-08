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
library(ggimage)
library(gt)

totalOutputF6 <- read.csv('~/downloads/totalOutputF6.csv') |> 
  select(gameId, playId, ballCarrierId, playDescription, defensiveTeam, epa = expectedPointsAdded, swarm, yac_allowed = yards_after_catch,
         yac_epa_allowed = yac_epa, xyac_epa_allowed = xyac_epa, xyac_mean_yardage_allowed = xyac_mean_yardage)
totalOutputF16<- read.csv('~/downloads/totalOutputF16.csv')|> 
  select(gameId, playId, ballCarrierId, playDescription, defensiveTeam, epa = expectedPointsAdded, tackle_frame,
         swarm1sec = rally1sec, yac_allowed = yards_after_catch, yac_epa_allowed = yac_epa, xyac_epa_allowed = xyac_epa, xyac_mean_yardage_allowed = xyac_mean_yardage)
totalOutputF16 = totalOutputF16[-1110, ]

pbp1018 <- read.csv('~/downloads/pbp1018.csv') |> 
  select(-X)

pacific = c()

proof <- totalOutputF16|>
  group_by(swarm1sec) |> 
  summarize(Z = mean(yac_allowed - xyac_mean_yardage_allowed)) 
pacific <- append(pacific, cor(proof$swarm1sec, proof$Z))


proof <- totalOutputF16|>
  group_by(swarm1sec) |> 
  summarize(Z = mean(yac_allowed))
pacific <- append(pacific, cor(proof$swarm1sec, proof$Z))

proof <- totalOutputF16|>
  group_by(swarm1sec) |> 
  summarize(Z = median(yac_allowed))
pacific <- append(pacific, cor(proof$swarm1sec, proof$Z))


atlantic = c()

W <-totalOutputF16 |> 
  group_by(swarm1sec) |> 
  summarize(Z = (mean(tackle_frame)-6)/10, r = n()) |> 
  filter(r > 10)
atlantic <- append(atlantic, cor(W$swarm1sec, W$Z))


ggplot(W, aes(x = swarm1sec, y = Z)) + 
  geom_bar(stat = "identity", position = "dodge", fill = "blue") +
  labs(x="Swarm Score 1 second after ball is caught", y="Average Seconds Between Catch and Tackle",
       title = "As Swarm Score Goes Up, Duration till Tackle Goes Down",
       subtitle = "Plays defined as described in the dataset paragraph, Swarm Score columns only showed for sample sizes of 10+, sample sizes labeled in bar") +
  theme_minimal() +
  geom_text(aes(label = r), nudge_y = -.2, color = "white")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
  theme(
    axis.title= element_text(size = 27.5),
    plot.title = element_text(size = 30),
    plot.subtitle = element_text(size = 12, color = "red")
  )
ggsave("SwarmScore1secvsTackleFrame.png", width = 14, height = 10, dpi = "retina")



viz10 <- totalOutputF6 |> 
  group_by(swarm) |> 
  summarize(Z = mean(epa), r = n()) |> 
  filter(r > 10)
atlantic <- append(atlantic, cor(viz10$swarm, viz10$Z))



ggplot(quid, aes(x = swarm, y = Z)) + 
  geom_bar(stat = "identity", position = "dodge", fill = "blue") +
  labs(x="Swarm Score", y="Average Yards After Catch Allowed per Play",
       title = "As Swarm Score Goes Up, YAC Allowed Goes Down",
       subtitle = "Plays defined as described in the dataset paragraph, Swarm Score columns only showed for sample sizes of 10+, sample sizes labeled in bar") +
  theme_minimal() +
  geom_text(aes(label = r), nudge_y = -.35, color = "white")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  theme(
    axis.title= element_text(size = 15),
    plot.title = element_text(size = 30),
    plot.subtitle = element_text(size = 12, color = "red")
  )
  
ggsave("SwarmScorevsYAC.png", width = 14, height = 10, dpi = "retina")

  


viz10 <- totalOutputF6|>
  group_by(swarm) |> 
  summarize(Z = mean(yac_allowed - xyac_mean_yardage_allowed), r = n()) |> 
  filter(r > 10)
atlantic <- append(atlantic, cor(viz10$swarm, viz10$Z))


quid <- totalOutputF6|>
  group_by(swarm) |> 
  summarize(Z = mean(yac_allowed), r = n()) |> 
  filter (r > 10)
atlantic <- append(atlantic, cor(quid$swarm, quid$Z))

 
think <- pbp1018 |> 
  group_by(defteam) |> 
  summarize(av_1018yac_allowed = mean(yac_allowed)) |> 
  arrange(av_1018yac_allowed)

compare_def <- totalOutputF16 |> 
  group_by(defensiveTeam) |> 
  summarize(av_swarm1sec = mean(swarm1sec), av_yac_allowed = mean(yac_allowed)) |> 
  arrange(-av_swarm1sec)

compare_def = left_join(compare_def, think, join_by(defensiveTeam == defteam))

cor(compare_def$av_swarm1sec, -compare_def$av_1018yac_allowed)
cor(compare_def$av_yac_allowed, compare_def$av_1018yac_allowed)

compare_def = left_join(compare_def, teams_colors_logos, by = c("defensiveTeam" = "team_abbr"))

compare_def |> 
  ggplot(aes(x = av_yac_allowed, y = av_1018yac_allowed)) +
  scale_x_reverse()+
  scale_y_reverse()+
  geom_hline(yintercept = mean(compare_def$av_1018yac_allowed), linetype = "dashed") +
  geom_vline(xintercept = mean(compare_def$av_yac_allowed), linetype = "dashed") +
  geom_smooth(method = "lm", color = "black", size = 1.5, alpha = 0.5, se = FALSE) +
  geom_image(aes(image = team_logo_espn), asp = 16/9, size = 0.05) +
  theme_bw() +
  labs (x = "Average YAC Allowed Per Play Weeks 1-9",
        y = "Average YAC Allowed Per Play Weeks 10-18",
        title = "Current YAC Allowed Predicts Future YAC Allowed",
        subtitle = "Plays are defined as completed non-screen passes with win probability between 5 and 95 percent and no tackle occuring in the first .8 seconds after ball is caught.")

ggsave("CurrentYACPredictsFutureYAC.png", width = 14, height = 10, dpi = "retina")

compare_def |> 
  ggplot(aes(x = av_swarm1sec, y = av_1018yac_allowed)) +
  scale_y_reverse()+
  geom_hline(yintercept = mean(compare_def$av_1018yac_allowed), linetype = "dashed") +
  geom_vline(xintercept = mean(compare_def$av_swarm1sec), linetype = "dashed") +
  geom_smooth(method = "lm", color = "black", size = 1.5, alpha = 0.5, se = FALSE) +
  geom_image(aes(image = team_logo_espn), asp = 16/9, size = 0.05) +
  theme_bw() +
  labs (x = "Swarm Score Per Play Weeks 1-9",
        y = "Average YAC Allowed Per Play Weeks 10-18",
        title = "Swarm Score Predicts Future YAC Allowed",
        subtitle = "Plays are defined as completed non-screen passes with win probability between 5 and 95 percent and no tackle occuring in the first .8 seconds after ball is caught.")

ggsave("SwarmScorePredictsFutureYAC.png", width = 14, height = 10, dpi = "retina")


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


info <- totalOutputF16 |> 
  group_by(defensiveTeam) |> 
  summarize(av_swarm1sec = mean(swarm1sec), av_yac_allowed = mean(yac_allowed)) |> 
  arrange(-av_swarm1sec)

info = left_join(info, teams_colors_logos, c("defensiveTeam" = "team_abbr"))
info <- info |> 
  select(team_wordmark, av_swarm1sec)

info |> 
  gt() |> 
  cols_align(align = "center") |> 
  gtExtras::gt_img_rows(team_wordmark) |> 
  cols_label(team_wordmark = "Defensive Unit",
             av_swarm1sec= "Average Swarm Score 1 Second After Ball is Caught") |>
  tab_header(
    title = "Defensive Swarm Rankings",
    subtitle = "Data from Week 1-9 2022 NFL Season, All Qualifying Plays Included",
  ) |> 
  gtExtras::gt_theme_espn() |> 
  gtExtras::gt_color_rows(column = av_swarm1sec, palette = c("green", "white"), direction = -1, domain = c(2,3))
