
library(tidyverse)
library(baseballr)
library(lubridate)
library(Lahman)
library(readr)

player_id_map_raw <- read_csv("C:/Users/zakri/Downloads/SFBB Player ID Map - PLAYERIDMAP.csv")
player_id_map <- 
  player_id_map_raw %>% 
  select(
    playerID = BREFID,
    person_id = MLBID
  )

pitcher_war <- read.csv("C:/Users/zakri/Documents/Models/war_pitch_df.csv")
war_model_pitchers <- xgboost::xgb.load("C:/Users/zakri/Documents/Models/war_pitching.xg")

teams <- mlb_teams(season = 2011, sport_ids = 1)
players <- People

test_10 <- mlb_rosters(team_id = 143, season = 2010, roster_type = 'active', date = date('2010-06-05'))
test_11 <- mlb_rosters(team_id = 143, season = 2011, roster_type = '40Man', date = date('2011-04-04'))

phillies_pitchers10 <- 
  Pitching %>% 
  filter(yearID == 2010) %>% 
  filter(teamID == 'PHI') %>% 
  select(playerID)

war_2011 <- 
  Pitching %>% 
  filter(yearID == 2010) |>
  mutate(
    teamID = as.character(teamID),
    teamID = case_when(
      teamID == 'CHA' ~ 'CHW',
      teamID == 'CHN' ~ 'CHC',
      teamID == 'FLO' ~ 'FLA',
      teamID == 'KCA' ~ 'KCR',
      teamID == 'LAN' ~ 'LAD',
      teamID == 'NYA' ~ 'NYY',
      teamID == 'NYN' ~ 'NYM',
      teamID == 'SDN' ~ 'SDP',
      teamID == 'SFN' ~ 'SFG',
      teamID == 'SLN' ~ 'STL',
      teamID == 'TBA' ~ 'TBR',
      teamID == 'WAS' ~ 'WSN',
      TRUE ~ teamID
    ),
    G_pitching = G
  ) |>
  select(
    -stint,
    -teamID,
    -lgID,
    -G
  ) |>
  group_by(
    playerID,
    yearID
  ) |>
  summarise_all(sum) %>% 
  ungroup() %>% 
  select(
    playerID,
    yearID,
    GS,
    ER,
    HR,
    BAOpp,
    ERA,
    SHO,
    SO,
    IBB
  )

war_2011$WAR_proj = round(predict(war_model_pitchers, war_2011 |> select(-playerID, -yearID) |> as.matrix()), 1)

pitchers_2010 <- 
  Pitching %>% 
  filter(yearID == 2010) %>% 
  select(-c(yearID, stint, teamID, lgID)) %>% 
  group_by(playerID) %>% 
  summarise(across(everything(), ~ sum(., is.na(.), 0))) %>% 
  ungroup() %>% 
  left_join(pitcher_war %>% filter(yearID == 2010) %>% select(playerID, WAR), by = "playerID") %>% 
  left_join(war_2011 %>% select(playerID, WAR_proj), by = "playerID") %>% 
  mutate(
    WAR_2010 = WAR,
    WAR_2011 = WAR_proj + rnorm(nrow(.), 0, 1),
  )

starting_pitchers10 <- 
  pitchers_2010 %>% 
  inner_join(phillies_pitchers10, by = "playerID") %>% 
  mutate(
    score = GS + CG
  ) %>% 
  arrange(-score) %>% 
  select(
    # person_full_name,
    playerID, 
    GS,
    CG,
    G,
    score,
    WAR_2010,
    WAR_2011
  ) %>% 
  filter(GS >= 12) %>% 
  slice(1:5)

starting_pitchers11 <- 
  test_11 %>% 
  filter(position_code == 1) %>% 
  select(
    person_id,
    person_full_name
  ) %>% 
  left_join(player_id_map, by = "person_id") %>% 
  left_join(pitchers_2010, by = "playerID") %>% 
  filter(!is.na(G)) %>% 
  mutate(score = GS + CG) %>% 
  arrange(-score) %>% 
  select(
    # person_full_name,
    playerID, 
    GS,
    CG,
    G,
    score,
    WAR_2010,
    WAR_2011
  ) %>% 
  filter(GS >= 12) %>% 
  slice(1:5)


sum(starting_pitchers10$WAR_2010)
sum(starting_pitchers11$WAR_2011)


# determine value of starting pitchers first season, 2010, WAR
# determine roster of next season, 2011, and its value, WAR
# calculate the difference in WAR/runs based on changes in roster/WAR
# bullpen is treated as league average