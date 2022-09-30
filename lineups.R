
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

war_model_pitchers <- xgboost::xgb.load("C:/Users/zakri/Documents/Models/war_pitching.xg")
war_model_fielding <- xgboost::xgb.load("C:/Users/zakri/Documents/Models/war_fielding.xg")

# teams <- mlb_teams(season = 2011, sport_ids = 1)

roster_11 <- mlb_rosters(team_id = 143, season = 2011, roster_type = '40Man', date = date('2011-04-04'))

phillies_pitchers_2010 <- read_csv("C:/Users/zakri/projects/baseball/Data/phillies_pitchers_2010.csv")

##################################
# Pitchers ----

# 2010 ---
phillies_pitchers10 <- 
  Pitching %>% 
  filter(yearID == 2010) %>% 
  filter(teamID == 'PHI') %>% 
  filter(IPouts > 10) %>% 
  select(playerID)

pitchers_2010 <- 
  Pitching %>% 
  filter(yearID == 2010) %>% 
  select(-c(yearID, stint, teamID, lgID)) %>% 
  group_by(playerID) %>% 
  summarise(across(everything(), ~ sum(., is.na(.), 0))) %>% 
  ungroup()

pitchers_2010$WAR_2010 = 
  round(
    predict(war_model_pitchers, 
            pitchers_2010 |> 
              select(
                GS,
                ER,
                HR,
                BAOpp,
                ERA,
                SHO,
                SO,
                IBB
              ) |> 
              as.matrix()), 
    1)

starting_pitchers10 <- 
  pitchers_2010 %>% 
  inner_join(phillies_pitchers10, by = "playerID") %>% 
  mutate(
    score = GS + CG
  ) %>% 
  arrange(-score) %>% 
  filter(GS > 0) %>% 
  transmute(
    playerID, 
    GS,
    CG,
    G,
    score,
    WAR = WAR_2010
  ) %>% 
  filter(GS >= 12)

bullpen_2010 <-
  pitchers_2010 %>% 
  inner_join(phillies_pitchers10, by = "playerID") %>% 
  mutate(
    score = GS + CG
  ) %>% 
  arrange(-score) %>% 
  transmute(
    playerID, 
    GS,
    CG,
    G,
    score,
    WAR = WAR_2010
  ) %>% 
  anti_join(starting_pitchers10, 'playerID')



# 2011 ---

pitchers_proj_2011 <- read_csv("C:/Users/zakri/projects/baseball/Data/pitchers_proj_2011.csv")

proj_war_pitcher_2011 <- 
  pitchers_proj_2011 %>% 
  select(
    playerID = bdbID,
    yearID = Year,
    GS,
    ER,
    HR,
    BAOpp = BAopp,
    ERA,
    SHO,
    SO,
    IBB,
    CG,
    G
  )
  
proj_war_pitcher_2011$WAR_2011 = round(predict(war_model_pitchers, proj_war_pitcher_2011 |> select(-playerID, -yearID, -CG, -G) |> as.matrix()), 1)

starting_pitchers11 <- 
  roster_11 %>% 
  filter(position_code == 1) %>% 
  select(
    person_id,
    person_full_name
  ) %>% 
  left_join(player_id_map, by = "person_id") %>% 
  left_join(
    proj_war_pitcher_2011, by = "playerID"
  ) %>% 
  filter(G > 0) %>% 
  mutate(score = GS + CG) %>% 
  arrange(-score) %>% 
  select(
    playerID, 
    GS,
    CG,
    G,
    score,
    WAR = WAR_2011
  ) %>% 
  filter(GS >= 12)

bullpen_2011 <- 
  roster_11 %>% 
  filter(position_code == 1) %>% 
  select(
    person_id,
    person_full_name
  ) %>% 
  left_join(player_id_map, by = "person_id") %>% 
  left_join(
    proj_war_pitcher_2011 
    %>% select(
        playerID,
        WAR_2011,
        CG,
        G,
        GS
      )
    , by = "playerID"
  ) %>% 
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
    WAR = WAR_2011
  ) %>% 
  anti_join(starting_pitchers11, 'playerID')


sum(starting_pitchers10$WAR)
sum(starting_pitchers11$WAR)

sum(bullpen_2010$WAR)
sum(bullpen_2011$WAR)

bullpen_2010_br <- 
  phillies_pitchers_2010 %>% 
  filter(Name != 'Team Total') %>% 
  filter(GS == 0)

sum(bullpen_2010_br$`WAR▼`)

##################################
# Batters ----

# 2010 ---
phillies_batters_10 <- 
  Batting %>% 
  filter(yearID == 2010) %>% 
  filter(teamID == 'PHI') %>% 
  filter(AB > 100) %>% 
  select(playerID)

fielding_2010 <-
  Fielding %>%  
  filter(yearID == 2010) %>% 
  select(-c(yearID, stint, teamID, lgID, POS, G, SB, CS)) %>% 
  group_by(playerID) %>% 
  summarise(across(everything(), ~ sum(., is.na(.), 0))) %>% 
  ungroup() 

batters_2010 <- 
  Batting %>% 
  filter(yearID == 2010) %>% 
  select(-c(yearID, stint, teamID, lgID)) %>% 
  group_by(playerID) %>% 
  summarise(across(everything(), ~ sum(., is.na(.), 0))) %>% 
  ungroup() %>% 
  mutate(
    X1B = H - X2B - X3B - HR,
    PA = AB + SH + SF + BB + HBP,
    TB = X1B + 2*X2B + 3*X3B + 4*HR,
    SlugPct = TB / AB,
    OBP = (H + BB + HBP) / (AB + BB + HBP + SF)
  ) %>% 
  left_join(fielding_2010, 'playerID')

batters_2010$WAR_2010 = 
  round(
    predict(war_model_fielding, 
            batters_2010 |> 
              select(
                PA,
                TB,
                SlugPct,
                OBP,
                InnOuts,
                PO,
                A,
                E,
                DP
              ) |> 
              as.matrix()), 
    1)

batting_lineup_10 <- 
  batters_2010 %>% 
  inner_join(phillies_batters_10, by = "playerID") %>% 
  arrange(-AB) %>% 
  rename(
    WAR = WAR_2010
  ) 


# 2011 ---

batters_proj_2011 <- read_csv("C:/Users/zakri/projects/baseball/Data/batters_proj_2011.csv")

proj_war_batter_2011 <- 
  batters_proj_2011 %>% 
  transmute(
    playerID = bdbID,
    PA,
    TB = (H - D - `T` - HR) + 2*D + 3*`T` + 4*HR,
    SlugPct = SLG,
    OBP,
    InnOuts = mean(batters_2010$InnOuts),
    PO = mean(batters_2010$PO),
    A = mean(batters_2010$A),
    E = mean(batters_2010$E),
    DP = mean(batters_2010$DP)
  )

proj_war_batter_2011$WAR_2011 = round(predict(war_model_fielding, proj_war_batter_2011 |> select(-playerID) |> as.matrix()), 1)

batting_lineup_11 <- 
  roster_11 %>% 
  filter(position_code != 1) %>% 
  select(
    person_id,
    person_full_name
  ) %>% 
  left_join(player_id_map, by = "person_id") %>% 
  left_join(
    proj_war_batter_2011, by = "playerID"
  ) %>% 
  filter(!is.na(WAR_2011))%>% 
  arrange(-PA) %>% 
  transmute(
    playerID, 
    WAR = WAR_2011
  ) 


sum(batting_lineup_10$WAR)
sum(batting_lineup_11$WAR)











