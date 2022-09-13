
library(readr)
library(tidyverse)
library(baseballr)
library(Lahman)
library(xgboost)
library(glue)

war_daily_bat <- read_csv("C:/Users/zakri/Downloads/war_daily_bat.txt")
war_daily_pitch <- read_csv("C:/Users/zakri/Downloads/war_daily_pitch.txt")
war_model_batters <- xgb.load("C:/Users/zakri/Documents/Models/war_fielding.xg")
war_model_pitchers <- xgb.load("C:/Users/zakri/Documents/Models/war_pitching.xg")

start_year <- 2010
end_year <- 2021

war_bat_df <- data.frame()
for (year_tmp in start_year:end_year) {
  
  print(glue("Run year: {year_tmp}"))
  
  batting_stats_df <-
    Batting |>
    filter(yearID == year_tmp) |>
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
      G_batting = G
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
    summarise_all(sum) |>
    ungroup() %>% 
    left_join(
      battingStats(
        idvars = c("playerID", "yearID"),
        cbind = FALSE
      ),
      by = c("playerID", "yearID")
    )
  
  fielding_stats_df <-
    Fielding |>
    filter(yearID == year_tmp) |>
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
      G_fielding = G,
      SB_catcher = SB,
      CS_catcher = CS
    ) |>
    select(
      -stint,
      -teamID,
      -lgID,
      -POS,
      -G,
      -SB,
      -CS
    ) |>
    group_by(
      playerID,
      yearID
    ) |>
    summarise_all(sum) %>% 
    ungroup()
  
  
  war_tmp <-
    batting_stats_df |>
    left_join(
      fielding_stats_df,
      by = c('playerID', 'yearID')
    ) |>
    filter(PA > 0) |>
    filter(!is.na(G_batting)) |>
    mutate(across(BA:BABIP, ~replace_na(.x, 0))) %>%
    select(
      playerID,
      yearID,
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
    filter(!is.na(PO))
  
  war_tmp$WAR = round(predict(war_model_batters, war_tmp |> select(-playerID, -yearID) |> as.matrix()), 1)
  
  
  war_bat_df <- 
    war_bat_df %>% 
    bind_rows(war_tmp)
  
}



# pitching 

war_pitch_df <- data.frame()
for (year_tmp in start_year:end_year) {
  
  print(glue("Run year: {year_tmp}"))
  
  war_tmp <- 
    Pitching %>% 
    filter(yearID == year_tmp) |>
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
  
  war_tmp$WAR = round(predict(war_model_pitchers, war_tmp |> select(-playerID, -yearID) |> as.matrix()), 1)
  
  war_pitch_df <- 
    war_pitch_df %>% 
    bind_rows(war_tmp)
  
}


write.csv(war_bat_df, "C:/Users/zakri/Documents/Models/war_bat_df.csv", row.names = F)
write.csv(war_pitch_df, "C:/Users/zakri/Documents/Models/war_pitch_df.csv", row.names = F)


