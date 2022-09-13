
library(readr)
library(tidyverse)
library(baseballr)
library(Lahman)
library(xgboost)
library(glue)

war_daily_bat <- read_csv("C:/Users/zakri/Downloads/war_daily_bat.txt")
war_daily_pitch <- read_csv("C:/Users/zakri/Downloads/war_daily_pitch.txt")

# utils

rmse <- function(actual, predicted){
  sqrt(mean((actual - predicted)^2))
}

mae <- function(actual, predicted){
  sum(abs(actual - predicted)) / length(actual)
}

check_predictions <- function(df_with_preds){

  max_war <- max(df_with_preds$WAR)
  min_war <- min(df_with_preds$WAR)

  ranges <- seq(floor(min_war), ceiling(max_war), by = 2)
  ranges2 <- ranges |> append(10)

  for (i in 1:length(ranges)) {


    war_df_tmp <-
      df_with_preds |>
      filter(WAR >= ranges2[i] & WAR <= ranges2[i+1])

    rmse_tmp <- round(rmse(war_df_tmp$WAR, war_df_tmp$WAR_pred_xgb), 3)
    mae_tmp <- round(mae(war_df_tmp$WAR, war_df_tmp$WAR_pred_xgb), 3)
    corr_tmp <- round(cor(war_df_tmp$WAR, war_df_tmp$WAR_pred_xgb), 3)

    print(glue("bWAR range: {ranges2[i]} to {ranges2[i+1]}. Obs: {nrow(war_df_tmp)}. RMSE: {rmse_tmp}. MAE: {mae_tmp}, Correlation: {corr_tmp}"))

  }


}

# parameters

start_year <- 2010
end_year <- 2021

# batting model



batting_stats_df <-
  Batting |>
  filter(yearID >= start_year & yearID <= end_year) |>
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
  left_join(
      battingStats(
        idvars = c("playerID", "yearID"),
        cbind = FALSE
      ),
    by = c("playerID", "yearID")
  ) |>
  select(-PA)



fielding_stats_df <-
  Fielding |>
  filter(yearID >= start_year & yearID <= end_year) |>
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
  summarise_all(sum)

main_df <-
  war_daily_bat |>
  filter(year_ID >= start_year & year_ID <= end_year) |>
  filter(WAR != 'NULL') |>
  transmute(
    name_common,
    playerID = player_ID,
    teamID = team_ID,
    yearID = year_ID,
    stint = stint_ID,
    lg_ID,
    WAR_tmp = as.numeric(WAR),
    PA
  ) |>
  group_by(name_common, playerID, yearID) |>
  mutate(
    WAR_share = case_when(
      WAR_tmp == 0 ~ 1,
      PA == 0 ~ WAR_tmp,
      TRUE ~ PA / sum(PA)
    ),
    WAR = round(WAR_tmp * WAR_share, 1)
  ) |>
  select(
    -WAR_share,
    -WAR_tmp,
    -teamID,
    -stint,
    -lg_ID
  ) |>
  summarise_all(sum) |>
  ungroup() |>
  left_join(
    batting_stats_df,
    by = c('playerID', 'yearID')
  ) |>
  left_join(
    fielding_stats_df,
    by = c('playerID', 'yearID')
  ) |>
  filter(PA > 0) |>
  filter(!is.na(G_batting)) |>
  mutate(across(BA:BABIP, ~replace_na(.x, 0)))



# Modeling ----

# simple models
model_df <-
  main_df |>
  filter(yearID != 2021) |>
  select(
    WAR,
    H,
    R,
    HR
  )

train_x = model_df |> select(-WAR) |> as.matrix()
train_y = model_df |> select(WAR) |> as.matrix()

simple_model <- xgboost(
  data = as.matrix(train_x),
  label = as.matrix(train_y),
  nrounds = 150,
  objective = "reg:squarederror",
  early_stopping_rounds = 3,
  max_depth = 6,
  eta = .25
)

simple_lm <- lm(WAR ~ H + R + HR, data = model_df)

test <-
  main_df |>
  filter(yearID == 2021) |>
  select(
    WAR,
    H,
    R,
    HR
  )

test$WAR_pred_xgb = round(predict(simple_model, test |> select(-WAR) |> as.matrix()), 1)
test$WAR_lm <- round(predict(simple_lm, test |> select(-WAR)), 1)
rmse(test$WAR, test$WAR_pred_xgb)
rmse(test$WAR, test$WAR_lm)
cor(test$WAR, test$WAR_pred_xgb)
cor(test$WAR, test$WAR_lm)


# more advanced model
model_df_adv <-
  main_df |>
  filter(yearID != 2021) |>
  select(
    WAR,
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

train_x = model_df_adv |> select(-WAR) |> as.matrix()
train_y = model_df_adv |> select(WAR) |> as.matrix()

adv_model <- xgboost(
  data = as.matrix(train_x),
  label = as.matrix(train_y),
  nrounds = 150,
  objective = "reg:squarederror",
  early_stopping_rounds = 3,
  max_depth = 6,
  eta = .25
)

adv_lm <- lm(WAR ~ ., data = model_df_adv)
summary(adv_lm)

test_adv <-
  main_df |>
  filter(yearID == 2021) |>
  select(
    name_common,
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
    DP,
    WAR
  ) |>
  filter(!is.na(PO))

test_adv$WAR_pred_xgb = round(predict(adv_model, test_adv |> select(-WAR, -name_common, -playerID, -yearID) |> as.matrix()), 1)
test_adv$WAR_lm <- round(predict(adv_lm, test_adv |> select(-WAR, -name_common, -playerID, -yearID)), 1)
rmse(test_adv$WAR, test_adv$WAR_pred_xgb)
rmse(test_adv$WAR, test_adv$WAR_lm)
mae(test_adv$WAR, test_adv$WAR_pred_xgb)
mae(test_adv$WAR, test_adv$WAR_lm)
cor(test_adv$WAR, test_adv$WAR_pred_xgb)
cor(test_adv$WAR, test_adv$WAR_lm)



check_predictions(test)
check_predictions(test_adv)

xgb.save(adv_model, "C:/Users/zakri/Documents/Models/war_fielding.xg")



