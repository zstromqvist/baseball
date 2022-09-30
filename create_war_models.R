
library(readr)
library(tidyverse)
library(baseballr)
library(Lahman)
library(xgboost)
library(lightgbm)
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

start_year <- 2000
end_year <- 2021

# batting model ----



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



# Modeling ---

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

# pitching ----

pitching_stats_df <- 
  Pitching %>% 
  filter(yearID >= start_year & yearID <= end_year) |>
  filter(IPouts > 0) |> 
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
  summarise_all(sum) |> 
  mutate(
    AB = BFP - BB - HBP - SF - SH,
    BA = round(H / AB, 3),
    OBP = round((H + BB + HBP) / (AB + BB + HBP + SF), 3),
    BAbip = round((H - HR) / (AB - SO - HR + SF), 3),
    FIP = round((13*HR + 3*(BB+HBP) - 2*SO) / (IPouts/3) + 3.1, 3),
    K_rate = round(SO / BFP, 3),
    HR_rate = round(HR / BFP, 3),
  )


main_df <-
  war_daily_pitch |>
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
    IPouts
  ) |>
  group_by(name_common, playerID, yearID) |>
  mutate(
    WAR_share = case_when(
      WAR_tmp == 0 ~ 1,
      IPouts == 0 ~ WAR_tmp,
      TRUE ~ IPouts / sum(IPouts)
    ),
    WAR = round(WAR_tmp * WAR_share, 1)
  ) |>
  select(
    -WAR_share,
    -WAR_tmp,
    -teamID,
    -stint,
    -lg_ID,
    -IPouts
  ) |>
  summarise_all(sum) |>
  ungroup() |>
  left_join(
    pitching_stats_df,
    by = c('playerID', 'yearID')
  ) |>
  filter(IPouts > 0) |> 
  arrange(playerID, yearID) |> 
  group_by(playerID) |> 
  mutate(
    WAR_lagged_1 = lag(WAR)
  ) |> 
  ungroup() |> 
  filter(!is.na(WAR_lagged_1))

# selected_vars <- 
#   c(
#     'WAR',
#     'WAR_lagged_1',
#     'W',
#     'IPouts',
#     'ER',
#     'ERA',
#     'OBP',
#     'SO'
#   )

selected_vars <- 
  c(
    'WAR',
    'ERA',
    'W',
    'IPouts',
    'SO',
    'OBP',
    'R',
    'BAOpp',
    'ER',
    'WAR_lagged_1',
    'AB',
    'FIP'
  )

# train first model
model_df_adv <-
  main_df |>
  filter(yearID < 2020) |>
  select(all_of(selected_vars)) %>% 
  drop_na()

train_x = model_df_adv |> select(-WAR)
train_y = model_df_adv |> select(WAR)

adv_model <-
  xgboost(
    data = as.matrix(train_x),
    label = as.matrix(train_y),
    nrounds = 300,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 6,
    eta = .2
  )

# xgb.plot.importance(xgb.importance(model = adv_model))

# LGBM ---
lgbm_train <- 
  lgb.Dataset(train_x |> data.matrix(), label = train_y$WAR |> as.numeric())

params <- 
  list(
    objective = "regression_l1", 
    metric = 'rmse',
    num_leaves = 4L, 
    learning_rate = .2,
    max_depth = 5,
    num_iterations = 300
  )

adv_lgbm <- 
  lgb.train(
    params,
    data = lgbm_train, 
    nrounds = 150L,
    verbose = -1L
  )


# LM ---
adv_lm <- lm(WAR ~ ., data = model_df_adv)


# Test ---
test_adv <-
  main_df |>
  filter(yearID >= 2020) |>
  select(
    name_common,
    playerID,
    yearID,
    all_of(selected_vars)
  ) |> 
  filter(ER > 0)

test_adv$WAR_pred_xgb = round(predict(adv_model, test_adv |> select(-WAR, -name_common, -playerID, -yearID) |> as.matrix()), 1)
test_adv$WAR_lgbm <- round(predict(adv_lgbm, test_adv |> select(-WAR, -name_common, -playerID, -yearID, -WAR_pred_xgb) |> data.matrix()), 1)
test_adv$WAR_lm <- round(predict(adv_lm, test_adv |> select(-WAR, -name_common, -playerID, -yearID)), 1)

test_adv <- 
  test_adv |> 
  mutate(
    WAR_ensamble = round((WAR_pred_xgb + WAR_lgbm + WAR_lm) / 3, 1)
  )

rmse(test_adv$WAR, test_adv$WAR_pred_xgb)
rmse(test_adv$WAR, test_adv$WAR_lm)
rmse(test_adv$WAR, test_adv$WAR_lgbm)
rmse(test_adv$WAR, test_adv$WAR_ensamble)

cor(test_adv$WAR, test_adv$WAR_pred_xgb)
cor(test_adv$WAR, test_adv$WAR_lm)
cor(test_adv$WAR, test_adv$WAR_lgbm)
cor(test_adv$WAR, test_adv$WAR_ensamble)

# xgb.save(adv_model, "C:/Users/zakri/Documents/Models/war_pitching.xg")


# test ----

test_pitch <- 
  test_adv |> 
  mutate(
    WAR_group = case_when(
      WAR < -1 ~ -1,
      WAR < 0 ~ 0,
      WAR < 1 ~ 1,
      WAR < 2 ~ 2,
      WAR < 3 ~ 3,
      WAR < 4 ~ 4,
      WAR < 5 ~ 5,
      WAR < 6 ~ 6,
      T ~ 7
    )
  ) |> 
  group_by(WAR_group) |> 
  summarise(
    n = n(),
    avg_WAR = round(mean(WAR), 1),
    avg_WAR_pred = round(mean(WAR_ensamble), 1),
    avg_WAR_pred_xgb = round(mean(WAR_pred_xgb), 1),
    avg_WAR_pred_lgbm = round(mean(WAR_lgbm), 1),
    avg_WAR_pred_lm = round(mean(WAR_lm), 1),
  )










