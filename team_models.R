
library(Lahman)
library(tidyverse)

modeling_years <- c(2000:2009)

# batting
team_batting_model_df <- 
  Teams %>% 
  filter(
    yearID %in% modeling_years
  ) %>% 
  transmute(
    teamID,
    W,
    L,
    R,
    RA,
    Rdiff = R - RA,
    AB,
    TB = H + X2B + 2 * X3B + 3 * HR,
    H,
    X2B,
    X3B,
    HR,
    BB,
    SO,
    SB,
    SF,
    BA = H / AB,
    OBP = (H + BB + HBP) / (AB + BB + HBP + SF),
    SLG = TB / AB,
    OPS = OBP + SLG,
    ISO = SLG - BA,
    W_pct = W / (W + L),
    W_pct_pyth = R^1.81 / (R^1.81 + RA^1.81),
    W_pyth = round(162 * W_pct_pyth),
    L_pyth = round(162 * (1-W_pct_pyth)),
    Luck = W - W_pyth
  ) %>% 
  mutate_if(is.numeric, round, 3)


batting_model <- lm(R ~ OBP + SLG, data = team_batting_model_df)
summary(batting_model)

# pitching
team_pitching_model_df <- 
  Pitching %>% 
  filter(yearID %in% modeling_years) %>% 
  group_by(team = teamID, year = yearID) %>% 
  summarise(
    RA = sum(R),
    H = sum(H),
    BB = sum(BB),
    HBP = sum(HBP),
    SF = sum(SF),
    BFP = sum(BFP),
    HR = sum(HR)
  ) %>% 
  ungroup() %>% 
  mutate(
    AB = BFP - BB - HBP - SF,
    OBP = (H + BB + HBP) / BFP,
    SLG_simple = (H + 4*HR) / AB,
    BA = H / AB,
    ISO_simple = SLG_simple - BA
  ) %>% 
  mutate_if(is.numeric, round, 3)

pitch_model <- lm(RA ~ OBP + SLG_simple, data = team_pitching_model_df)
summary(pitch_model)

saveRDS(object = batting_model, file = "C:/Documents and Settings/zakri/Documents/Models/batting_model.rda")
saveRDS(object = pitch_model, file = "C:/Documents and Settings/zakri/Documents/Models/pitch_model.rds")
