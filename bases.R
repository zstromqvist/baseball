
library(Lahman)
library(tidyverse)

batting_model <- readRDS("C:/Documents and Settings/zakri/Documents/Models/batting_model.rda")
pitch_model <- readRDS("C:/Documents and Settings/zakri/Documents/Models/pitch_model.rds")

# batting
team_batting_2010 <- 
  Teams %>% 
  filter(
    yearID == 2010
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

batting_proj_2010_df <-
  team_batting_2010 %>% 
  transmute(
    team = teamID,
    H,
    W,
    L,
    BA,
    OBP,
    SLG,
    ISO,
    R,
    R_proj = round(predict(batting_model, team_batting_2010)),
    HpR = H / R,
    HpR_proj = H / R_proj,
    Runs_scored_diff = R - R_proj
  )



# pitching
team_pitching_2010 <- 
  Pitching %>% 
  filter(yearID == 2010) %>% 
  group_by(team = teamID) %>% 
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


pitching_proj_2010_df <-
  team_pitching_2010 %>% 
  transmute(
    team,
    RA,
    RA_proj = round(predict(pitch_model, team_pitching_2010)),
    Runs_allowed_diff = RA - RA_proj
  )





# get everything together

main_2010_df <- 
  batting_proj_2010_df %>% 
  left_join(pitching_proj_2010_df, by = 'team') %>% 
  mutate(
    RD = R - RA,
    RD_proj = R_proj - RA_proj,
    W_pct_proj = R_proj^1.81 / (R_proj^1.81 + RA_proj^1.81),
    W_proj = round(162 * W_pct_proj),
    L_proj = round(162 * (1-W_pct_proj)),
    Wdiff = W - W_proj
  )
