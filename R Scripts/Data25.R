raw_player_box25 = read_csv(
  paste0("https://raw.githubusercontent.com/jeremydumalig/SlowScout/main/",
         "player_box25.csv")
)
raw_team_box25 = read_csv(
  paste0("https://raw.githubusercontent.com/jeremydumalig/SlowScout/main/",
         "team_box25.csv")
)
raw_plays25 = read_csv(
  paste0("https://raw.githubusercontent.com/jeremydumalig/SlowScout/main/",
         "plays25.csv")
)

player_box25 = 
  raw_player_box25 %>%
  merge(uaa_teams_csv, by="Team", all.x=T) %>%
  separate(`FGM-A`, into = c("FGM", "FGA"), sep = "-") %>%
  separate(`3PM-A`, into = c("3PM", "3PA"), sep = "-") %>%
  separate(`FTM-A`, into = c("FTM", "FTA"), sep = "-") %>%
  mutate(FGM = as.integer(FGM),
         FGA = as.integer(FGA),
         `3PM` = as.integer(`3PM`),
         `3PA` = as.integer(`3PA`),
         `2PM` = FGM - `3PM`,
         `2PA` = FGA - `3PA`,
         FTM = as.integer(FTM),
         FTA = as.integer(FTA),
         `FG%` = 100 * FGM / FGA,
         `3P%` = 100 * `3PM` / `3PA`,
         `FT%` = 100 * FTM / FTA,
         PPS = PTS / (FGA + 0.44*FTA),
         `AST/TO` = AST / TO,
         `FT-R` = 100 * FTA / FGA,
         `3P-R` = 100 * `3PA` / `FGA`,
         across(where(is.numeric), ~ round(., 1)),
         `FGM-A` = paste(as.character(FGM), as.character(FGA), sep="-"),
         `3PM-A` = paste(as.character(`3PM`), as.character(`3PA`), sep="-"),
         `FTM-A` = paste(as.character(FTM), as.character(FTA), sep="-"))

player_averages25 =
  player_box25 %>%
  rbind(player_box25) %>%
  filter(Team %in% uaa_teams) %>%
  group_by(League, Team, Conference, Player, URL) %>%
  summarize(URL = first(URL),
            across(where(is.numeric), mean, na.rm=TRUE),
            .groups='drop') %>%
  mutate(`FG%` = 100 * FGM / FGA,
         `3P%` = 100 * `3PM` / `3PA`,
         `FT%` = 100 * FTM / FTA,
         `FT-R` = 100 * FTA / FGA,
         `3P-R` = 100 * `3PA` / FGA,
         `AST/TO` = AST / TO,
         PPS = PTS / (FGA + 0.44*FTA),
         across(where(is.numeric), ~ round(., 1)),
         `FGM-A` = paste(as.character(FGM), as.character(FGA), sep="-"),
         `3PM-A` = paste(as.character(`3PM`), as.character(`3PA`), sep="-"),
         `FTM-A` = paste(as.character(FTM), as.character(FTA), sep="-"))

team_box25 = 
  raw_team_box25 %>%
  merge(uaa_teams_csv, by="Team", all.x=T) %>%
  merge(select(raw_team_box25,
               League, Date, Opponent, 
               PTS, `FGM-A`, `3PM-A`, `FTM-A`, OREB, DREB, REB, TO),
        by.x = c("League", "Date", "Team"),
        by.y = c("League", "Date", "Opponent"),
        suffixes = c("", "_OPP")) %>%
  separate(`FGM-A`, into = c("FGM", "FGA"), sep = "-") %>%
  separate(`3PM-A`, into = c("3PM", "3PA"), sep = "-") %>%
  separate(`FTM-A`, into = c("FTM", "FTA"), sep = "-") %>%
  separate(`FGM-A_OPP`, into = c("FGM_OPP", "FGA_OPP"), sep = "-") %>%
  separate(`3PM-A_OPP`, into = c("3PM_OPP", "3PA_OPP"), sep = "-") %>%
  separate(`FTM-A_OPP`, into = c("FTM_OPP", "FTA_OPP"), sep = "-") %>%
  mutate(FGM = as.integer(FGM),
         FGA = as.integer(FGA),
         `3PM` = as.integer(`3PM`),
         `3PA` = as.integer(`3PA`),
         `2PM` = FGM - `3PM`,
         `2PA` = FGA - `3PA`,
         FTM = as.integer(FTM),
         FTA = as.integer(FTA),
         FGM_OPP = as.integer(FGM_OPP),
         FGA_OPP = as.integer(FGA_OPP),
         `3PM_OPP` = as.integer(`3PM_OPP`),
         `3PA_OPP` = as.integer(`3PA_OPP`),
         `2PM_OPP` = FGM_OPP - `3PM_OPP`,
         `2PA_OPP` = FGA_OPP - `3PA_OPP`,
         FTM_OPP = as.integer(FTM_OPP),
         FTA_OPP = as.integer(FTA_OPP),
         `FG%` = 100 * FGM / FGA,
         `3P%` = 100 * `3PM` / `3PA`,
         `FT%` = 100 * FTM / FTA,
         POSS = 0.96 * (FGA + 0.44*FTA + TO - OREB),
         POSS_OPP = 0.96 * (FGA_OPP + 0.44*FTA_OPP + TO_OPP - OREB_OPP),
         `+/-` = PTS - PTS_OPP,
         `AST%` = 100 * AST / TO,
         `FT-R` = 100 * FTA / FGA,
         `3P-R` = 100 * `3PA` / `FGA`,
         `ORB%` = 100 * OREB / (OREB + DREB_OPP),
         `DRB%` = 100 * DREB / (DREB + OREB_OPP),
         `REB%` = 100 * REB / (REB + REB_OPP),
         `TO%` = 100 * TO / POSS,
         `POT %` = 100 * `Points off Turnovers` / PTS,
         `2ND %` = 100 * `2nd Chance Points` / PTS,
         `PITP %` = 100 * `Points in the Paint` / PTS,
         `FB %` = 100 * `Fastbreak Points` / PTS,
         `BENCH %` = 100 * `Bench Points` / PTS,
         across(where(is.numeric), ~ round(., 1)),
         PPP = round(PTS / POSS, 2),
         PPP_OPP = round(PTS_OPP / POSS_OPP, 2),
         NET = round(PPP - PPP_OPP, 2),
         `FGM-A` = paste(as.character(FGM), as.character(FGA), sep="-"),
         `3PM-A` = paste(as.character(`3PM`), as.character(`3PA`), sep="-"),
         `FTM-A` = paste(as.character(FTM), as.character(FTA), sep="-"))

team_averages25 = 
  team_box25 %>%
  rbind(team_box25) %>%
  filter(Team %in% uaa_teams) %>%
  group_by(League, Team, Conference, URL) %>%
  summarize(URL = first(URL),
            across(where(is.numeric), mean, na.rm=TRUE),
            .groups='drop') %>%
  mutate(`FG%` = 100 * FGM / FGA,
         `3P%` = 100 * `3PM` / `3PA`,
         `FT%` = 100 * FTM / FTA,
         POSS = 0.96 * (FGA + 0.44*FTA + TO - OREB),
         POSS_OPP = 0.96 * (FGA_OPP + 0.44*FTA_OPP + TO_OPP - OREB_OPP),
         `+/-` = PTS - PTS_OPP,
         `AST%` = 100 * AST / TO,
         `FT-R` = 100 * FTA / FGA,
         `3P-R` = 100 * `3PA` / `FGA`,
         `ORB%` = 100 * OREB / (OREB + DREB_OPP),
         `DRB%` = 100 * DREB / (DREB + OREB_OPP),
         `REB%` = 100 * REB / (REB + REB_OPP),
         `TO%` = 100 * TO / POSS,
         `POT %` = 100 * `Points off Turnovers` / PTS,
         `2ND %` = 100 * `2nd Chance Points` / PTS,
         `PITP %` = 100 * `Points in the Paint` / PTS,
         `FB %` = 100 * `Fastbreak Points` / PTS,
         `BENCH %` = 100 * `Bench Points` / PTS,
         across(where(is.numeric), ~ round(., 1)),
         PPP = round(PTS / POSS, 2),
         PPP_OPP = round(PTS_OPP / POSS_OPP, 2),
         NET = round(PPP - PPP_OPP, 2),
         `FGM-A` = paste(as.character(FGM), as.character(FGA), sep="-"),
         `3PM-A` = paste(as.character(`3PM`), as.character(`3PA`), sep="-"),
         `FTM-A` = paste(as.character(FTM), as.character(FTA), sep="-"))

opponent_averages25 = 
  team_box25 %>%
  rbind(team_box25) %>%
  filter(Opponent %in% uaa_teams) %>%
  group_by(League, Opponent, Conference) %>%
  summarize(across(where(is.numeric), mean, na.rm=TRUE),
            .groups='drop') %>%
  mutate(`FG%` = 100 * FGM / FGA,
         `3P%` = 100 * `3PM` / `3PA`,
         `FT%` = 100 * FTM / FTA,
         POSS = 0.96 * (FGA + 0.44*FTA + TO - OREB),
         POSS_OPP = 0.96 * (FGA_OPP + 0.44*FTA_OPP + TO_OPP - OREB_OPP),
         `+/-` = PTS - PTS_OPP,
         `AST%` = 100 * AST / TO,
         `FT-R` = 100 * FTA / FGA,
         `3P-R` = 100 * `3PA` / `FGA`,
         `ORB%` = 100 * OREB / (OREB + DREB_OPP),
         `DRB%` = 100 * DREB / (DREB + OREB_OPP),
         `REB%` = 100 * REB / (REB + REB_OPP),
         `TO%` = 100 * TO / POSS,
         `POT %` = 100 * `Points off Turnovers` / PTS,
         `2ND %` = 100 * `2nd Chance Points` / PTS,
         `PITP %` = 100 * `Points in the Paint` / PTS,
         `FB %` = 100 * `Fastbreak Points` / PTS,
         `BENCH %` = 100 * `Bench Points` / PTS,
         across(where(is.numeric), ~ round(., 1)),
         PPP = round(PTS / POSS, 2),
         PPP_OPP = round(PTS_OPP / POSS_OPP, 2),
         NET = round(PPP - PPP_OPP, 2),
         `FGM-A` = paste(as.character(FGM), as.character(FGA), sep="-"),
         `3PM-A` = paste(as.character(`3PM`), as.character(`3PA`), sep="-"),
         `FTM-A` = paste(as.character(FTM), as.character(FTA), sep="-")) %>%
  merge(uaa_teams_csv, by.x="Opponent", by.y="Team", all.x=TRUE)

plays25 = 
  raw_plays25 %>%
  mutate(Score_copy = Score) %>%
  separate(Score_copy, into = c("AwayScore", "HomeScore"), sep = "-") %>%
  mutate(AwayScore = as.integer(AwayScore),
         HomeScore = as.integer(HomeScore),
         Minute = ceiling(Seconds / 60))

lineup_presences25 =
  plays25 %>%
  mutate(Team = AwayTeam,
         Opponent = HomeTeam,
         Location = "Away",
         Lineup = AwayLineup) %>%
  rbind( mutate(plays25, 
                Team = HomeTeam,
                Opponent = AwayTeam,
                Location = "Home",
                Lineup = HomeLineup) ) %>%
  filter(!is.na(Lineup),
         !Substitution) %>%
  group_by(League, Date, Team, Opponent, Location) %>%
  mutate(Regulation = case_when((League == "WBB") ~ 4,
                                TRUE ~ 2),
         OT = (Period > Regulation),
         Endgame = ifelse((OT), 
                          40*60 + 5*60*(max(Period) - Regulation), 
                          40*60)) %>%
  ungroup() %>%
  mutate(rolledLineup = lag(Lineup, n=1),
         NewLineup = replace_na((Lineup == rolledLineup), FALSE),
         Stint = cumsum(!NewLineup),
         rolledPeriod = lag(Period, n=1),
         unrolledPeriod = lead(Period, n=1),
         gameStart = replace_na((Period < rolledPeriod), TRUE),
         gameEnd = replace_na((Period > unrolledPeriod), TRUE)) %>%
  group_by(League, Date, Team, Opponent, Location, Lineup, Stint) %>%
  summarize(gameStart = any(gameStart),
            gameEnd = any(gameEnd),
            Enter = min(case_when(gameStart ~ 0, 
                                  TRUE ~ Seconds)),
            Exit = max(case_when(gameEnd ~ Endgame, 
                                 TRUE ~ Seconds)),
            .groups='drop') %>%
  ungroup() %>%
  arrange(Stint) %>%
  mutate(rolledExit = lag(Exit, n=1),
         Enter = ifelse((!gameStart),
                        ifelse((Enter != rolledExit),
                               rolledExit,
                               Enter),
                        Enter),
         Seconds = Exit - Enter) %>%
  select(-Stint, -gameStart, -gameEnd, -rolledExit) %>%
  filter(Enter != Exit)

player_presences25 =
  lineup_presences25 %>%
  separate_rows(Lineup, sep=", ") %>%
  mutate(Player = Lineup) %>%
  select(-Lineup)

time_played_df25 <-
  player_presences25 %>%
  group_by(League, Team, Player) %>% 
  summarize(Time = sum(Seconds),
            .groups='drop') %>%
  arrange(desc(Time))
rotation_order25 <- time_played_df25$Player

raw_possessions25 =
  plays25 %>%
  mutate(unrolledPeriod = lead(Period, n=1),
         rolledPeriod = lag(Period, n=1),
         periodStart = replace_na((Period != rolledPeriod), TRUE),
         periodEnd = replace_na((Period != unrolledPeriod), TRUE),
         Action = ifelse((is.na(AwayAction)), HomeAction, AwayAction),
         possEnd = (str_detect(Action, "REBOUND DEF") |
                      str_detect(Action, "REBOUND DEADB") |
                      str_detect(Action, "TURNOVER") |
                      str_detect(Action, "GOOD") | 
                      (periodEnd)),
         rolledSeconds = lag(Seconds, n=1),
         Stint = cumsum(possEnd),
         periodLength = ifelse((League == "WBB"), 10, 20),
         Regulation = ifelse((League == "WBB"), 4, 2),
         OT = (Period > Regulation),
         PeriodSeconds = ifelse((OT), 
                                40*60 + 5*60*(Period - 1), 
                                periodLength*60*(Period - 1)),
         PeriodSecondsEnd = ifelse((OT), 
                                   40*60 + 5*60*(Period - Regulation), 
                                   periodLength*60*Period)) %>%
  group_by(League, Date, Conference, AwayTeam, HomeTeam, AwayLineup, HomeLineup, Stint) %>%
  summarize(Start = min(case_when((periodStart) ~ PeriodSeconds,
                                  (n() == 1) ~ rolledSeconds,
                                  TRUE ~ Seconds)),
            End = max(case_when((periodEnd) ~ PeriodSecondsEnd,
                                (n() == 1) ~ Seconds,
                                TRUE ~ Seconds)),
            .groups='drop') %>%
  mutate(Length = End - Start) %>%
  filter(Start != End,
         Length >= 0)
possessions25 =
  raw_possessions25 %>%
  mutate(Team = AwayTeam,
         Opponent = HomeTeam,
         Location = "Away",
         Lineup = AwayLineup,
         OpponentLineup = HomeLineup) %>%
  rbind(mutate(raw_possessions25,
               Team = HomeTeam,
               Opponent = AwayTeam,
               Location = "Home",
               Lineup = HomeLineup,
               OpponentLineup = AwayLineup)) %>%
  # filter(!is.na(Lineup)) %>%
  select(League, Date, Team, Opponent, Conference, Location, 
         Lineup, OpponentLineup, Length)

team_box25 =
  team_box25 %>%
  merge(possessions25 %>%
          group_by(League, Date, Team, Opponent) %>%
          summarize(`Poss. Length` = round(mean(Length), 1),
                    .groups='drop'),
        on=c("League", "Date", "Team", "Opponent"),
        all.x=TRUE)
team_averages25 =
  team_averages25 %>%
  merge(possessions25 %>%
          group_by(League, Team, Conference) %>%
          summarize(`Poss. Length` = round(mean(Length), 1),
                    .groups='drop'),
        on=c("League", "Team", "Conference"),
        all.x=TRUE)
opponent_averages25 =
  opponent_averages25 %>%
  merge(possessions25 %>%
          group_by(League, Opponent, Conference) %>%
          summarize(`Poss. Length` = round(mean(Length), 1),
                    .groups='drop'),
        on=c("League", "Opponent", "Conference"),
        all.x=TRUE)

refresh_dates25 = function() {
  w_all_dates25 <<- 
    unique(c(get_shots("WBB", "Chicago", y=2025)$Date, 
             get_events("WBB", "Chicago", y=2025)$Date)) %>%
    date_filter_helper(y=2025)
  w_game_dates25 <<- unique(filter(team_box25, 
                                   League == "WBB",
                                   Team == "Chicago")$Date)
  w_practices25 <<- w_all_dates25[!(w_all_dates25 %in% w_game_dates25)]
  
  w_all_dates25 <<- 
    unique(c(get_shots("MBB", "Chicago", y=2025)$Date, 
             get_events("MBB", "Chicago", y=2025)$Date)) %>%
    date_filter_helper(y=2025)
  m_game_dates25 <<- unique(filter(team_box25, 
                                   League == "MBB",
                                   Team == "Chicago")$Date)
  m_practices25 <<- m_all_dates25[!(m_all_dates25 %in% m_game_dates25)]
}

refresh_dates25()
