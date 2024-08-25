get_poss_length = function(on_players, off_players, lineup="", y=2024) {
  if (y == 2024) {
    possessions = possessions24
  } else {
    possessions = possessions25
  }
  
  if (lineup != "") {
    return( mean(filter(possessions, Lineup == lineup)$Length) )
  } else {
    # Filter for present players
    for (p in on_players) {
      possessions = filter(possessions, str_detect(Lineup, p))
    }
    # Filter for absent players
    for (p in off_players) {
      possessions = filter(possessions, !str_detect(Lineup, p))
    }
    
    return( mean(possessions$Length) )
  }
}

# Return all lineups with on/off players
splits_helper = function(league, team, on_players, off_players,
                         opp=F, adv=F, conf=F, y=2024) {
  if (y == 2024) {
    plays = plays24
    lineup_presences = lineup_presences24
    possessions = possessions24
  }  else {
    plays = plays25
    lineup_presences = lineup_presences25
    possessions = possessions25
  }
  
  plays = 
    plays %>%
    mutate(Team = AwayTeam,
           Action = AwayAction,
           OpponentAction = HomeAction,
           Lineup = AwayLineup,
           OpponentLineup = HomeLineup) %>%
    rbind(plays %>%
            mutate(Team = HomeTeam,
                   Action = HomeAction,
                   OpponentAction = AwayAction,
                   Lineup = HomeLineup,
                   OpponentLineup = AwayLineup)) %>%
    filter(League == league, 
           if (conf) Conference else TRUE)
  
  # Filter for present players
  for (p in on_players) {
    plays = 
      filter(plays, 
             str_detect(if (opp) OpponentLineup else Lineup, p))
    lineup_presences = 
      filter(lineup_presences, 
             str_detect(Lineup, p))
    possessions = 
      filter(possessions, 
             str_detect(if (opp) OpponentLineup else Lineup, p))
  }
  # Filter for absent players
  for (p in off_players) {
    plays = 
      filter(plays, 
             League == league,
             Team == team,
             !str_detect(if (opp) OpponentLineup else Lineup, p))
    lineup_presences = 
      filter(lineup_presences, 
             League == league,
             Team == team,
             !str_detect(Lineup, p))
    possessions = 
      filter(possessions, 
             League == league,
             Team == team,
             !str_detect(if (opp) OpponentLineup else Lineup, p))
  }
  
  if ((length(on_players) == 0) & (length(off_players) > 0)) {
    players_list = paste0("OFF: ",
                          paste(off_players, collapse=", "))
  } else if ((length(on_players) > 0) & (length(off_players) == 0)) {
    players_list = paste0("ON: ", 
                          paste(on_players, collapse=", "))
  } else if ((length(on_players) > 0) & (length(off_players) > 0)) {
    players_list = paste0("ON: ", 
                          paste(on_players, collapse=", "),
                          " | OFF: ",
                          paste(off_players, collapse=", "))
  } else {
    players_list = ""
  }
  
  plays %>%
    mutate(FGM = ((str_detect(Action, "GOOD")) & 
                    (!str_detect(Action, "FT"))),
           FGM_OPP = ((str_detect(OpponentAction, "GOOD")) & 
                        (!str_detect(OpponentAction, "FT"))),
           FGA = (((str_detect(Action, "GOOD")) | 
                     (str_detect(Action, "MISS"))) & 
                    (!str_detect(Action, "FT"))),
           FGA_OPP = (((str_detect(OpponentAction, "GOOD")) | 
                         (str_detect(OpponentAction, "MISS"))) & 
                        (!str_detect(OpponentAction, "FT"))),
           FTM = (str_detect(Action, "GOOD FT")),
           FTM_OPP = (str_detect(OpponentAction, "GOOD FT")),
           FTA = ((str_detect(Action, "GOOD FT")) | 
                    (str_detect(Action, "MISS FT"))),
           FTA_OPP = ((str_detect(OpponentAction, "GOOD FT")) | 
                        (str_detect(OpponentAction, "MISS FT"))),
           `3PM` = (str_detect(Action, "GOOD 3PTR")),
           `3PM_OPP` = (str_detect(OpponentAction, "GOOD 3PTR")),
           `3PA` = ((str_detect(Action, "GOOD 3PTR")) | 
                      (str_detect(Action, "MISS 3PTR"))),
           AST = (str_detect(Action, "ASSIST")),
           OREB = (str_detect(Action, "REBOUND OFF")),
           OREB_OPP = (str_detect(OpponentAction, "REBOUND OFF")),
           DREB = (str_detect(Action, "REBOUND DEF")),
           DREB_OPP = (str_detect(OpponentAction, "REBOUND DEF")),
           TO = (str_detect(Action, "TURNOVER")),
           TO_OPP = (str_detect(OpponentAction, "TURNOVER"))) %>%
    summarize(across(where(is.logical), sum, na.rm=TRUE)) %>%
    mutate(Lineup = players_list,
           MIN = round(sum(lineup_presences$Seconds) / 60, 1),
           PTS = 2*FGM + `3PM` + FTM,
           PTS_OPP = 2*FGM_OPP + `3PM_OPP` + FTM_OPP,
           `FG%` = 100 * FGM / FGA,
           `3P%` = 100 * `3PM` / `3PA`,
           `FT%` = 100 * FTM / FTA,
           REB = OREB + DREB,
           REB_OPP = OREB_OPP + DREB_OPP,
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
           across(where(is.numeric), ~ round(., 1)),
           PPP = round(PTS / POSS, 2),
           PPP_OPP = round(PTS_OPP / POSS_OPP, 2),
           NET = round(PPP - PPP_OPP, 2),
           `FGM-A` = paste(as.character(FGM), as.character(FGA), sep="-"),
           `3PM-A` = paste(as.character(`3PM`), as.character(`3PA`), sep="-"),
           `FTM-A` = paste(as.character(FTM), as.character(FTA), sep="-"),
           `Poss. Length` = round(mean(possessions$Length), 1)) %>%
    select(Lineup, MIN,
           if (!adv) trad_cols else adv_cols)
}

# Return aggregate on/off splits (input on and off players, output gt)
lineup_splits = function(league, team, on_players, off_players, 
                         opp=F, adv=F, conf=F, y=2024) {
  if ((length(on_players) == 0) & (length(off_players) > 0)) {
    df = splits_helper(league, team, c(), off_players, 
                       opp=opp, adv=adv, conf=conf, y=y)
  } else if ((length(on_players) > 0) & (length(off_players) == 0)) {
    df = splits_helper(league, team, on_players, c(), 
                       opp=opp, adv=adv, conf=conf, y=y)
  } else if ((length(on_players) > 0) & (length(off_players) > 0)) {
    df = rbind(
      splits_helper(league, team, on_players, c(), 
                    opp=opp, adv=adv, conf=conf, y=y),
      splits_helper(league, team, on_players, off_players, 
                    opp=opp, adv=adv, conf=conf, y=y),
      splits_helper(league, team, c(), off_players, 
                    opp=opp, adv=adv, conf=conf, y=y)
    ) 
  } else {
    df = splits_helper(league, team, c(), c(), 
                       opp=opp, adv=adv, conf=conf, y=y)
  }
  
  df %>%
    gt() %>%
    gt_shading() %>%
    cols_width(Lineup ~ px(300),
               ends_with("-A") ~ px(90),
               ends_with("%") ~ px(82),
               everything() ~ px(75)) %>%
    tab_header(title="Aggregate Lineup Stats") %>%
    opt_interactive(use_search=TRUE,
                    use_highlight=TRUE,
                    use_page_size_select=TRUE,
                    page_size_values=c(10, 20, 30))
}
