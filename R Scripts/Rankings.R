team_rankings = function(league, opp=F, adv=F, misc=F, conf=F, y=2024) {
  if (y == 2024) {
    team_averages = team_averages24
    opponent_averages = opponent_averages24
  } else {
    team_averages = team_averages25
    opponent_averages = opponent_averages25
  }
  
  if (opp) {
    df = opponent_averages %>%
      mutate(Team = Opponent)
  } else {
    df = team_averages
  }
  
  df %>%
    filter(League == league,
           Conference == conf) %>%
    select(URL, Team,
           if ((!adv) & (!misc)) trad_cols else c(),
           if (adv) adv_cols else c(),
           if (misc) misc_cols else c()) %>%
    gt() %>%
    cols_label(URL = "") %>%
    fmt_image(URL, height=30) %>%
    gt_shading() %>%
    cols_width(Team ~ px(134),
               ends_with("-A") ~ px(90),
               everything() ~ px(82)) %>%
    opt_interactive(use_search=TRUE,
                    use_highlight=TRUE)
}

player_rankings = function(league, min=0, conf=F, y=2024) {
  if (y == 2024) {
    player_averages = player_averages24
  } else {
    player_averages = player_averages25
  }
  
  player_averages %>%
    filter(League == league,
           Conference == conf,
           MIN >= min) %>%
    replace(is.na(.), 0) %>%
    arrange(desc(PTS)) %>%
    select(URL, Team, Player, player_cols) %>%
    gt() %>%
    cols_label(URL = "") %>%
    fmt_image(URL, height=30) %>%
    gt_shading() %>%
    cols_width(Team ~ px(120),
               Player ~ px(150),
               ends_with("-A") ~ px(90),
               ends_with("%") ~ px(82),
               everything() ~ px(75)) %>%
    opt_interactive(use_search=TRUE,
                    use_highlight=TRUE,
                    use_page_size_select=TRUE,
                    page_size_values=c(10, 20, 30))
}

assist_rankings = function(league, conf=F, y=2024) {
  if (y == 2024) {
    plays = plays24
  } else {
    plays = plays25
  }
  
  assist_plays =
    plays %>%
    rowwise() %>%
    mutate(Team = if (is.na(AwayAction)) HomeTeam else AwayTeam,
           Opponent = if (is.na(AwayAction)) AwayTeam else HomeTeam,
           Action = if (is.na(AwayAction)) HomeAction else AwayAction) %>%
    ungroup() %>%
    mutate(rolledAction = lead(Action, n=1),
           `Assist?` = str_detect(lead(Action, n=1), "ASSIST")) %>%
    filter(`Assist?`) %>%
    rowwise() %>%
    mutate(Scorer = extract_player(Action),
           Assister = extract_player(rolledAction)) %>%
    ungroup() %>%
    select(League, Date, Team, Opponent, Conference, Scorer, Assister)
  
  assist_plays %>%
    filter(League == league,
           if (conf) Conference else TRUE) %>%
    group_by(League, Team, Conference, Assister, Scorer) %>%
    summarize(Assists = n(),
              .groups='drop') %>%
    ungroup() %>%
    merge(select(uaa_teams_csv, Team, URL), by="Team", all.x=T) %>%
    arrange(desc(Assists)) %>%
    select(URL, Team, Assister, Scorer, Assists) %>%
  gt() %>%
  cols_label(URL = "") %>%
  fmt_image(URL, height=30) %>%
  cols_width(Scorer ~ px(150),
             Assister ~ px(150),
             everything() ~ px(82)) %>%
  opt_interactive(use_search=TRUE,
                  use_highlight=TRUE,
                  use_page_size_select=TRUE,
                  page_size_default=10)
}