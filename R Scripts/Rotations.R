minute_presence = function(df, minute) {
  new_df =
    df %>%
    mutate(start = pmax(Enter, 60*(minute - 1)),
           end = pmin(Exit, 60*minute),
           amount_played = pmax(0, end - start)) %>%
    select(-start, -end)
  
  colnames(new_df) = c(colnames(df), as.character(minute))
  
  new_df
}

get_titles = function(league, team, conf=F, y=2024) {
  if (y == 2024) {
    player_presences = player_presences24
  } else {
    player_presences = player_presences25
  }
  
  df =
    player_presences %>%
    mutate(Conference = (Opponent %in% uaa_teams)) %>%
    filter(League == league,
           Team == team,
           if (conf) Conference else TRUE) %>%
    group_by(Date, Location, Opponent) %>%
    slice_head(n=1) %>%
    mutate(Opponent = case_when((Location == "Away") ~ 
                                  paste("at", Opponent, sep=" "),
                                TRUE ~ 
                                  paste("vs", Opponent, sep=" ")),
           Title = paste(as.character(Date), Opponent, sep=" "))
  
  return( rev(df$Title) )
}

remove_consecutive_dupes = function(vec) {
  vec %>%
    lag() %>%
    {vec[is.na(.) | vec != .]}
}

season_rotation_df = function(league, team, margin, conf=F, y=2024) {
  if (y == 2024) {
    player_presences = player_presences24
    team_box = team_box24
    start_date="2023-10-01"
    end_date="2024-04-01"
  } else {
    player_presences = player_presences25
    team_box = team_box25
    start_date="2024-10-01"
    end_date="2025-04-01"
  }
  
  player_presences = 
    player_presences %>%
    mutate(Conference = (Opponent %in% uaa_teams)) %>%
    filter(if (conf) Conference else TRUE)
  
  df = 
    player_presences %>%
    merge(select(team_box,
                 League, Date, Team, Opponent, `+/-`), 
          on=c("League", "Date", "Team", "Opponent"), 
          all.x=TRUE) %>%
    mutate(Margin = `+/-`,
           Date = as.Date(Date, format="%Y-%m-%d")) %>%
    filter(League == league,
           Team == team,
           between(Date, 
                   as.Date(start_date, format="%Y-%m-%d"), 
                   as.Date(end_date, format="%Y-%m-%d")),
           Margin >= -margin,
           Margin <= margin)
  
  for (i in 1:50) {
    df = minute_presence(df, i)
  }
  
  df %>%
    select(Player, to_vec(for(i in 1:50) as.character(i))) %>%
    group_by(Player) %>%
    summarize(across(everything(), sum),
              .groups='drop') %>%
    mutate_at(to_vec(for(i in 1:50) as.character(i)), ~ . / 
                length(unique(df$Date))) %>%
    ungroup() %>%
    pivot_longer(cols=`1`:`50`) %>%
    mutate(minute = as.integer(name),
           Type = "season")
}

game_rotation_df = function(league, team, date, y=2024) {
  if (y == 2024) {
    player_presences = player_presences24
  } else {
    player_presences = player_presences25
  }
  
  game = 
    player_presences %>%
    filter(League == league,
           Team == team,
           Date == as.Date(date, format="%Y-%m-%d"))
  
  for (i in 1:50) {
    game = minute_presence(game, i)
  }
  
  game %>% 
    select(Player, to_vec(for(i in 1:50) as.character(i))) %>%
    group_by(Player) %>% 
    summarise(across(everything(), sum),
              .groups='drop') %>%
    ungroup() %>%
    pivot_longer(cols=`1`:`50`) %>%
    mutate(minute = as.integer(name),
           Type = "game")
}

player_rotation_df = function(league, team, player, conf=F, y=2024) {
  if (y == 2024) {
    player_presences = player_presences24
  } else {
    player_presences = player_presences25
  }
  
  team_df =
    player_presences %>%
    mutate(Conference = (Opponent %in% uaa_teams)) %>%
    filter(League == league,
           Team == team,
           if (conf) Conference else TRUE) %>%
    arrange(Date, Enter)
  
  player_df = 
    team_df %>%
    filter(Player == player)
  
  team_df = 
    team_df %>%
    group_by(Date, Opponent) %>%
    slice_head(n=1) %>%
    ungroup() %>%
    select(Date, Opponent) %>%
    mutate(Enter = 0, Exit = 0)
  
  for (i in 1:50) {
    team_df = minute_presence(team_df, i)
    player_df = minute_presence(player_df, i)
  }
  
  player_df = 
    player_df %>% 
    select(Date, Opponent, to_vec(for(i in 1:50) as.character(i))) %>%
    rbind( select(team_df, -Enter, -Exit) ) %>%
    group_by(Date, Opponent) %>% 
    summarise(across(everything(), sum),
              .groups='drop') %>%
    ungroup()
  
  player_df %>%
    replace(is.na(.), 0) %>%
    arrange(Date) %>%
    pivot_longer(cols=`1`:`50`) %>%
    mutate(minute = as.integer(name),
           Type = "player")
}

lineup_rotation_df = function(league, team, on_players, off_players, 
                              conf=F, y=2024) {
  if (y == 2024) {
    lineup_presences = lineup_presences24
  } else {
    lineup_presences = lineup_presences25
  }
  
  lineup_presences =
    lineup_presences %>%
    mutate(Conference = (Opponent %in% uaa_teams)) %>%
    filter(if (conf) Conference else TRUE)
  
  team_df = 
    lineup_presences %>%
    filter(League == league,
           Team == team)
  
  lineup_df = team_df
  for (p in on_players) {
    lineup_df = filter(lineup_df, str_detect(Lineup, p))
  }
  for (p in off_players) {
    lineup_df = filter(lineup_df, !str_detect(Lineup, p))
  }
  
  team_df = 
    team_df %>%
    group_by(Date, Opponent) %>%
    slice_head(n=1) %>%
    ungroup() %>%
    select(Date, Opponent) %>%
    mutate(Enter = 0, Exit = 0)
  
  for (i in 1:50) {
    team_df = minute_presence(team_df, i)
    lineup_df = minute_presence(lineup_df, i)
  }
  
  lineup_df = 
    lineup_df %>% 
    select(Date, Opponent, to_vec(for(i in 1:50) as.character(i))) %>%
    rbind( select(team_df, -Enter, -Exit) ) %>%
    group_by(Date, Opponent) %>% 
    summarise(across(everything(), sum),
              .groups='drop') %>%
    ungroup()
  
  lineup_df %>%
    replace(is.na(.), 0) %>%
    arrange(Date) %>%
    pivot_longer(cols=`1`:`50`) %>%
    mutate(minute = as.integer(name),
           Type = "lineup")
}
rotation_chart = function(type, league, team,
                           player="", on=c(), off=c(), 
                           date="",  start_date="", end_date="", 
                           margin=100, conf=F, y=2024) {
  if (y == 2024) {
    rotation_order = rotation_order24
    player_presences = player_presences24
  } else {
    rotation_order = rotation_order25
    player_presences = player_presences25
  }
  
  if (type == "season") {
    df = season_rotation_df(league, team, margin, conf=conf, y=y)
  } else if (type == "game") {
    df = game_rotation_df(league, team, date, y=y)
  } else if (type == "player") {
    df = player_rotation_df(league, team, player, conf=conf, y=y)
  } else if (type == "lineup") {
    df = lineup_rotation_df(league, team, on, off, conf=conf, y=y)
  }
  
  if (max(filter(df, value != 0)$minute) == 40) {
    ot = 0
  } else if (max(filter(df, value != 0)$minute) == 45) {
    ot = 1
  } else {
    ot = 2
  }
  
  if (league == "MBB") {
    min_breaks = c(0.5, 20.5)
    min_labels = c("H1", "H2")
  } else {
    min_breaks = c(0.5, 10.5, 20.5, 30.5)
    min_labels = c("Q1", "Q2", "Q3", "Q4")
  }
  
  if (ot == 1) {
    min_limits = c(0, 46)
    min_breaks = c(min_breaks, 40.5)
    min_labels = c(min_labels, "OT")
  } else if (ot == 2) {
    min_limits = c(0, 51)
    min_breaks = c(min_breaks, 40.5, 45.5)
    min_labels = c(min_labels, "OT", "2OT")
  } else {
    min_limits = c(0, 41)
  }
  
  if (type %in% c("season", "game")) {
    x_aes = "Player"
    axis_sort = rotation_order[rotation_order %in% unique(df$Player)]
    y_labels = rev(axis_sort)
  } else {
    df = mutate(df, Date = as.character(Date))
    x_aes = "Date"
    axis_sort = unique(df$Date)
    y_labels = get_titles(league, team, y=y, conf=conf)
  }
  
  color = paste0("#", filter(uaa_teams_csv, Team == team)$`Color Primary`)
  
  plot =
    df %>%
    ggplot(aes(x=minute,
               y=.data[[x_aes]],
               fill=value)) +
    geom_tile(height=0.75,
              linewidth=0.1,
              color='lightgray',
              show.legend=FALSE) +
    geom_vline(xintercept=0.5,
               linewidth=1) +
    geom_vline(xintercept=20.5,
               linewidth=1) +
    geom_vline(xintercept=40.5,
               linewidth=1) +
    scale_x_continuous(limits=min_limits,
                       breaks=min_breaks,
                       labels=min_labels) +
    scale_y_discrete(limits=rev(axis_sort),
                     labels=y_labels) +
    scale_fill_gradient(low="white", 
                        high=color,
                        limits=c(0, 60)) +
    labs(x="", y="") +
    theme_classic() +
    theme(axis.text=element_text(size=12))
  
  if (league == "WBB") {
    plot =
      plot +
      geom_vline(xintercept=10.5,
                 linewidth=1) +
      geom_vline(xintercept=30.5,
                 linewidth=1)
  }
  
  if (ot == 1) {
    plot +
      geom_vline(xintercept=45.5,
                 linewidth=1)
  } else if (ot == 2) {
    plot +
      geom_vline(xintercept=45.5,
                 linewidth=1) +
      geom_vline(xintercept=50.5,
                 linewidth=1)
  } else {
    plot
  }
}