get_players = function(league, team, y=2024) {
  if ((league == "MBB") & (team == "Chicago") & (y == 2025)) {
    return(data.frame(Player=m_roster25,
                      MIN = seq(1:length(m_roster25))))
  } else if ((league == "WBB") & (team == "Chicago") & (y == 2025)) {
    return(data.frame(Player=w_roster25,
                      MIN = seq(1:length(w_roster25))))
  } else if ((league == "MBB") & (team == "Non-UAA Scout")) {
    return(data.frame(Player=m_non_conf_opponents))
  } else if ((league == "WBB") & (team == "Non-UAA Scout")) {
    return(data.frame(Player=w_non_conf_opponents))
  }
  
  if (y == 2024) {
    player_box = player_box24
  } else {
    player_box = player_box25
  }
  
  team_df = 
    player_box %>%
    filter(League == league,
           Team == team) %>%
    group_by(Player) %>%
    summarize(`#` = as.integer(first(`#`)),
              MIN = sum(MIN)) %>%
    ungroup() %>%
    arrange(`#`)
  
  return( team_df )
}

retrieve_dates = function(league, team, date, y=2024) {
  if ((!str_detect(date, "All")) & !(str_detect(date, "UAA"))) {
    return( c( str_split(date, " ")[[1]][1] ) )
  }
  
  if (y == 2024) {
    w_practices = w_practices24
    w_game_dates = w_game_dates24
    m_practices = m_practices24
    m_game_dates = m_game_dates24
  } else {
    w_practices = w_practices25
    w_game_dates = w_game_dates25
    m_practices = m_practices25
    m_game_dates = m_game_dates25
  }
  
  if (team == "Chicago") {
    if (date == "All Practices") {
      dates = (if (league == "WBB") w_practices else m_practices)
    } else if (date == "UAA Practices") {
      dates = (if (league == "WBB") w_practices else m_practices)
      dates = dates[grep(if (y == 2024) "2024" else "2025", dates)]
    } else if (date == "Non-UAA Practices") {
      dates = (if (league == "WBB") w_practices else m_practices)
      dates = dates[grep(if (y == 2024) "2023" else "2024", dates)]
    } else if (date == "UAA Games") {
      dates = (if (league == "WBB") w_game_dates else m_game_dates)
      dates = dates[grep(if (y == 2024) "2024" else "2025", dates)]
    } else if (date == "Non-UAA Games") {
      dates = (if (league == "WBB") w_game_dates else m_game_dates)
      dates = dates[grep(if (y == 2024) "2023" else "2024", dates)]
    } else if (date == "All Games") {
      dates = (if (league == "WBB") w_game_dates else m_game_dates)
    } else {
      dates = c(date)
    }
  } else {
    if (str_detect(date, "All")) {
      dates = unique(c(get_shots(league, team, y=y)$Date, 
                       get_events(league, team, y=y)$Date)) %>%
        date_filter_helper(y=y)
    } else {
      dates = c(date)
    }
  }
  
  return(dates)
}

box_score = function(league, team, players, date="All Practices", y=2024) {
  dates = retrieve_dates(league, team, date, y=y)
  
  get_shots(league, team, y=y) %>%
    filter(Date %in% dates,
           Player %in% players) %>%
    group_by(Player) %>%
    summarize(`2PM` = sum(!(Region %in% regions3) & 
                            (Outcome == "Make")),
              `2PA` = sum(!(Region %in% regions3) &
                            (!str_detect(Outcome, "Foul"))),
              `3PM` = sum((Region %in% regions3) & 
                            (Outcome == "Make")),
              `3PA` = sum((Region %in% regions3) & 
                            (!str_detect(Outcome, "Foul"))),
              FGM = `2PM` + `3PM`,
              FGA = `2PA` + `3PA`,
              `Fouls` = sum(str_detect(Outcome, "Foul")),
              FTM = sum(Outcome == "Foul (+1)") + 
                2*sum(Outcome == "Foul (+2)") + 
                3*sum(Outcome == "Foul (+3)") + 
                2*sum(Outcome == "Foul"),
              PTS = 2*`2PM` + 3*`3PM` + FTM,
              `Paint PTS` = 2 * sum((Region == "Paint") & 
                                      (Outcome == "Make"))) %>%
    ungroup() %>%
    adorn_totals("row") %>%
    mutate(`Drive PTS` = `Paint PTS` + FTM,
           `Drive %` = 100 * `Drive PTS` / PTS,
           `2P` = paste(as.character(`2PM`), as.character(`2PA`), sep="-"),
           `2P%` = 100 * `2PM` / `2PA`,
           `3P` = paste(as.character(`3PM`), as.character(`3PA`), sep="-"),
           `3P%` = 100 * `3PM` / `3PA`,
           `3P-R` = 100 * `3PA` / FGA,
           `FG` = paste(as.character(FGM), as.character(FGA), sep="-"),
           `FG%` = 100 * FGM / FGA,
           `Foul %` = Fouls / (Fouls + FGA),
           across(where(is.numeric), ~ round(., 1)),
           PPS = round(PTS / (FGA + Fouls), 2)) %>%
    merge(
      (get_events(league, team, y=y) %>%
         filter(Date %in% dates,
                Player %in% players) %>%
         group_by(Player) %>%
         summarize(OREB = sum(OREB),
                   DREB = sum(DREB),
                   REB = OREB + DREB,
                   AST = sum(AST),
                   TO = sum(TO)) %>%
         ungroup() %>%
         adorn_totals("row") %>%
         mutate(`AST/TO` = round(AST / TO, 2))),
      on="Player",
      all=TRUE
    ) %>%
    select(c("Player", "2P", "2P%", "3P", "3P%", "3P-R", 
             "FG", "FG%", "PTS", "PPS", 
             "Fouls", "FTM", "Foul %", "Paint PTS", "Drive PTS", "Drive %",
             "OREB", "DREB", "REB", "AST", "TO", "AST/TO")) %>%
    gt() %>%
    gt_shading %>%
    cols_width(Player ~ "154px",
                   contains("AST/TO") ~ "85px",
                   everything() ~ "80px") %>%
    tab_style(style=list(cell_text(weight="bold")),
              locations = cells_body(rows = (Player == "Total"))) %>%
    opt_interactive(use_highlight=TRUE,
                    page_size_default=20)
}

box_log = function(league, team, players, date="All Practices", y=2024) {
  dates = retrieve_dates(league, team, date, y=y)
  
  get_shots(league, team, y=y) %>%
    filter(Date %in% dates,
           Player %in% players,
           `Shot Type` %in% shot_types) %>%
    group_by(Date) %>%
    summarize(`2PM` = sum(!(Region %in% regions3) & 
                            (Outcome == "Make")),
              `2PA` = sum(!(Region %in% regions3) & 
                            (!str_detect(Outcome, "Foul"))),
              `3PM` = sum((Region %in% regions3) & 
                            (Outcome == "Make")),
              `3PA` = sum((Region %in% regions3) & 
                            (!str_detect(Outcome, "Foul"))),
              FGM = `2PM` + `3PM`,
              FGA = `2PA` + `3PA`,
              `Fouls` = sum(str_detect(Outcome, "Foul")),
              FTM = sum(Outcome == "Foul (+1)") + 
                2*sum(Outcome == "Foul (+2)") + 
                3*sum(Outcome == "Foul (+3)") + 
                2*sum(Outcome == "Foul"),
              PTS = 2*`2PM` + 3*`3PM` + FTM,
              `Paint PTS` = 2 * sum((Region == "Paint") & 
                                      (Outcome == "Make"))) %>%
    ungroup() %>%
    adorn_totals("row") %>%
    mutate(`Drive PTS` = `Paint PTS` + FTM,
           `Drive %` = 100 * `Drive PTS` / PTS,
           `2P` = paste(as.character(`2PM`), as.character(`2PA`), sep="-"),
           `2P%` = 100 * `2PM` / `2PA`,
           `3P` = paste(as.character(`3PM`), as.character(`3PA`), sep="-"),
           `3P%` = 100 * `3PM` / `3PA`,
           `3P-R` = 100 * `3PA` / FGA,
           `FG` = paste(as.character(FGM), as.character(FGA), sep="-"),
           `FG%` = 100 * FGM / FGA,
           across(where(is.numeric), ~ round(., 1))) %>%
    merge(
      (get_events(league, team, y=y) %>%
         filter(Date %in% dates,
                Player %in% players) %>%
         group_by(Date) %>%
         summarize(OREB = sum(OREB),
                   DREB = sum(DREB),
                   REB = OREB + DREB,
                   AST = sum(AST),
                   TO = sum(TO)) %>%
         ungroup() %>%
         adorn_totals("row") %>%
         mutate(`AST/TO` = round(AST / TO, 2))),
      on="Date",
      all.x=TRUE
    ) %>%
    gt()
}

turnover_table = function(league, team, players, dates) {
  get_turnovers(league, team) %>%
    filter(Player %in% players,
           Date %in% dates) %>%
    mutate(`Type` = TO) %>%
    select(-TO) %>%
    group_by(`Type`) %>%
    summarize(Total = n()) %>%
    ungroup() %>%
    mutate(Frequency = round(100 * Total / sum(Total), 1)) %>%
    arrange(desc(Frequency)) %>%
    gt() %>%
    cols_width(Type ~ "150px",
               Frequency ~ "120px",
               contains("AST/TO") ~ "85px",
               everything() ~ "75px") %>%
    opt_interactive(use_highlight=TRUE,
                    page_size_default=20)
}

shooting_table = function(league, team, date, type, 
                          min_shots, player=c(), y=2024) {
  if ((length(player) == 0) | ("All Players" %in% player)) {
    players = get_players(league, team, y=y)$Player
  } else {
    players = player
  }
  
  dates = retrieve_dates(league, team, date, y=y)
  
  if (type == "Turnovers") {
    return( turnover_table(league, team, players, dates) )
  }
  
  type = (if (type == "Shots") "Shot Type" else type)
  
  get_shots(league, team, y=y) %>%
    filter(Player %in% players,
           Date %in% dates) %>%
    group_by(across(all_of(c(type)))) %>%
    summarize(`2PM` = sum(!(Region %in% regions3) & 
                            (Outcome == "Make")),
              `2PA` = sum(!(Region %in% regions3) & 
                            (!str_detect(Outcome, "Foul"))),
              `3PM` = sum((Region %in% regions3) & 
                            (Outcome == "Make")),
              `3PA` = sum((Region %in% regions3) & 
                            (!str_detect(Outcome, "Foul"))),
              FGM = `2PM` + `3PM`,
              FGA = `2PA` + `3PA`,
              PTS = 2*`2PM` + 3*`3PM`,
              `2P` = paste(as.character(`2PM`), as.character(`2PA`), sep="-"),
              `2P%` = 100 * `2PM` / `2PA`,
              `3P` = paste(as.character(`3PM`), as.character(`3PA`), sep="-"),
              `3P%` = 100 * `3PM` / `3PA`,
              `FG` = paste(as.character(FGM), as.character(FGA), sep="-"),
              `FG%` = 100 * FGM / FGA,
              `Fouls` = sum(str_detect(Outcome, "Foul"))) %>%
    ungroup() %>%
    replace(is.na(.), 0) %>%
    mutate(across(where(is.numeric), \(x) round(x, 1)),
           Splits = paste(as.character(FGM), as.character(FGA), sep="-"),
           PTS = PTS + `Fouls`*2,
           PPS = round(PTS / (FGA + `Fouls`), 2),
           Fouls = `Fouls`,
           Frequency = round(100 *(FGA + Fouls) / sum(FGA + Fouls), 1)) %>%
    replace(is.na(.), 0) %>%
    filter(FGA >= min_shots) %>%
    select(all_of(c(type)), Splits, Fouls, Frequency, `FG%`, PPS) %>%
    arrange(desc(Frequency)) %>%
    gt() %>%
    gt_shading() %>%
    cols_width(contains("Region") ~ "130px",
               contains("Shot Type") ~ "130px",
               contains("AST/TO") ~ "85px",
               Splits ~ "80px",
               Frequency ~ "110px",
               everything() ~ "75px") %>%
    opt_interactive(use_highlight=TRUE,
                    page_size_default=10,
                    use_page_size_select=TRUE,
                    page_size_values=c(10, 20, 30))
}

player_turnover_table = function(league, team, players, dates) {
  get_turnovers(league, team) %>%
    filter(Player %in% players,
           Date %in% dates) %>%
    mutate(`Type` = TO) %>%
    select(-TO) %>%
    group_by(Player, `Type`) %>%
    summarize(Total = n(),
              .groups='drop') %>%
    ungroup() %>%
    mutate(Frequency = round(100 * Total / sum(Total), 1)) %>%
    arrange(desc(Frequency)) %>%
    gt() %>%
    cols_width(Player ~ "154px",
               Type ~ "150px",
               Frequency ~ "120px",
               contains("AST/TO") ~ "85px",
               everything() ~ "75px") %>%
    opt_interactive(use_highlight=TRUE,
                    use_search=TRUE,
                    page_size_default=20)
}

player_shot_type_table = function(league, team, date, type, 
                                  min_shots, player=c(), y=2024) {
  dates = retrieve_dates(league, team, date, y=y)
  
  if ((length(player) == 0) | ("All Players" %in% players)) {
    players = get_players(league, team, y=y)$Player
  } else {
    players = player
  }
  
  if (type == "Turnovers") {
    return( player_turnover_table(league, team, players, dates) )
  } else {
    type = (if (type == "Shots") "Shot Type" else type)
  }
  
  get_shots(league, team, y=y) %>%
    filter(Date %in% dates,
           Player %in% players) %>%
    group_by(Player, across(type)) %>%
    summarize(`2PM` = sum(!(Region %in% regions3) & 
                            (Outcome == "Make")),
              `2PA` = sum(!(Region %in% regions3) & 
                            (!str_detect(Outcome, "Foul"))),
              `3PM` = sum((Region %in% regions3) & 
                            (Outcome == "Make")),
              `3PA` = sum((Region %in% regions3) & 
                            (!str_detect(Outcome, "Foul"))),
              FGM = `2PM` + `3PM`,
              FGA = `2PA` + `3PA`,
              PTS = 2*`2PM` + 3*`3PM`,
              `2P` = paste(as.character(`2PM`), as.character(`2PA`), sep="-"),
              `2P%` = 100 * `2PM` / `2PA`,
              `3P` = paste(as.character(`3PM`), as.character(`3PA`), sep="-"),
              `3P%` = 100 * `3PM` / `3PA`,
              `FG` = paste(as.character(FGM), as.character(FGA), sep="-"),
              `FG%` = 100 * FGM / FGA,
              `Fouls` = sum(str_detect(Outcome, "Foul")),
              .groups='drop') %>%
    ungroup() %>%
    group_by(Player) %>%
    mutate(Frequency = 100 * (FGA + Fouls) / (sum(FGA) + sum(Fouls))) %>%
    ungroup() %>%
    replace(is.na(.), 0) %>%
    mutate(across(where(is.numeric), \(x) round(x, 1)),
           Splits = paste(as.character(FGM), as.character(FGA), sep="-"),
           PTS = PTS + `Fouls`*2,
           PPS = round(PTS / (FGA + `Fouls`), 2),
           Fouls = `Fouls`) %>%
    replace(is.na(.), 0) %>%
    filter(FGA >= min_shots) %>%
    select(Player, all_of(type), Splits, Fouls, Frequency, `FG%`, PPS) %>%
    arrange(desc(PPS)) %>%
    gt() %>%
    gt_shading() %>%
    cols_width(contains("Region") ~ "130px",
               Player ~ "154px",
               Splits ~ "80px",
               contains("Shot Type") ~ "130px",
               contains("AST/TO") ~ "85px",
               everything() ~ "75px") %>%
    opt_interactive(use_highlight=TRUE,
                    use_search=TRUE,
                    page_size_default=10,
                    use_page_size_select=TRUE,
                    page_size_values=c(10, 20, 30))
}

shot_plot = function(league, team, date, players, shot_types, y=2024) {
  dates = retrieve_dates(league, team, date, y=y)
  
  get_shots(league, team, y=y) %>%
    filter(Player %in% players,
           Date %in% dates,
           `Shot Type` %in% shot_types) %>%
    shot_chartly()
}

hex_map = function(league, team, date, players, shot_types, y=2024) {
  dates = retrieve_dates(league, team, date, y=y)
  
  df =
    get_shots(league, team, y=y) %>%
    filter(Player %in% players,
           Date %in% dates,
           `Shot Type` %in% shot_types)
  
  labels =
    df %>%
    filter(!str_detect(Outcome, "Foul")) %>%
    group_by(Region) %>%
    summarize(FGM = sum(Outcome == "Make"),
              Misses = sum(Outcome == "Miss"),
              FGA = (FGM + Misses)) %>%
    ungroup() %>%
    mutate(`FG%` = paste0(round(100 * FGM / FGA), "%"),
           Splits = paste(as.character(FGM), as.character(FGA), sep="-")) %>%
    merge(region_labels,
          by="Region",
          all.x=TRUE)
  
  (df %>%
      filter(Outcome == "Make") %>%
      ggplot(aes(x=x, y=y)) +
      geom_hex(bins=20,
               alpha=0.75,
               show.legend=FALSE) +
      geom_label(
        data=labels, 
        aes(x=x, y=y, label=`FG%`),
        size=5,
        color='black',
        fill = alpha(c("white"), 0.5)) +
      scale_fill_continuous(low="lightblue", high="darkblue")) %>%
    geom_half_court()
}

heat_map = function(league, team, date, players, shot_types, miss=F, y=2024) {
  dates = retrieve_dates(league, team, date, y=y)
  
  raw_df =
    get_shots(league, team, y=y) %>%
    filter(Player %in% players,
           Date %in% dates,
           `Shot Type` %in% shot_types)
  
  threshold_count = 
    (raw_df %>%
       filter(Outcome == "Make") %>%
       group_by(Region) %>%
       summarize(FGM = n()) %>%
       ungroup() %>%
       arrange(desc(FGM)))$FGM %>%
    first()
  
  df = 
    raw_df %>%
    filter(Outcome == "Make") %>%
    group_by(Region) %>%
    group_modify(~ {
      current_count = nrow(.)
      if (current_count < threshold_count) {
        repeat_factor = ceiling(threshold_count / current_count)
        .[rep(seq_len(nrow(.)), each = repeat_factor), ]
      } else {
        .
      }
    })
  
  double_regions =
    (raw_df %>%
       group_by(Region) %>%
       summarize(FGM = sum(Outcome == "Make"),
                 Misses = sum(Outcome == "Miss"),
                 FGA = (FGM + Misses)) %>%
       ungroup() %>%
       mutate(`FG%` = round(100 * FGM / FGA),
              Range = (Region %in% regions3),
              Double = ((`FG%` >= 50)) | (Range & (`FG%` >= 37))) %>%
       filter(Double))$Region
  
  df =
    df %>%
    filter(Region %in% double_regions) %>%
    slice(rep(1:n(), each = 1)) %>%
    bind_rows() %>%
    rbind(df)
  
  labels =
    raw_df %>%
    group_by(Region) %>%
    summarize(FGM = sum(Outcome == "Make"),
              Misses = sum(Outcome == "Miss"),
              FGA = (FGM + Misses)) %>%
    ungroup() %>%
    mutate(`FG%` = paste0(round(100 * FGM / FGA), "%"),
           Splits = paste(as.character(FGM), as.character(FGA), sep="-")) %>%
    merge(region_labels,
          by="Region",
          all.x=TRUE)
  
  (df %>%
      ggplot(aes(x=x, y=y)) +
      geom_density_2d_filled(bins=6,
                             alpha=0.75,
                             show.legend=FALSE) +
      geom_label(
        data=labels, 
        aes(x=x, y=y, label=`FG%`),
        size=5,
        color='black',
        fill = alpha(c("white"), 0.5)) +
      scale_fill_manual(values = colorRampPalette(
        paletteer_d("RColorBrewer::YlOrRd", n=9, direction=-1))(6), 
        aesthetics = c("fill"))) %>%
    geom_half_court()
}

shot_trends = function(league, team, players, date, shot_types, 
                       min=5, three=T, rate=T, y=2024) {
  dates = retrieve_dates(league, team, date, y=y)
  
  all_shots = 
    get_all_shots(y=y) %>%
    filter(League == league,
           Team == team,
           Player %in% players,
           Date %in% dates,
           `Shot Type` %in% shot_types,
           !str_detect(Outcome, "Foul"))
  
  twos =
    all_shots %>%
    filter(!(Region %in% regions3)) %>%
    arrange(`Shot ID`) %>%
    rownames_to_column(var="FGA") %>%
    group_by(Date) %>%
    summarize(FGM = sum(Outcome == "Make"),
              FGA = sum(Outcome %in% c("Miss", "Make"))) %>%
    ungroup() %>%
    rownames_to_column(var="Day") %>%
    mutate(rolledFGM = rollsum(FGM, k=7, fill=NA, align="right"),
           rolledFGA = rollsum(FGA, k=7, fill=NA, align="right"),
           `FG%` = 100 * rolledFGM / rolledFGA,
           Statistic = "2P%",
           Date = as.Date(Date, format="%Y-%m-%d")) %>%
    filter(rolledFGA >= min)
  
  threes =
    all_shots %>%
    filter(Region %in% regions3) %>%
    arrange(`Shot ID`) %>%
    rownames_to_column(var="FGA") %>%
    group_by(Date) %>%
    summarize(FGM = sum(Outcome == "Make"),
              FGA = sum(Outcome %in% c("Miss", "Make"))) %>%
    ungroup() %>%
    rownames_to_column(var="Day") %>%
    mutate(rolledFGM = rollsum(FGM, k=7, fill=NA, align="right"),
           rolledFGA = rollsum(FGA, k=7, fill=NA, align="right"),
           `FG%` = 100 * rolledFGM / rolledFGA,
           Statistic = "3P%",
           Date = as.Date(Date, format="%Y-%m-%d")) %>%
    filter(rolledFGA >= min)
  
  threes_rate =
    all_shots %>%
    arrange(`Shot ID`) %>%
    rownames_to_column(var="FGA") %>%
    group_by(Date) %>%
    summarize(`3PA` = sum(Region %in% regions3),
              FGA = sum(Outcome %in% c("Miss", "Make"))) %>%
    ungroup() %>%
    rownames_to_column(var="Day") %>%
    mutate(rolled3PA = rollsum(`3PA`, k=7, fill=NA, align="right"),
           rolledFGA = rollsum(FGA, k=7, fill=NA, align="right"),
           `3P-R` = 100 * rolled3PA / rolledFGA,
           Statistic = "3P-R",
           Date = as.Date(Date, format="%Y-%m-%d")) %>%
    filter(rolledFGA >= min)
  
  field_goals =
    all_shots %>%
    filter(`Shot Type` %in% shot_types) %>%
    arrange(`Shot ID`) %>%
    rownames_to_column(var="FGA") %>%
    group_by(Date) %>%
    summarize(FGM = sum(Outcome == "Make"),
              FGA = sum(Outcome %in% c("Miss", "Make"))) %>%
    ungroup() %>%
    rownames_to_column(var="Day") %>%
    mutate(rolledFGM = rollsum(FGM, k=7, fill=NA, align="right"),
           rolledFGA = rollsum(FGA, k=7, fill=NA, align="right"),
           `FG%` = 100 * rolledFGM / rolledFGA,
           Statistic = "FG%",
           Date = as.Date(Date, format="%Y-%m-%d")) %>%
    filter(rolledFGA >= min)
  
  plot =
    ggplot() +
    geom_smooth(data=twos,
                aes(x=Date, y=`FG%`, color=Statistic),
                se=F, linewidth=2)
  
  if (three) {
    plot =
      plot +
      geom_smooth(data=threes,
                  aes(x=Date, y=`FG%`, color=Statistic),
                  se=F, linewidth=2)
  }
  
  if (rate) {
    plot =
      plot +
      geom_smooth(data=threes_rate,
                  aes(x=Date, y=`3P-R`, color=Statistic),
                  se=F, linewidth=2)
  }
  
  plot +
    geom_vline(xintercept=as.Date("2024-01-01"),
               linetype='dashed',
               color='black') +
    geom_smooth(data=field_goals,
                aes(x=Date, y=`FG%`, color=Statistic),
                se=F, linewidth=3) +
    scale_color_manual(values = c("2P%" = "lightblue", 
                                  "3P%" = "darkblue", 
                                  "3P-R" = "darkgreen", 
                                  "FG%" = "#862633")) +
    scale_x_date(date_breaks = "1 week", 
                 date_labels = "%b %d") +
    # lims(y=c(0, 60)) +
    labs(x="", y="",
         color = "Statistic") +
    theme_bw() +
    theme(axis.title=element_text(size=16),
          axis.text.x=element_text(angle=45, vjust=0.5),
          axis.text=element_text(size=12),
          legend.title=element_text(size=16),
          legend.text=element_text(size=14))
}
