generate_lead_df = function(league, team, date, y=2024) {
  if (y == 2024) {
    plays = plays24
  } else {
    plays = plays25
  }
  
  game = 
    plays %>%
    filter(League == league,
           ((AwayTeam == team) | (HomeTeam == team)),
           Date == date)
  
  game %>%
    merge(data.frame(Seconds=1:last(game$Seconds)),
          by="Seconds",
          all.y=TRUE) %>%
    mutate(Date = as.character(Date)) %>%
    select(League, AwayTeam, HomeTeam, Date,
           Minute, Seconds, Score, AwayScore, HomeScore) %>%
    add_row(League=league,
            AwayTeam=first(game$AwayTeam), HomeTeam=first(game$HomeTeam),
            Date=date, Minute=0, Seconds=0,
            Score="0-0", AwayScore=0, HomeScore=0) %>%
    arrange(Seconds) %>%
    mutate(across(everything(), \(x) na.locf(x)),
           Margin = AwayScore - HomeScore,
           Ahead = (AwayScore > HomeScore)) %>%
    group_by(Seconds) %>%
    slice_tail(n=1) %>%
    ungroup()
}

generate_lead_tracker = function(league, team, date, y=2025) {
  game = generate_lead_df(league, team, date, y=y)
  
  if (max(game$Minute) <= 40) {
    ot = 0
  } else if (max(game$Minute) <= 45) {
    ot = 1
  } else {
    ot = 2
  }
  
  if (league == "MBB") {
    min_breaks = c(0.5*60, 20.5*60)
  } else {
    min_breaks = c(0.5*60, 10.5*60, 20.5*60, 30.5*60)
  }
  
  if (ot == 1) {
    min_limits = c(0, 46*60)
    min_breaks = c(min_breaks, 40.5*60)
  } else if (ot == 2) {
    min_limits = c(0, 51*60)
    min_breaks = c(min_breaks, 40.5*60, 45.5*60)
  } else {
    min_limits = c(0, 41*60)
  }
  
  team_color = filter(uaa_teams_csv, Team == team)$`Color Primary`
  if (first(game$AwayTeam) == team) {
    set_colors = c("gray", paste("#", team_color, sep=""))
  } else {
    set_colors = c(paste("#", team_color, sep=""), "gray")
  }
  
  plot =
    game %>%
    mutate(Seconds = Seconds + 30) %>%
    ggplot(aes(x=Seconds,
               y=Margin,
               fill=Ahead)) +
    geom_bar(width=1,
             stat='identity',
             show.legend=FALSE) +
    geom_hline(yintercept=0, linetype='dashed') +
    geom_vline(xintercept=0.5*60,
               linewidth=1) +
    geom_vline(xintercept=20.5*60,
               linewidth=1) +
    geom_vline(xintercept=40.5*60,
               linewidth=1) +
    scale_x_continuous(limits=min_limits,
                       breaks=min_breaks) +
    scale_fill_manual(values=set_colors) +
    ylim(range(game$Margin)) +
    theme_classic() +
    theme(axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  
  if (league == "WBB") {
    plot =
      plot +
      geom_vline(xintercept=10.5*60,
                 linewidth=1) +
      geom_vline(xintercept=30.5*60,
                 linewidth=1)
  }
  
  if (ot == 1) {
    plot +
      geom_vline(xintercept=45.5*60,
                 linewidth=1)
  } else if (ot == 2) {
    plot +
      geom_vline(xintercept=45.5*60,
                 linewidth=1) +
      geom_vline(xintercept=50.5*60,
                 linewidth=1)
  } else {
    plot
  }
}

generate_game_flow = function(league, team, date, y=2024) {
  if (y == 2024) {
    plays = plays24
  } else {
    plays = plays25
  }
  
  plays =
    plays %>%
    filter(League == league,
           Date == date,
           ((AwayTeam == team) | (HomeTeam == team)))
  
  if ((first(plays$AwayTeam) %in% uaa_teams_csv$Team) &
      (first(plays$HomeTeam) %in% uaa_teams_csv$Team)) {
    plot_grid(generate_lead_tracker(league, team, date, y=y),
              rotation_chart("game", league, first(plays$AwayTeam), date=date, y=y),
              rotation_chart("game", league, first(plays$HomeTeam), date=date, y=y),
              ncol=1,
              align='v')
  } else {
    plot_grid(generate_lead_tracker(league, team, date, y=y),
              rotation_chart("game", league, team, date=date, y=y),
              ncol=1,
              align='v')
  }
}
