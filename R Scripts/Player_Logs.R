trad_shading <- function(gt, df) {
  gt %>%
    data_color(columns = PTS,
               palette = c("white", "white", "#e9f8ef"),
    ) %>%
    data_color(columns = MIN,
               palette = c("white", "white", "#e9f8ef"),
    ) %>%
    data_color(columns = OREB,
               palette = c("white", "white", "#e9f8ef"),
    ) %>%
    data_color(columns = DREB,
               palette = c("white", "white", "#e9f8ef"),
    ) %>%
    data_color(columns = REB,
               palette = c("white", "white", "#e9f8ef"),
    ) %>%
    data_color(columns = AST,
               palette = c("white", "white", "#e9f8ef"),
    ) %>%
    data_color(columns = STL,
               palette = c("white", "white", "#e9f8ef"),
    ) %>%
    data_color(columns = BLK,
               palette = c("white", "white", "#e9f8ef"),
    ) %>%
    data_color(columns = TO,
               palette = c("white", "white", "#fae5e2"),
    ) %>%
    data_color(columns = PF,
               palette = c("white", "white", "#fae5e2"),
    )
}

player_season_trend <- function(player, stat, y=2024) {
  if (y == 2024) {
    player_box = player_box24
  } else {
    player_box = player_box25
  }
  
  if (stat == "FG%") {
    stats <- c("FGM", "FGA")
  } else if (stat == "2P%") {
    stats <- c("2PM", "2PA")
  } else if (stat == "3P%") {
    stats <- c("3PM", "3PA")
  } else if (stat == "3P-R") {
    stats <- c("3PA", "FGA")
  } else if (stat == "FT%") {
    stats <- c("FTM", "FTA")
  } else if (stat == "FT-R") {
    stats <- c("FTA", "FGA")
  }
  
  player_box =
    player_box %>%
    filter(Player == player)
  
  colors = c(paste0("#", head(player_box, 1)$`Color Secondary`), 
             paste0("#", head(player_box, 1)$`Color Primary`))
  
  rolling <-
    player_box %>%
    filter(Player == player) %>%
    arrange(desc(Date)) %>%
    mutate(`Game Average` = 100 * .data[[stats[1]]] / .data[[stats[2]]],
           cMakes = cumsum(.data[[stats[1]]]),
           cAttempts = cumsum(.data[[stats[2]]]),
           cPercentage = round(100 * cMakes / cAttempts, 1),
           MovingM = rollsumr(.data[[stats[1]]], k=3, fill=NA, align='right'),
           MovingA = rollsumr(.data[[stats[2]]], k=3, fill=NA, align='right'),
           `Rolling Average` = 100 * MovingM / MovingA,
           GameNumber = row_number(),
           across(c('Game Average', 'Rolling Average'), \(x) round(x, 1)))
  
  (rolling %>%
      ggplot(aes(x=GameNumber,
                 y=`Game Average`,
                 group=1,
                 label1=Date,
                 label2=Opponent)) +
      geom_hline(color="black",
                 linetype="dashed",
                 yintercept=tail(rolling, 1)$cPercentage) +
      geom_line(color=colors[2],
                alpha=0.25,
                linewidth=1) +
      geom_point(color=colors[2],
                 alpha=0.25,
                 size=1.5) +
      geom_line(aes(y=`Rolling Average`),
                color=colors[2],
                linewidth=1) +
      geom_point(aes(y=`Rolling Average`),
                 color=colors[2],
                 alpha=0.75,
                 size=1.5) +
      labs(x="Game",
           y=stat,
           title="Season Trends") +
      theme_bw()) %>%
    ggplotly(tooltip=c("Game Average", "Rolling Average", 
                       "Date", "Opponent")) %>%
    layout(showlegend = FALSE)
}

player_game_log = function(player, conf=F, y=2024) {
  if (y == 2024) {
    player_box = player_box24
  } else {
    player_box = player_box25
  }
  
  player_box =
    player_box %>%
    filter(Player == player,
           if (conf) Conference else TRUE)
  
  (player_box %>% 
      arrange(desc(Date)) %>%
      mutate(Opponent = case_when((Location == "Away") ~ paste("at", Opponent),
                                  TRUE ~ paste("vs", Opponent))) %>%
      select(Date, Opponent, MIN, player_cols)) %>%
    gt() %>%
    gt_shading() %>%
    trad_shading(player_box) %>%
    cols_width(
      Date ~ px(120),
      Opponent ~ px(210),
      everything() ~ px(82)
    ) %>%
    tab_header(title="Game Log") %>%
    opt_interactive(use_search=TRUE,
                    use_highlight=TRUE,
                    use_page_size_select=TRUE,
                    page_size_values=c(10, 20, 30))
}
