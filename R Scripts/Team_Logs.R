# library(scales)
# library(comprehenr)
# library(magrittr)

# Get formatted list of games
get_games <- function(league, team, y=2024) {
  if (y == 2024) {
    team_box = team_box24
  } else {
    team_box = team_box25
  }
  
  team_box %>%
    filter(League == league,
           Team == team) %>%
    mutate(Subtitle = case_when((Location == "Away") ~ 
                                  paste("at", Opponent, sep=" "),
                                TRUE ~ 
                                  paste("vs", Opponent, sep=" ")),
           Title = paste(as.character(Date), Subtitle, sep=" ")) %>% 
    arrange(Date)
}

margin_of_victory <- function(league, team, sort, conf=F, y=2024) {
  if (y == 2024) {
    team_box <- team_box24
  } else {
    team_box <- team_box25
  }
  
  team_df =
    team_box %>%
    filter(League == league,
           Team == team,
           if (conf) Conference else TRUE) %>%
    mutate(Margin = `+/-`,
           Win = Margin > 0)
  
  colors = c(paste0("#", head(team_df, 1)$`Color Secondary`), 
             paste0("#", head(team_df, 1)$`Color Primary`))
  
  if (sort == "team") {
    plot =
      team_df %>%
      ggplot(aes(x=Opponent,
                 y=Margin,
                 group=Margin,
                 fill=factor(Win),
                 label1=Date,
                 label2=Location)) +
      geom_bar(stat='identity',
               color='white',
               position='dodge',
               show.legend=FALSE) +
      scale_fill_manual(values=colors,
                        labels=unique(team_df$Opponent)) +
      labs(x="Opponent", 
           y="",
           title="Margin of Victory") +
      theme_bw() +
      theme(axis.text.x = element_text(angle=30))
  } else {
    plot <-
      team_df %>%
      arrange(!!sym( if (sort == "date") "Date" else "Margin" )) %>%
      mutate(Game = row_number()) %>%
      ggplot(aes(x=Game,
                 y=Margin,
                 fill=factor(Win),
                 label1=Date,
                 label2=Opponent,
                 label3=Location)) +
      geom_bar(stat='identity',
               show.legend=F) +
      scale_fill_manual(values=colors) +
      labs(x="Game", 
           y="",
           title="Margin of Victory") +
      theme_bw()
  }
  
  plot %>%
    ggplotly(tooltip = c('Date', 'Location', 'Opponent')) %>%
    layout(showlegend = FALSE)
}

season_trend <- function(league, team, stat, conf=F, opp=F, y=2024) {
  if (y == 2024) {
    team_box = team_box24
  } else {
    team_box = team_box25
  }
  
  team_box =
    team_box %>%
    filter(if (conf) Conference else TRUE)
  
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
  } else if (stat == "ORB%") {
    stats <- c("OREB", "REB")
  } else if (stat == "TO%") {
    stats <- c("TO", "POSS")
  }
  
  if (opp) {
    stats <- paste(stats, "_OPP", sep="")
  }
  
  rolling <-
    team_box %>%
    filter(League == league,
           Team == team) %>%
    arrange(`Date`) %>%
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

team_game_log <- function(league, team, opp=F, adv=F, misc=F, conf=F, y=2024) {
  if (y == 2024) {
    team_box = team_box24
  } else {
    team_box = team_box25
  }
  
  colors = c(paste0("#", head(team_box, 1)$`Color Secondary`), 
             paste0("#", head(team_box, 1)$`Color Primary`))
  
  team_box %>%
    filter(League == league,
           if (opp) (Opponent == team) else (Team == team),
           if (conf) Conference else TRUE) %>%
    arrange(desc(Date)) %>%
    rowwise() %>%
    mutate(Opponent = case_when((opp) ~ Team,
                                (Location == "Away") ~ paste("at", Opponent),
                                TRUE ~ paste("vs", Opponent)),
           `W/L` = `+/-`) %>%
    ungroup() %>%
    select(Date, Opponent, `W/L`,
           if ((!adv) & (!misc)) trad_cols else c(),
           if (adv) adv_cols else c(),
           if (misc) misc_cols else c()) %>%
    gt() %>%
    gt_shading() %>%
    cols_width(
      Date ~ px(120),
      Opponent ~ px(210),
      `W/L` ~ px(60),
      ends_with("PPP") ~ px(70),
      ends_with("-R") ~ px(70),
      ends_with("TO%") ~ px(65),
      everything() ~ px(82)
    ) %>%
    tab_style(
      locations = cells_body(
        columns = `W/L`,
        rows = `W/L` > 0
      ),
      style = list(cell_fill(color = '#dbf1e5'))
    ) %>%
    tab_style(
      locations = cells_body(
        columns = `W/L`,
        rows = `W/L` < 0
      ),
      style = list(cell_fill(color = '#fdf2f0'))
    ) %>%
    opt_interactive(use_search=TRUE,
                    use_highlight=TRUE,
                    use_page_size_select=TRUE,
                    page_size_values=c(10, 20, 30))
}