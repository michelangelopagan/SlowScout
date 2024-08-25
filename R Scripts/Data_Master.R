trad_cols = c("+/-", "PTS", 
              "FGM-A", "FG%", "3PM-A", "3P%", "FTM-A", "FT%", 
              "OREB", "DREB", "REB", "TO")
adv_cols = c("POSS", "Poss. Length", 
             "NET", "PPP", "FT-R", "3P-R", 
             "ORB%", "DRB%", "REB%", 
             "AST%", "TO%")
misc_cols = c("PTS", 
              "Points off Turnovers", "POT %", 
              "2nd Chance Points", "2ND %", 
              "Points in the Paint", "PITP %", 
              "Fastbreak Points", "FB %",
              "Bench Points", "BENCH %")

player_cols = c("PTS", "FGM-A", "FG%", 
                "3PM-A", "3P%", "3P-R", "FTM-A", "FT%", "FT-R",
                "OREB", "DREB", "REB", "AST", "TO", "AST/TO", 
                "STL", "BLK", "PF")

w_non_conf_opponents25 = read_csv("w_non_conf_opponents.csv")$Team
m_non_conf_opponents25 = read_csv("m_non_conf_opponents.csv")$Team

w_roster25 = read_csv("w_chicago_roster.csv")$Player
m_roster25 = read_csv("m_chicago_roster.csv")$Player

region_labels <- read_csv(file="region_labels.csv")

shading_df <- read_csv("shading.csv")

uaa_teams_csv <- read_csv("uaa_teams.csv")
uaa_teams = uaa_teams_csv$Team

extract_player <- function(action) {
  raw_player <- str_split(action, " by ")[[1]][2] 
  
  first_name <- str_split(raw_player, ",")[[1]][2]
  last_name <- str_split(raw_player, ",")[[1]][1]
  
  return( str_to_title(paste(first_name, last_name)) )
}

threshold = as.Date("2024-09-01", format="%Y-%m-%d")

date_filter_helper = function(dates, y=2024) {
  dates =
    dates %>%
    as.Date(format="%Y-%m-%d")
  
  if (y == 2024) {
    return( dates[(dates < threshold)] )
  } else {
    return( dates[(dates >= threshold)] )
  }
}
