game_df <- function(league, team, date, adv=F, misc=F, y=2024) {
  if (y == 2024) {
    team_box = team_box24
  } else {
    team_box = team_box25
  }
  
  team_box %>%
    filter(League == league, 
           Team == team,
           Date == date)
}
game_table <- function(league, away, home, date, adv=F, misc=F, y=2024) {
  if (y == 2024) {
    team_box = team_box24
  } else {
    team_box = team_box25
  }
  
  rbind(game_df(league, away, date, adv=adv, misc=misc, y=y),
        game_df(league, home, date, adv=adv, misc=misc, y=y)) %>%
    select(URL, Team,
           if ((!adv) & (!misc)) trad_cols else c(),
           if (adv) adv_cols else c(),
           if (misc) misc_cols else c()) %>%
    gt() %>%
    cols_label(URL = "") %>%
    cols_width(Team ~ px(134)) %>%
  gt_img_rows(columns=URL,
              height=30) %>%
    gt_shading()
}