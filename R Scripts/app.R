library(gt)
library(DT)
library(zoo)
library(pak)
library(shiny)
library(plotly)
library(reactR)
library(cowplot)
library(ggplot2)
library(janitor)
library(gtExtras)
library(reactable)
library(tidyverse)
library(paletteer)
library(mongolite)
library(comprehenr)
library(shinytitle)
library(shinyWidgets)
library(shinyvalidate)
library(shinydashboard)

# setwd("/Users/jdumalig/Desktop/Productivity/SlowScout")

source("Data_Master.R")
source("Database.R")
source("Data24.R")
source("Data25.R")
source("Color.R")
source("Rankings.R")
source("Rotations.R")
source("Flow.R")
source("Game.R")
source("Lineups.R")
source("Player_Logs.R")
source("Team_Logs.R")
source("Court.R")
source("Practice.R")
source("setSliderColor.R")

ui = dashboardPage(
  dashboardHeader(title="SlowScout"),
  dashboardSidebar(
    sidebarMenu(
      radioGroupButtons(inputId="season",
                        label="Select season:",
                        choices=c(2024, 2025),
                        selected=2025,
                        status='default',
                        justified=TRUE),
      radioGroupButtons(
        inputId = "league",
        label = "Select league:", 
        choices = c("WBB", "MBB"),
        status='default',
        justified = TRUE),
      pickerInput(inputId="team",
                  label="Select team:",
                  choices=c(uaa_teams, "Non-UAA Scout"),
                  selected="Chicago"),
      menuItem("Team/Scout Dashboard", tabName="practice_dash"),
      menuItem("UAA Rankings", tabName="rankings_dash"),
      menuItem("Lineup Splits", tabName="lineup_dash"),
      menuItem("Rotation Charts", tabName="rotation_dash"),
      menuItem("Single-Game View", tabName="single_game"),
      menuItem("Team Logs", tabName="team_dash"),
      menuItem("Player Logs", tabName="player_dash"),
      menuItem("Shot Plotter", tabName="plotter_dash"),
      menuItem("Stats Glossary", tabName="glossary"))),
  dashboardBody(
    tabItems(
      tabItem(tabName="practice_dash",
              h1("Team/Scout Dashboard"),
              br(),
              fluidRow(
                uiOutput("box_selections")
              ),
              gt_output("practice_box"),
              br(),
              fluidRow(
                column(6,
                       fluidRow(
                         align='center',
                         fluidRow(
                           column(1),
                           column(12, 
                                  radioGroupButtons(
                                    inputId="breakdown_type",
                                    label="Table Type:",
                                    choices=c("Only Shot Type", 
                                              "Player + Shot Type"),
                                    selected="Only Shot Type",
                                    status='default')),
                           column(1)
                         ),
                         column(6,
                                radioGroupButtons(
                                  inputId = "table_type",
                                  label = "Data Type:", 
                                  choices = c("Shots", "Region", "Turnovers"),
                                  status='default')),
                         column(6,
                                radioGroupButtons(inputId="breakdown_min",
                                                  label="Minimum FGA:",
                                                  choices=c(0, 5, 10, 15),
                                                  selected=10,
                                                  status='default'))
                       ),
                       gt_output("team_shooting")),
                column(6,
                       align='center',
                       uiOutput("plot_type_choices"),
                       fluidRow(
                         column(12,
                                uiOutput("pick_shot_types"))
                       ),
                       conditionalPanel(
                         condition = 'input.plot_type == "Hex Map"',
                         plotOutput('hex_map_plot')
                       ),
                       conditionalPanel(
                         condition = 'input.plot_type == "Shot Chart"',
                         plotlyOutput('practice_shot_plotly')
                       ),
                       conditionalPanel(
                         condition = 'input.plot_type == "Heat Map"',
                         plotOutput('practice_shot_plot')
                       ),
                       conditionalPanel(
                         condition = 'input.plot_type == "Shot Trends"',
                         plotOutput('shot_trends_plot')
                       ))
              )
      ),
      tabItem(tabName="plotter_dash",
              fluidPage(fluidRow(
                column(2,
                       radioGroupButtons(
                         inputId = "track_team_type",
                         label = "", 
                         choices = c("TEAM", "OPP"),
                         status = "default"
                       ),
                       uiOutput("tracker_pick_opponent"),
                       uiOutput("tracker_pick_player")),
                column(3,
                       dateInput("plotter_date", 
                                 "Date", 
                                 value = Sys.Date()),
                       radioGroupButtons(
                         inputId = "track_shot_type",
                         label = "Shot Type", 
                         choices = shot_types,
                         status = "default"
                       ),
                       radioGroupButtons(
                         inputId = "track_outcome",
                         label = "Outcome", 
                         choices = c("Make", "Miss", "Foul (+0)", "Foul (+1)", 
                                     "Foul (+2)", "Foul (+3)"),
                         status = "default"
                       )),
                column(7,
                       fluidRow(
                         column(1),
                         column(3,
                                fluidRow(
                                  align='center',
                                  actionButton("record_oreb", "OREB"),
                                  actionButton("record_dreb", "DREB")
                                ),
                                fluidRow(
                                  align='center',
                                  actionButton("record_ast", "AST")
                                )
                         ),
                         column(7,
                                fluidRow(
                                  align='center',
                                  actionButton("record_to_ps", 
                                               "Perimter/Strip"),
                                  actionButton("record_to_bp", "Bad Pass"),
                                  actionButton("record_to_d", "Drive")
                                ),
                                fluidRow(
                                  align='center',
                                  actionButton("record_to_dp", "Drive + Pass"),
                                  actionButton("record_to_p", "Post Entry"),
                                  actionButton("record_to_o", "Other")
                                )
                         ),
                         column(1)
                       ),
                       br(),
                       fluidRow(
                         align='center',
                         uiOutput("plotter_score")
                       ),
                       plotOutput("court", 
                                  height="425px",
                                  click="court_click"),
                       fluidRow(
                         align='center',
                         actionButton("rotate_court", "Rotate Court Clockwise"),
                         actionButton("refresh_db", "Refresh Data")
                       )
                )
              ),
              br(),
              fluidRow(
                column(9,
                       gt_output("tracked_shots_table")),
                column(3,
                       uiOutput("remove_shot"))
              ),
              br(),
              fluidRow(
                column(9,
                       gt_output("tracked_events_table")),
                column(3,
                       uiOutput("remove_event"))
              )
              )),
      tabItem(tabName="single_game",
              h1("Single-Game View"),
              br(),
              fluidPage(
                fluidRow(
                  column(4, align="center",
                         radioGroupButtons(
                           inputId="single_conf",
                           label="", 
                           choices=c("Full Season", "Conference"),
                           status="default")),
                  column(4, align="center",
                         uiOutput("single_select_game")),
                  column(4, align="center",
                         radioGroupButtons(
                           inputId="single_adv",
                           label="", 
                           choices=c("Traditional", 
                                     "Advanced", 
                                     "Miscellaneous"),
                           status="default")))),
              br(),
              gt_output("single_game_table"),
              br(),
              plotOutput("game_flow",
                         height=750)),
      tabItem(tabName="rotation_dash",
              h1("Rotations Dashboard"),
              br(),
              p(paste("Each box below represents a minute of the game, with",
                      "shaded boxes indicating a player's on-court presence.", 
                      sep=" ")),
              p(paste("Lighter boxes indicate a player only played a portion",
                      "of that minute due to a substitution.", sep=" ")),
              br(),
              setSliderColor(c("#862633", "#862633", 
                               "#862633", "#862633", "#862633"), 
                             c(1, 2, 3, 4, 5)),
              fluidPage(fluidRow(
                column(4, align="center",
                       radioGroupButtons(
                         inputId="rotation_conf",
                         label="", 
                         choices=c("Full Season", "Conference"),
                         status="default")),
                column(8, align="center",
                       uiOutput("rotation_margin_slider")))),
              plotOutput("season_rotation_chart")),
      tabItem(tabName="rankings_dash",
              h1("Rankings Dashboard"),
              br(),
              fluidPage(
                fluidRow(
                  column(4, align="center",
                         radioGroupButtons(
                           inputId="rankings_adv",
                           label="", 
                           choices=c("Traditional", 
                                     "Advanced", 
                                     "Miscellaneous"),
                           status="default")),
                  column(4, align="center",
                         radioGroupButtons(
                           inputId="rankings_conf",
                           label="", 
                           choices=c("Full Season", "Conference"),
                           status="default")),
                  column(4, align="center",
                         radioGroupButtons(
                           inputId="rankings_opp",
                           label="", 
                           choices=c("Team", "Opponent"),
                           status="default"))
                )
              ),
              fluidRow(
                column(3),
                column(6, align="center",
                       sliderInput(
                         inputId="rankings_min", 
                         label="Select minutes threshold:", 
                         min=0, max=40,
                         value=10,
                         width="100%")),
                column(3)
              ),
              br(),
              tabsetPanel(
                type="tabs",                     
                tabPanel("Teams", gt_output("team_rankings")),
                tabPanel("Players", gt_output("player_rankings")),
                tabPanel("Assists", gt_output("assist_rankings"))
              )
      ),
      tabItem(tabName="lineup_dash",
              h1("Lineup Dashboard"),
              br(),
              fluidPage(
                fluidRow(
                  column(6, align="center",
                         uiOutput("select_on_players")),
                  column(6, align="center",
                         uiOutput("select_off_players"))
                )
              ),
              br(),
              fluidPage(
                fluidRow(
                  column(3, align="center",
                         sliderInput(
                           inputId="lineup_min", 
                           label="Select minutes threshold:", 
                           min=0, max=40,
                           value=5,
                           width="100%")),
                  column(3, align="center",
                         radioGroupButtons(
                           inputId="lineup_conf",
                           label="", 
                           choices=c("Full Season", "Conference"),
                           status="default")),
                  column(3, align="center",
                         radioGroupButtons(
                           inputId="lineup_opp",
                           label="", 
                           choices=c("Team", "Opponent"),
                           status="default")),
                  column(3, align="center",
                         radioGroupButtons(
                           inputId="lineup_adv",
                           label="", 
                           choices=c("Traditional", "Advanced"),
                           status="default")))
              ),
              br(),
              gt_output("lineup_splits"),
              br(),
              h3("How Often This Lineup Shared the Court"),
              plotOutput("lineup_rotation")),
      tabItem(tabName="player_dash",
              h1("Player Dashboard"),
              br(),
              fluidPage(
                fluidRow(
                  column(8, uiOutput("select_player")),
                  column(4, 
                         radioGroupButtons(
                           inputId="player_conf",
                           label="", 
                           choices=c("Full Season", "Conference"),
                           status="default")
                  )
                )
              ),
              br(), 
              gt_output("player_game_log"),
              br(),
              splitLayout(
                cellWidths=c("50%", "50%"), 
                tabsetPanel(type="tabs",                     
                            tabPanel("FG%", plotlyOutput("player_trends_fg")),
                            tabPanel("2P%", plotlyOutput("player_trends_2p")),
                            tabPanel("3P%", plotlyOutput("player_trends_3p")),
                            tabPanel("3P-R", plotlyOutput("player_trends_3pr")),
                            tabPanel("FT%", plotlyOutput("player_trends_ft")),
                            tabPanel("FT-R", plotlyOutput("player_trends_ftr"))),
                tabsetPanel(type="tabs",                     
                            tabPanel("FG%", plotlyOutput("player_trends_fg2")),
                            tabPanel("2P%", plotlyOutput("player_trends_2p2")),
                            tabPanel("3P%", plotlyOutput("player_trends_3p2")),
                            tabPanel("3P-R", plotlyOutput("player_trends_3pr2")),
                            tabPanel("FT%", plotlyOutput("player_trends_ft2")),
                            tabPanel("FT-R", plotlyOutput("player_trends_ftr2")))
              ),
              br(),
              h3("Rotation Game Log"),
              br(),
              plotOutput("player_rotation",
                         height=500)),
      tabItem(tabName="team_dash",
              h1("Team Dashboard"),
              fluidPage(fluidRow(
                column(4, align="center",
                       radioGroupButtons(
                         inputId="team_conf",
                         label="", 
                         choices=c("Full Season", "Conference"),
                         status="default")),
                column(4, align="center",
                       radioGroupButtons(
                         inputId="team_adv",
                         label="", 
                         choices=c("Traditional", "Advanced", "Miscellaneous"),
                         status="default")),
                column(4, align="center",
                       radioGroupButtons(
                         inputId="team_side",
                         label="", 
                         choices=c("Team", "Opponent"),
                         status="default")))),
              gt_output("team_game_log"),
              br(),
              splitLayout(
                cellWidths=c("50%", "50%"), 
                tabsetPanel(type="tabs",                     
                            tabPanel("Chronological Order", 
                                     plotlyOutput("margin_date")),
                            tabPanel("Ascending Order", 
                                     plotlyOutput("margin_margin")),
                            tabPanel("Team Order", 
                                     plotlyOutput("margin_team"))),
                tabsetPanel(type="tabs",                     
                            tabPanel("FG%", plotlyOutput("trends_fg")),
                            tabPanel("2P%", plotlyOutput("trends_2p")),
                            tabPanel("3P%", plotlyOutput("trends_3p")),
                            tabPanel("3P-R", plotlyOutput("trends_3pr")),
                            tabPanel("FT%", plotlyOutput("trends_ft")),
                            tabPanel("FT-R", plotlyOutput("trends_ftr")),
                            tabPanel("ORB%", plotlyOutput("trends_orb")),
                            tabPanel("TO%", plotlyOutput("trends_to"))))
      ),
      tabItem(tabName="glossary",
              h1("SlowScout Manual"),
              h4(
                HTML(
                  paste0(
                    "For a more in depth breakdown on the back-end and front-e", 
                    "nd development of SlowScout, you can reference the <a hre", 
                    "f='https://drive.google.com/file/d/1SL34nTnWYzgQimhKOLY1b", 
                    "DN2Aib4oeu8/view?usp=sharing'>manual</a> or the <a href='", 
                    "https://github.com/jeremydumalig/SlowScout'>GitHub reposi", 
                    "tory</a>."
                  )
                )
              ),
              br(),
              h1("Advanced Stats Glossary"),
              br(),
              h4("Possessions (POSS)"),
              p(paste("An estimation of possessions, defined by plays that end",
                      "in a FGA/FTA/TO (OREBs extend possessions)", sep=" ")),
              br(),
              h4("Points Per Possession (PPP)"),
              p("Total points divided by total possessions"),
              br(),
              h4("Points Per Shot (PPS)"),
              p(paste0("Total points divided by total true shot attempts ",
                       "(number of possessions that ended in a field goal ",
                       "or shooting foul)")),
              br(),
              h4("Free-Throw Rate (FT-R)"),
              p(HTML(paste("<em>The ratio of free-throw attempts to field goal",
                           "attempts</em>", sep=" "))),
              p(paste("Total free-throw attempts divided by total field goal",
                      "attempts", sep=" ")),
              br(),
              h4("Three-Point Rate (3P-R)"),
              p(HTML(paste("<em>The percentage of field goals that come from",
                           "long range</em>", sep=" "))),
              p(paste("Total three-point attempts divided by total field goal",
                      "attempts", sep=" ")),
              br(),
              h4("Offensive Rebound Rate (ORB%)"),
              p(HTML(paste("<em>The percentage of offensive rebound",
                           "opportunities that were converted</em>"))),
              p(paste("Total offensive rebounds divided by total offensive",
                      "rebound opportunities (OREB + Opponent DREB)", sep=" ")),
              br(),
              h4("Defensive Rebound Rate (DRB%)"),
              p(HTML(paste("<em>The percentage of defensive rebound",
                           "opportunities that were converted</em>"))),
              p(paste("Total defensive rebounds divided by total defensive",
                      "rebound opportunities (DREB + Opponent OREB)", sep=" ")),
              br(),
              h4("Overall Rebound Rate (REB%)"),
              p(HTML(paste("<em>The percentage of total rebound",
                           "opportunities that were converted</em>", sep=" "))),
              p(paste("Total rebounds divided by total rebound opportunities",
                      "(REB + Opponent REB)", sep=" ")),
              br(),
              h4("Turnover Rate (TO%)"),
              p(HTML(paste("<em>The percentage of possessions that ended in",
                           "turnovers</em>", sep=" "))),
              p("Total turnovers divided by total possessions"))
    ),
    tags$head(
      tags$style(
        HTML('.selected 
                  {background-color:#222D32 !important;}
              .skin-blue .main-header .logo 
                  {background-color: #862633;}
              .skin-blue .main-header .logo:hover 
                  {background-color: #C4949A;}
              .skin-blue .main-header .navbar .sidebar-toggle:hover 
                  {background-color: #C4949A;}
              .skin-blue .main-header .navbar 
                  {background-color: #862633;}
              .content-wrapper, .right-side 
                  {background-color: #ffffff;}
              .nav-tabs>li>a
                  {color: #862633;}
              .skin-blue .sidebar-menu > li.active > a 
                  {border-left-color: #862633;}
              .skin-blue .sidebar-menu > li:hover > a 
                  {border-left-color: #862633;}
              h3 
                  {font-weight: bold;}')))))

server = function(input, output, session) {
  
  choices_df = reactive({get_players(input$league, input$team, y=input$season)})
  
  games_df = reactive({get_games(input$league, input$team, y=input$season)})
  
  ########## practice_dash ########## 
  
  output$box_selections = renderUI({
    if (input$team == "Non-UAA Scout") {
      team_box = if (input$season == 2024) team_box24 else team_box25
      
      box_players =
        get_shots(input$league, "Non-UAA Scout", y=input$season)$Player %>%
        unique()
      
      box_selected_players =
        (get_shots(input$league, "Non-UAA Scout", y=input$season) %>%
           filter(Player %in% unique(team_box$Team)))$Player %>%
        unique()
    } else {
      box_players = get_players(input$league, input$team,
                                y=input$season)$Player
      box_selected_players = box_players
    }
    
    div(
      align='center',
      column(4,
             uiOutput("box_date_picker")),
      column(4,
             radioGroupButtons(
               inputId="box_or_log",
               label="Table Type:",
               choices=c("Box Score", "Practice Log"),
               status="default")),
      column(4,
             pickerInput(
               inputId = "box_players",
               label = "Players (entire page):",
               choices = sort(box_players),
               options = list(`actions-box` = TRUE),
               multiple = TRUE,
               selected = box_selected_players
             ))
    )
  })
  
  output$box_date_picker = renderUI({
    if (input$team == "Chicago") {
      descriptions_df =
        team_box24 %>%
        filter(League == input$league,
               Team == input$team) %>%
        mutate(Description = 
                 case_when((Location == "Away") ~ paste(Date,
                                                        Opponent,
                                                        sep=" at "),
                           TRUE ~ paste(Date, Opponent,
                                        sep=" vs "))) %>%
        select(Date, Description) %>%
        merge(data.frame(Date=as.character(get_dates(input$league, 
                                                     input$team, 
                                                     y=input$season))),
              by="Date",
              all.y=TRUE) %>%
        mutate(Description = case_when(is.na(Description) ~ as.character(Date),
                                       TRUE ~ Description))
      
      date_choices = c("All Practices", "All Games",
                       "UAA Practices", "UAA Games",
                       "Non-UAA Practices", "Non-UAA Games",
                       rev(descriptions_df$Description))
      selected_choice = "All Practices"
    } else if (input$team == 'Non-UAA Scout') {
      date_choices = c("All Games")
      selected_choice = "All Games"
    } else {
      date_choices = c("All Games", rev(get_dates(input$league, 
                                                  input$team,
                                                  y=input$season)))
      selected_choice = "All Games"
    }
    
    pickerInput(
      inputId = "box_date",
      label = "Time Range (entire page):",
      choices = date_choices,
      selected = selected_choice
    )
  })
  
  output$practice_box = renderUI({
    if (input$box_or_log == "Box Score") {
      box_score(input$league, input$team, input$box_players, 
                date=input$box_date, y=input$season)
    } else {
      box_log(input$league, input$team, input$box_players, 
              date=input$box_date, y=input$season)
    }
  })
  
  output$team_shooting = render_gt({
    if (input$breakdown_type == "Only Shot Type") {
      shooting_table(input$league, input$team,
                     input$box_date, input$table_type,
                     as.integer(input$breakdown_min),
                     player=input$box_players,
                     y=input$season)
    } else {
      player_shot_type_table(input$league, input$team,
                             input$box_date, input$table_type,
                             as.integer(input$breakdown_min),
                             player=input$box_players,
                             y=input$season)
    }
  })
  
  output$plot_type_choices = renderUI({
    if (input$team == "Chicago") {
      radioGroupButtons(
        inputId = "plot_type",
        label = "Plot Type:",
        choices = c("Hex Map", "Shot Chart", "Heat Map", "Shot Trends"),
        status='default')
    } else {
      radioGroupButtons(
        inputId = "plot_type",
        label = "Plot Type:",
        choices = c("Hex Map", "Shot Chart", "Heat Map"),
        status='default')
    }
  })
  
  output$pick_shot_types = renderUI({
    dates = retrieve_dates(input$league, input$team,
                           input$box_date, y=input$season)
    
    f_shot_types =
      (get_shots(input$league, input$team, y=input$season) %>%
         filter(Date %in% dates,
                Player %in% input$box_players) %>%
         group_by(`Shot Type`) %>%
         summarize(Count = n()) %>%
         ungroup() %>%
         arrange(desc(Count)))$`Shot Type` %>%
      unique()
    
    pickerInput(
      inputId = "plot_types",
      label = "Shot Type(s):",
      choices = f_shot_types,
      selected = f_shot_types,
      options = list(`actions-box` = TRUE),
      multiple = TRUE
    )
  })
  
  output$hex_map_plot = renderPlot({
    hex_map(input$league, input$team, input$box_date,
            input$box_players, input$plot_types,
            y=input$season)
  })
  
  output$practice_shot_plotly = renderPlotly({
    shot_plot(input$league, input$team, input$box_date,
              input$box_players, input$plot_types,
              y=input$season)
  })
  
  output$practice_shot_plot = renderPlot({
    heat_map(input$league, input$team, input$box_date,
             input$box_players, input$plot_types,
             y=input$season)
  })
  
  output$shot_trends_plot = renderPlot({
    shot_trends(input$league, input$team, input$box_players,
                input$box_date, input$plot_types,
                y=input$season)
  })
  
  ########## rankings_dash ########## 
  
  output$team_rankings = render_gt({
    team_rankings(input$league,
                  opp=(input$rankings_opp == "Opponent"),
                  adv=(input$rankings_adv == "Advanced"),
                  misc=(input$rankings_adv == "Miscellaneous"),
                  conf=(input$rankings_conf == "Conference"),
                  y=input$season)
  })
  output$player_rankings = render_gt({
    player_rankings(input$league,
                    min=input$rankings_min,
                    conf=(input$rankings_conf == "Conference"),
                    y=input$season)
  })
  output$assist_rankings = render_gt({
    assist_rankings(input$league,
                    conf=(input$rankings_conf == "Conference"),
                    y=input$season)
  })
  
  ########## lineup_dash ##########
  
  output$select_on_players = renderUI({
    awesomeCheckboxGroup("on_players",
                         "Select ON Player(s):",
                         choices=sort(choices_df()$Player),
                         inline=TRUE,
                         status='danger')
  })
  
  output$select_off_players = renderUI({
    awesomeCheckboxGroup("off_players",
                         "Select OFF Player(s):",
                         choices=sort(choices_df()$Player),
                         inline=TRUE,
                         status='danger')
  })
  
  output$lineup_splits = render_gt({
    lineup_splits(input$league, input$team,
                  input$on_players, input$off_players,
                  opp=(input$lineup_opp == "Opponent"),
                  adv=(input$lineup_adv == "Advanced"),
                  conf=(input$lineup_conf == "Conference"),
                  y=input$season)
  })
  
  output$lineup_rotation = renderPlot({
    rotation_chart("lineup", input$league, input$team,
                   on=input$on_players,
                   off=input$off_players,
                   conf=(input$lineup_conf == "Conference"),
                   y=input$season)
  })
  
  ########## rotation_dash ##########
  
  output$rotation_margin_slider = renderUI({
    sliderInput(
      inputId="rotation_margin",
      label="Filter for games decided by ___ point(s) or less:",
      min=1,
      max=max(abs(games_df()$`+/-`)),
      value=max(abs(games_df()$`+/-`)),
      width="100%")
  })
  
  output$season_rotation_chart = renderPlot({
    rotation_chart("season", input$league, input$team,
                   margin=input$rotation_margin,
                   conf=(input$rotation_conf == "Conference"),
                   y=input$season)
  })
  
  ########## single_game ##########
  
  output$single_select_game = renderUI({
    if (input$single_conf == "Conference") {
      filtered_games = filter(games_df(), Opponent %in% uaa_teams)
    } else {
      filtered_games = games_df()
    }
    
    selectInput(
      inputId="selected_game",
      label="",
      choices=rev(filtered_games$Title))
  })
  
  output$single_game_table = render_gt({
    split = str_split(input$selected_game, " ")
    
    opponent = paste(split[[1]][-1][-1], collapse=" ")
    away = (split[[1]][2] == "at")
    date = split[[1]][1]
    
    game_table(input$league,
               if (away) input$team else opponent,
               if (away) opponent else input$team,
               date,
               adv=(input$single_adv == "Advanced"),
               misc=(input$single_adv == "Miscellaneous"),
               y=input$season)
  })
  
  output$game_flow = renderPlot({
    date = str_split(input$selected_game, " ")[[1]][1]
    
    generate_game_flow(input$league,
                       input$team,
                       date,
                       y=input$season)
  })
  
  ########## team_dash ##########
  
  output$team_game_log = render_gt({
    team_game_log(input$league, input$team,
                  opp=(input$team_side == "Opponent"),
                  adv=(input$team_adv == "Advanced"),
                  misc=(input$team_adv == "Miscellaneous"),
                  conf=(input$team_conf == "Conference"),
                  y=input$season)
  })
  
  output$margin_date = renderPlotly({
    margin_of_victory(input$league, input$team, "date",
                      conf=(input$team_conf == "Conference"),
                      y=input$season)
  })
  output$margin_team = renderPlotly({
    margin_of_victory(input$league, input$team, "team",
                      conf=(input$team_conf == "Conference"),
                      y=input$season)
  })
  output$margin_margin = renderPlotly({
    margin_of_victory(input$league, input$team, "margin",
                      conf=(input$team_conf == "Conference"),
                      y=input$season)
  })
  
  output$trends_fg = renderPlotly({
    season_trend(input$league, input$team, "FG%",
                 opp=(input$team_side == "Opponent"),
                 conf=(input$team_conf == "Conference"),
                 y=input$season)
  })
  output$trends_2p = renderPlotly({
    season_trend(input$league, input$team, "2P%",
                 conf=(input$team_conf == "Conference"),
                 opp=(input$team_side == "Opponent"),
                 y=input$season)
  })
  output$trends_3p = renderPlotly({
    season_trend(input$league, input$team, "3P%",
                 conf=(input$team_conf == "Conference"),
                 opp=(input$team_side == "Opponent"),
                 y=input$season)
  })
  output$trends_3pr = renderPlotly({
    season_trend(input$league, input$team, "3P-R",
                 conf=(input$team_conf == "Conference"),
                 opp=(input$team_side == "Opponent"),
                 y=input$season)
  })
  output$trends_ft = renderPlotly({
    season_trend(input$league, input$team, "FT%",
                 conf=(input$team_conf == "Conference"),
                 opp=(input$team_side == "Opponent"),
                 y=input$season)
  })
  output$trends_ftr = renderPlotly({
    season_trend(input$league, input$team, "FT-R",
                 conf=(input$team_conf == "Conference"),
                 opp=(input$team_side == "Opponent"),
                 y=input$season)
  })
  output$trends_orb = renderPlotly({
    season_trend(input$league, input$team, "ORB%",
                 conf=(input$team_conf == "Conference"),
                 opp=(input$team_side == "Opponent"),
                 y=input$season)
  })
  output$trends_to = renderPlotly({
    season_trend(input$league, input$team, "TO%",
                 conf=(input$team_conf == "Conference"),
                 opp=(input$team_side == "Opponent"),
                 y=input$season)
  })
  
  ########## player_dash ##########
  
  output$select_player = renderUI({
    players = rotation_order24[(rotation_order24 %in% choices_df()$Player)]
    
    selectInput(inputId="player",
                label="Select player:",
                choices=players
    )
  })
  
  output$player_game_log = render_gt(
    player_game_log(input$player,
                    conf=(input$player_conf == "Conference"),
                    y=input$season)
  )
  
  output$player_rotation = renderPlot({
    rotation_chart("player", input$league, input$team, player=input$player,
                   conf=(input$player_conf == "Conference"),
                   y=input$season)
  })
  
  output$player_trends_fg = renderPlotly({
    player_season_trend(input$player, "FG%",
                        y=input$season)
  })
  output$player_trends_2p = renderPlotly({
    player_season_trend(input$player, "2P%",
                        y=input$season)
  })
  output$player_trends_3p = renderPlotly({
    player_season_trend(input$player, "3P%",
                        y=input$season)
  })
  output$player_trends_3pr = renderPlotly({
    player_season_trend(input$player, "3P-R%",
                        y=input$season)
  })
  output$player_trends_ft = renderPlotly({
    player_season_trend(input$player, "FT%%",
                        y=input$season)
  })
  output$player_trends_ftr = renderPlotly({
    player_season_trend(input$player, "FT-R",
                        y=input$season)
  })
  
  output$player_trends_fg2 = renderPlotly({
    player_season_trend(input$player, "FG%",
                        y=input$season)
  })
  output$player_trends_2p2 = renderPlotly({
    player_season_trend(input$player, "2P%",
                        y=input$season)
  })
  output$player_trends_3p2 = renderPlotly({
    player_season_trend(input$player, "3P%",
                        y=input$season)
  })
  output$player_trends_3pr2 = renderPlotly({
    player_season_trend(input$player, "3P-R%",
                        y=input$season)
  })
  output$player_trends_ft2 = renderPlotly({
    player_season_trend(input$player, "FT%%",
                        y=input$season)
  })
  output$player_trends_ftr2 = renderPlotly({
    player_season_trend(input$player, "FT-R",
                        y=input$season)
  })
  
  ########## plotter_dash ##########
  
  output$tracker_pick_opponent = renderUI({
    conditionalPanel(
      condition = 'input.track_team_type == "OPP"',
      pickerInput(inputId="track_team",
                  label="",
                  choices=c("Non-UAA Scout", uaa_teams)
      )
    )
  })
  
  output$tracker_pick_player = renderUI({
    
    if (input$track_team == "Chicago") {
      
    } else if (input$track_team %in% uaa_teams) {
      if (exists("raw_player_box25")) {
        opponent_players =
          (get_players(input$league, input$track_team, y=input$season) %>%
             mutate(Name = paste(`#`, Player, sep=" - ")))$Name
      } else {
        opponent_players = c()
      }
    } else if (input$league == "WBB") {
      opponent_players = w_non_conf_opponents25
    } else {
      opponent_players = m_non_conf_opponents25
    }
    
    div(
      radioGroupButtons(
        inputId = "track_player",
        label = "Player",
        choices = (if (input$track_team_type == "TEAM")
          c(sort(get_players(input$league, "Chicago", y=input$season)$Player), "Other")
          else opponent_players),
        status = "default"
      ),
      searchInput(
        inputId = "track_player_input",
        label = "",
        placeholder = "Player"
      )
    )
  })
  
  observeEvent(input$record_oreb, {
    if (input$track_player == "Other") {
      player = input$track_player_input
    } else {
      player = input$track_player
    }
    
    new_team = (if (input$track_team_type == "TEAM")
      input$team else input$track_team)
    
    new_event =
      data.frame("Event ID"=new_row_id(event=TRUE),
                 League=input$league,
                 Team=new_team,
                 Date=input$plotter_date,
                 Player=(if (str_detect(player, " - "))
                   strsplit(player, " - ")[[1]][2] else player),
                 OREB=1,
                 DREB=0,
                 AST=0,
                 TO=0,
                 check.names=FALSE)
    
    add_oreb(new_event)
    tracked_events$df = rbind(tracked_events$df, new_event)
  })
  
  observeEvent(input$record_dreb, {
    if (input$track_player == "Other") {
      player = input$track_player_input
    } else {
      player = input$track_player
    }
    
    new_team = (if (input$track_team_type == "TEAM")
      input$team else input$track_team)
    
    new_event =
      data.frame("Event ID"=new_row_id(event=TRUE),
                 League=input$league,
                 Team=new_team,
                 Date=input$plotter_date,
                 Player=(if (str_detect(player, " - "))
                   strsplit(player, " - ")[[1]][2] else player),
                 OREB=0,
                 DREB=1,
                 AST=0,
                 TO=0,
                 check.names=FALSE)
    
    add_dreb(new_event)
    tracked_events$df = rbind(tracked_events$df, new_event)
  })
  
  observeEvent(input$record_ast, {
    if (input$track_player == "Other") {
      player = input$track_player_input
    } else {
      player = input$track_player
    }
    
    new_team = (if (input$track_team_type == "TEAM")
      input$team else input$track_team)
    
    new_event =
      data.frame("Event ID"=new_row_id(event=TRUE),
                 League=input$league,
                 Team=new_team,
                 Date=input$plotter_date,
                 Player=(if (str_detect(player, " - "))
                   strsplit(player, " - ")[[1]][2] else player),
                 OREB=0,
                 DREB=0,
                 AST=1,
                 TO=0,
                 check.names=FALSE)
    
    add_ast(new_event)
    tracked_events$df = rbind(tracked_events$df, new_event)
  })
  
  observeEvent(input$record_to_ps, {
    if (input$track_player == "Other") {
      player = input$track_player_input
    } else {
      player = input$track_player
    }
    
    new_team = (if (input$track_team_type == "TEAM")
      input$team else input$track_team)
    
    new_event =
      data.frame("Event ID"=new_row_id(event=TRUE),
                 League=input$league,
                 Team=new_team,
                 Date=input$plotter_date,
                 Player=(if (str_detect(player, " - "))
                   strsplit(player, " - ")[[1]][2] else player),
                 OREB=0,
                 DREB=0,
                 AST=0,
                 TO="Perimeter/Strip",
                 check.names=FALSE)
    
    add_to(mutate(new_event, TO=1))
    tracked_events$df = rbind(tracked_events$df, new_event)
    
    new_event %>%
      select(-OREB, -DREB, -AST, -TO) %>%
      mutate(TO = "Perimeter/Strip") %>%
      add_turnover()
  })
  observeEvent(input$record_to_bp, {
    if (input$track_player == "Other") {
      player = input$track_player_input
    } else {
      player = input$track_player
    }
    
    new_team = (if (input$track_team_type == "TEAM")
      input$team else input$track_team)
    
    new_event =
      data.frame("Event ID"=new_row_id(event=TRUE),
                 League=input$league,
                 Team=new_team,
                 Date=input$plotter_date,
                 Player=(if (str_detect(player, " - "))
                   strsplit(player, " - ")[[1]][2] else player),
                 OREB=0,
                 DREB=0,
                 AST=0,
                 TO="Bad Pass",
                 check.names=FALSE)
    
    add_to(mutate(new_event, TO=1))
    tracked_events$df = rbind(tracked_events$df, new_event)
    
    new_event %>%
      select(-OREB, -DREB, -AST, -TO) %>%
      mutate(TO = "Bad Pass") %>%
      add_turnover()
  })
  observeEvent(input$record_to_d, {
    if (input$track_player == "Other") {
      player = input$track_player_input
    } else {
      player = input$track_player
    }
    
    new_team = (if (input$track_team_type == "TEAM")
      input$team else input$track_team)
    
    new_event =
      data.frame("Event ID"=new_row_id(event=TRUE),
                 League=input$league,
                 Team=new_team,
                 Date=input$plotter_date,
                 Player=(if (str_detect(player, " - "))
                   strsplit(player, " - ")[[1]][2] else player),
                 OREB=0,
                 DREB=0,
                 AST=0,
                 TO="Drive",
                 check.names=FALSE)
    
    add_to(mutate(new_event, TO=1))
    tracked_events$df = rbind(tracked_events$df, new_event)
    
    new_event %>%
      select(-OREB, -DREB, -AST, -TO) %>%
      mutate(TO = "Drive") %>%
      add_turnover()
  })
  observeEvent(input$record_to_dp, {
    if (input$track_player == "Other") {
      player = input$track_player_input
    } else {
      player = input$track_player
    }
    
    new_team = (if (input$track_team_type == "TEAM")
      input$team else input$track_team)
    
    new_event =
      data.frame("Event ID"=new_row_id(event=TRUE),
                 League=input$league,
                 Team=new_team,
                 Date=input$plotter_date,
                 Player=(if (str_detect(player, " - "))
                   strsplit(player, " - ")[[1]][2] else player),
                 OREB=0,
                 DREB=0,
                 AST=0,
                 TO="Drive + Pass",
                 check.names=FALSE)
    
    add_to(mutate(new_event, TO=1))
    tracked_events$df = rbind(tracked_events$df, new_event)
    
    new_event %>%
      select(-OREB, -DREB, -AST, -TO) %>%
      mutate(TO = "Drive + Pass") %>%
      add_turnover()
  })
  observeEvent(input$record_to_p, {
    if (input$track_player == "Other") {
      player = input$track_player_input
    } else {
      player = input$track_player
    }
    
    new_team = (if (input$track_team_type == "TEAM")
      input$team else input$track_team)
    
    new_event =
      data.frame("Event ID"=new_row_id(event=TRUE),
                 League=input$league,
                 Team=new_team,
                 Date=input$plotter_date,
                 Player=(if (str_detect(player, " - "))
                   strsplit(player, " - ")[[1]][2] else player),
                 OREB=0,
                 DREB=0,
                 AST=0,
                 TO="Post Entry",
                 check.names=FALSE)
    
    add_to(mutate(new_event, TO=1))
    tracked_events$df = rbind(tracked_events$df, new_event)
    
    new_event %>%
      select(-OREB, -DREB, -AST, -TO) %>%
      mutate(TO = "Post Entry") %>%
      add_turnover()
  })
  observeEvent(input$record_to_o, {
    if (input$track_player == "Other") {
      player = input$track_player_input
    } else {
      player = input$track_player
    }
    
    new_team = (if (input$track_team_type == "TEAM")
      input$team else input$track_team)
    
    new_event =
      data.frame("Event ID"=new_row_id(event=TRUE),
                 League=input$league,
                 Team=new_team,
                 Date=input$plotter_date,
                 Player=(if (str_detect(player, " - "))
                   strsplit(player, " - ")[[1]][2] else player),
                 OREB=0,
                 DREB=0,
                 AST=0,
                 TO="Other",
                 check.names=FALSE)
    
    add_to(mutate(new_event, TO=1))
    tracked_events$df = rbind(tracked_events$df, new_event)
    
    new_event %>%
      select(-OREB, -DREB, -AST, -TO) %>%
      mutate(TO = "Other") %>%
      add_turnover()
  })
  
  tracked_shots = reactiveValues(df = NULL)
  tracked_events = reactiveValues(df = NULL)
  
  plot_team_score = reactive({
    master_shots %>%
      filter(League == input$league,
             Date == input$plotter_date,
             Team == "Chicago") %>%
      get_points()
  })
  plot_opponent_score = reactive({
    master_shots %>%
      filter(League == input$league,
             Date == input$plotter_date,
             Team != "Chicago") %>%
      get_points()
  })
  
  output$plotter_score = renderUI({
    
    if (input$season == 2024) {
      players = c(get_players("WBB", "Chicago")$Player,
                  get_players("MBB", "Chicago")$Player)
    } else {
      players = c(w_roster25, m_roster25)
    }
    
    if (!is.null(tracked_shots$df)) {
      team_score =
        tracked_shots$df %>%
        filter(League == input$league,
               Team == "Chicago",
               Date == input$plotter_date,
               Player %in% players) %>%
        get_points()
      opponent_score =
        tracked_shots$df %>%
        filter(League == input$league,
               Team != "Chicago",
               Date == input$plotter_date,
               !(Player %in% players)) %>%
        get_points()
    } else {
      team_score = 0
      opponent_score = 0
    }
    
    h3( paste(plot_team_score() + team_score, 
              plot_opponent_score() + opponent_score, sep="-") )
  })
  
  output$court = renderPlot({
    if (is.null(tracked_shots$df)) {
      get_court( court_indexer() )
    } else {
      df =
        tracked_shots$df %>%
        filter(League == input$league,
               Team == (if (input$track_team_type == "TEAM") input$team
                        else input$track_team))
      
      get_court( court_indexer() ) +
        geom_point(data=filter(df, Outcome == "Make"),
                   aes(x=x,
                       y=y),
                   size=5,
                   stroke=1,
                   color="darkgreen",
                   shape=1,
                   show.legend=FALSE) +
        geom_point(data=filter(df, Outcome == "Miss"),
                   aes(x=x,
                       y=y),
                   size=5,
                   stroke=1,
                   color="red",
                   shape=4,
                   show.legend=FALSE) +
        geom_point(data=filter(df, str_detect(Outcome, "Foul")),
                   aes(x=x,
                       y=y),
                   size=5,
                   stroke=1,
                   color="darkorange",
                   shape=2,
                   show.legend=FALSE)
    }
  })
  
  observeEvent(input$court_click, {
    if (input$track_player == "Other") {
      player = input$track_player_input
    } else {
      player = input$track_player
    }
    
    new_x = get_x(input$court_click$x, input$court_click$y, court_indexer())
    new_y = get_y(input$court_click$x, input$court_click$y, court_indexer())
    new_team = (if (input$track_team_type == "TEAM")
      input$team else input$track_team)
    
    new_shot =
      data.frame(
        "Shot ID"=new_row_id(shot=TRUE),
        League=input$league,
        Team=new_team,
        Date=input$plotter_date,
        Player=(if (str_detect(player, " - "))
          strsplit(player, " - ")[[1]][2] else player),
        x=new_x,
        y=new_y,
        Region=get_region(new_x, new_y),
        "Shot Type"=input$track_shot_type,
        Outcome=input$track_outcome,
        check.names=FALSE
      )
    
    add_shot(new_shot)
    tracked_shots$df = rbind(tracked_shots$df, new_shot)
  })
  
  court_indexer = reactiveVal(1)
  observeEvent(input$rotate_court, {
    court_indexer(court_indexer() + 1)
  })
  
  observeEvent(input$refresh_db, {
    master_shots <<- mongo(db="STACKS",
                           collection="shots",
                           url=connection_url)$find()
    master_events <<- mongo(db="STACKS",
                            collection="events",
                            url=connection_url)$find()
    master_turnovers <<- mongo(db="STACKS",
                               collection="turnovers",
                               url=connection_url)$find()
    
    refresh_dates24()
    # refresh_dates25()
  })
  
  output$tracked_shots_table = render_gt({
    if (!is.null(tracked_shots$df)) {
      current_shots =
        tracked_shots$df %>%
        filter(Date == input$plotter_date,
               League == input$league,
               Team == (if (input$track_team_type == "TEAM") input$team
                        else input$track_team)) %>%
        mutate(across(where(is.numeric), \(x) round(x, 1))) %>%
        select(`Shot ID`, Player, x, y, Region, `Shot Type`, Outcome) %>%
        arrange(desc(`Shot ID`))
      
      current_shots %>%
        gt() %>%
        cols_width(`Shot ID` ~ "100px",
                   x ~ "50px",
                   y ~ "50px") %>%
        opt_interactive(use_search=TRUE,
                        use_highlight=TRUE,
                        use_page_size_select=TRUE,
                        page_size_default=5,
                        page_size_values=c(5, 10))
    } else {
      gt( data.frame() )
    }
  })
  
  output$tracked_events_table = render_gt({
    if (!is.null(tracked_events$df)) {
      current_events =
        tracked_events$df %>%
        filter(Date == input$plotter_date,
               League == input$league,
               Team == (if (input$track_team_type == "TEAM") input$team
                        else input$track_team)) %>%
        mutate(across(where(is.numeric), \(x) round(x, 1))) %>%
        select(`Event ID`, Player, OREB, DREB, AST, TO) %>%
        arrange(desc(`Event ID`))
      
      current_events %>%
        gt() %>%
        cols_width(`Event ID` ~ "100px") %>%
        opt_interactive(use_search=TRUE,
                        use_highlight=TRUE,
                        use_page_size_select=TRUE,
                        page_size_default=5,
                        page_size_values=c(5, 10))
    } else {
      gt( data.frame() )
    }
  })
  
  output$remove_shot = renderUI({
    if (!is.null(tracked_shots$df)) {
      remove_shot_choices =
        (tracked_shots$df %>%
           filter(Date == input$plotter_date,
                  League == input$league,
                  Team == (if (input$track_team_type == "TEAM") input$team
                           else input$track_team)))$`Shot ID`
      
      div(
        pickerInput(
          inputId="shot_to_remove",
          label="Remove shot by ID:",
          choices=rev(remove_shot_choices),
          options=list(title="Find shot ID")),
        actionButton("remove_shot_button", "Remove")
      )
    }
  })
  
  output$remove_event = renderUI({
    if (!is.null(tracked_events$df)) {
      remove_event_choices =
        (tracked_events$df %>%
           filter(Date == input$plotter_date,
                  League == input$league,
                  Team == (if (input$track_team_type == "TEAM") input$team
                           else input$track_team)))$`Event ID`
      
      div(
        pickerInput(
          inputId="event_to_remove",
          label="Remove event by ID:",
          choices=rev(remove_event_choices),
          options=list(title="Find event ID")),
        actionButton("remove_event_button", "Remove")
      )
    }
  })
  
  observeEvent(input$remove_shot_button,
               if (!is.na(as.numeric(input$shot_to_remove))) {
                 remove_shot(input$shot_to_remove)
                 tracked_shots$df =
                   filter(tracked_shots$df,
                          `Shot ID` != input$shot_to_remove)
               }
  )
  observeEvent(input$remove_event_button,
               if (!is.na(as.numeric(input$event_to_remove))) {
                 remove_event(input$event_to_remove)
                 tracked_events$df =
                   filter(tracked_events$df,
                          `Event ID` != input$event_to_remove)
               }
  )
  
}

shinyApp(ui, server)

