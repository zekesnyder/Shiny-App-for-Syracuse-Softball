library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(readxl)
library(scales)
library(stringr)
library(rlang)

# ==============================================================================
# DATA LOADING
# ==============================================================================

df_complete_1 <- read.csv("atbat_with_players",
                          header = TRUE,
                          stringsAsFactors = FALSE,
                          strip.white = TRUE,
                          na.strings = c("", "NA"))

df_complete <- df_complete_1 %>% select(-X)
softball_pbp <- df_complete

SB_team_stats_all <- read_excel('SB_team_stats_all.xlsx')
SB_team_stats_conf <- read_excel('SB_team_stats_conf.xlsx')

SB_player_stats_hitting <- read_excel('SB_player_stats_hitting.xlsx')
SB_player_stats_bat_conf <- read_excel('SB_player_stats_bat_conf.xlsx')
SB_player_stats_pitching <- read_excel('SB_player_stats_pitching.xlsx')
SB_player_stats_pitch_conf <- read_excel('SB_player_stats_pitch_conf.xlsx')
SB_player_stats_fielding <- read_excel('SB_player_stats_fielding.xlsx')
SB_player_stat_field_conf <- read_excel('SB_player_stat_field_conf (1).xlsx')

colnames(SB_player_stats_bat_conf)[colnames(SB_player_stats_bat_conf) == "Player"] <- "Name"
colnames(SB_player_stats_bat_conf)[colnames(SB_player_stats_bat_conf) == "AVG"] <- "Avg"

SB_player_stats_hitting <- SB_player_stats_hitting %>%
  mutate(across(c(Avg, OPS, H, HR, RBI, R, BB, SO), as.numeric))
SB_player_stats_bat_conf <- SB_player_stats_bat_conf %>%
  mutate(across(c(Avg, OPS, H, HR, RBI, R, BB, SO), as.numeric))
SB_player_stats_pitching <- SB_player_stats_pitching %>%
  mutate(across(c(ERA, WHIP, SO, BB, IP, H, R, ER), as.numeric))
SB_player_stats_pitch_conf <- SB_player_stats_pitch_conf %>%
  mutate(across(c(ERA, WHIP, SO, BB, IP, H, R, ER), as.numeric))
SB_player_stats_fielding <- SB_player_stats_fielding %>%
  mutate(across(c(`FLD%`, C, PO, A, E), as.numeric))
SB_player_stat_field_conf <- SB_player_stat_field_conf %>%
  mutate(across(c(`FLD%`, C, PO, A, E), as.numeric))

# ==============================================================================
# INNING ANALYSIS DATA PREP
# ==============================================================================

TEAM_NAME <- "Syracuse"

syracuse_pitching <- softball_pbp %>% filter(pitching_team == TEAM_NAME)
pitcher_runs_by_inning <- syracuse_pitching %>%
  group_by(pitcher, inning, opponent) %>%
  summarise(
    runs_allowed = sum(rbi, na.rm = TRUE),
    at_bats_faced = n(),
    hits_allowed = sum(is_hit, na.rm = TRUE),
    walks_allowed = sum(is_walk, na.rm = TRUE),
    strikeouts = sum(is_strikeout, na.rm = TRUE),
    .groups = "drop"
  )
syracuse_pitchers <- unique(syracuse_pitching$pitcher)
pitching_opponents <- unique(syracuse_pitching$opponent)

syracuse_hitting <- softball_pbp %>% filter(batting_team == TEAM_NAME)
hitter_hits_by_inning <- syracuse_hitting %>%
  group_by(hitter, inning, opponent) %>%
  summarise(
    total_hits = sum(is_hit, na.rm = TRUE),
    singles = sum(is_single, na.rm = TRUE),
    doubles = sum(is_double, na.rm = TRUE),
    triples = sum(is_triple, na.rm = TRUE),
    homers = sum(is_homer, na.rm = TRUE),
    rbis = sum(rbi, na.rm = TRUE),
    walks = sum(is_walk, na.rm = TRUE),
    strikeouts = sum(is_strikeout, na.rm = TRUE),
    at_bats = n(),
    .groups = "drop"
  )
syracuse_hitters <- unique(syracuse_hitting$hitter)
hitting_opponents <- unique(syracuse_hitting$opponent)

# ==============================================================================
# SPLIT STATS DATA PREP
# ==============================================================================

acc_teams <- c(
  "Clemson", "Duke", "Florida State", "Georgia Tech", "Louisville",
  "North Carolina", "NC State", "Notre Dame", "Pitt", "Syracuse",
  "Virginia", "Virginia Tech", "Boston College"
)

softball_hitters_split <- softball_pbp %>%
  filter(batting_team == "Syracuse") %>%
  mutate(
    batter = hitter,
    game_date = as.Date("2024-02-01") + dense_rank(game_id) - 1,
    split_type = if_else(opponent %in% acc_teams, "Conference", "Non-Conference")
  )

split_player_list <- sort(unique(softball_hitters_split$batter))

calc_batting <- function(df, groups = NULL) {
  df %>%
    mutate(
      AB_flag  = batting_outcome %in% c("single","double","triple","home_run",
                                        "groundout","flyout","reached_on_error","other"),
      H_flag   = batting_outcome %in% c("single","double","triple","home_run"),
      BB_flag  = is_walk == 1,
      HBP_flag = is_hbp == 1,
      SF_flag  = is_sac_fly == 1,
      SH_flag  = is_sac_bunt == 1,
      TB_val   = is_single + 2*is_double + 3*is_triple + 4*is_homer
    ) %>%
    group_by(across(all_of(groups))) %>%
    summarise(
      PA = sum(AB_flag + BB_flag + HBP_flag + SF_flag + SH_flag),
      AB = sum(AB_flag),
      H  = sum(H_flag),
      BB = sum(BB_flag),
      HBP = sum(HBP_flag),
      TB = sum(TB_val),
      R  = sum(rbi),
      AVG = round(ifelse(AB>0, H/AB, NA), 3),
      OBP = round(ifelse(PA>0, (H+BB+HBP)/PA, NA), 3),
      SLG = round(ifelse(AB>0, TB/AB, NA), 3),
      OPS = round(OBP + SLG, 3),
      .groups = "drop"
    )
}

# ==============================================================================
# SYRACUSE COLORS
# ==============================================================================

CUSE_ORANGE <- "#F76900"
CUSE_BLUE <- "#000E54"

# ==============================================================================
# UI
# ==============================================================================

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "Syracuse Softball"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Team Stats", tabName = "team_stats", icon = icon("users")),
      menuItem("By Inning Analysis", tabName = "inning_analysis", icon = icon("chart-bar")),
      menuItem("Split Stats", tabName = "split_stats", icon = icon("arrows-alt-h")),
      menuItem("Spray Chart", tabName = "spray_chart", icon = icon("map")),
      menuItem("Player Comparison", tabName = "stats_comparison", icon = icon("balance-scale")),
      menuItem("Leaderboards", tabName = "leaderboards", icon = icon("trophy"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # ========================================================================
      # TEAM STATS TAB
      # ========================================================================
      tabItem(tabName = "team_stats",
              h2("Team Statistics"),
              fluidRow(
                box(title = "Conference Stats", width = 12, status = "primary", solidHeader = TRUE,
                    div(style = "overflow-x: auto;", DT::dataTableOutput("conference_stats"))
                )
              ),
              fluidRow(
                box(title = "All Games Stats", width = 12, status = "primary", solidHeader = TRUE,
                    div(style = "overflow-x: auto;", DT::dataTableOutput("all_stats"))
                )
              )
      ),
      
      # ========================================================================
      # BY INNING ANALYSIS TAB
      # ========================================================================
      tabItem(tabName = "inning_analysis",
              h2("By Inning Analysis"),
              
              fluidRow(
                column(12, h3("Pitcher Analysis - Runs Allowed by Inning"),
                       p("Analyze how Syracuse pitchers perform across innings based on runs allowed."))
              ),
              fluidRow(
                column(3,
                       wellPanel(
                         h4("Pitcher Filters"),
                         selectInput("selected_pitcher", "Select Pitcher:",
                                     choices = c("All Pitchers", syracuse_pitchers), selected = "All Pitchers"),
                         selectInput("pitcher_opponent", "Select Opponent:",
                                     choices = c("All Opponents", sort(pitching_opponents)), selected = "All Opponents"),
                         sliderInput("pitcher_inning_range", "Inning Range:",
                                     min = 1, max = max(syracuse_pitching$inning, na.rm = TRUE),
                                     value = c(1, max(syracuse_pitching$inning, na.rm = TRUE)), step = 1),
                         hr(),
                         h5("Pitcher Summary"),
                         verbatimTextOutput("pitcher_summary")
                       )
                ),
                column(9, plotOutput("runs_allowed_plot", height = "350px"))
              ),
              
              hr(),
              
              fluidRow(
                column(12, h3("Hitting Analysis - Hits by Inning"),
                       p("Analyze Syracuse hitters' performance across innings."))
              ),
              fluidRow(
                column(3,
                       wellPanel(
                         h4("Hitter Filters"),
                         selectInput("selected_hitter", "Select Hitter:",
                                     choices = c("All Hitters", syracuse_hitters), selected = "All Hitters"),
                         selectInput("hitter_opponent", "Select Opponent:",
                                     choices = c("All Opponents", sort(hitting_opponents)), selected = "All Opponents"),
                         sliderInput("hitter_inning_range", "Inning Range:",
                                     min = 1, max = max(syracuse_hitting$inning, na.rm = TRUE),
                                     value = c(1, max(syracuse_hitting$inning, na.rm = TRUE)), step = 1),
                         hr(),
                         h5("Hitter Summary"),
                         verbatimTextOutput("hitter_summary")
                       )
                ),
                column(9,
                       plotOutput("hits_by_inning_plot", height = "350px"),
                       plotOutput("hit_type_breakdown_plot", height = "350px")
                )
              )
      ),
      
      # ========================================================================
      # SPLIT STATS TAB (Conference vs Non-Conference)
      # ========================================================================
      tabItem(tabName = "split_stats",
              h2("Conference vs Non-Conference Splits"),
              
              fluidRow(
                box(width = 4, status = "primary", solidHeader = TRUE, title = "Player",
                    selectInput("split_player", "Select Player:", c("All Players", split_player_list))),
                box(width = 4, status = "primary", solidHeader = TRUE, title = "Metric",
                    selectInput("split_metric", "Select Metric:", c("AVG", "OBP", "SLG", "OPS", "H", "R"))),
                box(width = 4, status = "primary", solidHeader = TRUE, title = "View",
                    radioButtons("split_view", "View Type:", c("Side-by-Side", "Combined")))
              ),
              
              fluidRow(
                box(width = 12, status = "primary", solidHeader = TRUE, title = "Filters",
                    column(6,
                           dateRangeInput("split_date_range", "Date Range:",
                                          start = min(softball_hitters_split$game_date),
                                          end = max(softball_hitters_split$game_date))
                    ),
                    column(6,
                           selectizeInput("split_opp_filter", "Filter by Opponent:",
                                          choices = c("All", sort(unique(softball_hitters_split$opponent))),
                                          multiple = TRUE, selected = "All")
                    )
                )
              ),
              
              uiOutput("split_plot_row"),
              
              fluidRow(
                box(width = 6, title = "Conference Stats", status = "primary", solidHeader = TRUE,
                    DTOutput("table_conf")),
                box(width = 6, title = "Non-Conference Stats", status = "primary", solidHeader = TRUE,
                    DTOutput("table_nonconf"))
              )
      ),
      
      # ========================================================================
      # SPRAY CHART TAB
      # ========================================================================
      tabItem(tabName = "spray_chart",
              h2("Spray Chart"),
              fluidRow(
                box(title = "Filters", width = 12, status = "primary", solidHeader = TRUE,
                    column(6,
                           selectizeInput("player_select_spray", "Select Player(s):",
                                          choices = c("All Players", sort(unique(df_complete$hitter[!is.na(df_complete$hitter)]))),
                                          selected = "All Players", multiple = TRUE)
                    ),
                    column(6,
                           actionButton("apply_filters_spray", "Apply Filters", class = "btn-primary btn-lg",
                                        style = "margin-top: 25px;")
                    )
                )
              ),
              fluidRow(
                box(title = "Spray Chart Visualization", width = 12, status = "primary", solidHeader = TRUE,
                    plotOutput("spray_chart_plot", height = "600px")
                )
              )
      ),
      
      # ========================================================================
      # PLAYER COMPARISON TAB 
      # ========================================================================
      tabItem(tabName = "stats_comparison",
              h2("Player Season Comparison"),
              p("Compare individual player stats between overall season and conference games."),
              fluidRow(
                box(title = "Comparison Settings", status = "primary", solidHeader = TRUE, width = 12,
                    column(4, selectInput("stat_category", "Select Category:",
                                          choices = c("Batting", "Pitching", "Fielding"), selected = "Batting")),
                    column(4, uiOutput("player_select_conf")),
                    column(4, uiOutput("metric_select_conf"))
                )
              ),
              fluidRow(
                box(title = "Overall vs Conference Comparison", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("comparison_chart", height = "400px")
                )
              ),
              fluidRow(
                box(title = textOutput("overall_box_title"), status = "primary", solidHeader = TRUE, width = 6,
                    plotOutput("overall_metric_plot", height = "300px")
                ),
                box(title = textOutput("conf_box_title"), status = "primary", solidHeader = TRUE, width = 6,
                    plotOutput("conf_metric_plot", height = "300px")
                )
              )
      ),
      
      # ========================================================================
      # LEADERBOARDS TAB
      # ========================================================================
      tabItem(tabName = "leaderboards",
              h2("Leaderboards"),
              fluidRow(
                box(title = "Leaderboard Settings", width = 12, status = "primary", solidHeader = TRUE,
                    column(4, selectInput("leaderboard_category", "Category:",
                                          choices = c("Batting", "Pitching", "Fielding"), selected = "Batting")),
                    column(4, uiOutput("leaderboard_metric_ui")),
                    column(4, radioButtons("sort_order", "Sort Order:",
                                           choices = c("Descending", "Ascending"), selected = "Descending"))
                )
              ),
              fluidRow(
                box(title = "Team Leaderboard", width = 12, status = "primary", solidHeader = TRUE,
                    DT::dataTableOutput("team_leaderboard"),
                    br(),
                    plotOutput("leaderboard_plot", height = "400px")
                )
              ),
              fluidRow(
                box(title = "Export Options", width = 12,
                    downloadButton("download_leaderboard", "Download Leaderboard (CSV)")
                )
              )
      )
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================

server <- function(input, output, session) {
  
  # ============================================================================
  # TEAM STATS TAB
  # ============================================================================
  
  output$conference_stats <- DT::renderDataTable({
    SB_team_stats_conf
  }, options = list(pageLength = 10, scrollX = TRUE))
  
  output$all_stats <- DT::renderDataTable({
    SB_team_stats_all
  }, options = list(pageLength = 10, scrollX = TRUE))
  
  # ============================================================================
  # BY INNING ANALYSIS TAB
  # ============================================================================
  
  filtered_pitcher_data <- reactive({
    df <- pitcher_runs_by_inning %>%
      filter(inning >= input$pitcher_inning_range[1] & inning <= input$pitcher_inning_range[2])
    if (input$selected_pitcher != "All Pitchers") df <- df %>% filter(pitcher == input$selected_pitcher)
    if (input$pitcher_opponent != "All Opponents") df <- df %>% filter(opponent == input$pitcher_opponent)
    df
  })
  
  filtered_hitter_data <- reactive({
    df <- hitter_hits_by_inning %>%
      filter(inning >= input$hitter_inning_range[1] & inning <= input$hitter_inning_range[2])
    if (input$selected_hitter != "All Hitters") df <- df %>% filter(hitter == input$selected_hitter)
    if (input$hitter_opponent != "All Opponents") df <- df %>% filter(opponent == input$hitter_opponent)
    df
  })
  
  output$runs_allowed_plot <- renderPlot({
    df <- filtered_pitcher_data()
    if (nrow(df) == 0) return(NULL)
    
    plot_data <- df %>% group_by(inning) %>%
      summarise(runs_allowed = sum(runs_allowed, na.rm = TRUE), .groups = "drop")
    
    pitcher_part <- ifelse(input$selected_pitcher == "All Pitchers", "All Pitchers", input$selected_pitcher)
    opponent_part <- ifelse(input$pitcher_opponent == "All Opponents", "", paste("vs", input$pitcher_opponent))
    plot_title <- paste("Runs Allowed by Inning -", pitcher_part, opponent_part)
    
    ggplot(plot_data, aes(x = factor(inning), y = runs_allowed)) +
      geom_bar(stat = "identity", fill = CUSE_ORANGE, color = "black", width = 0.7) +
      geom_text(aes(label = runs_allowed), vjust = -0.5, size = 4) +
      labs(title = plot_title, x = "Inning", y = "Runs Allowed") +
      theme_minimal() +
      theme(plot.title = element_text(size = 16, face = "bold"),
            axis.text = element_text(size = 12), axis.title = element_text(size = 14)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
  })
  
  output$hits_by_inning_plot <- renderPlot({
    df <- filtered_hitter_data()
    if (nrow(df) == 0) return(NULL)
    
    plot_data <- df %>% group_by(inning) %>%
      summarise(total_hits = sum(total_hits, na.rm = TRUE), .groups = "drop")
    
    hitter_part <- ifelse(input$selected_hitter == "All Hitters", "All Hitters", input$selected_hitter)
    opponent_part <- ifelse(input$hitter_opponent == "All Opponents", "", paste("vs", input$hitter_opponent))
    plot_title <- paste("Hits by Inning -", hitter_part, opponent_part)
    
    ggplot(plot_data, aes(x = factor(inning), y = total_hits)) +
      geom_bar(stat = "identity", fill = CUSE_ORANGE, color = "black", width = 0.7) +
      geom_text(aes(label = total_hits), vjust = -0.5, size = 4) +
      labs(title = plot_title, x = "Inning", y = "Total Hits") +
      theme_minimal() +
      theme(plot.title = element_text(size = 16, face = "bold"),
            axis.text = element_text(size = 12), axis.title = element_text(size = 14)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
  })
  
  output$hit_type_breakdown_plot <- renderPlot({
    df <- filtered_hitter_data()
    if (nrow(df) == 0) return(NULL)
    
    plot_data <- df %>% group_by(inning) %>%
      summarise(Singles = sum(singles, na.rm = TRUE), Doubles = sum(doubles, na.rm = TRUE),
                Triples = sum(triples, na.rm = TRUE), "Home Runs" = sum(homers, na.rm = TRUE), .groups = "drop") %>%
      pivot_longer(cols = c(Singles, Doubles, Triples, "Home Runs"), names_to = "hit_type", values_to = "count") %>%
      mutate(hit_type = factor(hit_type, levels = c("Singles", "Doubles", "Triples", "Home Runs")))
    
    hitter_part <- ifelse(input$selected_hitter == "All Hitters", "All Hitters", input$selected_hitter)
    opponent_part <- ifelse(input$hitter_opponent == "All Opponents", "", paste("vs", input$hitter_opponent))
    plot_title <- paste("Hit Type Breakdown -", hitter_part, opponent_part)
    
    ggplot(plot_data, aes(x = factor(inning), y = count, fill = hit_type)) +
      geom_bar(stat = "identity", position = "stack", color = "black", width = 0.7) +
      scale_fill_manual(values = c("Singles" = "#F76900", "Doubles" = "#FF8C00",
                                   "Triples" = "#FFA500", "Home Runs" = "#D44500")) +
      labs(title = plot_title, x = "Inning", y = "Count", fill = "Hit Type") +
      theme_minimal() +
      theme(plot.title = element_text(size = 16, face = "bold"),
            axis.text = element_text(size = 12), axis.title = element_text(size = 14), legend.position = "bottom") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
  })
  
  output$pitcher_summary <- renderPrint({
    df <- filtered_pitcher_data()
    if (nrow(df) == 0) { cat("No data available."); return() }
    cat("Total Runs Allowed:", sum(df$runs_allowed, na.rm = TRUE), "\n")
    cat("Total Hits Allowed:", sum(df$hits_allowed, na.rm = TRUE), "\n")
    cat("Total Walks:", sum(df$walks_allowed, na.rm = TRUE), "\n")
    cat("Total Strikeouts:", sum(df$strikeouts, na.rm = TRUE), "\n")
    cat("At Bats Faced:", sum(df$at_bats_faced, na.rm = TRUE), "\n")
  })
  
  output$hitter_summary <- renderPrint({
    df <- filtered_hitter_data()
    if (nrow(df) == 0) { cat("No data available."); return() }
    total_ab <- sum(df$at_bats, na.rm = TRUE)
    total_hits <- sum(df$total_hits, na.rm = TRUE)
    avg <- ifelse(total_ab > 0, round(total_hits / total_ab, 3), 0)
    cat("Batting Avg:", format(avg, nsmall = 3), "\n")
    cat("Total Hits:", total_hits, "\n")
    cat("  Singles:", sum(df$singles, na.rm = TRUE), "\n")
    cat("  Doubles:", sum(df$doubles, na.rm = TRUE), "\n")
    cat("  Triples:", sum(df$triples, na.rm = TRUE), "\n")
    cat("  Home Runs:", sum(df$homers, na.rm = TRUE), "\n")
    cat("RBIs:", sum(df$rbis, na.rm = TRUE), "\n")
    cat("Walks:", sum(df$walks, na.rm = TRUE), "\n")
    cat("Strikeouts:", sum(df$strikeouts, na.rm = TRUE), "\n")
  })
  
  # ============================================================================
  # SPLIT STATS TAB 
  # ============================================================================
  
  filtered_split_hitters <- reactive({
    df <- softball_hitters_split
    
    if (!"All" %in% input$split_opp_filter) {
      df <- df %>% filter(opponent %in% input$split_opp_filter)
    }
    
    df <- df %>% filter(
      game_date >= input$split_date_range[1],
      game_date <= input$split_date_range[2]
    )
    
    df
  })
  
  split_df <- reactive({
    df <- filtered_split_hitters()
    stats <- calc_batting(df, c("batter", "split_type"))
    stats <- stats %>% filter(PA > 0)
    stats
  })
  
  output$split_plot_row <- renderUI({
    if (input$split_view == "Combined") {
      fluidRow(
        box(width = 12, status = "primary", solidHeader = TRUE, title = "Combined View",
            plotOutput("plot_combined", height = "550px"))
      )
    } else {
      fluidRow(
        box(width = 6, status = "primary", solidHeader = TRUE, title = "Conference",
            plotOutput("plot_conf", height = "450px")),
        box(width = 6, status = "primary", solidHeader = TRUE, title = "Non-Conference",
            plotOutput("plot_nonconf", height = "450px"))
      )
    }
  })
  
  output$plot_combined <- renderPlot({
    df <- split_df()
    m <- sym(input$split_metric)
    
    if (input$split_player != "All Players") df <- df %>% filter(batter == input$split_player)
    
    req(nrow(df) > 0)
    
    ggplot(df, aes(x = reorder(batter, !!m), y = !!m, fill = split_type)) +
      geom_col(position = position_dodge(width = 0.7), width = 0.6, color = "black") +
      coord_flip() +
      scale_fill_manual(values = c("Conference" = CUSE_BLUE, "Non-Conference" = CUSE_ORANGE)) +
      theme_minimal(base_size = 15) +
      labs(x = "", y = input$split_metric, fill = "Split Type",
           title = paste(input$split_metric, "- Conference vs Non-Conference")) +
      theme(plot.title = element_text(face = "bold"))
  })
  
  output$plot_conf <- renderPlot({
    df <- split_df()
    m <- sym(input$split_metric)
    
    if (input$split_player != "All Players") df <- df %>% filter(batter == input$split_player)
    
    df <- df %>% filter(split_type == "Conference")
    
    req(nrow(df) > 0)
    
    ggplot(df, aes(x = reorder(batter, !!m), y = !!m)) +
      geom_col(fill = CUSE_BLUE, color = "black") +
      coord_flip() +
      theme_minimal(base_size = 14) +
      labs(x = "", y = input$split_metric, title = paste("Conference -", input$split_metric)) +
      theme(plot.title = element_text(face = "bold"))
  })
  
  output$plot_nonconf <- renderPlot({
    df <- split_df()
    m <- sym(input$split_metric)
    
    if (input$split_player != "All Players") df <- df %>% filter(batter == input$split_player)
    
    df <- df %>% filter(split_type == "Non-Conference")
    
    req(nrow(df) > 0)
    
    ggplot(df, aes(x = reorder(batter, !!m), y = !!m)) +
      geom_col(fill = CUSE_ORANGE, color = "black") +
      coord_flip() +
      theme_minimal(base_size = 14) +
      labs(x = "", y = input$split_metric, title = paste("Non-Conference -", input$split_metric)) +
      theme(plot.title = element_text(face = "bold"))
  })
  
  dt_opts <- list(scrollX = TRUE, scrollY = "350px", paging = FALSE, autoWidth = TRUE,
                  deferRender = TRUE, scrollCollapse = TRUE)
  
  output$table_conf <- renderDT({
    split_df() %>% filter(split_type == "Conference") %>% select(-split_type)
  }, options = dt_opts)
  
  output$table_nonconf <- renderDT({
    split_df() %>% filter(split_type == "Non-Conference") %>% select(-split_type)
  }, options = dt_opts)
  
  # ============================================================================
  # SPRAY CHART TAB
  # ============================================================================
  
  output$spray_chart_plot <- renderPlot({
    plot_data <- df_complete %>% filter(is_hit == TRUE) %>%
      {if (!"All Players" %in% input$player_select_spray) filter(., hitter %in% input$player_select_spray) else .}
    
    zone_counts <- plot_data %>% count(zone_10) %>% mutate(percentage = n / sum(n) * 100)
    
    create_wedge <- function(start_angle, end_angle, inner_r = 50, outer_r = 90) {
      angles <- seq(start_angle, end_angle, length.out = 50)
      data.frame(x = c(cos(angles) * inner_r, rev(cos(angles) * outer_r)),
                 y = c(sin(angles) * inner_r, rev(sin(angles) * outer_r)))
    }
    create_inf_wedge <- function(start_angle, end_angle, inner_r = 25, outer_r = 50) {
      angles <- seq(start_angle, end_angle, length.out = 50)
      data.frame(x = c(cos(angles) * inner_r, rev(cos(angles) * outer_r)),
                 y = c(sin(angles) * inner_r, rev(sin(angles) * outer_r)))
    }
    create_infield_box <- function(x_start, x_end, y_start, y_end) {
      data.frame(x = c(x_start, x_end, x_end, x_start), y = c(y_start, y_start, y_end, y_end))
    }
    
    zone_wedges <- list(
      zone_lf_line = create_wedge(pi * 0.70, pi * 0.75), zone_lf = create_wedge(pi * 0.625, pi * 0.70),
      zone_lcf = create_wedge(pi * 0.55, pi * 0.625), zone_cf = create_wedge(pi * 0.45, pi * 0.55),
      zone_rcf = create_wedge(pi * 0.375, pi * 0.45), zone_rf = create_wedge(pi * 0.30, pi * 0.375),
      zone_rf_line = create_wedge(pi * 0.25, pi * 0.30)
    )
    infield_wedges <- list(
      zone_3b = create_inf_wedge(pi * 0.675, pi * 0.75), zone_ss = create_inf_wedge(pi * 0.525, pi * 0.675),
      zone_2b = create_inf_wedge(pi * 0.375, pi * 0.525), zone_1b = create_inf_wedge(pi * 0.25, pi * 0.375)
    )
    infield_zones <- list(zone_p = create_infield_box(-5, 5, 14, 24), zone_c = create_infield_box(-5, 5, 2, 12))
    all_zones <- c(zone_wedges, infield_wedges, infield_zones)
    
    plot_zones <- bind_rows(lapply(names(all_zones), function(zone_name) {
      zone_poly <- all_zones[[zone_name]]
      count_data <- zone_counts %>% filter(zone_10 == zone_name)
      zone_poly$zone <- zone_name
      zone_poly$count <- ifelse(nrow(count_data) > 0, count_data$n[1], 0)
      zone_poly$pct <- ifelse(nrow(count_data) > 0, count_data$percentage[1], 0)
      zone_poly
    }))
    
    label_data <- plot_zones %>% group_by(zone) %>%
      summarise(x = mean(x), y = mean(y), count = first(count), pct = first(pct), .groups = "drop") %>%
      mutate(text_color = ifelse(pct < 8, "black", "white"))
    
    ggplot() +
      geom_polygon(data = plot_zones, aes(x = x, y = y, group = zone, fill = count),
                   color = "white", size = 1.5, alpha = 0.8) +
      geom_text(data = label_data, aes(x = x, y = y, label = paste0(round(pct, 1), "%\n(", count, ")"),
                                       color = text_color), size = 3.5, fontface = "bold") +
      scale_color_identity() +
      geom_point(aes(x = 0, y = 0), size = 5, shape = 23, fill = "white", color = "black", stroke = 2) +
      geom_point(aes(x = -30, y = 30), size = 4, shape = 22, fill = "white", color = "black") +
      geom_point(aes(x = 0, y = 42), size = 4, shape = 22, fill = "white", color = "black") +
      geom_point(aes(x = 30, y = 30), size = 4, shape = 22, fill = "white", color = "black") +
      scale_fill_gradient(low = "#fee5d9", high = "#a50f15", name = "Hits", na.value = "gray80") +
      coord_fixed(ratio = 1, xlim = c(-100, 100), ylim = c(-5, 95)) +
      theme_void() +
      theme(legend.position = "right", plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
            plot.background = element_rect(fill = "palegreen", color = NA)) +
      labs(title = ifelse(!"All Players" %in% input$player_select_spray,
                          paste("Spray Chart:", paste(input$player_select_spray, collapse = ", ")), "Team Spray Chart"))
  })
  
  # ============================================================================
  # PLAYER COMPARISON TAB 
  # ============================================================================
  
  current_overall_data <- reactive({
    switch(input$stat_category, "Batting" = SB_player_stats_hitting,
           "Pitching" = SB_player_stats_pitching, "Fielding" = SB_player_stats_fielding)
  })
  current_conf_data <- reactive({
    switch(input$stat_category, "Batting" = SB_player_stats_bat_conf,
           "Pitching" = SB_player_stats_pitch_conf, "Fielding" = SB_player_stat_field_conf)
  })
  
  output$player_select_conf <- renderUI({
    overall <- current_overall_data(); conf <- current_conf_data()
    overall_name_col <- if("Name" %in% colnames(overall)) "Name" else "Player"
    conf_name_col <- if("Name" %in% colnames(conf)) "Name" else "Player"
    players <- intersect(overall[[overall_name_col]], conf[[conf_name_col]])
    selectInput("player_select_conf", "Select Player:", choices = sort(players), selected = players[1])
  })
  
  output$metric_select_conf <- renderUI({
    metrics <- switch(input$stat_category,
                      "Batting" = c("Avg", "OPS", "H", "HR", "RBI", "R", "BB", "SO"),
                      "Pitching" = c("ERA", "WHIP", "SO", "BB", "IP", "H", "R", "ER"),
                      "Fielding" = c("FLD%", "C", "PO", "A", "E"))
    selectInput("metric_select_conf", "Select Metric:", choices = metrics, selected = metrics[1])
  })
  
  output$overall_box_title <- renderText({ paste(input$player_select_conf, "- Overall Season") })
  output$conf_box_title <- renderText({ paste(input$player_select_conf, "- Conference Games") })
  
  output$comparison_chart <- renderPlot({
    req(input$player_select_conf, input$metric_select_conf)
    overall <- current_overall_data(); conf <- current_conf_data()
    overall_name_col <- if("Name" %in% colnames(overall)) "Name" else "Player"
    conf_name_col <- if("Name" %in% colnames(conf)) "Name" else "Player"
    overall_val <- as.numeric(overall[overall[[overall_name_col]] == input$player_select_conf, input$metric_select_conf, drop = TRUE])
    conf_val <- as.numeric(conf[conf[[conf_name_col]] == input$player_select_conf, input$metric_select_conf, drop = TRUE])
    if(length(overall_val) == 0 || length(conf_val) == 0 || is.na(overall_val) || is.na(conf_val)) return(NULL)
    
    comp_data <- data.frame(Type = c("Overall", "Conference"), Value = c(overall_val, conf_val))
    diff <- overall_val - conf_val
    diff_pct <- if(conf_val != 0) (diff / conf_val) * 100 else 0
    
    ggplot(comp_data, aes(x = Type, y = Value, fill = Type)) +
      geom_col(width = 0.6, color = "black") +
      geom_text(aes(label = round(Value, 3)), vjust = -0.5, size = 7, fontface = "bold") +
      scale_fill_manual(values = c("Overall" = CUSE_ORANGE, "Conference" = CUSE_BLUE)) +
      labs(title = paste(input$player_select_conf, "-", input$metric_select_conf, "Comparison"),
           subtitle = sprintf("Difference: %+.3f (%+.1f%%)", diff, diff_pct), y = input$metric_select_conf) +
      theme_minimal() +
      theme(legend.position = "none", axis.title.x = element_blank(), axis.text = element_text(size = 12),
            plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
            plot.subtitle = element_text(size = 14, hjust = 0.5, color = if(diff > 0) "darkgreen" else "red"),
            panel.grid.major.x = element_blank()) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
  })
  
  output$overall_metric_plot <- renderPlot({
    req(input$player_select_conf, input$metric_select_conf)
    overall <- current_overall_data()
    name_col <- if("Name" %in% colnames(overall)) "Name" else "Player"
    player_data <- overall[overall[[name_col]] == input$player_select_conf, ]
    if(nrow(player_data) == 0) return(NULL)
    value <- as.numeric(player_data[[input$metric_select_conf]])
    if(is.na(value)) return(NULL)
    
    ggplot(data.frame(x = input$metric_select_conf, y = value), aes(x = x, y = y)) +
      geom_col(fill = CUSE_ORANGE, width = 0.5, color = "black") +
      geom_text(aes(label = round(y, 3)), vjust = -0.5, size = 6, fontface = "bold") +
      theme_minimal() +
      theme(axis.title = element_blank(), axis.text.x = element_text(size = 14, face = "bold"),
            axis.text.y = element_text(size = 12), panel.grid.major.x = element_blank()) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
  })
  
  output$conf_metric_plot <- renderPlot({
    req(input$player_select_conf, input$metric_select_conf)
    conf <- current_conf_data()
    name_col <- if("Name" %in% colnames(conf)) "Name" else "Player"
    player_data <- conf[conf[[name_col]] == input$player_select_conf, ]
    if(nrow(player_data) == 0) return(NULL)
    value <- as.numeric(player_data[[input$metric_select_conf]])
    if(is.na(value)) return(NULL)
    
    ggplot(data.frame(x = input$metric_select_conf, y = value), aes(x = x, y = y)) +
      geom_col(fill = CUSE_BLUE, width = 0.5, color = "black") +
      geom_text(aes(label = round(y, 3)), vjust = -0.5, size = 6, fontface = "bold") +
      theme_minimal() +
      theme(axis.title = element_blank(), axis.text.x = element_text(size = 14, face = "bold"),
            axis.text.y = element_text(size = 12), panel.grid.major.x = element_blank()) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
  })
  
  # ============================================================================
  # LEADERBOARDS TAB
  # ============================================================================
  
  output$leaderboard_metric_ui <- renderUI({
    metrics <- switch(input$leaderboard_category,
                      "Batting" = c("Avg", "OPS", "H", "HR", "RBI", "R", "BB", "SO"),
                      "Pitching" = c("ERA", "WHIP", "SO", "BB", "IP"),
                      "Fielding" = c("FLD%", "PO", "A", "E"))
    selectInput("leaderboard_metric", "Metric:", choices = metrics, selected = metrics[1])
  })
  
  leaderboard_data <- reactive({
    req(input$leaderboard_metric)
    data <- switch(input$leaderboard_category,
                   "Batting" = SB_player_stats_hitting,
                   "Pitching" = SB_player_stats_pitching,
                   "Fielding" = SB_player_stats_fielding)
    name_col <- if("Name" %in% colnames(data)) "Name" else "Player"
    metric <- input$leaderboard_metric
    if(!metric %in% colnames(data)) return(NULL)
    
    result <- data %>% select(all_of(c(name_col, metric))) %>%
      rename(Player = all_of(name_col), Value = all_of(metric)) %>%
      mutate(Value = as.numeric(Value)) %>% filter(!is.na(Value))
    
    if(input$sort_order == "Descending") result <- result %>% arrange(desc(Value))
    else result <- result %>% arrange(Value)
    result
  })
  
  output$team_leaderboard <- DT::renderDataTable({
    leaderboard_data()
  }, options = list(pageLength = 15))
  
  output$leaderboard_plot <- renderPlot({
    df <- leaderboard_data()
    if(is.null(df) || nrow(df) == 0) return(NULL)
    
    df <- df %>% head(10) %>% mutate(Player = factor(Player, levels = rev(Player)))
    
    ggplot(df, aes(x = Player, y = Value)) +
      geom_bar(stat = "identity", fill = CUSE_ORANGE, color = "black") +
      geom_text(aes(label = round(Value, 3)), hjust = -0.1, size = 4, fontface = "bold") +
      coord_flip() +
      labs(title = paste("Top 10 -", input$leaderboard_metric), x = "", y = input$leaderboard_metric) +
      theme_minimal() +
      theme(legend.position = "none", plot.title = element_text(size = 16, face = "bold"),
            axis.text = element_text(size = 12)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
  })
  
  output$download_leaderboard <- downloadHandler(
    filename = function() { paste("leaderboard_", input$leaderboard_category, "_", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write.csv(leaderboard_data(), file, row.names = FALSE) }
  )
}

shinyApp(ui = ui, server = server)

