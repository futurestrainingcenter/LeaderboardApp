library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(gt)
library(gtExtras)
library(lubridate)

# Load hitting data
hittingData <- read_csv("HittingFacilityData.csv")
# Load pitching data
pitchingData <- read_csv("PitchingFacilityData.csv")
# Load Strength Data
strengthData <- read_csv("StrengthFacilityData.csv")
# Load Speed Data
speedData <- read_csv("SpeedFacilityData.csv")

# Get the most recent month and year from the datasets
#most_recent_date <- max(strengthData$Date, na.rm = TRUE)
most_recent_month <- "January" #month.name[month(most_recent_date)]
most_recent_year <- "2025" #year(most_recent_date)

# Define UI for the application
ui <- navbarPage(theme = shinytheme("darkly"),
                 title = tags$img(src = "LOGO White Transparent.png", height = "30px"),
                 
                 tabPanel("Hitting Leaderboard",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("month", "Select Month:", choices = month.name, selected = most_recent_month),
                              selectInput("year", "Select Year:", choices = unique(hittingData$Year), selected = most_recent_year),
                              selectInput("level", "Select Level:", choices = c("L1", "L2", "L3", "Collegiate", "Pro")),                              
                              radioButtons("gender", "Select Gender:", choices = c("Male", "Female"))
                            ),
                            
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Exit Velocity", tableOutput("maxVelTable")),
                                tabPanel("Distance", tableOutput("maxDistTable")),
                                tabPanel("Bat Speed", tableOutput("batSpeedTable"))
                              )
                            )
                          )
                 ),
                 
                 tabPanel("Pitching Leaderboard",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("pitching_month", "Select Month:", choices = month.name, selected = most_recent_month),
                              selectInput("pitching_year", "Select Year:", choices = unique(pitchingData$Year), selected = most_recent_year),
                              selectInput("pitching_level", "Select Level:", choices = c("L1", "L2", "L3", "Collegiate", "Pro")), 
                            ),
                            
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Release Speed", tableOutput("pitchingMaxVelTable"))
                              )
                            )
                          )
                 ),
                 
                 tabPanel("Strength Leaderboard",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("strength_month", "Select Month:", choices = month.name, selected = most_recent_month),
                              selectInput("strength_year", "Select Year:", choices = unique(strengthData$Year), selected = most_recent_year),
                              selectInput("strength_level", "Select Level:", choices = c("L1", "L2", "L3", "Collegiate", "Pro")), 
                              radioButtons("strenth_gender", "Select Gender:", choices = unique(strengthData$Gender))
                            ),
                            
                            mainPanel(
                              tabsetPanel(
                                tabPanel("ISO Belt Squat", tableOutput("strengthIsoBeltSquatTable")),
                                tabPanel("Countermovement Jump", tableOutput("CMJ_Table")),
                                tabPanel("Shoulder ISO-Y", tableOutput("SHLDISOY_Table"))
                                # tabPanel("Proteus Power", tableOutput("strengthProteusPowerTable"))
                              )
                            )
                          )
                 ),
                 
                 tabPanel("Speed Leaderboard",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("speed_month", "Select Month:", choices = month.name, selected = most_recent_month),
                              selectInput("speed_year", "Select Year:", choices = unique(speedData$Year), selected = most_recent_year),
                              selectInput("speed_level", "Select Level:", choices = c("L1", "L2", "L3", "Collegiate", "Pro")), 
                              radioButtons("speed_gender", "Select Gender:", choices = unique(speedData$Gender))
                            ),
                            
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Top Speed", tableOutput("speedMaxVelTable")),
                                tabPanel("10 Yard Acceleration", tableOutput("speed10YardAccelTable")),
                                tabPanel("30 Yard Sprint", tableOutput("speed30YardSprintTable")),
                              )
                            )
                          )
                 )
)

# Define server logic
server <- function(input, output) {
  
  output$maxVelTable <- render_gt({
    ranked_hittingData <- hittingData %>% 
      mutate(Date = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%B-%d")) %>%
      group_by(Date, `Skill Training Level`) %>%
      arrange(Date, `Skill Training Level`, desc(MaxVel)) %>%
      mutate(Rank = row_number()) %>%
      ungroup() %>%
      arrange(Name, Date) %>%
      group_by(Name) %>%
      mutate(`Rank Change` = lag(Rank, order_by = Date) - Rank) %>%
      ungroup()
    
    final_data <- ranked_hittingData %>%
      filter(Month == input$month, Year == input$year, `Skill Training Level` == input$level, Gender == input$gender) %>%
      arrange(desc(MaxVel)) %>%
      select(Rank, Name, MaxVel, `Rank Change`)
    
    row_count <- nrow(final_data)
    
    table <- final_data %>%
      gt() %>%
      cols_add(`  ` = 1, .after = Rank) %>% 
      cols_align(
        align = "center",
        columns = everything()
      ) %>%
      tab_style(
        style = cell_text(size = px(18), weight = "bold"),
        locations = cells_stub()
      ) %>%
      tab_style(
        style = cell_borders(sides = c("top"),  weight = px(1)),
        locations = cells_body(rows = 1)
      ) %>% 
      gt_fa_rank_change(
        `Rank Change`, palette = c("green", "lightgrey", "red"), font_color = "match"
      ) %>%
      cols_width(
        Name ~ px(250),
        MaxVel ~ px(200)
      ) %>%
      cols_label(
        `Rank Change` = "", 
        MaxVel = "Max Exit Velocity (mph)") %>%
      opt_row_striping(row_striping = TRUE) %>% 
      tab_options(
        table.background.color = "#FFFFFF00",
        table.font.color = "white",
        table.align = "left",
        table.border.top.style = "hidden",
        table.border.bottom.style = "hidden",
        table_body.hlines.style = "hidden",
        row.striping.background_color = "grey20",
      ) %>%
      fmt_image(
        columns = `  `,
        file_pattern = "blank.png"
      )
    
    if (row_count >= 1) {
      table <- table %>%
        fmt_image(
          columns = Rank,
          rows = 1,
          file_pattern = "firstplace.png"
        )
    }
    
    if (row_count >= 2) {
      table <- table %>%
        fmt_image(
          columns = Rank,
          rows = 2,
          file_pattern = "secondplace.png"
        )
    }
    
    if (row_count >= 3) {
      table <- table %>%
        fmt_image(
          columns = Rank,
          rows = 3,
          file_pattern = "thirdplace.png"
        )
    }
    table
  })
  
  
  # Filter and rank by Max Distance
  output$maxDistTable <- render_gt({
    ranked_hittingData <- hittingData %>% 
      mutate(Date = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%B-%d")) %>%
      group_by(Date, `Skill Training Level`) %>%
      arrange(Date, `Skill Training Level`, desc(MaxDist)) %>%
      mutate(Rank = row_number()) %>%
      ungroup() %>%
      arrange(Name, Date) %>%
      group_by(Name) %>%
      mutate(`Rank Change` = lag(Rank, order_by = Date) - Rank) %>%
      ungroup()
    
    final_data <- ranked_hittingData %>%
      filter(Month == input$month, Year == input$year, `Skill Training Level` == input$level, Gender == input$gender) %>%
      arrange(desc(MaxDist)) %>%
      select(Rank, Name, MaxDist, `Rank Change`)
    
    row_count <- nrow(final_data)
    
    table <- final_data %>%
      gt() %>%
      cols_add(`  ` = 1, .after = Rank) %>% 
      cols_align(
        align = "center",
        columns = everything()
      ) %>%
      tab_style(
        style = cell_text(size = px(18), weight = "bold"),
        locations = cells_stub()
      ) %>%
      tab_style(
        style = cell_borders(sides = c("top"),  weight = px(1)),
        locations = cells_body(rows = 1)
      ) %>% 
      gt_fa_rank_change(
        `Rank Change`, palette = c("green", "lightgrey", "red"), font_color = "match"
      ) %>%
      cols_width(
        Name ~ px(250),
        MaxDist ~ px(200)
      ) %>%
      cols_label(
        `Rank Change` = "",
        MaxDist = "Max Distance (ft)") %>%
      opt_row_striping(row_striping = TRUE) %>% 
      tab_options(
        table.background.color = "#FFFFFF00",
        table.font.color = "white",
        table.align = "left",
        table.border.top.style = "hidden",
        table.border.bottom.style = "hidden",
        table_body.hlines.style = "hidden",
        row.striping.background_color = "grey20",
      ) %>%
      fmt_image(
        columns = `  `,
        file_pattern = "blank.png"
      )
    
    if (row_count >= 1) {
      table <- table %>%
        fmt_image(
          columns = Rank,
          rows = 1,
          file_pattern = "firstplace.png"
        )
    }
    
    if (row_count >= 2) {
      table <- table %>%
        fmt_image(
          columns = Rank,
          rows = 2,
          file_pattern = "secondplace.png"
        )
    }
    
    if (row_count >= 3) {
      table <- table %>%
        fmt_image(
          columns = Rank,
          rows = 3,
          file_pattern = "thirdplace.png"
        )
    }
    
    table
  })
  
  # Filter and rank by Bat Speed
  output$batSpeedTable <- render_gt({
    ranked_hittingData <- hittingData %>% 
      filter(!is.na(`Bat Speed`)) %>%
      mutate(Date = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%B-%d")) %>%
      group_by(Date, `Skill Training Level`) %>%
      arrange(Date, `Skill Training Level`, desc(`Bat Speed`)) %>%
      mutate(Rank = row_number()) %>%
      ungroup() %>%
      arrange(Name, Date) %>%
      group_by(Name) %>%
      mutate(`Rank Change` = lag(Rank, order_by = Date) - Rank) %>%
      ungroup()
    
    final_data <- ranked_hittingData %>%
      filter(Month == input$month, Year == input$year, `Skill Training Level` == input$level, Gender == input$gender) %>%
      arrange(desc(`Bat Speed`)) %>%
      select(Rank, Name, `Bat Speed`, `Rank Change`)
    
    row_count <- nrow(final_data)
    
    table <- final_data %>%
      gt() %>%
      cols_add(`  ` = 1, .after = Rank) %>% 
      cols_align(
        align = "center",
        columns = everything()
      ) %>%
      tab_style(
        style = cell_text(size = px(18), weight = "bold"),
        locations = cells_stub()
      ) %>%
      tab_style(
        style = cell_borders(sides = c("top"),  weight = px(1)),
        locations = cells_body(rows = 1)
      ) %>% 
      gt_fa_rank_change(
        `Rank Change`, palette = c("green", "lightgrey", "red"), font_color = "match"
      ) %>%
      cols_width(
        Name ~ px(250),
        `Bat Speed` ~ px(200)
      ) %>%
      cols_label(
        `Rank Change` = "",
        `Bat Speed` = "Bat Speed (mph)") %>%
      opt_row_striping(row_striping = TRUE) %>% 
      tab_options(
        table.background.color = "#FFFFFF00",
        table.font.color = "white",
        table.align = "left",
        table.border.top.style = "hidden",
        table.border.bottom.style = "hidden",
        table_body.hlines.style = "hidden",
        row.striping.background_color = "grey20",
      ) %>%
      fmt_image(
        columns = `  `,
        file_pattern = "blank.png"
      )
    
    if (row_count >= 1) {
      table <- table %>%
        fmt_image(
          columns = Rank,
          rows = 1,
          file_pattern = "firstplace.png"
        )
    }
    
    if (row_count >= 2) {
      table <- table %>%
        fmt_image(
          columns = Rank,
          rows = 2,
          file_pattern = "secondplace.png"
        )
    }
    
    if (row_count >= 3) {
      table <- table %>%
        fmt_image(
          columns = Rank,
          rows = 3,
          file_pattern = "thirdplace.png"
        )
    }
    
    table
  })
  
  # Filter and rank by Pitching Speed
  output$pitchingMaxVelTable <- render_gt({
    ranked_pitchingData <- pitchingData %>% 
      filter(!is.na(Max_RelSpeed), TaggedPitchType == "Fastball") %>%
      mutate(Date = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%B-%d")) %>%
      group_by(Date, `Skill Training Level`) %>%
      arrange(Date, `Skill Training Level`, desc(Max_RelSpeed)) %>%
      mutate(Rank = row_number()) %>%
      ungroup() %>%
      arrange(Name, Date) %>%
      group_by(Name) %>%
      mutate(`Rank Change` = lag(Rank, order_by = Date) - Rank) %>%
      ungroup()
    
    final_data <- ranked_pitchingData %>%
      filter(Month == input$pitching_month, Year == input$pitching_year, `Skill Training Level` == input$pitching_level) %>%
      arrange(desc(Max_RelSpeed)) %>% 
      select(Rank, Name, Max_RelSpeed, `Rank Change`)
    
    row_count <- nrow(final_data)
    
    table <- final_data %>%
      gt() %>%
      cols_add(`  ` = 1, .after = Rank) %>% 
      cols_align(
        align = "center",
        columns = everything()
      ) %>%
      tab_style(
        style = cell_text(size = px(18), weight = "bold"),
        locations = cells_stub()
      ) %>%
      tab_style(
        style = cell_borders(sides = c("top"), weight = px(1)),
        locations = cells_body(rows = 1)
      ) %>% 
      gt_fa_rank_change(
        `Rank Change`, palette = c("green", "lightgrey", "red"), font_color = "match"
      ) %>%
      cols_width(
        Name ~ px(250),
        Max_RelSpeed ~ px(200)
      ) %>%
      cols_label(
        `Rank Change` = "",
        Max_RelSpeed = "Throwing Velocity (mph)") %>%
      opt_row_striping(row_striping = TRUE) %>% 
      tab_options(
        table.background.color = "#FFFFFF00",
        table.font.color = "white",
        table.align = "left",
        table.border.top.style = "hidden",
        table.border.bottom.style = "hidden",
        table_body.hlines.style = "hidden",
        row.striping.background_color = "grey20",
      ) %>%
      fmt_image(
        columns = `  `,
        file_pattern = "blank.png"
      )
    
    if (row_count >= 1) {
      table <- table %>%
        fmt_image(
          columns = Rank,
          rows = 1,
          file_pattern = "firstplace.png"
        )
    }
    
    if (row_count >= 2) {
      table <- table %>%
        fmt_image(
          columns = Rank,
          rows = 2,
          file_pattern = "secondplace.png"
        )
    }
    
    if (row_count >= 3) {
      table <- table %>%
        fmt_image(
          columns = Rank,
          rows = 3,
          file_pattern = "thirdplace.png"
        )
    }
    
    table
    
  })
  
  # Filter and rank by ISO Belt Squat
  output$strengthIsoBeltSquatTable <- render_gt({
    ranked_ISOSQT <- strengthData %>% 
      filter(`Exercise Name` == "IBSQT") %>% 
      mutate(Date = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%B-%d")) %>%
      group_by(Date, `Sp Training Level`) %>%
      arrange(Date, `Sp Training Level`, desc(Result)) %>%
      mutate(Rank = row_number()) %>%
      ungroup() %>%
      arrange(Name, Date) %>%
      group_by(Name) %>%
      mutate(`Rank Change` = lag(Rank, order_by = Date) - Rank) %>%
      ungroup()
    
    final_data <- ranked_ISOSQT %>%
      filter(Month == input$strength_month, Year == input$strength_year, `Sp Training Level` == input$strength_level, Gender == input$strenth_gender) %>%
      arrange(desc(Result)) %>% 
      select(Rank, Name, Result, `Rank Change`)
    
    row_count <- nrow(final_data)
    
    table <- final_data %>%
      gt() %>%
      cols_add(`  ` = 1, .after = Rank) %>% 
      cols_align(
        align = "center",
        columns = everything()
      ) %>%
      tab_style(
        style = cell_text(size = px(18), weight = "bold"),
        locations = cells_stub()
      ) %>%
      tab_style(
        style = cell_borders(sides = c("top"),  weight = px(1)),
        locations = cells_body(rows = 1)
      ) %>% 
      gt_fa_rank_change(
        `Rank Change`, palette = c("green", "lightgrey", "red"), font_color = "match"
      ) %>%
      cols_width(
        Name ~ px(250),
        Result ~ px(200)
      ) %>%
      cols_label(
        `Rank Change` = "",
        Result = "Peak Vertical Force [N]") %>%
      opt_row_striping(row_striping = TRUE) %>% 
      tab_options(
        table.background.color = "#FFFFFF00",
        table.font.color = "white",
        table.align = "left",
        table.border.top.style = "hidden",
        table.border.bottom.style = "hidden",
        table_body.hlines.style = "hidden",
        row.striping.background_color = "grey20",
      ) %>%
      fmt_image(
        columns = `  `,
        file_pattern = "blank.png"
      )
    
    if (row_count >= 1) {
      table <- table %>%
        fmt_image(
          columns = Rank,
          rows = 1,
          file_pattern = "firstplace.png"
        )
    }
    
    if (row_count >= 2) {
      table <- table %>%
        fmt_image(
          columns = Rank,
          rows = 2,
          file_pattern = "secondplace.png"
        )
    }
    
    if (row_count >= 3) {
      table <- table %>%
        fmt_image(
          columns = Rank,
          rows = 3,
          file_pattern = "thirdplace.png"
        )
    }
    
    table
  })
  
  # Filter and rank by Countermovement Jump
  output$CMJ_Table <- render_gt({
    ranked_CMJ <- strengthData %>% 
      filter(`Exercise Name` == "CMJ") %>% 
      mutate(Date = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%B-%d")) %>%
      group_by(Date, `Sp Training Level`) %>%
      arrange(Date, `Sp Training Level`, desc(Result)) %>%
      mutate(Rank = row_number()) %>%
      ungroup() %>%
      arrange(Name, Date) %>%
      group_by(Name) %>%
      mutate(`Rank Change` = lag(Rank, order_by = Date) - Rank) %>%
      ungroup()
    
    final_data <- ranked_CMJ %>%
      filter(Month == input$strength_month, Year == input$strength_year, `Sp Training Level` == input$strength_level, Gender == input$strenth_gender) %>%
      arrange(desc(Result)) %>% 
      select(Rank, Name, Result, `Rank Change`)
    
    row_count <- nrow(final_data)
    
    table <- final_data %>%
      gt() %>%
      cols_add(`  ` = 1, .after = Rank) %>% 
      cols_align(
        align = "center",
        columns = everything()
      ) %>%
      tab_style(
        style = cell_text(size = px(18), weight = "bold"),
        locations = cells_stub()
      ) %>%
      tab_style(
        style = cell_borders(sides = c("top"),  weight = px(1)),
        locations = cells_body(rows = 1)
      ) %>% 
      gt_fa_rank_change(
        `Rank Change`, palette = c("green", "lightgrey", "red"), font_color = "match"
      ) %>%
      cols_width(
        Name ~ px(250),
        Result ~ px(200)
      ) %>%
      cols_label(
        `Rank Change` = "",
        Result = "Peak Power [W]") %>%
      opt_row_striping(row_striping = TRUE) %>% 
      tab_options(
        table.background.color = "#FFFFFF00",
        table.font.color = "white",
        table.align = "left",
        table.border.top.style = "hidden",
        table.border.bottom.style = "hidden",
        table_body.hlines.style = "hidden",
        row.striping.background_color = "grey20",
      ) %>%
      fmt_image(
        columns = `  `,
        file_pattern = "blank.png"
      )
    
    if (row_count >= 1) {
      table <- table %>%
        fmt_image(
          columns = Rank,
          rows = 1,
          file_pattern = "firstplace.png"
        )
    }
    
    if (row_count >= 2) {
      table <- table %>%
        fmt_image(
          columns = Rank,
          rows = 2,
          file_pattern = "secondplace.png"
        )
    }
    
    if (row_count >= 3) {
      table <- table %>%
        fmt_image(
          columns = Rank,
          rows = 3,
          file_pattern = "thirdplace.png"
        )
    }
    
    table
  })
  
  # Filter and rank by Shoulder ISO-Y
  output$SHLDISOY_Table <- render_gt({
    ranked_SHLDISOY <- strengthData %>% 
      filter(`Exercise Name` == "SHLDISOY") %>% 
      mutate(Date = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%B-%d")) %>%
      group_by(Date, `Sp Training Level`) %>%
      arrange(Date, `Sp Training Level`, desc(Result)) %>%
      mutate(Rank = row_number()) %>%
      ungroup() %>%
      arrange(Name, Date) %>%
      group_by(Name) %>%
      mutate(`Rank Change` = lag(Rank, order_by = Date) - Rank) %>%
      ungroup()
    
    final_data <- ranked_SHLDISOY %>%
      filter(Month == input$strength_month, Year == input$strength_year, `Sp Training Level` == input$strength_level, Gender == input$strenth_gender) %>%
      arrange(desc(Result)) %>% 
      select(Rank, Name, Result, `Rank Change`)
    
    row_count <- nrow(final_data)
    
    table <- final_data %>%
      gt() %>%
      cols_add(`  ` = 1, .after = Rank) %>% 
      cols_align(
        align = "center",
        columns = everything()
      ) %>%
      tab_style(
        style = cell_text(size = px(18), weight = "bold"),
        locations = cells_stub()
      ) %>%
      tab_style(
        style = cell_borders(sides = c("top"),  weight = px(1)),
        locations = cells_body(rows = 1)
      ) %>% 
      gt_fa_rank_change(
        `Rank Change`, palette = c("green", "lightgrey", "red"), font_color = "match"
      ) %>%
      cols_width(
        Name ~ px(250),
        Result ~ px(200)
      ) %>%
      cols_label(
        `Rank Change` = "",
        Result = "Peak Vertical Force [N]") %>%
      opt_row_striping(row_striping = TRUE) %>% 
      tab_options(
        table.background.color = "#FFFFFF00",
        table.font.color = "white",
        table.align = "left",
        table.border.top.style = "hidden",
        table.border.bottom.style = "hidden",
        table_body.hlines.style = "hidden",
        row.striping.background_color = "grey20",
      ) %>%
      fmt_image(
        columns = `  `,
        file_pattern = "blank.png"
      )
    
    if (row_count >= 1) {
      table <- table %>%
        fmt_image(
          columns = Rank,
          rows = 1,
          file_pattern = "firstplace.png"
        )
    }
    
    if (row_count >= 2) {
      table <- table %>%
        fmt_image(
          columns = Rank,
          rows = 2,
          file_pattern = "secondplace.png"
        )
    }
    
    if (row_count >= 3) {
      table <- table %>%
        fmt_image(
          columns = Rank,
          rows = 3,
          file_pattern = "thirdplace.png"
        )
    }
    
    table
  })
  
  # Filter and rank by Proteus Power
  # output$strengthProteusPowerTable <- render_gt({
  #   grouped_ProteusPower <- strengthData %>%
  #     filter(`Exercise Name` == "Proteus Full Test") %>% 
  #     mutate(Date = make_date(Year, match(Month, month.name), 1)) %>%
  #     group_by(Name, Level, Date, Month, Year, Gender) %>% 
  #     summarize(`Power (W)` = round(max(`power - high`, na.rm = TRUE), 1), .groups = 'drop') %>% 
  #     ungroup()
  #     
  #     ranked_ProteusPower <- grouped_ProteusPower %>% 
  #       group_by(Date, Level) %>%
  #       arrange(Date, Level, desc(`Power (W)`)) %>%
  #       mutate(Rank = row_number()) %>%
  #       ungroup() %>%
  #       arrange(Name, Date) %>%
  #       group_by(Name) %>%
  #       mutate(`Rank Change` = lag(Rank, order_by = Date) - Rank) %>%
  #       ungroup()
  #     
  #     final_data <- ranked_ProteusPower %>%
  #       filter(Month == input$strength_month, Year == input$strength_year, Level == input$strength_level, Gender == input$strenth_gender) %>%
  #       arrange(desc(`Power (W)`)) %>% 
  #       select(Rank, Name, `Power (W)`, `Rank Change`)
  #     
  #     row_count <- nrow(final_data)
  #     
  #     table <- final_data %>%
  #       gt() %>%
  #       cols_add(`  ` = 1, .after = Rank) %>% 
  #       cols_align(
  #         align = "center",
  #         columns = everything()
  #       ) %>%
  #       tab_style(
  #         style = cell_text(size = px(18), weight = "bold"),
  #         locations = cells_stub()
  #       ) %>%
  #       tab_style(
  #         style = cell_borders(sides = c("top"),  weight = px(1)),
  #         locations = cells_body(rows = 1)
  #       ) %>% 
  #       gt_fa_rank_change(
  #         `Rank Change`, palette = c("green", "lightgrey", "red"), font_color = "match"
  #       ) %>%
  #       cols_width(
  #         Name ~ px(250),
  #         `Power (W)` ~ px(200)
  #       ) %>%
  #       cols_label(`Rank Change` = "") %>%
  #       opt_row_striping(row_striping = TRUE) %>% 
  #       tab_options(
  #         table.background.color = "#FFFFFF00",
  #         table.font.color = "white",
  #         table.align = "left",
  #         table.border.top.style = "hidden",
  #         table.border.bottom.style = "hidden",
  #         table_body.hlines.style = "hidden",
  #         row.striping.background_color = "grey20",
  #       ) %>%
  #       fmt_image(
  #         columns = `  `,
  #         file_pattern = "blank.png"
  #       )
  #     
  #     if (row_count >= 1) {
  #       table <- table %>%
  #         fmt_image(
  #           columns = Rank,
  #           rows = 1,
  #           file_pattern = "firstplace.png"
  #         )
  #     }
  #     
  #     if (row_count >= 2) {
  #       table <- table %>%
  #         fmt_image(
  #           columns = Rank,
  #           rows = 2,
  #           file_pattern = "secondplace.png"
  #         )
  #     }
  #     
  #     if (row_count >= 3) {
  #       table <- table %>%
  #         fmt_image(
  #           columns = Rank,
  #           rows = 3,
  #           file_pattern = "thirdplace.png"
  #         )
  #     }
  #     
  #     table
  # })
  
  # Filter and rank by Max Running Speed
  output$speedMaxVelTable <- render_gt({
    ranked_MaxSpeed <- speedData %>% 
      mutate(Date = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%B-%d")) %>%
      group_by(Date, `Sp Training Level`) %>%
      arrange(Date, `Sp Training Level`, desc(max_velocity)) %>%
      mutate(Rank = row_number()) %>%
      ungroup() %>%
      arrange(Name, Date) %>%
      group_by(Name) %>%
      mutate(`Rank Change` = lag(Rank, order_by = Date) - Rank) %>%
      ungroup()
    
    final_data <- ranked_MaxSpeed %>%
      filter(Month == input$speed_month, Year == input$speed_year, `Sp Training Level` == input$speed_level, Gender == input$speed_gender) %>%
      arrange(desc(max_velocity)) %>% 
      select(Rank, Name, max_velocity, `Rank Change`)
    
    row_count <- nrow(final_data)
    
    table <- final_data %>%
      gt() %>%
      cols_add(`  ` = 1, .after = Rank) %>% 
      cols_align(
        align = "center",
        columns = everything()
      ) %>%
      tab_style(
        style = cell_text(size = px(18), weight = "bold"),
        locations = cells_stub()
      ) %>%
      tab_style(
        style = cell_borders(sides = c("top"),  weight = px(1)),
        locations = cells_body(rows = 1)
      ) %>% 
      gt_fa_rank_change(
        `Rank Change`, palette = c("green", "lightgrey", "red"), font_color = "match"
      ) %>%
      cols_width(
        Name ~ px(250),
        max_velocity ~ px(200)
      ) %>%
      cols_label(
        `Rank Change` = "",
        max_velocity = "Top Speed (mph)") %>%
      opt_row_striping(row_striping = TRUE) %>% 
      tab_options(
        table.background.color = "#FFFFFF00",
        table.font.color = "white",
        table.align = "left",
        table.border.top.style = "hidden",
        table.border.bottom.style = "hidden",
        table_body.hlines.style = "hidden",
        row.striping.background_color = "grey20",
      ) %>%
      fmt_image(
        columns = `  `,
        file_pattern = "blank.png"
      )
    
    if (row_count >= 1) {
      table <- table %>%
        fmt_image(
          columns = Rank,
          rows = 1,
          file_pattern = "firstplace.png"
        )
    }
    
    if (row_count >= 2) {
      table <- table %>%
        fmt_image(
          columns = Rank,
          rows = 2,
          file_pattern = "secondplace.png"
        )
    }
    
    if (row_count >= 3) {
      table <- table %>%
        fmt_image(
          columns = Rank,
          rows = 3,
          file_pattern = "thirdplace.png"
        )
    }
    
    table
  })
  
  # Filter and rank by 10 Yard Acceleration
  output$speed10YardAccelTable <- render_gt({
    ranked_SpeedAcc <- speedData %>% 
      mutate(Date = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%B-%d")) %>%
      group_by(Date, `Sp Training Level`) %>%
      arrange(Date, `Sp Training Level`, early_acceleration) %>%
      mutate(Rank = row_number()) %>%
      ungroup() %>%
      arrange(Name, Date) %>%
      group_by(Name) %>%
      mutate(`Rank Change` = lag(Rank, order_by = Date) - Rank) %>%
      ungroup()
    
    final_data <- ranked_SpeedAcc %>%
      filter(Month == input$speed_month, Year == input$speed_year, `Sp Training Level` == input$speed_level, Gender == input$speed_gender) %>%
      arrange(early_acceleration) %>% 
      select(Rank, Name, early_acceleration, `Rank Change`)
    
    row_count <- nrow(final_data)
    
    table <- final_data %>%
      gt() %>%
      cols_add(`  ` = 1, .after = Rank) %>% 
      cols_align(
        align = "center",
        columns = everything()
      ) %>%
      tab_style(
        style = cell_text(size = px(18), weight = "bold"),
        locations = cells_stub()
      ) %>%
      tab_style(
        style = cell_borders(sides = c("top"),  weight = px(1)),
        locations = cells_body(rows = 1)
      ) %>% 
      gt_fa_rank_change(
        `Rank Change`, palette = c("green", "lightgrey", "red"), font_color = "match"
      ) %>%
      cols_width(
        Name ~ px(250),
        early_acceleration ~ px(200)
      ) %>%
      cols_label(
        `Rank Change` = "",
        early_acceleration = "Early Accel. (sec)") %>%
      opt_row_striping(row_striping = TRUE) %>% 
      tab_options(
        table.background.color = "#FFFFFF00",
        table.font.color = "white",
        table.align = "left",
        table.border.top.style = "hidden",
        table.border.bottom.style = "hidden",
        table_body.hlines.style = "hidden",
        row.striping.background_color = "grey20",
      ) %>%
      fmt_image(
        columns = `  `,
        file_pattern = "blank.png"
      )
    
    if (row_count >= 1) {
      table <- table %>%
        fmt_image(
          columns = Rank,
          rows = 1,
          file_pattern = "firstplace.png"
        )
    }
    
    if (row_count >= 2) {
      table <- table %>%
        fmt_image(
          columns = Rank,
          rows = 2,
          file_pattern = "secondplace.png"
        )
    }
    
    if (row_count >= 3) {
      table <- table %>%
        fmt_image(
          columns = Rank,
          rows = 3,
          file_pattern = "thirdplace.png"
        )
    }
    
    table
  })
  
  # Filter and rank by 30 Yard Sprint
  output$speed30YardSprintTable <- render_gt({
    ranked_hardNinety <- speedData %>% 
      mutate(Date = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%B-%d")) %>%
      group_by(Date, `Sp Training Level`) %>%
      arrange(Date, `Sp Training Level`, thirty_yard) %>%
      mutate(Rank = row_number()) %>%
      ungroup() %>%
      arrange(Name, Date) %>%
      group_by(Name) %>%
      mutate(`Rank Change` = lag(Rank, order_by = Date) - Rank) %>%
      ungroup()
    
    final_data <- ranked_hardNinety %>%
      filter(Month == input$speed_month, Year == input$speed_year, `Sp Training Level` == input$speed_level, Gender == input$speed_gender) %>%
      arrange(thirty_yard) %>% 
      select(Rank, Name, thirty_yard, `Rank Change`)
    
    row_count <- nrow(final_data)
    
    table <- final_data %>%
      gt() %>%
      cols_add(`  ` = 1, .after = Rank) %>% 
      cols_align(
        align = "center",
        columns = everything()
      ) %>%
      tab_style(
        style = cell_text(size = px(18), weight = "bold"),
        locations = cells_stub()
      ) %>%
      tab_style(
        style = cell_borders(sides = c("top"),  weight = px(1)),
        locations = cells_body(rows = 1)
      ) %>% 
      gt_fa_rank_change(
        `Rank Change`, palette = c("green", "lightgrey", "red"), font_color = "match"
      ) %>%
      cols_width(
        Name ~ px(250),
        thirty_yard ~ px(200)
      ) %>%
      cols_label(
        `Rank Change` = "",
        thirty_yard = "30 Yard (sec)") %>%
      opt_row_striping(row_striping = TRUE) %>% 
      tab_options(
        table.background.color = "#FFFFFF00",
        table.font.color = "white",
        table.align = "left",
        table.border.top.style = "hidden",
        table.border.bottom.style = "hidden",
        table_body.hlines.style = "hidden",
        row.striping.background_color = "grey20",
      ) %>%
      fmt_image(
        columns = `  `,
        file_pattern = "blank.png"
      )
    
    if (row_count >= 1) {
      table <- table %>%
        fmt_image(
          columns = Rank,
          rows = 1,
          file_pattern = "firstplace.png"
        )
    }
    
    if (row_count >= 2) {
      table <- table %>%
        fmt_image(
          columns = Rank,
          rows = 2,
          file_pattern = "secondplace.png"
        )
    }
    
    if (row_count >= 3) {
      table <- table %>%
        fmt_image(
          columns = Rank,
          rows = 3,
          file_pattern = "thirdplace.png"
        )
    }
    
    table
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
