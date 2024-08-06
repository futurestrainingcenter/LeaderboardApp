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
most_recent_date <- max(hittingData$FakeDate, na.rm = TRUE)
most_recent_month <- month.name[month(most_recent_date)]
most_recent_year <- year(most_recent_date)

# Define UI for the application
ui <- navbarPage(theme = shinytheme("darkly"),
                 title = tags$img(src = "LOGO White Transparent.png", height = "30px"),
                 
                 tabPanel("Hitting Leaderboard",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("month", "Select Month:", choices = month.name, selected = most_recent_month),
                              selectInput("year", "Select Year:", choices = unique(hittingData$Year), selected = most_recent_year),
                              selectInput("level", "Select Level:", choices = unique(hittingData$Level)),
                              radioButtons("sport", "Select Sport:", choices = unique(hittingData$Sport))
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
                              selectInput("pitching_level", "Select Level:", choices = unique(pitchingData$Level))
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
                              selectInput("strength_level", "Select Level:", choices = unique(strengthData$Level)),
                              radioButtons("gender", "Select Gender:", choices = unique(strengthData$Gender))
                            ),
                            
                            mainPanel(
                              tabsetPanel(
                                tabPanel("ISO Belt Squat", tableOutput("strengthIsoBeltSquatTable")),
                                tabPanel("Squat Jump", tableOutput("strengthSquatJumpTable")),
                                tabPanel("Proteus Power", tableOutput("strengthProteusPowerTable")),
                                tabPanel("Proteus Acceleration", tableOutput("strengthProteusAccelerationTable"))
                              )
                            )
                          )
                 ),
                 
                 tabPanel("Speed Leaderboard",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("speed_month", "Select Month:", choices = month.name, selected = most_recent_month),
                              selectInput("speed_year", "Select Year:", choices = unique(speedData$Year), selected = most_recent_year),
                              selectInput("speed_level", "Select Level:", choices = unique(speedData$Level)),
                              radioButtons("speed_gender", "Select Gender:", choices = unique(speedData$Gender))
                            ),
                            
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Top Speed", tableOutput("speedMaxVelTable")),
                                tabPanel("10 Yard Acceleration", tableOutput("speed10YardAccelTable")),
                                tabPanel("30 Yard Sprint", tableOutput("speed30YardSprintTable")),
                                tabPanel("RSI", tableOutput("speedRSITable"))
                              )
                            )
                          )
                 )
)

# Define server logic
server <- function(input, output) {
  
  # Filter and rank by Max Velocity
  output$maxVelTable <- render_gt({
    grouped_hittingData <- hittingData %>%
      group_by(FakeDate, Level, Sport) %>%
      arrange(FakeDate, Level, Sport, desc(MaxVel)) %>%
      mutate(Rank = row_number()) %>%
      ungroup() %>%
      arrange(Name, FakeDate) %>%
      group_by(Name) %>%
      mutate(`Rank Change` = lag(Rank, order_by = FakeDate) - Rank) %>%
      rename(`Exit Velocity (mph)` = MaxVel) %>%
      ungroup()
    
    final_data <- grouped_hittingData %>%
      filter(Month == input$month, Year == input$year, Level == input$level, Sport == input$sport) %>%
      arrange(desc(`Exit Velocity (mph)`)) %>%
      select(Rank, Name, `Exit Velocity (mph)`, `Rank Change`)
    
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
        `Exit Velocity (mph)` ~ px(200)
      ) %>%
      cols_label(`Rank Change` = "") %>%
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
    grouped_hittingData <- hittingData %>%
      group_by(FakeDate, Level, Sport) %>%
      arrange(FakeDate, Level, Sport, desc(MaxDist)) %>%
      mutate(Rank = row_number()) %>%
      ungroup() %>%
      arrange(Name, FakeDate) %>%
      group_by(Name) %>%
      mutate(`Rank Change` = lag(Rank, order_by = FakeDate) - Rank) %>%
      rename(`Distance (ft)` = MaxDist) %>%
      ungroup()
    
    final_data <- grouped_hittingData %>%
      filter(Month == input$month, Year == input$year, Level == input$level, Sport == input$sport) %>%
      arrange(desc(`Distance (ft)`)) %>%
      select(Rank, Name, `Distance (ft)`, `Rank Change`)
    
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
        `Distance (ft)` ~ px(200)
      ) %>%
      cols_label(`Rank Change` = "") %>%
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
    grouped_hittingData <- hittingData %>%
      filter(!is.na(`Bat Speed (mph)`)) %>% 
      group_by(FakeDate, Level, Sport) %>%
      arrange(FakeDate, Level, Sport, desc(`Bat Speed (mph)`)) %>%
      mutate(Rank = row_number()) %>%
      ungroup() %>%
      arrange(Name, FakeDate) %>%
      group_by(Name) %>%
      mutate(`Rank Change` = lag(Rank, order_by = FakeDate) - Rank) %>%
      ungroup()
    
    final_data <- grouped_hittingData %>%
      filter(Month == input$month, Year == input$year, Level == input$level, Sport == input$sport) %>%
      arrange(desc(`Bat Speed (mph)`)) %>%
      select(Rank, Name, `Bat Speed (mph)`, `Rank Change`)
    
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
        `Bat Speed (mph)` ~ px(200)
      ) %>%
      cols_label(`Rank Change` = "") %>%
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
    grouped_pitchingData <- pitchingData %>%
      filter(!is.na(RelSpeed)) %>%
      mutate(FakeDate = make_date(Year, match(Month, month.name), 1)) %>%
      group_by(Name, Level, FakeDate, Month, Year) %>%
      summarize(`Release Speed (mph)` = round(max(RelSpeed, na.rm = TRUE), 1), .groups = "drop") %>% 
      ungroup()
    
    ranked_pitchingData <- grouped_pitchingData %>% 
      group_by(FakeDate, Level) %>%
      arrange(FakeDate, Level, desc(`Release Speed (mph)`)) %>%
      mutate(Rank = row_number()) %>%
      ungroup() %>%
      arrange(Name, FakeDate) %>%
      group_by(Name) %>%
      mutate(`Rank Change` = lag(Rank, order_by = FakeDate) - Rank) %>%
      ungroup()
    
    final_data <- ranked_pitchingData %>%
      filter(Month == input$pitching_month, Year == input$pitching_year, Level == input$pitching_level) %>%
      arrange(desc(`Release Speed (mph)`)) %>% 
      select(Rank, Name, `Release Speed (mph)`, `Rank Change`)
    
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
        `Release Speed (mph)` ~ px(200)
      ) %>%
      cols_label(`Rank Change` = "") %>%
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
    grouped_ISOSQT <- strengthData %>%
      filter(`Exercise Type` == "ForceDeck: ISO Belt Squat") %>% 
      mutate(FakeDate = make_date(Year, match(Month, month.name), 1)) %>%
      group_by(Name, Level, FakeDate, Month, Year, Gender) %>% 
      summarize(`Peak Vertical Force (N)` = max(`Peak Vertical Force [N]`, na.rm = TRUE), .groups = 'drop') %>% 
      ungroup()
    
    ranked_ISOSQT <- grouped_ISOSQT %>% 
      group_by(FakeDate, Level) %>%
      arrange(FakeDate, Level, desc(`Peak Vertical Force (N)`)) %>%
      mutate(Rank = row_number()) %>%
      ungroup() %>%
      arrange(Name, FakeDate) %>%
      group_by(Name) %>%
      mutate(`Rank Change` = lag(Rank, order_by = FakeDate) - Rank) %>%
      ungroup()
    
    final_data <- ranked_ISOSQT %>%
      filter(Month == input$strength_month, Year == input$strength_year, Level == input$strength_level, Gender == input$gender) %>%
      arrange(desc(`Peak Vertical Force (N)`)) %>% 
      select(Rank, Name, `Peak Vertical Force (N)`, `Rank Change`)
    
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
        `Peak Vertical Force (N)` ~ px(200)
      ) %>%
      cols_label(`Rank Change` = "") %>%
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
  
  # Filter and rank by Squat Jump
  output$strengthSquatJumpTable <- render_gt({
    grouped_SJ <- strengthData %>%
      filter(`Exercise Type` == "ForceDeck: Squat Jump") %>% 
      mutate(FakeDate = make_date(Year, match(Month, month.name), 1)) %>%
      group_by(Name, Level, FakeDate, Month, Year, Gender) %>% 
      summarize(`Takeoff Peak Force (N)` = max(`Takeoff Peak Force [N]`, na.rm = TRUE), .groups = 'drop') %>% 
      ungroup()
    
    ranked_ISOSQT <- grouped_SJ %>% 
      group_by(FakeDate, Level) %>%
      arrange(FakeDate, Level, desc(`Takeoff Peak Force (N)`)) %>%
      mutate(Rank = row_number()) %>%
      ungroup() %>%
      arrange(Name, FakeDate) %>%
      group_by(Name) %>%
      mutate(`Rank Change` = lag(Rank, order_by = FakeDate) - Rank) %>%
      ungroup()
    
    final_data <- ranked_ISOSQT %>%
      filter(Month == input$strength_month, Year == input$strength_year, Level == input$strength_level, Gender == input$gender) %>%
      arrange(desc(`Takeoff Peak Force (N)`)) %>% 
      select(Rank, Name, `Takeoff Peak Force (N)`, `Rank Change`)
    
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
        `Takeoff Peak Force (N)` ~ px(200)
      ) %>%
      cols_label(`Rank Change` = "") %>%
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
  output$strengthProteusPowerTable <- render_gt({
    grouped_ProteusPower <- strengthData %>%
      filter(`Exercise Name` == "Proteus Full Test") %>% 
      mutate(FakeDate = make_date(Year, match(Month, month.name), 1)) %>%
      group_by(Name, Level, FakeDate, Month, Year, Gender) %>% 
      summarize(`Power (W)` = round(max(`power - high`, na.rm = TRUE), 1), .groups = 'drop') %>% 
      ungroup()
      
      ranked_ProteusPower <- grouped_ProteusPower %>% 
        group_by(FakeDate, Level) %>%
        arrange(FakeDate, Level, desc(`Power (W)`)) %>%
        mutate(Rank = row_number()) %>%
        ungroup() %>%
        arrange(Name, FakeDate) %>%
        group_by(Name) %>%
        mutate(`Rank Change` = lag(Rank, order_by = FakeDate) - Rank) %>%
        ungroup()
      
      final_data <- ranked_ProteusPower %>%
        filter(Month == input$strength_month, Year == input$strength_year, Level == input$strength_level, Gender == input$gender) %>%
        arrange(desc(`Power (W)`)) %>% 
        select(Rank, Name, `Power (W)`, `Rank Change`)
      
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
          `Power (W)` ~ px(200)
        ) %>%
        cols_label(`Rank Change` = "") %>%
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
  
  # Filter and rank by Proteus Acceleration
  output$strengthProteusAccelerationTable <- render_gt({
    grouped_ProteusAcc <- strengthData %>%
      filter(`Exercise Name` == "Proteus Full Test") %>% 
      mutate(FakeDate = make_date(Year, match(Month, month.name), 1)) %>%
      group_by(Name, Level, FakeDate, Month, Year, Gender) %>% 
      summarize(`Acceleration (m/s²)` = round(max(`acceleration - high`, na.rm = TRUE), 2), .groups = 'drop') %>% 
      ungroup()
    
    ranked_ProteusAcc <- grouped_ProteusAcc %>% 
      group_by(FakeDate, Level) %>%
      arrange(FakeDate, Level, desc(`Acceleration (m/s²)`)) %>%
      mutate(Rank = row_number()) %>%
      ungroup() %>%
      arrange(Name, FakeDate) %>%
      group_by(Name) %>%
      mutate(`Rank Change` = lag(Rank, order_by = FakeDate) - Rank) %>%
      ungroup()
    
    final_data <- ranked_ProteusAcc %>%
      filter(Month == input$strength_month, Year == input$strength_year, Level == input$strength_level, Gender == input$gender) %>%
      arrange(desc(`Acceleration (m/s²)`)) %>% 
      select(Rank, Name, `Acceleration (m/s²)`, `Rank Change`)
    
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
        `Acceleration (m/s²)` ~ px(200)
      ) %>%
      cols_label(`Rank Change` = "") %>%
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
  
  # Filter and rank by Max Running Speed
  output$speedMaxVelTable <- render_gt({
    grouped_MaxSpeed <- speedData %>%
      filter(`Exercise Name` == "Max Velocity") %>% 
      mutate(FakeDate = make_date(Year, match(Month, month.name), 1)) %>%
      group_by(Name, Level, FakeDate, Month, Year, Gender) %>% 
      summarize(`Top Speed (mph)` = round(max(MPH, na.rm = TRUE), 2), .groups = 'drop') %>% 
      ungroup()
    
    ranked_MaxSpeed <- grouped_MaxSpeed %>% 
      group_by(FakeDate, Level) %>%
      arrange(FakeDate, Level, desc(`Top Speed (mph)`)) %>%
      mutate(Rank = row_number()) %>%
      ungroup() %>%
      arrange(Name, FakeDate) %>%
      group_by(Name) %>%
      mutate(`Rank Change` = lag(Rank, order_by = FakeDate) - Rank) %>%
      ungroup()
    
    final_data <- ranked_MaxSpeed %>%
      filter(Month == input$speed_month, Year == input$speed_year, Level == input$speed_level, Gender == input$speed_gender) %>%
      arrange(desc(`Top Speed (mph)`)) %>% 
      select(Rank, Name, `Top Speed (mph)`, `Rank Change`)
    
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
        `Top Speed (mph)` ~ px(200)
      ) %>%
      cols_label(`Rank Change` = "") %>%
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
    grouped_SpeedAcc <- speedData %>%
      filter(`Exercise Name` == "Acceleration") %>% 
      mutate(FakeDate = make_date(Year, match(Month, month.name), 1)) %>%
      group_by(Name, Level, FakeDate, Month, Year, Gender) %>% 
      summarize(`10 Yard Acceleration (sec)` = max(Split1, na.rm = TRUE), .groups = 'drop') %>% 
      ungroup()
    
    ranked_SpeedAcc <- grouped_SpeedAcc %>% 
      group_by(FakeDate, Level) %>%
      arrange(FakeDate, Level, `10 Yard Acceleration (sec)`) %>%
      mutate(Rank = row_number()) %>%
      ungroup() %>%
      arrange(Name, FakeDate) %>%
      group_by(Name) %>%
      mutate(`Rank Change` = lag(Rank, order_by = FakeDate) - Rank) %>%
      ungroup()
    
    final_data <- ranked_SpeedAcc %>%
      filter(Month == input$speed_month, Year == input$speed_year, Level == input$speed_level, Gender == input$speed_gender) %>%
      arrange(`10 Yard Acceleration (sec)`) %>% 
      select(Rank, Name, `10 Yard Acceleration (sec)`, `Rank Change`)
    
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
        `10 Yard Acceleration (sec)` ~ px(200)
      ) %>%
      cols_label(`Rank Change` = "") %>%
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
    grouped_hardNinety <- speedData %>%
      filter(`Exercise Name` == "Hard 90") %>% 
      mutate(FakeDate = make_date(Year, match(Month, month.name), 1)) %>%
      group_by(Name, Level, FakeDate, Month, Year, Gender) %>% 
      summarize(`30 Yard Sprint (sec)` = max(Cumulative2), .groups = 'drop') %>% 
      ungroup()
    
    ranked_hardNinety <- grouped_hardNinety %>% 
      group_by(FakeDate, Level) %>%
      arrange(FakeDate, Level, `30 Yard Sprint (sec)`) %>%
      mutate(Rank = row_number()) %>%
      ungroup() %>%
      arrange(Name, FakeDate) %>%
      group_by(Name) %>%
      mutate(`Rank Change` = lag(Rank, order_by = FakeDate) - Rank) %>%
      ungroup()
    
    final_data <- ranked_hardNinety %>%
      filter(Month == input$speed_month, Year == input$speed_year, Level == input$speed_level, Gender == input$speed_gender) %>%
      arrange(`30 Yard Sprint (sec)`) %>% 
      select(Rank, Name, `30 Yard Sprint (sec)`, `Rank Change`)
    
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
        `30 Yard Sprint (sec)` ~ px(200)
      ) %>%
      cols_label(`Rank Change` = "") %>%
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
  
  # Filter and rank by RSI
  output$speedRSITable <- render_gt({
    grouped_RSI <- speedData %>%
      filter(`Exercise Name` == "RSI") %>% 
      mutate(FakeDate = make_date(Year, match(Month, month.name), 1)) %>%
      group_by(Name, Level, FakeDate, Month, Year, Gender) %>% 
      summarize(`RSI (Jump Height/Contact Time)` = max(`Mean RSI (Jump Height/Contact Time) [m/s]`), .groups = 'drop') %>% 
      ungroup()
    
    ranked_RSI <- grouped_RSI %>% 
      group_by(FakeDate, Level) %>%
      arrange(FakeDate, Level, desc(`RSI (Jump Height/Contact Time)`)) %>%
      mutate(Rank = row_number()) %>%
      ungroup() %>%
      arrange(Name, FakeDate) %>%
      group_by(Name) %>%
      mutate(`Rank Change` = lag(Rank, order_by = FakeDate) - Rank) %>%
      ungroup()
    
    final_data <- ranked_RSI %>%
      filter(Month == input$speed_month, Year == input$speed_year, Level == input$speed_level, Gender == input$speed_gender) %>%
      arrange(desc(`RSI (Jump Height/Contact Time)`)) %>% 
      select(Rank, Name, `RSI (Jump Height/Contact Time)`, `Rank Change`)
    
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
        `RSI (Jump Height/Contact Time)` ~ px(200)
      ) %>%
      cols_label(`Rank Change` = "") %>%
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
