library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(gt)
library(gtExtras)
library(lubridate)

# Load hitting data
hittingData <- read_csv("HittingFacilityData.csv")
hittingData$FakeDate <- as.Date(hittingData$FakeDate, format="%m/%d/%y")
# Load pitching data
pitchingData <- read_csv("PitchingFacilityData.csv")
pitchingData$Date <- as.Date(pitchingData$Date, format="%m/%d/%y")
# Load Strength Data
strengthData <- read_csv("StrengthFacilityData.csv")
strengthData$Date <- as.Date(strengthData$Date, format="%m/%d/%y")
# Load Speed Data
speedData <- read_csv("SpeedFacilityData.csv")
speedData$Date <- as.Date(speedData$Date, format="%m/%d/%y")

# Get the most recent month and year from the datasets
#most_recent_date <- max(strengthData$Date, na.rm = TRUE)
most_recent_month <- "December" #month.name[month(most_recent_date)]
most_recent_year <- "2024" #year(most_recent_date)

# Define UI for the application
ui <- navbarPage(theme = shinytheme("darkly"),
                 title = tags$img(src = "LOGO White Transparent.png", height = "30px"),
                 
                 tabPanel("Hitting Leaderboard",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("month", "Select Month:", choices = month.name, selected = most_recent_month),
                              selectInput("year", "Select Year:", choices = unique(hittingData$Year), selected = most_recent_year),
                              selectInput("level", "Select Level:", choices = c("L1", "L2", "L3", "Collegiate", "Pro")),                              
                              radioButtons("sport", "Select Sport:", choices = c("Baseball", "Softball"))
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
                                tabPanel("Shoulder ISO-Y", tableOutput("SHLDISOY_Table")),
                                tabPanel("Proteus Power", tableOutput("strengthProteusPowerTable"))
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
    grouped_hittingData <- hittingData %>%
      filter(!is.na(MaxVel)) %>% 
      group_by(Name, Level, FakeDate, Month, Year, Sport) %>%
      summarize(`Exit Velocity (mph)` = max(MaxVel), .groups = "drop") %>% 
      ungroup()
    
    ranked_hittingData <- grouped_hittingData %>% 
      group_by(FakeDate, Level) %>%
      arrange(FakeDate, Level, desc(`Exit Velocity (mph)`)) %>%
      mutate(Rank = row_number()) %>%
      ungroup() %>%
      arrange(Name, FakeDate) %>%
      group_by(Name) %>%
      mutate(`Rank Change` = lag(Rank, order_by = FakeDate) - Rank) %>%
      ungroup()
    
    final_data <- ranked_hittingData %>%
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
      filter(!is.na(MaxDist)) %>% 
      group_by(Name, Level, FakeDate, Month, Year, Sport) %>%
      summarize(`Distance (ft)` = max(MaxDist, na.rm = TRUE), .groups = "drop") %>% 
      ungroup()
    
    ranked_hittingData <- grouped_hittingData %>% 
      group_by(FakeDate, Level) %>%
      arrange(FakeDate, Level, desc(`Distance (ft)`)) %>%
      mutate(Rank = row_number()) %>%
      ungroup() %>%
      arrange(Name, FakeDate) %>%
      group_by(Name) %>%
      mutate(`Rank Change` = lag(Rank, order_by = FakeDate) - Rank) %>%
      ungroup()
    
    final_data <- ranked_hittingData %>%
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
      group_by(Name, Level, FakeDate, Month, Year, Sport) %>%
      summarize(`Bat Speed (mph)` = max(`Bat Speed (mph)`, na.rm = TRUE), .groups = "drop") %>% 
      ungroup()
    
    ranked_hittingData <- grouped_hittingData %>% 
      group_by(FakeDate, Level) %>%
      arrange(FakeDate, Level, desc(`Bat Speed (mph)`)) %>%
      mutate(Rank = row_number()) %>%
      ungroup() %>%
      arrange(Name, FakeDate) %>%
      group_by(Name) %>%
      mutate(`Rank Change` = lag(Rank, order_by = FakeDate) - Rank) %>%
      ungroup()
    
    final_data <- ranked_hittingData %>%
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
      filter(`Exercise Name` == "IBSQT") %>% 
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
      filter(Month == input$strength_month, Year == input$strength_year, Level == input$strength_level, Gender == input$strenth_gender) %>%
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
  
  # Filter and rank by Countermovement Jump
  output$CMJ_Table <- render_gt({
    grouped_CMJ <- strengthData %>%
      filter(`Exercise Name` == "CMJ") %>% 
      mutate(FakeDate = make_date(Year, match(Month, month.name), 1)) %>%
      group_by(Name, Level, FakeDate, Month, Year, Gender) %>% 
      summarize(`Peak Power [W]` = max(`Peak Power [W]`, na.rm = TRUE), .groups = 'drop') %>% 
      ungroup()
    
    ranked_CMJ <- grouped_CMJ %>% 
      group_by(FakeDate, Level) %>%
      arrange(FakeDate, Level, desc(`Peak Power [W]`)) %>%
      mutate(Rank = row_number()) %>%
      ungroup() %>%
      arrange(Name, FakeDate) %>%
      group_by(Name) %>%
      mutate(`Rank Change` = lag(Rank, order_by = FakeDate) - Rank) %>%
      ungroup()
    
    final_data <- ranked_CMJ %>%
      filter(Month == input$strength_month, Year == input$strength_year, Level == input$strength_level, Gender == input$strenth_gender) %>%
      arrange(desc(`Peak Power [W]`)) %>% 
      select(Rank, Name, `Peak Power [W]`, `Rank Change`)
    
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
        `Peak Power [W]` ~ px(200)
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
  
  # Filter and rank by Shoulder ISO-Y
  output$SHLDISOY_Table <- render_gt({
    grouped_SHLDISOY <- strengthData %>%
      filter(`Exercise Name` == "SHLDISOY") %>% 
      mutate(FakeDate = make_date(Year, match(Month, month.name), 1)) %>%
      group_by(Name, Level, FakeDate, Month, Year, Gender) %>% 
      summarize(`Peak Vertical Force [N]` = max(`Peak Vertical Force [N]`, na.rm = TRUE), .groups = 'drop') %>% 
      ungroup()
    
    ranked_SHLDISOY <- grouped_SHLDISOY %>% 
      group_by(FakeDate, Level) %>%
      arrange(FakeDate, Level, desc(`Peak Vertical Force [N]`)) %>%
      mutate(Rank = row_number()) %>%
      ungroup() %>%
      arrange(Name, FakeDate) %>%
      group_by(Name) %>%
      mutate(`Rank Change` = lag(Rank, order_by = FakeDate) - Rank) %>%
      ungroup()
    
    final_data <- ranked_SHLDISOY %>%
      filter(Month == input$strength_month, Year == input$strength_year, Level == input$strength_level, Gender == input$strenth_gender) %>%
      arrange(desc(`Peak Vertical Force [N]`)) %>% 
      select(Rank, Name, `Peak Vertical Force [N]`, `Rank Change`)
    
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
        `Peak Vertical Force [N]` ~ px(200)
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
        filter(Month == input$strength_month, Year == input$strength_year, Level == input$strength_level, Gender == input$strenth_gender) %>%
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
  
  # Filter and rank by Max Running Speed
  output$speedMaxVelTable <- render_gt({
    grouped_MaxSpeed <- speedData %>%
      filter(`Exercise Name` == "Max Velocity") %>% 
      mutate(FakeDate = make_date(Year, match(Month, month.name), 1)) %>%
      group_by(Name, Level, FakeDate, Month, Year, Gender) %>% 
      summarize(`Top Speed (mph)` = round(max(`Max Velocity`, na.rm = TRUE), 2), .groups = 'drop') %>% 
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
      summarize(`10 Yard Acceleration (sec)` = round(min(Acceleration, na.rm = TRUE), 2), .groups = 'drop') %>% 
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
      summarize(`30 Yard Sprint (sec)` = round(min(`Hard 90`, na.rm = TRUE), 2), .groups = 'drop') %>% 
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
  
}

# Run the application 
shinyApp(ui = ui, server = server)
