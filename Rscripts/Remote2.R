#Less data (only last three months)
library(dplyr)
library(readr)
library(lubridate)

# Helper function to load and filter the most recent month from a file
load_recent_month <- function(file, year) {
  df <- read_csv(file, show_col_types = FALSE) %>%
    select(GEOID, county, date, mean) %>%
    filter(!is.na(mean), county != "") %>%
    mutate(date = as.Date(date), year = year)
  
  max_date <- max(df$date, na.rm = TRUE)
  start_date <- max_date %m-% months(6) + days(1) 
  df %>% filter(date >= start_date & date <= max_date)
}

# Apply to each year
ndvi_2021 <- load_recent_month("NDVI_VA_County_Weekly_2021.csv", 2021)
ndvi_2022 <- load_recent_month("NDVI_VA_County_Weekly_2022.csv", 2022)
ndvi_2023 <- load_recent_month("NDVI_VA_County_Weekly_2023.csv", 2023)
ndvi_2024 <- load_recent_month("NDVI_VA_County_Weekly_2024.csv", 2024)

# Combine and save as NDVI_Recent2Weeks.csv (you can rename later)
ndvi_month_combined <- bind_rows(ndvi_2021, ndvi_2022, ndvi_2023, ndvi_2024)
write_csv(ndvi_month_combined, "NDVI_Recent2Weeks.csv")

library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)

# Load filtered NDVI data (last 2 weeks of 2021–2024)
ndvi_recent <- read_csv("NDVI_Recent2Weeks.csv", show_col_types = FALSE) %>%
  filter(!is.na(mean), county != "") %>%
  mutate(date = as.Date(date))

# Get unique county list and year list
county_list <- sort(unique(ndvi_recent$county))
year_list <- sort(unique(ndvi_recent$year))

# UI
ui <- fluidPage(
  titlePanel("NDVI by County – Most Recent 2 Weeks (2021–2024)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("counties", "Select Counties:", 
                  choices = county_list, 
                  selected = county_list[1],
                  multiple = TRUE),
      selectInput("year", "Select Year:", 
                  choices = year_list,
                  selected = max(year_list))
    ),
    
    mainPanel(
      plotlyOutput("ndvi_plot")
    )
  )
)

# Server
server <- function(input, output, session) {
  output$ndvi_plot <- renderPlotly({
    plot_data <- ndvi_recent %>%
      filter(county %in% input$counties, year == input$year)
    
    if (nrow(plot_data) == 0) return(NULL)
    
    p <- ggplot(plot_data, aes(x = date, y = mean, color = county, group = county)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(title = paste("NDVI by County –", input$year),
           x = "Date",
           y = "NDVI",
           color = "County") +
      theme_minimal()
    
    ggplotly(p, tooltip = c("x", "y", "color"))
  })
}

# Run the app
shinyApp(ui, server)


#Top ten counties 
library(dplyr)
library(readr)

process_top10_by_year <- function(file, year) {
  df <- read_csv(file, show_col_types = FALSE) %>%
    select(GEOID, county, date, mean) %>%
    filter(!is.na(mean), county != "") %>%
    mutate(date = as.Date(date), year = year)
  
  top10 <- df %>%
    count(county, sort = TRUE) %>%
    slice_head(n = 10) %>%
    pull(county)
  
  df_top10 <- df %>%
    filter(county %in% top10)
  
  write_csv(df_top10, paste0("Top10_", year, ".csv"))
  return(df_top10)
}

# Run for all years
ndvi_2021 <- process_top10_by_year("NDVI_VA_County_Weekly_2021.csv", 2021)
ndvi_2022 <- process_top10_by_year("NDVI_VA_County_Weekly_2022.csv", 2022)
ndvi_2023 <- process_top10_by_year("NDVI_VA_County_Weekly_2023.csv", 2023)
ndvi_2024 <- process_top10_by_year("NDVI_VA_County_Weekly_2024.csv", 2024)

#Shiny
library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)

# Load the cleaned top 10 data for each year
ndvi_2021 <- read_csv("Top10_2021.csv", show_col_types = FALSE)
ndvi_2022 <- read_csv("Top10_2022.csv", show_col_types = FALSE)
ndvi_2023 <- read_csv("Top10_2023.csv", show_col_types = FALSE)
ndvi_2024 <- read_csv("Top10_2024.csv", show_col_types = FALSE)

# Extract unique county lists for dropdowns
counties_2021 <- sort(unique(ndvi_2021$county))
counties_2022 <- sort(unique(ndvi_2022$county))
counties_2023 <- sort(unique(ndvi_2023$county))
counties_2024 <- sort(unique(ndvi_2024$county))

# UI
ui <- fluidPage(
  titlePanel("NDVI for Top 10 Corn Counties in Virginia (2021–2024)"),
  tabsetPanel(
    tabPanel("2021",
             sidebarLayout(
               sidebarPanel(selectInput("c2021", "Select County:", choices = counties_2021, selected = counties_2021[1], multiple = TRUE)),
               mainPanel(plotlyOutput("plot2021"))
             )
    ),
    tabPanel("2022",
             sidebarLayout(
               sidebarPanel(selectInput("c2022", "Select County:", choices = counties_2022, selected = counties_2022[1], multiple = TRUE)),
               mainPanel(plotlyOutput("plot2022"))
             )
    ),
    tabPanel("2023",
             sidebarLayout(
               sidebarPanel(selectInput("c2023", "Select County:", choices = counties_2023, selected = counties_2023[1], multiple = TRUE)),
               mainPanel(plotlyOutput("plot2023"))
             )
    ),
    tabPanel("2024",
             sidebarLayout(
               sidebarPanel(selectInput("c2024", "Select County:", choices = counties_2024, selected = counties_2024[1], multiple = TRUE)),
               mainPanel(plotlyOutput("plot2024"))
             )
    )
  )
)

# Server
server <- function(input, output, session) {
  make_plot <- function(data, selected_counties, year) {
    plot_data <- data %>%
      filter(county %in% selected_counties)
    
    if (nrow(plot_data) == 0) return(NULL)
    
    p <- ggplot(plot_data, aes(x = date, y = mean, color = county)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(title = paste("Weekly NDVI –", year),
           x = "Date", y = "NDVI", color = "County") +
      theme_minimal()
    
    ggplotly(p, tooltip = c("x", "y", "color"))
  }
  
  output$plot2021 <- renderPlotly({ make_plot(ndvi_2021, input$c2021, 2021) })
  output$plot2022 <- renderPlotly({ make_plot(ndvi_2022, input$c2022, 2022) })
  output$plot2023 <- renderPlotly({ make_plot(ndvi_2023, input$c2023, 2023) })
  output$plot2024 <- renderPlotly({ make_plot(ndvi_2024, input$c2024, 2024) })
}

# Run App
shinyApp(ui, server)
