#6 Months remote sensing data and Top 10

library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)

# ---- Load Data ----

# Top 10 NDVI full-year file
ndvi_top10 <- read_csv("Top10Counties_NDVI.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(date), year = lubridate::year(date))

# Load the 6-month CSVs
load_recent <- function(y) {
  read_csv(paste0("NDVI_Last6Months_", y, ".csv"), show_col_types = FALSE) %>%
    mutate(date = as.Date(date), year = y)
}
ndvi_recent <- bind_rows(lapply(2021:2024, load_recent))

# ---- UI ----
ui <- fluidPage(
  titlePanel("🌿 NDVI Trends – Top 10 vs Past 6 Months"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("source", "Select Dataset:",
                   choices = c("Top 10 Counties", "Last 6 Months"),
                   selected = "Last 6 Months"),
      
      selectInput("year", "Select Year:", choices = 2021:2024, selected = 2024),
      
      uiOutput("county_selector")
    ),
    
    mainPanel(
      plotlyOutput("ndvi_plot", height = "600px")
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  # Reactive data
  selected_data <- reactive({
    if (input$source == "Top 10 Counties") {
      ndvi_top10 %>% filter(year == input$year)
    } else {
      ndvi_recent %>% filter(year == input$year)
    }
  })
  
  # Update county dropdown
  output$county_selector <- renderUI({
    counties <- selected_data() %>%
      pull(county_name) %>%
      unique() %>%
      sort()
    
    selectInput("selected_counties", "Select Counties:",
                choices = counties,
                selected = head(counties, 3),
                multiple = TRUE)
  })
  
  # Plot
  output$ndvi_plot <- renderPlotly({
    req(input$selected_counties)
    
    df <- selected_data() %>%
      filter(county_name %in% input$selected_counties) %>%
      arrange(date)
    
    # Define soft pastel color palette
    pastel_colors <- c("#AEC6CF", "#FFB347", "#B39EB5", "#77DD77", "#F49AC2",
                       "#CFCFC4", "#DEA5A4", "#B0E0E6", "#FFD1DC", "#C6E2FF")
    
    p <- ggplot(df, aes(x = date, y = NDVI_mean, color = county_name, group = county_name)) +
      geom_line(linewidth = 1.3) +
      geom_point(size = 2) +
      scale_color_manual(values = pastel_colors) +
      labs(
        title = paste("NDVI –", input$source, "|", input$year),
        x = "Date", y = "NDVI", color = "County"
      ) +
      theme_light(base_size = 15) +
      theme(
        plot.title = element_text(face = "bold"),
        legend.position = "bottom"
      )
    
    ggplotly(p, tooltip = c("x", "y", "color"))
  })
}

# ---- Run App ----
shinyApp(ui, server)
