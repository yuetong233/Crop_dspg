#Planting Progress 

library(shiny)
library(shinyWidgets)
library(plotly)
library(dplyr)
library(ggplot2)
library(rnassqs)
nassqs_auth(key = "E0DE4B3D-0418-32C4-8541-6C4C8954534A")

#UI
ui <- fluidPage(
  titlePanel("🌽 Corn Planting Progress in Virginia"),
  p("View weekly planting progress for each year side by side."),
  
  do.call(tabsetPanel, c(
    list(id = "year_tabs", type = "tabs"),
    lapply(as.character(2021:(as.numeric(format(Sys.Date(), "%Y")))), function(yr) {
      tabPanel(yr, plotlyOutput(outputId = paste0("plot_", yr), height = "500px"))
    })
  ))
)

#Server 
server <- function(input, output, session) {
  
  get_year_data <- function(year) {
    tryCatch({
      data <- nassqs(list(
        source_desc = "SURVEY",
        sector_desc = "CROPS",
        group_desc = "FIELD CROPS",
        commodity_desc = "CORN",
        statisticcat_desc = "PROGRESS",
        unit_desc = "PCT PLANTED",
        state_name = "VIRGINIA",
        year = year
      ))
      data %>%
        mutate(
          week = as.Date(week_ending),
          value = as.numeric(Value),
          year = as.character(year)
        ) %>%
        filter(!is.na(week)) %>%
        arrange(week)
    }, error = function(e) NULL)
  }
  
  for (yr in 2021:(as.numeric(format(Sys.Date(), "%Y")))) {
    local({
      year_inner <- yr
      output[[paste0("plot_", year_inner)]] <- renderPlotly({
        data <- get_year_data(year_inner)
        req(data)
        
        p <- ggplot(data, aes(x = week, y = value)) +
          geom_line(color = "#2e7d32", size = 1.4) +
          geom_point(color = "#66bb6a", size = 2.5) +
          labs(
            title = paste("🌽 Corn Planting Progress —", year_inner),
            x = "Week Ending",
            y = "% Planted"
          ) +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(face = "bold", size = 16)
          )
        
        ggplotly(p, tooltip = c("x", "y"))
      })
    })
  }
}
shinyApp(ui, server)

#Five year avg
library(shiny)
library(shinyWidgets)
library(plotly)
library(dplyr)
library(ggplot2)
library(rnassqs)

# Authenticate NASS API
nassqs_auth(key = "E0DE4B3D-0418-32C4-8541-6C4C8954534A")
years <- 2021:(as.numeric(format(Sys.Date(), "%Y")))

# UI
ui <- fluidPage(
  titlePanel("🌽 Virginia Corn Planting Progress: Current & 5-Year Avg Comparison"),
  p("Each year shows two side-by-side plots: one for weekly progress, and one comparing to the 5-year average."),
  
  do.call(tabsetPanel, c(
    list(id = "year_tabs", type = "tabs"),
    lapply(as.character(years), function(yr) {
      tabPanel(yr,
               fluidRow(
                 column(6, plotlyOutput(outputId = paste0("plot_", yr))),
                 column(6, plotlyOutput(outputId = paste0("comparison_plot_", yr)))
               )
      )
    })
  ))
)

# Server
server <- function(input, output, session) {
  
  # Current year data
  get_year_data <- function(year) {
    tryCatch({
      nassqs(list(
        source_desc = "SURVEY",
        sector_desc = "CROPS",
        group_desc = "FIELD CROPS",
        commodity_desc = "CORN",
        statisticcat_desc = "PROGRESS",
        unit_desc = "PCT PLANTED",
        state_name = "VIRGINIA",
        year = year
      )) %>%
        mutate(week = as.Date(week_ending),
               value = as.numeric(Value),
               type = "Current") %>%
        filter(!is.na(week)) %>%
        arrange(week)
    }, error = function(e) NULL)
  }
  
  # 5-Year Avg data
  get_avg_data <- function(year) {
    tryCatch({
      nassqs(list(
        source_desc = "SURVEY",
        sector_desc = "CROPS",
        group_desc = "FIELD CROPS",
        commodity_desc = "CORN",
        statisticcat_desc = "PROGRESS, 5 YEAR AVG",
        unit_desc = "PCT PLANTED",
        state_name = "VIRGINIA",
        year = year
      )) %>%
        mutate(week = as.Date(week_ending),
               value = as.numeric(Value),
               type = "5-Year Avg") %>%
        filter(!is.na(week)) %>%
        arrange(week)
    }, error = function(e) NULL)
  }
  
  # For each year tab
  for (yr in years) {
    local({
      year_inner <- yr
      
      # Left plot — actual % planted
      output[[paste0("plot_", year_inner)]] <- renderPlotly({
        data <- get_year_data(year_inner)
        req(data)
        p <- ggplot(data, aes(x = week, y = value)) +
          geom_line(color = "#1b5e20", size = 1.4) +
          geom_point(color = "#66bb6a", size = 2.5) +
          labs(
            title = paste("🌱 Actual Corn Planting Progress —", year_inner),
            x = "Week Ending",
            y = "% Planted"
          ) +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(face = "bold", size = 16)
          )
        ggplotly(p, tooltip = c("x", "y"))
      })
      
      # Right plot — compare with 5-year average
      output[[paste0("comparison_plot_", year_inner)]] <- renderPlotly({
        current <- get_year_data(year_inner)
        avg <- get_avg_data(year_inner)
        req(current, avg)
        combined <- bind_rows(current, avg)
        p <- ggplot(combined, aes(x = week, y = value, color = type)) +
          geom_line(size = 1.4) +
          geom_point(size = 2.5) +
          labs(
            title = paste("📈", year_inner, "vs 5-Year Avg"),
            x = "Week Ending",
            y = "% Planted",
            color = "Type"
          ) +
          scale_color_manual(values = c("Current" = "#1b5e20", "5-Year Avg" = "#a5d6a7")) +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(face = "bold", size = 16)
          )
        ggplotly(p, tooltip = c("x", "y", "color"))
      })
    })
  }
}

shinyApp(ui, server)
