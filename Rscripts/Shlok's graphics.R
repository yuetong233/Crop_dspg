#Planting Progress

# --- Libraries ---
library(shiny)
library(shinyWidgets)
library(plotly)
library(dplyr)
library(ggplot2)
library(rnassqs)

# --- NASS API Auth ---
nassqs_auth(key = "E0DE4B3D-0418-32C4-8541-6C4C8954534A")

# --- UI ---
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

# --- Server ---
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
  
  # Render individual year plots
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
# --- Run App ---
shinyApp(ui, server)

