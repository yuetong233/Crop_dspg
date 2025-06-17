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

#adding more variety
library(shiny)
library(plotly)
library(dplyr)
library(ggplot2)
library(rnassqs)

# Authenticate NASS API
nassqs_auth("E0DE4B3D-0418-32C4-8541-6C4C8954534A")

# Years and categories
years <- 2021:(as.numeric(format(Sys.Date(), "%Y")))
categories <- c(
  "PCT PLANTED", "PCT EMERGED", "PCT SILKING", "PCT DOUGH",
  "PCT MATURE", "PCT HARVESTED", "PCT DENTED"
)

# Clean category for titles
clean_title <- function(cat) {
  gsub("^PCT ", "", cat)
}

# UI
ui <- fluidPage(
  titlePanel("🌽 Corn Progress in Virginia — Actual vs 5-Year Average"),
  p("Select a year tab to view weekly progress across different crop stages."),
  
  do.call(tabsetPanel, c(
    list(id = "year_tabs", type = "tabs"),
    lapply(as.character(years), function(yr) {
      tabPanel(yr,
               lapply(categories, function(cat) {
                 fluidRow(
                   column(6, plotlyOutput(outputId = paste0("plot_actual_", yr, "_", gsub("[^A-Za-z]", "_", cat)), height = "300px")),
                   column(6, plotlyOutput(outputId = paste0("plot_avg_", yr, "_", gsub("[^A-Za-z]", "_", cat)), height = "300px"))
                 )
               })
      )
    })
  ))
)

# Server
server <- function(input, output, session) {
  
  # Actual data
  get_data <- function(year, category) {
    tryCatch({
      nassqs(list(
        commodity_desc = "CORN",
        year = year,
        state_name = "VIRGINIA",
        statisticcat_desc = "PROGRESS",
        unit_desc = category
      )) %>%
        mutate(
          week = as.Date(week_ending),
          value = as.numeric(Value)
        ) %>%
        filter(!is.na(week)) %>%
        arrange(week)
    }, error = function(e) NULL)
  }
  
  # 5-Year Average
  get_avg_data <- function(year, category) {
    tryCatch({
      nassqs(list(
        commodity_desc = "CORN",
        year = year,
        state_name = "VIRGINIA",
        statisticcat_desc = "PROGRESS, 5 YEAR AVG",
        unit_desc = category
      )) %>%
        mutate(
          week = as.Date(week_ending),
          value = as.numeric(Value)
        ) %>%
        filter(!is.na(week)) %>%
        arrange(week)
    }, error = function(e) NULL)
  }
  
  # Loop through years and categories
  for (yr in years) {
    for (cat in categories) {
      safe_id <- gsub("[^A-Za-z]", "_", cat)
      actual_id <- paste0("plot_actual_", yr, "_", safe_id)
      avg_id <- paste0("plot_avg_", yr, "_", safe_id)
      
      local({
        year_inner <- yr
        category_inner <- cat
        actual_plot_id <- actual_id
        avg_plot_id <- avg_id
        
        # Actual
        output[[actual_plot_id]] <- renderPlotly({
          data <- get_data(year_inner, category_inner)
          
          if (is.null(data) || nrow(data) == 0) {
            return(plotly_empty(type = "scatter", mode = "lines") %>%
                     layout(title = list(text = paste("No Actual Data for", clean_title(category_inner), "-", year_inner))))
          }
          
          ggplotly(
            ggplot(data, aes(x = week, y = value)) +
              geom_line(color = "#2e7d32", size = 1.2) +
              geom_point(color = "#66bb6a", size = 2.5) +
              labs(
                title = paste("Actual", clean_title(category_inner), "-", year_inner),
                x = "Week Ending", y = "%"
              ) +
              theme_minimal() +
              theme(plot.title = element_text(size = 14, face = "bold"),
                    axis.text.x = element_text(angle = 45, hjust = 1)),
            tooltip = c("x", "y")
          )
        })
        
        # 5-Year Avg
        output[[avg_plot_id]] <- renderPlotly({
          data <- get_avg_data(year_inner, category_inner)
          
          if (is.null(data) || nrow(data) == 0) {
            return(plotly_empty(type = "scatter", mode = "lines") %>%
                     layout(title = list(text = paste("No 5-Year Avg for", clean_title(category_inner), "-", year_inner))))
          }
          
          ggplotly(
            ggplot(data, aes(x = week, y = value)) +
              geom_line(color = "#a5d6a7", size = 1.2) +
              geom_point(color = "#c8e6c9", size = 2.5) +
              labs(
                title = paste("5-Year Avg", clean_title(category_inner), "-", year_inner),
                x = "Week Ending", y = "%"
              ) +
              theme_minimal() +
              theme(plot.title = element_text(size = 14, face = "bold"),
                    axis.text.x = element_text(angle = 45, hjust = 1)),
            tooltip = c("x", "y")
          )
        })
      })
    }
  }
}

shinyApp(ui, server)


#making it pretty

library(shiny)
library(plotly)
library(dplyr)
library(ggplot2)
library(rnassqs)

# Authenticate API
nassqs_auth("E0DE4B3D-0418-32C4-8541-6C4C8954534A")

# Define years and categories
years <- 2021:(as.numeric(format(Sys.Date(), "%Y")))
categories <- c(
  "PCT PLANTED", "PCT EMERGED", "PCT SILKING", "PCT DOUGH",
  "PCT MATURE", "PCT HARVESTED", "PCT DENTED"
)

# Helper: clean titles
clean_title <- function(cat) {
  gsub("^PCT ", "", cat)
}

# UI
ui <- fluidPage(
  titlePanel("🌽 Virginia Corn Progress vs 5-Year Average"),
  p("Explore weekly progress for various crop stages. Each plot compares actual progress to the 5-year historical average."),
  
  do.call(tabsetPanel, c(
    list(id = "year_tabs", type = "tabs"),
    lapply(as.character(years), function(yr) {
      tabPanel(yr,
               lapply(categories, function(cat) {
                 tagList(
                   div(
                     style = "margin-top: 30px; margin-bottom: 5px;",
                     HTML(paste0("<h4>", clean_title(cat), " Progress</h4>")),
                     p(paste0("This chart compares weekly reported values to the 5-Year Average for corn ", 
                              tolower(clean_title(cat)), 
                              " during the ", yr, " season. Significant differences may reflect planting delays, environmental stress, or other agricultural factors."))
                   ),
                   plotlyOutput(outputId = paste0("plot_combined_", yr, "_", gsub("[^A-Za-z]", "_", cat)), height = "350px")
                 )
               })
      )
    })
  ))
)

# Server
server <- function(input, output, session) {
  
  # Pull actual data
  get_data <- function(year, category) {
    tryCatch({
      nassqs(list(
        commodity_desc = "CORN",
        year = year,
        state_name = "VIRGINIA",
        statisticcat_desc = "PROGRESS",
        unit_desc = category
      )) %>%
        mutate(
          week = as.Date(week_ending),
          value = as.numeric(Value)
        ) %>%
        filter(!is.na(week)) %>%
        arrange(week)
    }, error = function(e) NULL)
  }
  
  # Pull 5-Year Avg data
  get_avg_data <- function(year, category) {
    tryCatch({
      nassqs(list(
        commodity_desc = "CORN",
        year = year,
        state_name = "VIRGINIA",
        statisticcat_desc = "PROGRESS, 5 YEAR AVG",
        unit_desc = category
      )) %>%
        mutate(
          week = as.Date(week_ending),
          value = as.numeric(Value)
        ) %>%
        filter(!is.na(week)) %>%
        arrange(week)
    }, error = function(e) NULL)
  }
  
  # Render combined plot for each year and category
  for (yr in years) {
    for (cat in categories) {
      safe_id <- paste0("plot_combined_", yr, "_", gsub("[^A-Za-z]", "_", cat))
      
      local({
        year_inner <- yr
        category_inner <- cat
        output_id <- safe_id
        
        output[[output_id]] <- renderPlotly({
          actual <- get_data(year_inner, category_inner)
          avg <- get_avg_data(year_inner, category_inner)
          
          if ((is.null(actual) || nrow(actual) == 0) && (is.null(avg) || nrow(avg) == 0)) {
            return(plotly_empty(type = "scatter", mode = "lines") %>%
                     layout(title = list(text = paste("No data for", clean_title(category_inner), "-", year_inner))))
          }
          
          combined <- bind_rows(
            if (!is.null(actual)) mutate(actual, Type = "Actual") else NULL,
            if (!is.null(avg)) mutate(avg, Type = "5-Year Avg") else NULL
          )
          
          ggplotly(
            ggplot(combined, aes(x = week, y = value, color = Type)) +
              geom_line(size = 1.2) +
              geom_point(size = 2.5) +
              scale_color_manual(values = c("Actual" = "#1b5e20", "5-Year Avg" = "#a5d6a7")) +
              labs(
                title = paste(clean_title(category_inner), "Progress —", year_inner),
                x = "Week Ending", y = "%", color = "Legend"
              ) +
              theme_minimal() +
              theme(
                plot.title = element_text(size = 14, face = "bold"),
                axis.text.x = element_text(angle = 45, hjust = 1)
              ),
            tooltip = c("x", "y", "color")
          )
        })
      })
    }
  }
}

shinyApp(ui, server)

