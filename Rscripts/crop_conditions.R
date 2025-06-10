#This data set is from the USDA survey and represents 
#crop conditions of corn that were measured in a good year in weekly Virginia from 2021-2024
data = read.csv("GoodCorn.csv")
head(data)

library(ggplot2)
library(dplyr)

#Initial plot to view
ggplot(data, aes(x = Period, y =Value)) + geom_point()

#Updated plot
data$WeekNum <- as.numeric(gsub("[^0-9]", "", data$Period))
data$Period <- factor(data$Period, levels = data$Period[order(data$WeekNum)])
ggplot(data, aes(x = Period, y = Value, color = Year, group = Year)) +
  geom_line(size = 1.2) +
  labs(
    title = "Virginia Corn Rated 'Good' by Week (2021–2024)",
    x = "Week",
    y = "Percent Rated Good"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 8),
    plot.margin = margin(10, 20, 10, 10)
  )

#Splitting the plots into years
ggplot(data, aes(x = Period, y = Value, group = Year)) +
  geom_line(color = "steelblue", size = 1.2) +
  facet_wrap(~Year, ncol = 2) +
  labs(
    title = "Virginia Corn Rated 'Good' by Week (Faceted by Year)",
    x = "Week",
    y = "Percent Rated Good"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold")
  )



#Implementing the plot in R shiny
library(shiny)
library(readr)
library(plotly)

# Extract week number and safely order Period
data <- data %>%
  mutate(WeekNum = as.numeric(gsub("[^0-9]", "", Period))) %>%
  arrange(WeekNum) %>%
  mutate(Period = factor(Period, levels = unique(Period))) %>%
  mutate(Year = as.factor(Year))  # Ensure Year is a factor

# Define UI
ui <- fluidPage(
  titlePanel("Virginia Corn Condition Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "year",
        label = "Select Year(s):",
        choices = levels(data$Year),
        selected = levels(data$Year)
      ),
      sliderInput(
        inputId = "weekRange",
        label = "Select Week Range:",
        min = min(data$WeekNum, na.rm = TRUE),
        max = max(data$WeekNum, na.rm = TRUE),
        value = c(min(data$WeekNum, na.rm = TRUE), max(data$WeekNum, na.rm = TRUE)),
        step = 1,
        sep = ""
      )
    ),
    
    mainPanel(
      plotlyOutput("cornPlot")
    )
  )
)

# Define server
server <- function(input, output) {
  output$cornPlot <- renderPlotly({
    # Filter data by selected years and week range
    filtered_data <- data %>%
      filter(Year %in% input$year,
             WeekNum >= input$weekRange[1],
             WeekNum <= input$weekRange[2])
    
    # Create plot
    p <- ggplot(filtered_data, aes(
      x = Period, y = Value, color = Year, group = Year,
      text = paste("Year:", Year, "<br>Week:", Period, "<br>Value:", Value)
    )) +
      geom_line(linewidth = 1.2) +
      geom_point() +
      labs(
        title = "Corn Rated 'Good' by Week in Virginia",
        x = "Week",
        y = "Percent Rated Good"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 8))
    
    ggplotly(p, tooltip = "text")
  })
}

# Launch the app
shinyApp(ui = ui, server = server)

#Exploring the poor quality from 20212-2024
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(readr)

# Load datasets
good_data <- read.csv("GoodCorn.csv")
poor_data <- read.csv("PoorCorn.csv")

# Add quality type
good_data$Quality <- "Good"
poor_data$Quality <- "Poor"

# Combine and preprocess
data <- bind_rows(good_data, poor_data) %>%
  mutate(
    WeekNum = as.numeric(gsub("[^0-9]", "", Period)),
    Year = as.factor(Year)
  ) %>%
  arrange(WeekNum) %>%
  mutate(Period = factor(Period, levels = unique(Period)))

# UI
ui <- fluidPage(
  titlePanel("🌽 Virginia Corn Condition Dashboard"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "year",
        label = "Select Year(s):",
        choices = levels(data$Year),
        selected = levels(data$Year)
      ),
      sliderInput(
        inputId = "weekRange",
        label = "Select Week Range:",
        min = min(data$WeekNum, na.rm = TRUE),
        max = max(data$WeekNum, na.rm = TRUE),
        value = c(min(data$WeekNum, na.rm = TRUE), max(data$WeekNum, na.rm = TRUE)),
        step = 1,
        sep = ""
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Good vs Poor", plotlyOutput("comparePlot")),
        tabPanel("Poor Only", plotlyOutput("poorPlot"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  filtered_data <- reactive({
    data %>%
      filter(
        Year %in% input$year,
        WeekNum >= input$weekRange[1],
        WeekNum <= input$weekRange[2]
      )
  })
  
  output$comparePlot <- renderPlotly({
    p1 <- ggplot(filtered_data(), aes(
      x = Period, y = Value, color = Quality, group = interaction(Quality, Year),
      text = paste("Year:", Year, "<br>Week:", Period, "<br>", Quality, ":", Value)
    )) +
      geom_line(linewidth = 1.2) +
      geom_point() +
      labs(
        title = "Comparison of Corn Rated 'Good' vs 'Poor' by Week",
        x = "Week",
        y = "Percent"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 8))
    
    ggplotly(p1, tooltip = "text")
  })
  
  output$poorPlot <- renderPlotly({
    poor_only <- filtered_data() %>% filter(Quality == "Poor")
    
    p2 <- ggplot(poor_only, aes(
      x = Period, y = Value, color = Year, group = Year,
      text = paste("Year:", Year, "<br>Week:", Period, "<br>Value:", Value)
    )) +
      geom_line(linewidth = 1.2) +
      geom_point() +
      labs(
        title = "Corn Rated 'Poor' by Week in Virginia",
        x = "Week",
        y = "Percent Rated Poor"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 8))
    
    ggplotly(p2, tooltip = "text")
  })
}

# Run App
shinyApp(ui = ui, server = server)


#API stuff
library(rnassqs)
library(dplyr)
library(ggplot2)

nassqs_auth(key = "E0DE4B3D-0418-32C4-8541-6C4C8954534A")

corn_conditions_2025 <- nassqs(list(
  commodity_desc = "CORN",
  year = "2025",
  state_name = "VIRGINIA",
  statisticcat_desc = "CONDITION",
  agg_level_desc = "STATE"
))

corn_conditions_clean <- corn_conditions_2025 %>%
  filter(unit_desc %in% c("PCT EXCELLENT", "PCT GOOD", "PCT FAIR", "PCT POOR", "PCT VERY POOR")) %>%
  mutate(
    week = as.Date(week_ending),
    value = as.numeric(Value)
  )
corn_conditions_clean 

ggplot(corn_conditions_clean, aes(x = week, y = value, color = unit_desc, group = unit_desc)) +
  geom_line(size = 1.2) +
  labs(title = "2025 Virginia Corn Conditions",
       x = "Week Ending",
       y = "Percentage",
       color = "Condition") +
  theme_minimal()


#shiny
library(shiny)
library(plotly)
library(dplyr)
desired_order <- c("PCT VERY POOR", "PCT POOR", "PCT FAIR", "PCT GOOD", "PCT EXCELLENT")
corn_conditions_clean$unit_desc <- factor(
  corn_conditions_clean$unit_desc,
  levels = desired_order
)

condition_colors <- c(
  "PCT VERY POOR" = "#a50026",
  "PCT POOR"      = "#d73027",
  "PCT FAIR"      = "#fee08b",
  "PCT GOOD"      = "#66bd63",
  "PCT EXCELLENT" = "#1a9850"
)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .container {background-color: #fafafa; border-radius: 25px; padding: 30px;}
      h2 {color: #1a9850; font-family: 'Montserrat', sans-serif;}
    "))
  ),
  div(class = "container",
      titlePanel(h2("2025 Virginia Corn Conditions", style = "font-weight:600;")),
      mainPanel(
        plotlyOutput("stacked_area", height = "600px")
      )
  )
)

server <- function(input, output) {
  output$stacked_area <- renderPlotly({
    plot_ly(
      corn_conditions_clean %>% arrange(week, unit_desc),
      x = ~week,
      y = ~value,
      color = ~unit_desc,
      colors = condition_colors,
      type = "scatter",
      mode = "none",
      stackgroup = "one",
      fill = "tonexty",
      text = ~paste0(
        "<b>Week:</b> ", format(week, "%b %d, %Y"),
        "<br><b>Condition:</b> ", gsub("PCT ", "", unit_desc),
        "<br><b>Percent:</b> ", value, "%"
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(title = "Week Ending", tickfont = list(size=16, family="Montserrat")),
        yaxis = list(title = "Percent", range = c(0, 100), tickfont = list(size=16, family="Montserrat")),
        legend = list(title = list(text = "<b>Condition</b>"), font=list(size=16)),
        plot_bgcolor = "#fafafa",
        paper_bgcolor = "#fafafa",
        font = list(family = "Montserrat", size = 15)
      )
  })
}

shinyApp(ui, server)

#adding more years