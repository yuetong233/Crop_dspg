#Server
source("functions.R")
server <- function(input, output, session) {
  
  yield_data_all <- read_csv("yield_data_cache.csv", show_col_types = FALSE)
  
  yield_data <- reactive({
    req(input$yield_states)
    yield_data_all %>% filter(State %in% input$yield_states)
  })
  
  
  # --- Crop Condition Plots ---
  lapply(names(corn_data_list), function(yr) {
    output[[paste0("plot_", yr)]] <- renderPlotly({
      df <- corn_data_list[[yr]]
      
      # 🔁 Reapply factor level to lock order
      df$condition <- factor(df$condition, levels = c("VERY POOR", "POOR", "FAIR", "GOOD", "EXCELLENT"))
      
      plot_ly(
        data = df %>% arrange(week, condition),
        x = ~week,
        y = ~value,
        color = ~condition,
        colors = condition_colors,
        type = "scatter",
        mode = "none",
        stackgroup = "one",
        fill = "tonexty",
        text = ~paste0(
          "<b>Week:</b> ", format(week, "%b %d, %Y"),
          "<br><b>Condition:</b> ", condition,
          "<br><b>Percent:</b> ", value, "%"
        ),
        hoverinfo = "text"
      ) %>%
        layout(
          title = list(text = paste("Virginia Corn Conditions in", yr)),
          xaxis = list(title = "Week Ending"),
          yaxis = list(title = "Percent", range = c(0, 100)),
          legend = list(title = list(text = "Condition")),
          plot_bgcolor = "#fafafa",
          paper_bgcolor = "#fafafa"
        )
    })
  })
  
  # --- Summary Card ---
  output$summary_card <- renderPlotly({
    req(yield_data())
    df <- yield_data() %>%
      group_by(State) %>%
      summarise(
        `Average Yield` = mean(Value, na.rm = TRUE),
        `Max Yield` = max(Value, na.rm = TRUE),
        `Min Yield` = min(Value, na.rm = TRUE),
        `Yield Std Dev` = sd(Value, na.rm = TRUE)
      )
    
    plot_ly(
      type = 'table',
      header = list(values = c("State", names(df)[-1]), align = 'center',
                    fill = list(color = '#a5d6a7'), font = list(size = 14)),
      cells = list(
        values = rbind(df$State,
                       format(round(df[[2]], 1), nsmall = 1),
                       format(df[[3]], big.mark=","),
                       format(df[[4]], big.mark=","),
                       round(df[[5]], 1)),
        align = 'center', height = 30
      )
    )
  })
  
  # --- Yield Trends Plot ---
  output$yield_plot <- renderPlotly({
    req(yield_data())
    
    df <- yield_data() %>%
      group_by(State, year) %>%
      summarise(avg_yield = mean(Value, na.rm = TRUE), .groups = "drop") %>%
      group_by(State) %>%
      mutate(moving_avg = zoo::rollmean(avg_yield, k = input$ma_window, fill = NA, align = "right"))
    
    plot_ly(data = df, x = ~year, y = ~avg_yield, color = ~State, type = 'scatter', mode = 'lines+markers',
            name = ~paste(State, "Avg")) %>%
      add_lines(y = ~moving_avg, linetype = I("dash"), name = ~paste(State, "Moving Avg")) %>%
      layout(
        title = list(text = "Statewide Corn Yield Trends", font = list(family = "Times New Roman")),
        xaxis = list(title = "Year"),
        yaxis = list(title = "Average Yield (bushels per acre)"),
        plot_bgcolor = '#ffffff',
        paper_bgcolor = '#ffffff'
      )
  })
  
  selected_year <- reactiveVal("2024")
  
  lapply(years, function(yr) {
    observeEvent(input[[paste0("btn_", yr)]], {
      selected_year(as.character(yr))
    })
  })
  
  output$compare_map <- renderLeaflet({
    year <- selected_year()
    
    year_data <- clean_data %>% filter(Year == year)
    
    if (nrow(year_data) == 0) {
      leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        addLabelOnlyMarkers(
          lng = -78.6569, lat = 37.5,
          label = paste("Data not available for", year),
          labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE)
        ) %>%
        setView(lng = -78.6569, lat = 37.5, zoom = 6)
    } else {
      map_data <- left_join(all_counties, year_data, by = c("County", "State" = "state_alpha")) %>%
        st_as_sf()
      
      values <- map_data$Planted
      
      pal <- if (length(unique(na.omit(values))) > 1) {
        colorBin("YlGn", domain = values, bins = 5, na.color = "#f0f0f0")
      } else {
        colorBin("YlGn", domain = c(0, 1), bins = 5, na.color = "#f0f0f0")
      }
      
      leaflet(map_data) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(
          fillColor = ~pal(Planted),
          color = ~case_when(
            State == "VA" ~ "darkgreen",
            State == "NC" ~ "darkgreen",
            State == "MD" ~ "darkgreen",
            TRUE ~ "black"
          ),
          weight = 1.5,
          fillOpacity = 0.7,
          label = ~paste0(
            "<strong>", toupper(County), ", ", State, "</strong><br>",
            "Percent Harvested: ", ifelse(is.na(Harvested) | is.na(Planted) | Planted == 0, "N/A", paste0(round(100 * Harvested / Planted, 1), "%")), "<br>",
            "Planted: ", ifelse(is.na(Planted), "N/A", formatC(round(Planted), format = "d", big.mark = ",")), "<br>",
            "Harvested: ", ifelse(is.na(Harvested), "N/A", formatC(round(Harvested), format = "d", big.mark = ","))
          ) %>% lapply(htmltools::HTML),
          highlightOptions = highlightOptions(
            weight = 3,
            color = "#666",
            fillOpacity = 0.9,
            bringToFront = TRUE
          )
        ) %>%
        addLegend("bottomright", pal = pal, values = values, title = "Number of Acres", opacity = 1) %>%
        setView(lng = -78.6569, lat = 37.5, zoom = 6)
    }
  })
  
  #Planting Progress 
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
              geom_line(linewidth = 1.2) +
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
  
  # Get crop condition data for forecasting
  get_condition_data <- function() {
    tryCatch({
      data <- nassqs(list(
        commodity_desc = "CORN",
        year = format(Sys.Date(), "%Y"),
        state_name = "VIRGINIA",
        statisticcat_desc = "CONDITION",
        agg_level_desc = "STATE"
      ))
      
      data %>%
        filter(unit_desc %in% c("PCT EXCELLENT", "PCT GOOD", "PCT FAIR", "PCT POOR")) %>%
        mutate(
          week = as.Date(week_ending),
          value = as.numeric(Value),
          condition = gsub("PCT ", "", unit_desc)
        ) %>%
        filter(!is.na(week)) %>%
        arrange(week)
    }, error = function(e) NULL)
  }
  
  # Calculate yield deviation based on conditions
  calculate_yield_deviation <- function(condition_data) {
    if (is.null(condition_data)) return(NULL)
    
    # Calculate weighted score based on conditions
    condition_data %>%
      group_by(week) %>%
      summarise(
        weighted_score = sum(
          case_when(
            condition == "EXCELLENT" ~ value * 1.0,
            condition == "GOOD" ~ value * 0.5,
            condition == "FAIR" ~ value * 0.0,
            condition == "POOR" ~ value * -0.5,
            TRUE ~ 0
          )
        ),
        .groups = "drop"
      ) %>%
      mutate(
        # Convert weighted score to deviation percentage
        # Assuming 100% weighted score = 10% above trend
        deviation_pct = weighted_score / 10
      )
  }
  
  # Render condition distribution plot
  output$condition_distribution <- renderPlotly({
    condition_data <- get_condition_data()
    req(condition_data)
    
    plot_ly(
      data = condition_data,
      x = ~week,
      y = ~value,
      color = ~condition,
      type = "scatter",
      mode = "lines+markers",
      colors = c("EXCELLENT" = "#1a9850", "GOOD" = "#66bd63", 
                 "FAIR" = "#fee08b", "POOR" = "#d73027")
    ) %>%
      layout(
        title = "Current Crop Conditions",
        xaxis = list(title = "Week Ending"),
        yaxis = list(title = "Percentage", range = c(0, 100)),
        showlegend = TRUE
      )
  })
  
  # Render yield forecast plot
  output$yield_forecast <- renderPlotly({
    # 1. Query weekly crop condition data for Corn in Virginia, 2025
    corn_conditions <- nassqs(
      list(
        commodity_desc = "CORN",
        statisticcat_desc = "CONDITION",
        agg_level_desc = "STATE",
        state_name = "VIRGINIA",
        year = "2025"
      )
    )
    
    # Debug: print structure and head of corn_conditions
    print(head(corn_conditions))
    str(corn_conditions)
    
    # Add this check:
    if (nrow(corn_conditions) == 0) {
      return(plotly::plotly_empty(type = "scatter", mode = "lines") %>%
               layout(title = list(text = "No crop condition data available for 2025")))
    }
    
    # 2. Clean and reshape the data
    weekly_conditions <- corn_conditions %>%
      select(week_ending, short_desc, Value) %>%
      mutate(
        Week = as.Date(week_ending),
        Condition = dplyr::case_when(
          stringr::str_detect(short_desc, "EXCELLENT") ~ "Excellent",
          stringr::str_detect(short_desc, "GOOD") ~ "Good",
          stringr::str_detect(short_desc, "FAIR") ~ "Fair",
          stringr::str_detect(short_desc, "POOR") & !stringr::str_detect(short_desc, "VERY") ~ "Poor",
          TRUE ~ NA_character_
        ),
        Value = as.numeric(Value)
      ) %>%
      filter(!is.na(Condition)) %>%
      group_by(Week, Condition) %>%
      summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = Condition, values_from = Value) %>%
      arrange(Week)
    
    # Check for all NA in Week
    if (all(is.na(weekly_conditions$Week))) {
      return(plotly::plotly_empty(type = "scatter", mode = "lines") %>%
               layout(title = list(text = "No valid week-ending dates in crop condition data")))
    }
    
    # Ensure all expected columns exist
    for (col in c("Poor", "Fair", "Good", "Excellent")) {
      if (!col %in% names(weekly_conditions)) {
        weekly_conditions[[col]] <- 0
      }
    }
    
    # Replace NA with 0 for condition columns
    weekly_conditions <- weekly_conditions %>%
      mutate(
        Poor = ifelse(is.na(Poor), 0, Poor),
        Fair = ifelse(is.na(Fair), 0, Fair),
        Good = ifelse(is.na(Good), 0, Good),
        Excellent = ifelse(is.na(Excellent), 0, Excellent)
      )
    
    # 3. Regression coefficients and trend yield
    intercept <- -0.304165831
    coef_poor <- -0.000678391
    coef_fair <- 0.000165538
    coef_good <- 0.003902026
    coef_excellent <- 0.007679806
    trend_yield_2025 <- 146.0
    
    # 4. Forecast weekly yield
    forecast_df <- weekly_conditions %>%
      mutate(
        ForecastDeviation = intercept +
          coef_poor * Poor +
          coef_fair * Fair +
          coef_good * Good +
          coef_excellent * Excellent,
        ForecastYield = (1 + ForecastDeviation) * trend_yield_2025
      )
    
    # Filter out rows with NA in Week or ForecastYield
    forecast_df <- forecast_df %>%
      filter(!is.na(Week), !is.na(ForecastYield))
    
    # 5. Plot with Plotly
    p <- ggplot(forecast_df, aes(x = Week, y = ForecastYield)) +
      geom_line(color = "forestgreen", size = 1.5) +
      geom_point(color = "darkgreen") +
      geom_hline(yintercept = trend_yield_2025, linetype = "dashed", color = "gray30") +
      labs(
        title = "Weekly Forecasted Corn Yield (Virginia, 2025)",
        subtitle = "Based on USDA Crop Conditions & Regression Model",
        x = "Week Ending",
        y = "Forecasted Yield (bu/acre)"
      ) +
      theme_minimal(base_size = 14)
    
    plotly::ggplotly(p)
  })
  
  #Remote sensing data 
  # Load all data
  ndvi_recent_data <- ndvi_recent_all()
  ndvi_top10 <- ndvi_top10_data() %>%
    rename(mean_NDVI = NDVI_mean)
  temp_top10 <- temp_top10_all()
  temp_recent <- temp_recent_all()
  
  # Update county choices
  observe({
    req(input$ndvi_source)
    counties <- if (input$ndvi_source == "Top 10 Counties") {
      unique(ndvi_top10$county)
    } else {
      unique(ndvi_recent_data$county)
    }
    updateSelectInput(session, "ndvi_county_selector", choices = counties)
  })
  
  observe({
    req(input$temp_source)
    counties <- if (input$temp_source == "Top 10 Counties") {
      unique(temp_top10[[input$temp_type]]$county)
    } else {
      unique(temp_recent[[input$temp_type]]$county)
    }
    updateSelectInput(session, "temp_county_selector", choices = counties)
  })
  
  # NDVI Plot
  output$ndvi_plot <- renderPlotly({
    req(input$ndvi_source, input$ndvi_county_selector)
    
    df <- if (input$ndvi_source == "Top 10 Counties") {
      ndvi_top10
    } else {
      ndvi_recent_data
    }
    
    filtered_df <- df %>%
      filter(county %in% input$ndvi_county_selector)
    
    validate(
      need(nrow(filtered_df) > 0, "No NDVI data found for the selected county.")
    )
    
    p <- ggplot(filtered_df, aes(x = date, y = mean_NDVI, color = county)) +
      geom_point(size = 2) +  # scatter plot
      labs(
        title = paste("NDVI Trends -", input$ndvi_source),
        x = "Date", y = "NDVI"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  
  
  
  # Temperature Plot
  output$temp_plot <- renderPlotly({
    req(input$temp_source, input$temp_year, input$temp_type, input$temp_county_selector)
    df <- if (input$temp_source == "Top 10 Counties") {
      temp_top10[[input$temp_type]]
    } else {
      temp_recent[[input$temp_type]]
    }
    
    df %>%
      filter(year == input$temp_year, county %in% input$temp_county_selector) %>%
      ggplot(aes(x = date, y = T_avg, color = county)) +
      geom_line(size = 1) +
      labs(title = paste(input$temp_type, "Temperature -", input$temp_source),
           x = "Date", y = paste(input$temp_type, "Temperature (°C)")) +
      theme_minimal() -> p
    
    ggplotly(p)
  })
  
  
  
}