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
    # 1. Query yield data for 2014-2024
    yield_data <- nassqs(list(
      source_desc = "SURVEY",
      sector_desc = "CROPS",
      group_desc = "FIELD CROPS",
      commodity_desc = "CORN",
      statisticcat_desc = "YIELD",
      unit_desc = "BU / ACRE",
      agg_level_desc = "STATE",
      state_alpha = "VA",
      year = 2014:2024
    )) %>%
      mutate(Year = as.integer(year),
             Yield = as.numeric(Value)) %>%
      select(Year, Yield)
    
    # 2. Query crop condition data for 2014-2025
    condition_data_raw <- nassqs(list(
      source_desc = "SURVEY",
      sector_desc = "CROPS",
      group_desc = "FIELD CROPS",
      commodity_desc = "CORN",
      statisticcat_desc = "CONDITION",
      agg_level_desc = "STATE",
      state_alpha = "VA",
      year = 2014:2025
    ))
    
    # 3. Filter for just GOOD and EXCELLENT descriptions, get last week per year
    condition_data <- condition_data_raw %>%
      filter(grepl("MEASURED IN PCT GOOD|MEASURED IN PCT EXCELLENT", short_desc, ignore.case = TRUE)) %>%
      filter(!is.na(week_ending)) %>%
      mutate(Week_Ending = as.Date(week_ending)) %>%
      group_by(year, Week_Ending) %>%
      summarise(GE = sum(as.numeric(Value), na.rm = TRUE), .groups = "drop") %>%
      group_by(year) %>%
      filter(Week_Ending == max(Week_Ending)) %>%
      ungroup() %>%
      rename(Year = year)
    
    condition_data <- condition_data %>% mutate(Year = as.integer(Year))
    
    # 4. Merge yield and G+E data
    merged_data <- yield_data %>%
      inner_join(condition_data, by = "Year") %>%
      arrange(Year)
    
    # 5. Fit trend line: Yield ~ (Year - 2013)
    merged_data <- merged_data %>% mutate(Year_Index = Year - 2013)
    trend_model <- lm(Yield ~ Year_Index, data = merged_data)
    merged_data$Trend_Yield <- predict(trend_model, newdata = merged_data)
    merged_data <- merged_data %>% mutate(Deviation_Pct = ((Yield - Trend_Yield) / Trend_Yield))
    
    # 6. Fit regression model: Deviation_Pct ~ GE
    regression_model <- lm(Deviation_Pct ~ GE, data = merged_data)
    
    # 7. Get weekly Good+Excellent for 2025
    weekly_2025 <- condition_data_raw %>%
      filter(grepl("MEASURED IN PCT GOOD|MEASURED IN PCT EXCELLENT", short_desc, ignore.case = TRUE)) %>%
      filter(!is.na(week_ending) & year == 2025) %>%
      mutate(Week_Ending = as.Date(week_ending)) %>%
      group_by(Week_Ending) %>%
      summarise(GE = sum(as.numeric(Value), na.rm = TRUE), .groups = "drop") %>%
      arrange(Week_Ending)
    
    # 8. Predict % deviation for each week using regression
    weekly_2025$Deviation_Pct <- predict(regression_model, newdata = weekly_2025)
    
    # 9. Get 2024 trend yield to use as base
    trend_yield_2024 <- predict(trend_model, newdata = data.frame(Year_Index = 2024 - 2013))
    
    # 10. Compute forecasted yield for each 2025 week
    weekly_2025$Forecasted_Yield <- (1 + weekly_2025$Deviation_Pct / 100) * trend_yield_2024
    
    # 11. Plot with ggplotly
    p <- ggplot(weekly_2025, aes(x = Week_Ending, y = Forecasted_Yield)) +
      geom_line(color = "darkgreen", size = 1.5) +
      geom_point(color = "darkgreen", size = 2) +
      geom_hline(yintercept = trend_yield_2024, linetype = "dashed", color = "gray30") +
      labs(title = "Weekly Forecasted Corn Yield (Virginia, 2025)",
           subtitle = "Based on Good+Excellent Regression Model",
           x = "Week Ending",
           y = "Forecasted Yield (bu/acre)") +
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
    req(input$ndvi_source, input$ndvi_year)
    
    counties <- if (input$ndvi_source == "Top 10 Counties") {
      get_valid_ndvi_counties(ndvi_top10, input$ndvi_year)
    } else {
      get_valid_ndvi_counties(ndvi_recent_data, input$ndvi_year)
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
    req(input$ndvi_source, input$ndvi_county_selector, input$ndvi_year)
    
    df <- if (input$ndvi_source == "Top 10 Counties") {
      ndvi_top10
    } else {
      ndvi_recent_data
    }
    
    filtered_df <- df %>%
      filter(county %in% input$ndvi_county_selector) %>%
      filter(lubridate::year(date) == input$ndvi_year) %>%
      filter(lubridate::month(date) >= 5 & lubridate::month(date) <= 9)  # May–September only
    
    validate(
      need(nrow(filtered_df) > 0, "No NDVI data found for the selected county and year.")
    )
    
    p <- ggplot(filtered_df, aes(x = date, y = mean_NDVI, color = county)) +
      geom_point(size = 2) +
      labs(
        title = paste("NDVI Trends (May–Sept)", input$ndvi_year, "-", input$ndvi_source),
        x = "Date", y = "NDVI"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$temp_plot <- renderPlotly({
    req(input$temp_source, input$temp_county_selector, input$temp_year, input$temp_type)
    
    df <- if (input$temp_source == "Top 10 Counties") {
      temp_top10[[input$temp_type]]
    } else {
      temp_recent[[input$temp_type]]
    }
    
    filtered_df <- df %>%
      filter(county %in% input$temp_county_selector) %>%
      filter(year == input$temp_year) %>%
      filter(lubridate::month(date) >= 5 & lubridate::month(date) <= 9)
    
    validate(
      need(nrow(filtered_df) > 0, "No temperature data found for the selected county and year.")
    )
    
    y_col <- case_when(
      input$temp_type == "Average" ~ "T_avg",
      input$temp_type == "High" ~ "T_day",
      input$temp_type == "Low" ~ "T_night"
    )
    
    p <- ggplot(filtered_df, aes(x = date, y = .data[[y_col]], color = county)) +
      geom_line(size = 1) +
      labs(
        title = paste(input$temp_type, "Temperature (May–Sept)", input$temp_year),
        x = "Date", y = "Temperature (°C)"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  #county analysis
  # --- County Acres Picker UI ---
  output$county_picker_ui <- renderUI({
    req(input$county_state, input$county_year)
    
    counties <- clean_data %>%
      filter(
        state_alpha == input$county_state,
        Year == as.character(input$county_year)
      ) %>%
      pull(County) %>%
      tools::toTitleCase() %>%
      unique() %>%
      sort()
    
    pickerInput(
      inputId = "selected_counties",
      label = "Select County (Max 2):",
      choices = counties,
      multiple = TRUE,
      options = list(`max-options` = 2, `actions-box` = TRUE)
    )
  })
  
  # --- County Acres Plot ---
  output$county_acres_plot <- renderPlotly({
    req(input$selected_counties, input$county_state, input$county_year)
    
    sel <- input$selected_counties
    if (length(sel) == 0) {
      return(plotly_empty() %>% layout(title = "Select up to 2 counties"))
    }
    
    df <- clean_data %>%
      filter(
        state_alpha == input$county_state,
        Year == as.character(input$county_year),
        tools::toTitleCase(County) %in% sel
      ) %>%
      mutate(County = tools::toTitleCase(County)) %>%
      pivot_longer(c(Planted, Harvested), names_to = "Type", values_to = "Acres")
    
    gg <- ggplot(df, aes(x = Type, y = Acres, fill = Type)) +
      geom_bar(stat = "identity", position = "dodge") +
      facet_wrap(~County, scales = "free_y") +
      scale_fill_manual(values = c("Planted" = "#1b5e20", "Harvested" = "#a5d6a7")) +
      labs(
        title = paste("Acres Planted vs. Harvested (", input$county_year, ",", input$county_state, ")"),
        x = "", y = "Acres"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", color = "#2e7d32"),
        legend.position = "none"
      )
    
    ggplotly(gg)
  })
  # --- Yield Analysis Data Preparation (run once at startup) ---
  states <- c("VA", "MD", "NC")
  get_available_years <- function(state) {
    years <- nassqs_param_values("year", list(
      commodity_desc = "CORN",
      state_alpha = state,
      statisticcat_desc = "YIELD",
      agg_level_desc = "STATE"
    ))
    as.integer(years)
  }
  years_list <- lapply(states, get_available_years)
  names(years_list) <- states
  get_state_yield <- function(state) {
    years <- years_list[[state]]
    nassqs(list(
      source_desc = "SURVEY",
      sector_desc = "CROPS",
      group_desc = "FIELD CROPS",
      commodity_desc = "CORN",
      statisticcat_desc = "YIELD",
      unit_desc = "BU / ACRE",
      agg_level_desc = "STATE",
      state_alpha = state,
      year = years
    )) %>%
      mutate(
        State = state,
        Year = as.integer(year),
        Yield = as.numeric(Value)
      ) %>%
      select(State, Year, Yield)
  }
  get_county_yield <- function(state) {
    years <- years_list[[state]]
    nassqs(list(
      source_desc = "SURVEY",
      sector_desc = "CROPS",
      group_desc = "FIELD CROPS",
      commodity_desc = "CORN",
      statisticcat_desc = "YIELD",
      unit_desc = "BU / ACRE",
      agg_level_desc = "COUNTY",
      state_alpha = state,
      year = years
    )) %>%
      mutate(
        State = state,
        Year = as.integer(year),
        County = county_name,
        Yield = as.numeric(Value)
      ) %>%
      select(State, County, Year, Yield)
  }
  yield_state_all <- bind_rows(lapply(states, get_state_yield))
  yield_county_all <- bind_rows(lapply(states, get_county_yield))
  all_counties <- bind_rows(lapply(states, function(st) {
    counties(state = st, cb = TRUE, year = 2023) %>%
      st_transform(4326) %>%
      mutate(
        County = tolower(NAME),
        County = gsub(" county", "", County),
        County = trimws(County),
        State = st
      )
  }))
  
  
  # --- Yield Analysis Outputs ---
  output$county_map_year_ui <- renderUI({
    req(input$county_map_state)
    available_years <- sort(unique(yield_county_all$Year[yield_county_all$State == input$county_map_state]), decreasing = TRUE)
    selectInput("county_map_year", "Select Year for County Map:", choices = available_years, selected = max(available_years))
  })
  
  
  output$yield_state_plot <- renderPlotly({
    req(input$yield_states)
    plot_state <- yield_state_all %>% filter(State %in% input$yield_states)
    plot_ly(plot_state, x = ~Year, y = ~Yield, color = ~State, type = 'scatter', mode = 'lines+markers') %>%
      layout(
        title = 'Corn Yield by State (All Available Years)',
        xaxis = list(title = 'Year'),
        yaxis = list(title = 'Yield (bushels/acre)'),
        legend = list(title = list(text = 'State'))
      )
  })
  
  
  output$yield_county_map <- renderLeaflet({
    req(input$county_map_state, input$county_map_year)
    yield_map_data <- yield_county_all %>%
      filter(State == input$county_map_state, Year == input$county_map_year) %>%
      mutate(
        County = tolower(County),
        County = gsub(" county", "", County),
        County = trimws(County)
      )
    map_data <- left_join(
      all_counties %>% filter(State == input$county_map_state),
      yield_map_data,
      by = c("County", "State")
    ) %>% st_as_sf()
    pal <- colorBin("YlGnBu", domain = map_data$Yield, bins = 5, na.color = "#f0f0f0")
    leaflet(map_data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(Yield),
        color = "black",
        weight = 1,
        fillOpacity = 0.7,
        label = ~paste0(
          "<strong>", toupper(County), "</strong><br>",
          "Yield: ", ifelse(is.na(Yield), "N/A", round(Yield, 1)), " bu/acre"
        ) %>% lapply(htmltools::HTML),
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) %>%
      addLegend("bottomright", pal = pal, values = map_data$Yield, title = "Yield (bu/acre)", opacity = 1)
  })
  
  


  
  
  
  
  
  
  
  
  
  
  
  
  
  output$yield_comparison <- renderPlotly({
    # Read and process data as in Code.R
    library(dplyr)
    library(tidyr)
    library(readr)
    
    week_condition <- read.csv("../VA_weekly_condition.csv", fill = TRUE)
    yield <- read.csv("../VA_yield.csv", fill = TRUE)
    vis <- read.csv("../VA_multi.csv", fill = TRUE)
    
    wc_clean <- week_condition[, colSums(is.na(week_condition)) == 0]
    yield_clean <- yield[, colSums(is.na(yield)) == 0]
    
    filtered_yield <- yield_clean %>%
      filter(Period == "YEAR") %>%
      filter(Year >= 1984 & Year <= 2024)
    
    condition_summary <- wc_clean %>%
      select(Year, Period, Data.Item, Value) %>%
      mutate(
        Condition = toupper(sub(".*IN PCT\\s+", "", Data.Item)),
        Week = as.numeric(gsub("WEEK #", "", Period)),
        Value = as.numeric(Value)
      )
    
    yearly_summary <- condition_summary %>%
      group_by(Year, Condition) %>%
      filter(Week == max(Week, na.rm = TRUE)) %>%
      slice_tail(n = 1) %>%
      ungroup() %>%
      select(Year, Condition, Value) %>%
      pivot_wider(names_from = Condition, values_from = Value)
    
    yearly_summary$`G+E` <- yearly_summary$GOOD + yearly_summary$EXCELLENT
    
    years <- 1984:2025
    filtered_yield$year_order <- filtered_yield$Year - 1983
    model <- lm(Value ~ year_order, data = filtered_yield)
    alpha <- coef(model)[1]
    beta <- coef(model)[2]
    filtered_yield$trend_predicted <- alpha + beta * filtered_yield$year_order
    
    vis_wide <- vis %>%
      pivot_wider(
        id_cols = c(satellite, year),
        names_from = month_name,
        values_from = c(peak_NDVI, peak_GOSAVI, peak_GDVI, peak_EVI, peak_CVI),
        names_glue = "{.value}_{month_name}"
      ) %>%
      mutate(
        mNDVI_678   = rowMeans(across(c(peak_NDVI_Jun, peak_NDVI_Jul, peak_NDVI_Aug)), na.rm = TRUE),
        mGOSAVI_678 = rowMeans(across(c(peak_GOSAVI_Jun, peak_GOSAVI_Jul, peak_GOSAVI_Aug)), na.rm = TRUE),
        mGDVI_678   = rowMeans(across(c(peak_GDVI_Jun, peak_GDVI_Jul, peak_GDVI_Aug)), na.rm = TRUE),
        mEVI_678    = rowMeans(across(c(peak_EVI_Jun, peak_EVI_Jul, peak_EVI_Aug)), na.rm = TRUE),
        mCVI_678    = rowMeans(across(c(peak_CVI_Jun, peak_CVI_Jul, peak_CVI_Aug)), na.rm = TRUE)
      ) %>%
      filter(
        satellite != "Landsat4_TM",
        satellite != "Landsat9_OLI2",
        !(satellite == "Landsat7_ETM" & year != 2012)
      )
    
    merged_data <- filtered_yield %>%
      left_join(vis_wide, by = c("Year" = "year"))
    
    model_EVI <- lm(Value ~ year_order + mEVI_678, data = merged_data)
    merged_data$pred_yield_EVI <- predict(model_EVI, newdata = merged_data)
    
    filtered_subset <- merged_data[merged_data$Year >= 2014 & merged_data$Year <= 2025, ]
    filtered_subset <- merge(filtered_subset, yearly_summary[, c("Year", "G+E")],
                             by = "Year", all.x = TRUE)
    model_GE <- lm(Value ~ `G+E`, data = filtered_subset)
    filtered_subset$pred_yield_GE <- predict(model_GE, newdata = filtered_subset)
    
    plot_data <- filtered_subset[filtered_subset$Year >= 2014 & filtered_subset$Year <= 2024, ]
    plot_data$pred_yield_EVI <- merged_data$pred_yield_EVI[match(plot_data$Year, merged_data$Year)]
    
    fig <- plot_ly(plot_data, x = ~Year)
    fig <- fig %>%
      add_trace(y = ~Value, name = "Actual Yield", type = 'scatter', mode = 'lines+markers',
                line = list(dash = 'solid', width = 2), marker = list(size = 7))
    fig <- fig %>%
      add_trace(y = ~pred_yield_GE, name = "Predicted Yield (G+E)", type = 'scatter', mode = 'lines+markers',
                line = list(dash = 'dot', width = 2), marker = list(size = 7))
    fig <- fig %>%
      add_trace(y = ~pred_yield_EVI, name = "Predicted Yield (EVI)", type = 'scatter', mode = 'lines+markers',
                line = list(dash = 'dot', width = 2), marker = list(size = 7))
    fig <- fig %>%
      layout(title = "Actual vs Predicted Yield (G+E and EVI Models), 2014–2024",
             yaxis = list(title = "Yield"),
             xaxis = list(title = "Year"),
             legend = list(title = list(text = "Legend")))
    fig
  })
  
}