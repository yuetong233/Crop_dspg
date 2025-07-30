# Virginia Corn Crop Dashboard

An interactive web dashboard for monitoring and analyzing corn crop conditions, yield trends, and forecasting across Virginia, Maryland, and North Carolina. Developed through the Data Science for the Public Good (DSPG) program at Virginia Tech.

## 🌾 Overview

This dashboard provides comprehensive agricultural analytics by integrating multiple data sources:
- **USDA NASS API** for crop conditions and yield data
- **Google Earth Engine** for satellite vegetation indices
- **MODIS satellite data** for temperature monitoring
- **Real-time forecasting** using multi-model approaches

## 📊 Dashboard Features

### 1. **County Analysis**
- Interactive maps showing acres planted and harvested by county
- Covers Virginia, Maryland, and North Carolina (2000-2025)
- Harvest success rate calculations
- County-level choropleth visualizations

### 2. **Yield Analysis**
- Historical corn yield trends (2015-2023)
- Multi-state comparison (VA, NC, MD)
- County-level yield choropleth maps
- Interactive state and year selection

### 3. **Yield Forecast**
- **Multi-model forecasting system** combining:
  - Long-term trend analysis (1984-2025)
  - EVI-based forecasting using satellite vegetation health data
  - Crop condition-based forecasting using Good+Excellent ratings
- **Three visualization components**:
  - Crop condition distribution (real-time monitoring)
  - Weekly yield forecasts (short-term predictions)
  - Multi-model comparison plots (forecast accuracy validation)

### 4. **Planting Progress**
- Real-time planting progress tracking
- Weekly updates during growing season
- State-level progress monitoring

### 5. **Remote Sensing Data**
- **NDVI (Normalized Difference Vegetation Index)** data
- **Temperature monitoring** (day/night/average)
- County-level environmental data
- Top 10 counties analysis

## 🏗️ Project Structure

```
Crop_dspg/
├── Dashbaord/                    # Main dashboard application
│   ├── app.R                     # Main application entry point
│   ├── ui.R                      # User interface components
│   ├── server.R                  # Server-side logic and data processing
│   ├── functions.R               # Helper functions and utilities
│   ├── data/                     # Data directory
│   │   ├── county_analysis_nass_data_2000_2025.csv
│   │   └── nass_static_data_2000_2024.csv
│   ├── NDVI_SRbands_Weekly_*.csv # NDVI data files (2021-2025)
│   ├── Filtered_MODIS_Temp_*.csv # Temperature data files (2021-2025)
│   ├── Top10_Temp*.csv          # Top 10 counties temperature data
│   ├── Top10Counties_NDVI.csv   # Top 10 counties NDVI data
│   ├── county_data_cache.csv    # Cached county data
│   ├── yield_data_cache.csv     # Cached yield data
│   └── rsconnect/               # Deployment configuration
│
├── Rscripts/                     # Additional R scripts and analysis
│   ├── dashboard.R              # Alternative dashboard implementation
│   ├── acres_harvested.R        # Harvest analysis scripts
│   ├── acres_planted.R          # Planting analysis scripts
│   ├── crop_conditions.R        # Crop condition analysis
│   ├── yield_deviation_from_trend.R
│   └── various analysis files...
│
├── work_with_api/               # API integration scripts
│   ├── api_cornyieldcomparison.R
│   ├── combinedcode.R
│   ├── corn_historical_analysis.R
│   └── cornyieldanalysishiny.R
│
├── VA_*.csv                     # Virginia-specific data files
├── 2025_VI.csv                  # 2025 vegetation indices
├── yield_forecast_advanced.csv  # Advanced forecasting data
└── README.md                    # This file
```

## 🚀 Installation & Setup

### Prerequisites
- R (version 4.0 or higher)
- RStudio (recommended)
- Required R packages (see below)

### Required R Packages
```r
# Core packages
library(shiny)
library(shinyWidgets)
library(bslib)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(rnassqs)
library(zoo)
library(leaflet)
library(sf)
library(tigris)

# Additional packages for data processing
library(tidyr)
library(lubridate)
```

### Setup Instructions

1. **Clone the repository**
   ```bash
   git clone https://github.com/yuetong233/Crop_dspg.git
   cd Crop_dspg
   ```

2. **Install required packages**
   ```r
   install.packages(c("shiny", "shinyWidgets", "bslib", "readr", "dplyr", 
                      "ggplot2", "plotly", "rnassqs", "zoo", "leaflet", 
                      "sf", "tigris", "tidyr", "lubridate"))
   ```

3. **Set up API keys**
   - USDA NASS API key (already configured in the code)
   - Google Earth Engine access (for satellite data)

4. **Run the dashboard**
   ```r
   setwd("Dashbaord")
   shiny::runApp()
   ```

## 📈 Data Sources

### Primary Data Sources
- **USDA NASS API**: Crop conditions, yield data, planting progress
- **Google Earth Engine**: Satellite vegetation indices (EVI, NDVI)
- **MODIS (NASA)**: Temperature data
- **Landsat Satellite**: Remote sensing imagery

### Data Update Frequency
- **Crop Conditions**: Weekly during growing season
- **Yield Data**: Annually
- **Planting Progress**: Weekly during season
- **Satellite Data**: Weekly (manual export)
- **Temperature Data**: Weekly (manual export)

## 🔧 Key Features

### Real-time Data Integration
- Live API connections to USDA NASS
- Automatic data updates during growing season
- Cached data for improved performance

### Advanced Forecasting
- Multi-model approach combining:
  - Historical trend analysis
  - Satellite vegetation indices
  - Crop condition regression
- Weekly forecast updates
- Model accuracy validation

### Interactive Visualizations
- **Plotly charts** for interactive time series
- **Leaflet maps** for geographic data
- **Choropleth maps** for county-level analysis
- **Multi-panel layouts** for comprehensive analysis

## 🎯 Use Cases

### For Farmers
- Monitor current crop conditions
- Track planting and harvest progress
- Access yield forecasts for planning
- Compare performance across regions

### For Extension Agents
- Provide data-driven recommendations
- Monitor regional trends
- Support decision-making processes

### For Policymakers
- Understand agricultural trends
- Monitor crop health across regions
- Access forecasting for policy planning

## 🛠️ Technical Details

### API Integration
- **USDA NASS API**: Real-time crop data
- **Google Earth Engine**: Satellite imagery processing
- **Data filtering**: Automatic forecast data removal
- **Error handling**: Graceful API failure management

### Data Processing
- **Real-time filtering**: Removes forecast data from yield trends
- **Multi-source integration**: Combines API and static data
- **Geographic processing**: County and state-level aggregation
- **Time series analysis**: Trend detection and forecasting

### Performance Optimization
- **Data caching**: Reduces API calls
- **Efficient queries**: Optimized database requests
- **Responsive design**: Mobile-friendly interface

## 👥 Team

### Undergraduate Interns
- Maryam Rehmatulla
- Shlok Kulkarni

### Graduate Fellow
- Yuetong Zhang

### Faculty Advisors
- Dr. Michael Cary
- Dr. Mario Ortez Amador
- Dr. Le Wang

## 🤝 Collaborators

- **Virginia Corn Board**: Project support and stakeholder engagement
- **Kohl Centre for Agricultural Economic Development**: Technical guidance
- **Data Science for the Public Good (DSPG)**: Program support

## 📝 License

This project is developed through the DSPG program at Virginia Tech. Please contact the team for usage permissions.

## 🔗 Links

- **GitHub Repository**: [https://github.com/yuetong233/Crop_dspg](https://github.com/yuetong233/Crop_dspg)
- **DSPG Program**: [https://dspg.aaec.vt.edu/](https://dspg.aaec.vt.edu/)
- **Virginia Corn Board**: [https://www.vacorn.org/](https://www.vacorn.org/)

## 📞 Contact

For questions or support, please contact the DSPG team at Virginia Tech.

---

*Last updated: 2024*
