library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

co2_data <- read.csv("owid-co2-data.csv")
co2_data$year <- as.factor(co2_data$year)

function(input, output) {
  
  output$diff1 <- renderTable({
    co2_1980 <- co2_data[co2_data$year == 1980, c("country", "cumulative_co2")]
    co2_2000 <- co2_data[co2_data$year == 2000, c("country", "cumulative_co2")]
    
    # Merge the data frames by country
    merged_data <- merge(co2_1980, co2_2000, by = "country", suffixes = c("_1980", "_2000"))
    
    # Calculate the difference
    merged_data$difference <- merged_data$cumulative_co2_2000 - merged_data$cumulative_co2_1980
  
    print(merged_data)
  })  
  
  output$diff2 <- renderTable({  
    co2_2001 <- co2_data[co2_data$year == 2001, c("country", "cumulative_co2")]
    co2_2021 <- co2_data[co2_data$year == 2021, c("country", "cumulative_co2")]
    
    # Merge the data frames by country
    merged_data1 <- merge(co2_2001, co2_2021, by = "country", suffixes = c("_2001", "_2021"))
    
    # Calculate the difference
    merged_data1$difference <- merged_data1$cumulative_co2_2021 - merged_data1$cumulative_co2_2001
  
    print(merged_data1)
  
  })
  
  output$plot <- renderPlotly({
    plot_ly(co2_data, x = ~year, y = ~cumulative_co2, color = ~country, type = "scatter", mode = "lines") %>%
      layout(title = "Cumulative CO2 Across Years by Country",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Cumulative CO2"))
  })
}