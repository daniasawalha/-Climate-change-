library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(plotly)
library(ggridges)

# Load data
weather_data <- read_csv("C:/Users/FTS/OneDrive/Desktop/spring2024/visualization/datasets/Jordan_Weather.csv")

# Convert the date column to Date type and extract the year
weather_data$date <- as.Date(weather_data$date, format="%d/%m/%Y")
weather_data$year <- year(weather_data$date)

# Convert sunrise times to POSIXct
weather_data$sunrise <- as.POSIXct(weather_data$sunrise, format="%H:%M")
data <- read.csv("C:/Users/FTS/OneDrive/Desktop/spring2024/visualization/datasets/weather_DV.csv", stringsAsFactors = FALSE)
weather2016 <- read.csv('C:/Users/FTS/OneDrive/Desktop/spring2024/visualization/datasets/weather2016_nodupl1.csv')

# Sort unique weeks in ascending order
sorted_weeks <- sort(unique(data$Week))

# Group data by month and calculate mean temperatures
monthly_summary <- data %>%
  group_by(Month) %>%
  summarize(Mean_Min_Temp = mean(Min.temp),
            Mean_Max_Temp = mean(Max.temp),
            Mean_Avg_Temp = mean(Avg.temp))

# Define the states to visualize
selected_states <- c('California', 'Florida', 'Alaska', 'New York', 'New Mexico', 'Texas', 'West Virginia', 'North Carolina', 'Tennessee', 'Ohio')

# Filter the data to include only the selected states
selected_data <- weather2016 %>%
  filter(Station.State %in% selected_states)

# Convert 'Month' to a factor with abbreviated month names
selected_data$Month <- factor(selected_data$Month, levels = 1:12,
                              labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                         "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# Calculate the average temperature for each state
average_temp <- selected_data %>%
  group_by(Station.State) %>%
  summarise(Average_Temperature = mean(Avg.temp)) %>%
  arrange(Average_Temperature)  # Order the states by average temperature

# Order the selected states by average temperature
selected_states_ordered <- average_temp$Station.State

# Reorder the 'Station.State' factor levels by average temperature
selected_data$Station.State <- factor(selected_data$Station.State, levels = selected_states_ordered)
# Define UI for application
ui <- fluidPage(
  titlePanel("Combined Weather Analysis"),
  tabsetPanel(
    tabPanel("Jordan Weather Analysis", 
             fluidPage(
               titlePanel("Jordan Weather Analysis"),
               # Temperature plot
               fluidRow(
                 column(3,
                        div(
                          class = "plot-container",
                          selectInput("city_temp", "Choose a city for Temperature Plot:", 
                                      choices = c("Amman", "Irbid", "Aqaba")),
                          selectInput("year_temp", "Choose a year for Temperature Plot:", 
                                      choices = as.character(2009:2020)),
                          selectInput("variable_temp", "Choose a variable for Temperature Plot:",
                                      choices = c("Min Temperature" = "mintempC",
                                                  "Max Temperature" = "maxtempC",
                                                  "Average Temperature" = "avgtempC"))
                        )
                 ),
                 column(9,
                        div(
                          plotlyOutput("tempPlot"),
                          div(class = "figure-title", "Figure 1: Temperature Plot"),
                          div(class = "figure-description", textOutput("tempDescription")),
                          class = "space-below"
                        )
                 )
               ),
               # Ridgeline plot
               fluidRow(
                 column(3,
                        div(
                          class = "plot-container",
                          selectInput("city_ridgeline", "Choose a city for Ridgeline Plot:", 
                                      choices = c("Amman", "Irbid", "Aqaba")),
                          selectInput("year_ridgeline", "Choose a year for Ridgeline Plot:", 
                                      choices = as.character(2009:2020))
                        )
                 ),
                 column(9,
                        div(
                          plotOutput("ridgelinePlot"),
                          div(class = "figure-title", "Figure 2: Ridgeline Plot"),
                          div(class = "figure-description", textOutput("ridgelineDescription"))
                        )
                 )
               ),
               # UV histogram plot
               fluidRow(
                 column(3,
                        div(
                          class = "plot-container",
                          selectInput("year_uv", "Choose a year for Sunrise UV Levels:", choices = as.character(2008:2021))
                        )
                 ),
                 column(9,
                        div(
                          plotOutput("histogram"),
                          div(class = "figure-title", "Figure 3: Sunrise UV Levels"),
                          div(class = "figure-description", textOutput("histogramDescription")),
                          class = "space-below"
                        )
                 )
               ),
               # Moon phase plot
               fluidRow(
                 column(3,
                        div(
                          class = "plot-container",
                          selectInput("city_moon", "Choose a city for Moon Phase Distribution Plot:", 
                                      choices = c("Amman", "Irbid", "Aqaba")),
                          selectInput("year_moon", "Choose a year for Moon Phase Distribution Plot:", 
                                      choices = as.character(2009:2020)) # Excluding 2008 and 2021
                        )
                 ),
                 column(9,
                        div(
                          plotlyOutput("moonPhasePlot"),
                          div(class = "figure-title", "Figure 4: Moon Phase Distribution"),
                          div(class = "figure-description", textOutput("moonPhaseDescription")),
                          class = "space-below"
                        )
                 )
               ),
               
               # Layout for the strip chart plot
               fluidRow(
                 column(3,
                        div(
                          class = "plot-container",
                          selectInput("city_strip", "Select City for Strip Chart:", choices = unique(weather_data$loc_id)),
                          selectInput("year_strip", "Select Year for Strip Chart:", choices = as.character(2009:2020))
                        )
                 ),
                 column(9,
                        div(
                          plotlyOutput("stripChart"),
                          div(class = "figure-title", "Figure 5: Mean Daily Temperatures by Month"),
                          div(class = "figure-description", textOutput("stripChartDescription")),
                          class = "space-below"
                        )
                 )
               )
             )
    ),
    tabPanel("Weather Insights",
             fluidPage(
               titlePanel("Weather Insights"),
               # Sidebar for selecting plot type
               sidebarLayout(
                 sidebarPanel(
                   selectInput("plot_type", "Select Plot Type:",
                               choices = c("Bubble Plot" = "bubble",
                                           "Density Plot" = "density",
                                           "Correlarion Plot" = "weather",
                                           "Monthly Temperature Plot" = "monthly_temp",
                                           "Heatmap Plot" = "heatmap",
                                           "Monthly Average Temperature" = "monthly_avg_temp",
                                           "Seasonal Temperature Trend" = "seasonal_temp_trend")),
                   conditionalPanel(
                     condition = "input.plot_type == 'bubble'",
                     selectInput("state_bubble", "Select state for bubble plot:", choices = unique(data$Station.State), multiple = TRUE),
                     selectInput("week_bubble", "Select week for bubble plot:", choices = sorted_weeks)
                   ),
                   conditionalPanel(
                     condition = "input.plot_type == 'density'",
                     selectInput("state_density", "Select state for density plot:", choices = unique(data$Station.State))
                   ),
                   conditionalPanel(
                     condition = "input.plot_type == 'weather'",
                     selectInput("state", "Select State:", choices = unique(data$Station.State)),
                     selectInput("location", "Select Location:", choices = NULL)
                   ),
                   conditionalPanel(
                     condition = "input.plot_type == 'monthly_avg_temp'",
                     selectInput("state_monthly_avg_temp", "Select State for Monthly Average Temperature Plot:", choices = unique(data$Station.State))
                   ),
                   conditionalPanel(
                     condition = "input.plot_type == 'seasonal_temp_trend'",
                     selectInput("state_seasonal_temp_trend", "Select State for Seasonal Temperature Trend Plot:", choices = unique(data$Station.State))
                   )
                 ),
                 mainPanel(
                   plotlyOutput("plot", height = "600px"),
                   textOutput("description")
                 )
               )
             )
    )
  )
)
# Define server logic
server <- function(input, output, session) {
  
  # Define custom color palette
  custom_palette <- c("#9ebcda", "#8c96c6", "#8c6bb1", "#88419d", "#810f7c", "#6a0177", "#4a1486", "#f768a1")
  
  # Reactive expression for temperature plot data
  filtered_data_temp <- reactive({
    weather_data %>%
      filter(loc_id == input$city_temp & format(as.Date(date), "%Y") == input$year_temp)
  })
  
  # Generate the time series plot
  output$tempPlot <- renderPlotly({
    data <- filtered_data_temp()
    data$date <- as.Date(data$date)
    
    p <- ggplot(data, aes(x = date, y = !!sym(input$variable_temp))) +
      geom_smooth(method = "loess", span = 0.2) +
      scale_x_date(date_labels = "%b", date_breaks = "1 month", limits = c(as.Date(paste0(input$year_temp, "-01-01")), as.Date(paste0(input$year_temp, "-12-31")))) +
      scale_y_continuous(limits = c(0, max(data[[input$variable_temp]]) + 5)) + # Set y-axis limits
      labs(title = paste("Temperature in", input$city_temp, "in", input$year_temp),
           x = "Month", y = input$variable_temp) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Update the temperature plot description
  output$tempDescription <- renderText({
    paste("This plot shows the", input$variable_temp, "in", input$city_temp, "for the year", input$year_temp, ".")
  })
  filtered_data_ridgeline <- reactive({
    weather_data %>%
      filter(loc_id == input$city_ridgeline & format(as.Date(date), "%Y") == input$year_ridgeline) %>%
      mutate(month = month(as.Date(date), label = TRUE))
  })
  
  # Generate the ridgeline plot
  output$ridgelinePlot <- renderPlot({
    data <- filtered_data_ridgeline()
    
    # Customized ridgeline plot with pretty pastel purple color
    ggplot(data, aes(x = avgtempC, y = month, fill = ..x..)) +
      geom_density_ridges(fill = "#b19cd9", alpha = 0.7) +  # Pretty pastel purple color
      scale_fill_identity() +  # This ensures the fill color remains constant
      theme_ridges() +
      labs(title = "Temperature Distribution by Month",
           x = "Temperature (°C)",
           y = "Month")
  })
  
  # Update the ridgeline plot description
  output$ridgelineDescription <- renderText({
    paste("This plot shows the temperature distribution by month in", input$city_ridgeline, "for the year", input$year_ridgeline, ".")
  })
  
  # Reactive expression for sunrise UV histogram data
  filtered_data_uv <- reactive({
    weather_data %>% filter(year == input$year_uv)
  })
  
  # Generate the sunrise UV histogram
  output$histogram <- renderPlot({
    data <- filtered_data_uv()
    
    # Create sunrise bins
    data$sunrise_bin <- cut(data$sunrise, breaks="30 min")
    
    # Remove today's date from the sunrise_bin
    data <- data %>% filter(sunrise_bin != as.character(Sys.Date()))
    
    # Extract only the time component from sunrise_bin
    data$sunrise_bin <- format(as.POSIXct(data$sunrise_bin), "%H:%M")
    
    # Select the top 3 cities with the most data points for the selected year
    top_cities <- data %>%
      group_by(loc_id) %>%
      summarise(count = n()) %>%
      top_n(3, count) %>%
      pull(loc_id)
    
    data <- data %>% filter(loc_id %in% top_cities)
    
    # Create a grouped barplot
    p <- ggplot(data, aes(x = sunrise_bin, y = uvIndex, fill = loc_id)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "Sunrise Time", y = "UV Levels", title = NULL,  # No title
           fill = "City") +  # Change legend title to "City"
      scale_fill_manual(values = c("Amman" = "#1f77b4", "Irbid" = "#aec7e8", "Aqaba" = "#7b98d1")) +  # Adjust colors to blues
      theme_minimal() +
      theme(legend.position = "top")  # Move the legend to the top
    
    # Add figure number and description
    p <- p +
      ggtitle("Figure 3") +
      labs(caption = "This plot shows UV levels for the top 3 cities at different sunrise times.")
    
    # Print the plot
    print(p)
  })
  
  # Update the histogram plot description
  output$histogramDescription <- renderText({
    paste("This plot shows UV levels for the top 3 cities at different sunrise times for the year", input$year_uv, ".")
  })
  
  # Reactive expression for moon phase distribution plot data
  filtered_data_moon <- reactive({
    weather_data %>%
      filter(loc_id == input$city_moon & format(as.Date(date), "%Y") == input$year_moon)
  })
  
  # Generate the moon phase distribution plot
  output$moonPhasePlot <- renderPlotly({
    data <- filtered_data_moon()
    
    # Define custom color palette
    custom_palette <- c("#9ebcda", "#8c96c6", "#8c6bb1", "#88419d", "#810f7c", "#6a0177", "#4a1486", "#f768a1")
    
    # Calculate distribution of moon phases
    moon_phase_distribution <- data %>%
      group_by(moon_phase) %>%
      summarise(count = n()) %>%
      arrange(desc(count))
    
    # Plot pie chart with custom color palette
    plot_ly(data = moon_phase_distribution, labels = ~moon_phase, values = ~count, type = "pie",
            marker = list(colors = custom_palette)) %>%
      layout(title = paste("Moon Phase Distribution for", input$city_moon, "in Year", input$year_moon),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             showlegend = TRUE,
             hoverinfo = "label+percent+value")
  })
  
  # Update the moon phase distribution plot description
  output$moonPhaseDescription <- renderText({
    paste("This plot shows the distribution of moon phases in", input$city_moon, "for the year", input$year_moon, ".")
  })
  # Reactive expression for strip chart data
  filtered_data_strip <- reactive({
    weather_data %>%
      filter(loc_id == input$city_strip, year == input$year_strip)
  })
  
  # Generate the strip chart plot
  output$stripChart <- renderPlotly({
    data <- filtered_data_strip()
    data$month <- month(data$date, label = TRUE)
    
    p <- ggplot(data, aes(x = month, y = avgtempC)) +
      geom_violin(fill = "#7b98d1", alpha = 0.5) +  # Add violin plot silhouette
      geom_jitter(width = 0.2, alpha = 0.7, color = "#1f77b4") +  # Add data points
      labs(title = paste("Mean Daily Temperatures for", input$city_strip, "in", input$year_strip),
           x = "Month",
           y = "Temperature (Celsius)") +
      scale_x_discrete(labels = month.abb) +
      ylim(0, 40) +  # Set y-axis limits, adjust if necessary
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Update the strip chart description
  output$stripChartDescription <- renderText({
    paste("This plot shows the mean daily temperatures by month in", input$city_strip, "for the year", input$year_strip, ".")
  })
    
    output$plot <- renderPlotly({
      if (input$plot_type == "bubble") {
        filtered_data <- data %>%
          filter(Station.State %in% input$state_bubble, Week == input$week_bubble)
        
        p <- ggplot(filtered_data, aes(x = Wind.speed, y = Min.temp, size = Avg.temp, color = Year)) +
          geom_point(alpha = 0.7) +
          labs(title = "Wind Speed vs. Min Temperature by State",
               x = "Wind Speed", y = "Min Temperature", color = "Year", size = "Avg Temp") +
          theme_minimal() +
          theme(panel.background = element_rect(fill = "transparent", color = NA),
                plot.background = element_rect(fill = "transparent", color = NA))
        
        ggplotly(p)
      } else if (input$plot_type == "density") {
        filtered_data <- data %>%
          filter(Station.State %in% input$state_density)
        
        p <- ggplot(filtered_data, aes(x = Month, y = Max.temp, fill = Station.State)) +
          geom_density(alpha = 0.5, aes(y = ..count..)) +  
          labs(title = "Distribution of Maximum Temperatures by Month",
               x = "Month", y = "Density", fill = "State") +
          theme_minimal() +
          theme(panel.background = element_rect(fill = "transparent", color = NA),
                plot.background = element_rect(fill = "transparent", color = NA))
        
        ggplotly(p)
      } else if (input$plot_type == "weather") {
        req(input$state, input$location)
        plot_data <- data[data$Station.State == input$state & data$Station.Location == input$location, ]
        
        p <- ggplot(plot_data, aes(x = Wind.speed, y = Avg.temp)) +
          geom_point(color = "blue", size = 3, alpha = 0.7) +  
          geom_smooth(method = "lm", se = FALSE, color = "darkred", linetype = "dashed", size  = 1) +  
          labs(title = paste("Correlation between Wind Speed and Average Temperature for", input$location, "in", input$state),
               x = "Wind Speed", y = "Average Temperature") +
          theme_minimal() +
          theme(plot.title = element_text(size = 16, face = "bold"),
                axis.title = element_text(size = 14),
                axis.text = element_text(size = 12),
                legend.title = element_blank(),
                legend.text = element_text(size = 12),
                panel.background = element_rect(fill = "transparent", color = NA),
                plot.background = element_rect(fill = "transparent", color = NA))
        
        ggplotly(p)
      } else if (input$plot_type == "monthly_temp") {
        p <- ggplot(monthly_summary, aes(x = Month)) +
          geom_line(aes(y = Mean_Min_Temp, color = "Minimum"), size = 2) +
          geom_line(aes(y = Mean_Max_Temp, color = "Maximum"), size = 2) +
          geom_line(aes(y = Mean_Avg_Temp, color = "Average"), size = 2) +
          labs(title = "Temperature Variation by Month",
               x = "Month",
               y = "Temperature") +
          scale_color_manual(name = "Temperature Type",
                             values = c("Minimum" = "red", "Maximum" = "blue", "Average" = "green")) +
          theme_minimal() +
          theme(panel.background = element_rect(fill = "transparent", color = NA),
                plot.background = element_rect(fill = "transparent", color = NA))
        
        ggplotly(p)
      } else if (input$plot_type == "heatmap") {
        p <- ggplot(selected_data, aes(x = Month, y = Station.State, fill = Avg.temp, text = paste("Average Temperature: ", Avg.temp, "°F"))) +
          geom_tile() +
          scale_fill_gradient(low = "yellow", high = "red") +
          labs(title = "Average Temperature by Month and State (Selected States)",
               x = "Month", y = NULL, fill = "Average Temperature (°F)") +  
          theme_minimal() +
          theme(panel.background = element_rect(fill = "transparent", color = NA),
                plot.background = element_rect(fill = "transparent", color = NA))
        
        ggplotly(p)
      } else if (input$plot_type == "monthly_avg_temp") {
        # Filter data based on selected state
        state_data <- filter(data, Station.State == input$state_monthly_avg_temp)
        
        # Aggregate data by month
        monthly_data <- state_data %>%
          group_by(Month) %>%
          summarise(avg_temp = mean(Avg.temp))
        
        # Create time series plot
        plot_ly(data = monthly_data, x = ~Month, y = ~avg_temp, type = 'scatter', mode = 'lines') %>%
          layout(title = paste("Monthly Average Temperature for", input$state_monthly_avg_temp),
                 xaxis = list(title = "Month", tickmode = "array", tickvals = 1:12),
                 yaxis = list(title = "Average Temperature (°F)", range = c(0, max(monthly_data$avg_temp))),
                 hoverinfo = "text",
                 hovertext = ~paste("Month:", Month, "<br>Avg. Temp:", avg_temp))
      } else if (input$plot_type == "seasonal_temp_trend") {
        # Define colors for each season
        # Define colors for each season
        season_colors <- c("Winter" = "#4e79a7",  # Blue for Winter
                           "Spring" = "#f28e2b",  # Orange for Spring
                           "Summer" = "#e15759",  # Red for Summer
                           "Fall" = "#76b7b2")    # Green for Fall
        
        # Filter data based on selected state
        state_data <- filter(data, Station.State == input$state_seasonal_temp_trend)
        
        # Extract season from the Month column
        state_data$Season <- ifelse(state_data$Month %in% c(12, 1, 2), "Winter",
                                    ifelse(state_data$Month %in% c(3, 4, 5), "Spring",
                                           ifelse(state_data$Month %in% c(6, 7, 8), "Summer", "Fall")))
        
        # Aggregate data by season
        seasonal_data <- state_data %>%
          group_by(Season) %>%
          summarise(avg_temp = mean(Avg.temp))
        
        # Create 3D bar plot for seasonal temperature trend with distinct colors for each season
        plot_ly(data = seasonal_data, x = ~Season, y = ~avg_temp, type = 'bar', color = ~Season,
                colors = season_colors) %>%
          layout(title = paste("Seasonal Temperature Trend for", input$state_seasonal_temp_trend),
                 scene = list(xaxis = list(title = "Season"),
                              yaxis = list(title = "Average Temperature (°F)")))  # Set z-axis title to empty string to remove label
      }
      
    })
    
    output$description <- renderText({
      if (input$plot_type == "bubble") {
        "This bubble plot displays the relationship between wind speed and minimum temperature across different states. Each point represents a state's data for the selected week. The size of the points indicates the average temperature, and the color represents the year."
      } else if (input$plot_type == "density") {
        "This density plot shows the distribution of maximum temperatures by month and state. The x-axis represents the month and the y-axis represents the density of maximum temperatures."
      } else if (input$plot_type == "weather") {
        "This plot shows the correlation between wind speed and average temperature for the selected location in the selected state. Each point represents a data point, and the dashed line represents the linear regression model."
      } else if (input$plot_type == "monthly_temp") {
        "This plot shows the mean minimum, maximum, and average temperatures for each month. The lines represent different temperature types over the months."
      } else if (input$plot_type == "heatmap") {
        "This heatmap shows the average temperatures by month for selected states. The colors indicate the average temperature, with yellow representing lower temperatures and red representing higher temperatures."
      } else if (input$plot_type == "monthly_avg_temp") {
        "This plot displays the monthly average temperature for the selected state."
      } else if (input$plot_type == "seasonal_temp_trend") {
        "This plot shows the seasonal temperature trend for the selected state. It displays the average temperature for each season throughout the year."
      }
    })
    
    observeEvent(input$state, {
      updateSelectInput(session, "location", choices = unique(data$Station.Location[data$Station.State == input$state]))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)