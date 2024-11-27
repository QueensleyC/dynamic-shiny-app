#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
# install.packages("leaflet", repos = "https://cloud.r-project.org/")

# Install libraries
library("leaflet")
library(shiny)
library(dplyr)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("US Bikeshare"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          
          # Drop down input for city
          selectInput
          ("city", 
            "Select City:",
            c("Chicago" = "chicago.csv",
              "Washington" = "washington.csv",
              "New York City" = "new_york_city.csv")
          ),
          
          # Drop down input for month
          selectInput
          ("month", 
            "Select Month:",
            c("January", "Feburary", "March", "April", "May", "June")
          ),
          
          # Drop down input for day of week (dow)
          selectInput
          ("day", 
            "Select Day:",
            c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
          ),
          
          # Slider for number of data columns to display
          sliderInput("num_rows",
                        "Rows of Data:",
                        min = 5,
                        max = 50,
                        value = 5,
                        step = 5)
        ),

        # Show a plot of the generated distribution
        mainPanel(
         
          # Create tabs in the main panel
          tabsetPanel(
            
            tabPanel("Table", tableOutput("data_table")),
            
            # Tab 1: Summary Statistic
            tabPanel(
              "Summary",
              
              # Frequency
              h3("Most Frequent Times of Travel"),
              verbatimTextOutput("freq_month"),
              verbatimTextOutput("freq_dow"),
              verbatimTextOutput("freq_hour"),
              
              # Popularity
              h3("Most Popular Station and Trips"),
              verbatimTextOutput("most_frequent_start_station"),
              verbatimTextOutput("most_frequent_end_station"),
              verbatimTextOutput("most_plied_route"),
              
              # Total and Average Trip Duration
              h3("Total and Average Trip Duration"),
              verbatimTextOutput("total_trip_duration"),
              verbatimTextOutput("average_trip_duration"),
            
              
              # Most Common Birth Year
              h3("Birth Year"),
              verbatimTextOutput("most_frequent_birth_year"),
              verbatimTextOutput("earliest_birth_year"),
              verbatimTextOutput("latest_birth_year"),
              
              # User Type Count
              h3("User Type Count"),
              verbatimTextOutput("user_type_counts"),
              
              # Gender Count
              h3("Gender Count"),
              verbatimTextOutput("gender_count"),
            ),
            
            # Tab 2: Bar charts
            tabPanel(
              "Bar Charts",
              plotOutput("gender_bar_chart"),
              plotOutput("user_type_barplot"),
              plotOutput("duration_bar_chart"),
              plotOutput("top_start_stations"),
              plotOutput("top_end_stations"),
              plotOutput("most_common_routes"),
              verbatimTextOutput("debug")  # Debugging Output (for checking data)
              
            ),
            
            tabPanel("Histogram", plotOutput("hist_birth_year")),
           
            
            tabPanel("Map", leafletOutput("map", height = "600px")),
          )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Reactive expression to read the selected file
  read_file <- reactive({
    req(input$city)  # Ensure input is not null
    print(input$city)
    
    # Construct the path to the file based on selected value
    file_path <- paste0(input$city)
    
    # Read the CSV file with error handling
    tryCatch(
      {
        print(file_path)
        read.csv(file_path, header = TRUE)
      },
      error = function(e) {
        # Handle file reading errors
        data.frame(Error = "File could not be read. Please check the file path or format.")
      }
    )
  })
  
  
  output$freq_month <- renderText({
    "Most Common Month:"
  })
  
  output$freq_dow <- renderText({
    "Most Common Day of the Week:"
  })
  
  output$freq_hour <- renderText({
    "Most Common Hour:"
  })
  
  # Render most frequent start station
  output$most_frequent_start_station <- renderText({
    data <- read_file()  # Replace with your actual data reading function
    
    # Get the most frequent start station
    most_frequent_start <- names(which.max(table(data$Start_Station)))
    
    # Create formatted output
    paste("Most Frequent Start Station:", most_frequent_start)
  })
  
  # Render most frequent end station
  output$most_frequent_end_station <- renderText({
    data <- read_file()  # Replace with your actual data reading function
    
    # Get the most frequent end station
    most_frequent_end <- names(which.max(table(data$End_Station)))
    
    # Create formatted output
    paste("Most Frequent End Station:", most_frequent_end)
  })
  
  # Render most plied route
  output$most_plied_route <- renderText({
    data <- read_file()  # Replace with your actual data reading function
    
    # Create a combined route column
    data <- data %>%
      mutate(Route = paste(Start_Station, "->", End_Station))
    
    # Get the most frequent route
    most_frequent_route <- names(which.max(table(data$Route)))
    
    # Create formatted output
    paste("Most Plied Route:", most_frequent_route)
  })
  
  
  # Render total trip duration
  output$total_trip_duration <- renderText({
    data <- read_file()  # Replace with your actual data reading function
    
    # Calculate total trip duration
    total_duration <- sum(data$Trip_Duration, na.rm = TRUE)
    
    # Create formatted output
    paste("Total Trip Duration:", total_duration, "seconds")
  })
  
  # Render average trip duration
  output$average_trip_duration <- renderText({
    data <- read_file()  # Replace with your actual data reading function
    
    # Calculate average trip duration
    average_duration <- mean(data$Trip_Duration, na.rm = TRUE)
    
    # Create formatted output
    paste("Average Trip Duration:", round(average_duration, 2), "seconds")
  })
  
  output$user_type_counts <- renderText({
    data <- read_file()  # Replace with your actual data reading function
    
    # Ensure the User_Type column exists
    req("User_Type" %in% colnames(data))
    
    # Count the number of subscribers and customers
    subscriber_count <- sum(data$User_Type == "Subscriber", na.rm = TRUE)  # Count subscribers
    customer_count <- sum(data$User_Type == "Customer", na.rm = TRUE)      # Count customers
    
    # Create a formatted output
    paste("Subscribers:", subscriber_count, "\nCustomers:", customer_count)
  })
  
  output$gender_count <- renderText({
    data <- read_file()  # Replace with your actual data reading function
    
    # Ensure the Gender column exists
    req("Gender" %in% colnames(data))
    
    # Count the number of females and males
    female_count <- sum(data$Gender == "Female", na.rm = TRUE)  # Count females
    male_count <- sum(data$Gender == "Male", na.rm = TRUE)      # Count males
    
    # Create a formatted output
    paste("Female:", female_count, "\nMale:", male_count)
  })
  
  # Render most frequent birth year
  output$most_frequent_birth_year <- renderText({
    data <- read_file()  # Replace with your actual data reading function
    
    # Get the most frequent birth year
    most_frequent_year <- names(which.max(table(data$Birth_Year)))
    
    # Create formatted output
    paste("Most Frequent Birth Year:", most_frequent_year)
  })
  
  # Render earliest birth year
  output$earliest_birth_year <- renderText({
    data <- read_file()  # Replace with your actual data reading function
    
    # Get the earliest birth year
    earliest_year <- min(data$Birth_Year, na.rm = TRUE)
    
    # Create formatted output
    paste("Earliest Birth Year:", earliest_year)
  })
  
  # Render latest birth year
  output$latest_birth_year <- renderText({
    data <- read_file()  # Replace with your actual data reading function
    
    # Get the latest birth year
    latest_year <- max(data$Birth_Year, na.rm = TRUE)
    
    # Create formatted output
    paste("Latest Birth Year:", latest_year)
  })
  
  output$bike_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%  # Default OpenStreetMap tiles
      setView(lng = -87.6298, lat = 41.8781, zoom = 10)  # Centered on Chicago
  })
  
  output$map <- renderLeaflet({
    # Create a leaflet map
    map <- leaflet() %>%
      addTiles()  # Add default OpenStreetMap tiles
    
    # Obtain data from render function
    data <- read_file()
    
    # Add routes to the map
    for (i in 1:nrow(data)) {
      map <- map %>%
        addPolylines(
          lng = c(data$longitude_Start_Station[i], data$longitude_End_Station[i]),
          lat = c(data$latitude_Start_Station[i], data$latitude_End_Station[i]),
          color = "blue",  # Color of the routes
          weight = 2,      # Thickness of the lines
          opacity = 0.5,    # Opacity of the lines
          dashArray = "5, 5"       # Pattern for dotted lines
        )
    }
    
    map  # Return the map with all routes added
  })
  
  
  
  output$data_table =  renderTable({
    req(input$num_rows)  # Ensure slider input is not null
    head(read_file(), n = input$num_rows)  # Use the slider value for the number of rows
  })
  
  # Render the gender distribution bar chart
  output$gender_bar_chart <- renderPlot({
    data <- read_file()  # Read the selected file
    
    # Ensure the "Gender" column exists
    req("Gender" %in% colnames(data))
    
    # Filter out rows where "Gender" is missing (NA)
    filtered_data <- data[!is.na(data$Gender), ]
    
    # Create a bar plot of the gender distribution
    ggplot(filtered_data, aes(x = Gender)) +
      geom_bar(fill = "skyblue", color = "black") +
      theme_minimal() +
      labs(title = "Gender Distribution", x = "Gender", y = "Count")
  })
  
  # Render the User Type distribution bar chart without missing values
  output$user_type_barplot <- renderPlot({
    data <- read_file()
    req("User_Type" %in% colnames(data))  # Ensure column exists
    
    # Filter out missing values
    filtered_data <- data[!is.na(data$"User_Type"), ]
    
    # Create bar chart for User Type
    ggplot(filtered_data, aes(x = User_Type)) +
      geom_bar(fill = "lightcoral", color = "black") +
      theme_minimal() +
      labs(title = "User Type Distribution", x = "User Type", y = "Count")
  })
  
  # Render the Birth Year histogram without missing values
  output$hist_birth_year <- renderPlot({
    data <- read_file()
    req("Birth_Year" %in% colnames(data))  # Ensure column exists
    
    # Filter out missing values in Birth_Year
    filtered_data <- data[!is.na(data$Birth_Year), ]
    
    # Create histogram for Birth_Year
    ggplot(filtered_data, aes(x = Birth_Year)) +
      geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
      theme_minimal() +
      labs(title = "Birth Year Distribution", x = "Birth Year", y = "Frequency")
  })
  
  # Render the top 10 start stations
  output$top_start_stations <- renderPlot({
    data <- read_file()
    req("Start_Station" %in% colnames(data))  # Ensure column exists
    
    # Count occurrences of each Start Station and get top 10
    top_start <- data %>%
      group_by(Start_Station) %>%
      summarise(Count = n()) %>%
      arrange(desc(Count)) %>%
      top_n(10)
    
    # Create bar chart for top start stations
    ggplot(top_start, aes(x = reorder(Start_Station, -Count), y = Count)) +
      geom_bar(stat = "identity", fill = "lightgreen") +
      theme_minimal() +
      labs(title = "Top 10 Start Stations", x = "Start Station", y = "Count") +
      coord_flip()  # Flip for better readability
  })
  
  # Render the top 10 end stations
  output$top_end_stations <- renderPlot({
    data <- read_file()
    req("End_Station" %in% colnames(data))  # Ensure column exists
    
    # Count occurrences of each End Station and get top 10
    top_end <- data %>%
      group_by(End_Station) %>%
      summarise(Count = n()) %>%
      arrange(desc(Count)) %>%
      top_n(10)
    
    # Create bar chart for top end stations
    ggplot(top_end, aes(x = reorder(End_Station, -Count), y = Count)) +
      geom_bar(stat = "identity", fill = "lightblue") +
      theme_minimal() +
      labs(title = "Top 10 End Stations", x = "End Station", y = "Count") +
      coord_flip()  # Flip for better readability
  })
  
  # Render the top 10 most plied routes
  output$most_common_routes <- renderPlot({
    data <- read_file()
    req("Start_Station" %in% colnames(data), "End_Station" %in% colnames(data))  # Ensure columns exist
    
    # Create a combined column for the route
    data <- data %>%
      mutate(Route = paste(Start_Station, "->", End_Station))  # Merge Start and End stations
    
    # Count occurrences of each route and get the top 10
    most_common_routes <- data %>%
      group_by(Route) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      arrange(desc(Count)) %>%
      slice_max(order_by = Count, n = 10)  # Get the top 10 most plied routes
    
    # Create bar chart for most common routes
    ggplot(most_common_routes, aes(x = reorder(Route, -Count), y = Count)) +
      geom_bar(stat = "identity", fill = "orange") +
      theme_minimal() +
      labs(title = "Top 10 Most Common Routes", x = "Route (Start -> End)", y = "Count") +
      coord_flip() +  # Flip for better readability
      theme(axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5))
  })
  
  # Debugging output to display the column names
  output$debug <- renderPrint({
    data <- read_file()
    colnames(data)  # Print column names to ensure "User Type" exists
  })
   
}

# Run the application 
shinyApp(ui = ui, server = server)
