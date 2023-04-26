#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(lubridate) #working with dates
library(ggplot2)
# library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Guardian Headlines containing Syria"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId = "year",
                        "Year",
                        min = 2010,
                        max = 2023,
                        value = c(2010,2023),
                        dragRange = TRUE,
                        sep="")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("headline_plot"),
           plotOutput("headline2_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
# data input
  # Load vectors containing publication dates and headlines
  dates_vector <- readRDS(file = "Data files/syria_dates.rds")
  headline_vector <- readRDS(file = "Data files/syria_headlines.rds")
  
  # Convert character column to datetime
  datetime_vector <- lubridate::ymd_hms(dates_vector)
  
  #Convert vectors to dataframe
  headlines_dates <- data.frame(datetime_vector,headline_vector)
  
  #Count headlines per date
  headline_month <- headlines_dates %>% 
    mutate(Date=floor_date(as_date(datetime_vector), unit="month")) %>% 
    count(Date)
  # Group headlines by year
  headlines_yearly <- headlines_dates %>%
    mutate(Year = year(datetime_vector)) %>%
    group_by(Year) %>%
    summarise(Headline_Count = n())
  
  # Set custom colors for each year
  year_colors <- c("#0072B2", "#E69F00", "#009E73", "#F0E442", "#CC79A7", "#D55E00", "#56B4E9", "#999999", "#FF9DA7", "#D2F53C", "#D25A5A", "#FDAE61", "#AEC7E8", "#FFC0CB", "#B3DE69", "#FB8072", "#80B1D3")
  
  #Count headlines per date
  headline_month <- headlines_dates %>% 
    mutate(Year = year(datetime_vector),
           Month = month(datetime_vector)) %>% 
    count(Year, Month, sort = FALSE)
  
  # Define a color palette for each month
  month_palette <- c(
    "#FFC107", "#9C27B0", "#E91E63", "#00BCD4", "#8BC34A", "#FF5722",
    "#FF9800", "#4CAF50", "#2196F3", "#3F51B5", "#795548", "#9E9E9E"
  )
  
  # Create a vector to store the color of each month
  month_colors <- vector(mode = "character", length = 12)
  
  # Assign a color to each month
  for (i in 1:12) {
    month_colors[i] <- month_palette[i]
  }
  
  #set reactive shiny function for the first plot
  headlines_yearly1<-reactive({
    headlines_yearly %>% 
      filter(Year%in%c(input$year[1]:input$year[2]))
  })
  
  #set reactive shiny function for the second plot
  headline_month1<-reactive({
    headline_month %>% 
      filter(Year%in%c(input$year[1]:input$year[2]))
  })
  #set locale to uk
  Sys.setlocale("LC_TIME","en_UK")
  
  #output
    output$headline_plot <- renderPlot({
      ggplot(data = headlines_yearly1(), aes(x = as.factor(Year), y = Headline_Count, fill = as.factor(Year))) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = year_colors,name="Year") +
      ggtitle("Distribution of Headlines on Syria by Year") +
      ylab("Number of Headlines") +
      xlab("Year") +
      theme_minimal()
      })
    output$headline2_plot<-renderPlot({
      ggplot(data = headline_month1(), aes(x = as.factor(Year), y = n, fill = as.factor(Month))) +
        geom_col(position = "stack") +
        scale_fill_manual(values = month_colors,name="Months",labels=c(format(ISOdate(2004,1:12,1),"%B"))) +
        ggtitle("Distribution of Headlines on Syria by Month and Year") +
        ylab("Number of Headlines") +
        xlab("Year") +
        theme_minimal() +
        theme(legend.position = "bottom")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
