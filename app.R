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
library(plotly)

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
           plotOutput("headline_plot")
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
  
  headlines_yearly1<-reactive({
    headlines_yearly %>% 
      filter(Year%in%c(input$year[1]:input$year[2]))
  })
  #output
    output$headline_plot <- renderPlot({
      ggplot(data = headlines_yearly1(), aes(x = as.factor(Year), y = Headline_Count, fill = as.factor(Year))) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = year_colors) +
      ggtitle("Distribution of Headlines on Syria by Year") +
      ylab("Number of Headlines") +
      xlab("Year") +
      theme_minimal()
      })
}

# Run the application 
shinyApp(ui = ui, server = server)
