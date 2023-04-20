library(dplyr)
library(lubridate) #working with dates
library(ggplot2)
library(plotly)

### --- Group headlines by month---

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

#Plot headlines by date
headline_plot <- ggplot(data=headline_month, aes(x=Date, y=n))+
  geom_bar(stat="identity", fill="#445867")+
  ggtitle("Distribution of headlines on Syria")+
  ylab("Number of headlines")+
  scale_x_date(date_breaks = "1 year",date_labels="%Y")+
  theme_minimal()+
  theme(panel.grid.major = element_line(linetype="dashed"),
        panel.grid.minor = element_line(linetype="dashed"),
        axis.text.x=element_text(size=12))

#Export dataframe containing number of headlines per date
saveRDS(headlines_dates, file = "Data files/syrian_headlines_dataframe.rds")

### --- Add interactivity to bar chart plot

headlines_plotly <- ggplotly(headline_plot) %>%  # convert ggplot to interactive plotly chart
  add_trace(Date=Date,
            Articles=n,
            hovertemplate = paste('<i>Date</i>: <b>%{Date}</b>','<i>Article</i>: <b>%{Articles}</b>'))