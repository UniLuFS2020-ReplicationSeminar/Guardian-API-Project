library(dplyr)
library(lubridate) #working with dates
library(ggplot2)
library(plotly)

### --- Group headlines by month---

# Load vectors containing publication dates and headlines
dates_vector <- readRDS(file = "Data files/syria_dates.rds")
headline_vector <- readRDS(file = "Data files/syria_headlines.rds")
keywords <- readRDS(file="Data files/keywords_by_date_dataframe.rds")

keywords <- keywords %>% 
  mutate(Date=lubridate::ym(top_headline_dates)) %>% 
  mutate(Date=floor_date(as_date(Date), unit="month")) %>% 
  select(-top_headline_dates)
  
# Convert character column to datetime
datetime_vector <- lubridate::ymd_hms(dates_vector)

#Convert vectors to dataframe
headlines_dates <- data.frame(datetime_vector,headline_vector)

#Count headlines per date
headline_month <- headlines_dates %>% 
  mutate(Date=floor_date(as_date(datetime_vector), unit="month")) %>% 
  count(Date)

dates_keywords <- left_join(headline_month,keywords)

#Plot headlines by date
headline_plot <- ggplot(data=dates_keywords, aes(x=Date, y=n))+
  geom_bar(stat="identity", fill="#445867")+
  ggtitle("Distribution of headlines on Syria")+
  ylab("Number of headlines")+
  scale_x_date(date_breaks = "1 year",date_labels="%Y")+
  theme_minimal()+
  theme(panel.grid.major = element_line(linetype="dashed"),
        panel.grid.minor = element_line(linetype="dashed"),
        axis.text.x=element_text(size=12))

### --- Add interactivity to bar chart plot
headlines_plotly <- ggplotly(headline_plot) %>%  # convert ggplot to interactive plotly chart
  add_trace(Date=Date,
            Articles=n,
            hovertemplate = paste('<i>Date</i>: <b>%{Date}</b>','<i>Article</i>: <b>%{Articles}</b>'))