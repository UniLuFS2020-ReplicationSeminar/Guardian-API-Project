library(dplyr)
library(lubridate) #working with dates
library(ggplot2)
library(plotly)

### --- Group headlines by month---
# Load vectors containing publication dates and headlines
dates_vector <- readRDS(file = "Data files/syria_dates.rds")
headline_vector <- readRDS(file = "Data files/syria_headlines.rds")
keywords <- readRDS(file="Data files/keywords_by_date_dataframe.rds")

#Convert date in keywords dataframe and dates_vector to the same date format for joining later
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

# Join keywords and number of headline information for plotting later
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
  add_trace(Date=dates_keywords$Date,
            Articles=n,
            hovertemplate = paste('<i>Date</i>: <b>%{Date}</b>','<i>Article</i>: <b>%{Articles}</b>'))

# Find keywords for the top 5 most frequent articles

top_5 <- dates_keywords %>% 
  arrange(desc(n)) %>% 
  head(5)

# 2015-11-01: ISIS attacks Paris and UK parliament debate airstrikes on Syria
# 2013-09-01: Chemical attacks on Ghouta, Syria

#Why are there so many headlines on 2019-10-01?
october_2019 <- dates_keywords %>% 
  filter(Date=="2019-10-01")
# 2019-10-01: Trump thanks Kurds for role in killing ISIS leader Abu Bakr Al-Baghdadi
