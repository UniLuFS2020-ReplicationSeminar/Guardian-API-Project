library(dplyr)
library(lubridate)
library(ggplot2)

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
  geom_bar(stat="identity")+
  ggtitle("Distribution of headlines on Syria")+
  ylab("Number of headlines")+
  theme_minimal()

#Export dataframe
saveRDS(headlines_dates, file = "Data files/syrian_headlines_dataframe.rds")

