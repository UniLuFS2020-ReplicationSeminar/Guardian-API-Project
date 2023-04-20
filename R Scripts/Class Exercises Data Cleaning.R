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



### --- Colorful bar chart---

# Group headlines by year
headlines_yearly <- headlines_dates %>%
  mutate(Year = year(datetime_vector)) %>%
  group_by(Year) %>%
  summarise(Headline_Count = n())

# Set custom colors for each year
year_colors <- c("#0072B2", "#E69F00", "#009E73", "#F0E442", "#CC79A7", "#D55E00", "#56B4E9", "#999999", "#FF9DA7", "#D2F53C", "#D25A5A", "#FDAE61", "#AEC7E8", "#FFC0CB", "#B3DE69", "#FB8072", "#80B1D3")

# Create the bar chart
headline_plot <- ggplot(data = headlines_yearly, aes(x = as.factor(Year), y = Headline_Count, fill = as.factor(Year))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = year_colors) +
  ggtitle("Distribution of Headlines on Syria by Year") +
  ylab("Number of Headlines") +
  xlab("Year") +
  theme_minimal()

# Show the plot
headline_plot

#2
#Count headlines per date
headline_month <- headlines_dates %>% 
  mutate(Year = year(datetime_vector),
         Month = month(datetime_vector, label = TRUE)) %>% 
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

# Plot headlines by month and year
headline_plot2 <- ggplot(data = headline_month, aes(x = Year, y = n, fill = Month)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = month_colors) +
  ggtitle("Distribution of Headlines on Syria by Month and Year") +
  ylab("Number of Headlines") +
  xlab("Year") +
  theme_minimal() +
  theme(legend.position = "bottom")

#Export pdf of headlines by year and month
pdf("Headlines by year and month.pdf") 
headline_plot2
dev.off() 

