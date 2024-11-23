library(dplyr)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(scales)
library(gridExtra)

#__________________________________________________________________________________

# import data
GWL_data_1 <- read.csv("Cluster_1_Trend.csv") %>%
  as_tibble()


# prepare data for analysis
GWL_data_1 <- GWL_data_1 %>%
  pivot_longer(cols=2:ncol(GWL_data_1),values_to= "GWL_value", names_to = "Station_ID") %>%
  group_by(Station_ID) %>% 
  mutate(GWL_value=rescale(GWL_value))

#convert Date column from character to Date type
GWL_data_1$Date<- as.Date(GWL_data_1$Date)

# calculate cluster center
cluster_centre_1 <- GWL_data_1 %>%  
  group_by(Date) %>%
  summarise(GWL_value=median(GWL_value)) %>%
  ungroup()


cl1 <- ggplot() +
  
  # Individual time series (grey lines for each station)
  geom_step(data = GWL_data_1, aes(x = Date, y = GWL_value, group = Station_ID), color = "lightgrey", linewidth = 0.5) +

  # Median time series (green line for the cluster median)
  geom_step(data = cluster_centre_1, aes(x = Date, y = GWL_value, color = "Cluster 1 Centre GWL"), linewidth = 1) +
  
  # Customizing labels and colors
  labs(
    title = "Groundwater Levels : Cluster 1,2,3",
    x = "Date", 
    y = "GWL Value", 
    color = NULL, 
    fill = NULL
  ) +
  
  # Custom color scale for the step lines (Median GWL, Individual GWLs)
  scale_color_manual(values = c("Cluster 1 Centre GWL" = "green")) +
  
  # Date format and break
  scale_x_date(date_labels = "%b %y", date_breaks = "9 months") +
  
  # Theme customization
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(),
    legend.position = "top",
    axis.title.x=element_blank()
  )


#__________________________________________________________________________________


# import data
GWL_data_2 <- read.csv("Cluster_2_Trend.csv") %>%
  as_tibble()


# prepare data for analysis
GWL_data_2 <- GWL_data_2 %>%
  pivot_longer(cols=2:ncol(GWL_data_2),values_to= "GWL_value", names_to = "Station_ID") %>%
  group_by(Station_ID) %>% 
  mutate(GWL_value=rescale(GWL_value))

#convert Date column from character to Date type
GWL_data_2$Date<- as.Date(GWL_data_2$Date)

# calculate cluster center
cluster_centre_2 <- GWL_data_2 %>%  
  group_by(Date) %>%
  summarise(GWL_value=median(GWL_value)) %>%
  ungroup()


cl2 <- ggplot() +
  
  # Individual time series (grey lines for each station)
  geom_step(data = GWL_data_2, aes(x = Date, y = GWL_value, group = Station_ID), color = "lightgrey", linewidth = 0.5) +
  
  # Median time series (green line for the cluster median)
  geom_step(data = cluster_centre, aes(x = Date, y = GWL_value, color = "Cluster 2 Centre GWL"), linewidth = 1) +
  
  # Customizing labels and colors
  labs(
    x = "Date", 
    y = "GWL Value", 
    color = NULL, 
    fill = NULL
  ) +
  
  # Custom color scale for the step lines (Median GWL, Individual GWLs)
  scale_color_manual(values = c("Cluster 2 Centre GWL" = "red")) +
  
  # Date format and break
  scale_x_date(date_labels = "%b %y", date_breaks = "9 months") +
  
  # Theme customization
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(),
    legend.position = "top",
    axis.title.x=element_blank()
  )


#__________________________________________________________________________________



# import data
GWL_data_3 <- read.csv("Cluster_3_Trend.csv") %>%
  as_tibble()


# prepare data for analysis
GWL_data_3 <- GWL_data_3 %>%
  pivot_longer(cols=2:ncol(GWL_data_3),values_to= "GWL_value", names_to = "Station_ID") %>%
  group_by(Station_ID) %>% 
  mutate(GWL_value=rescale(GWL_value))

#convert Date column from character to Date type
GWL_data_3$Date<- as.Date(GWL_data_3$Date)

# calculate cluster center
cluster_centre_3 <- GWL_data_3 %>%  
  group_by(Date) %>%
  summarise(GWL_value=median(GWL_value)) %>%
  ungroup()


cl3 <- ggplot() +
  
  # Individual time series (grey lines for each station)
  geom_step(data = GWL_data_3, aes(x = Date, y = GWL_value, group = Station_ID), color = "lightgrey", linewidth = 0.5) +
  
  # Median time series (green line for the cluster median)
  geom_step(data = cluster_centre, aes(x = Date, y = GWL_value, color = "Cluster 3 Centre GWL"), linewidth = 1) +
  
  # Customizing labels and colors
  labs(
    x = "Date", 
    y = "GWL Value", 
    color = NULL, 
    fill = NULL
  ) +
  
  # Custom color scale for the step lines (Median GWL, Individual GWL)
  scale_color_manual(values = c("Cluster 3 Centre GWL" = "blue")) +
  
  # Date format and break
  scale_x_date(date_labels = "%b %y", date_breaks = "9 months") +
  
  # Theme customization
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(),
    legend.position = "top"
  )



# Display all plots in a grid
grid.arrange(cl1, cl2, cl3, ncol = 1)



###____________________________________________________________________________________________________________________________________________________



ggplot() +
  
  # Mean time series
  geom_step(data = cluster_centre_1, aes(x = Date, y = GWL_value, color = "Cluster 1"), 
            linewidth = 1) +
  
  # Mean time series
  geom_step(data = cluster_centre_2, aes(x = Date, y = GWL_value, color = "Cluster 2"),
            linewidth = 1) +
  
  # Mean time series
  geom_step(data = cluster_centre_3, aes(x = Date, y = GWL_value, color = "Cluster 3"), 
            linewidth = 1) +
  labs(title = "Groundwater Levels: Cluster's Centre",
       x = "Date", y = "GWL Value", color= NULL ) +
  scale_color_manual(values = c("Cluster 1" = "green","Cluster 2" = "red", "Cluster 3" = "blue" )) +
  
  scale_x_date(date_labels = "%b %y", date_breaks = "12 months") +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(), 
    axis.line = element_line()) 






