###################################

# Chapter 4.1: VIX Futures Daily Volume and Open Interest

#Fabian Welti 
#07.02.2024

##################################

#Packages ####
library(dplyr)
library(ggplot2)
library(extrafont)




#Average Daily Total Volume of VIX Futures by Year ####
# Sum up the values in "Total Volume" for each day
total_volume_vixfutures <- vix_futures_06_24 %>%
  group_by(Date) %>%
  summarise(Total_Volume = sum(`Total Volume`))

# Calculate the average total volume for each calendar years
average_daily_total_volume <- total_volume_vixfutures %>%
  mutate(Year = format(Date, "%Y")) %>%
  group_by(Year) %>%
  summarise(Average_Total_Volume = mean(Total_Volume))

average_daily_total_volume <- average_daily_total_volume[-nrow(average_daily_total_volume), ]

# Plot the Average Daily Total Volume for each calendar year with bars ####
ggplot(average_daily_total_volume, aes(x = Year, y = Average_Total_Volume)) +
  geom_bar(stat = "identity", fill = "black") +
  labs(title = "",
       x = "",  # X-axis label (empty here, add if needed)
       y = "Average Daily Total Volume") +  # Y-axis label
  scale_y_continuous(labels = scales::comma_format()) +
  theme_minimal() +
  theme(axis.text = element_text(family = "Times New Roman"),  # Set font for axis text
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12, family = "Times New Roman"),  # Set font and size for x-axis text
        axis.text.y = element_text(size = 12, family = "Times New Roman"),  # Set font and size for y-axis text
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16, family = "Times New Roman"),  # Set font and size for title
        axis.title = element_text(family = "Times New Roman"),  # Set font for axis titles
        axis.title.x = element_text(margin = margin(t = 10), size = 14, family = "Times New Roman"),  # Add space above the x-axis label and set size
        axis.title.y = element_text(margin = margin(r = 10), size = 12, family = "Times New Roman")) # Add space to the right of the y-axis label and set size





#Average Daily Open Interest of VIX Futures by Year ####
# Sum up the values in "Open Interest" for each day
open_interest_vixfutures <- vix_futures_06_24 %>%
  group_by(Date) %>%
  summarise(Total_Open_Interest = sum(`Open Interest`))

# Calculate the average total open interest for each calendar year
average_daily_open_interest <- open_interest_vixfutures %>%
  mutate(Year = format(Date, "%Y")) %>%
  group_by(Year) %>%
  summarise(Average_Open_Interest = mean(Total_Open_Interest))

# Exclude the last row (2024) if needed
average_daily_open_interest <- average_daily_open_interest[-nrow(average_daily_open_interest), ]

# Plot the Average Open Interest for each calendar year with bars ####
ggplot(average_daily_open_interest, aes(x = Year, y = Average_Open_Interest)) +
  geom_bar(stat = "identity", fill = "black") +
  labs(title = "",
       x = "",  # X-axis label (empty here, add if needed)
       y = "Average Daily Open Interest") +  # Y-axis label
  scale_y_continuous(labels = scales::comma_format()) +
  theme_minimal() +
  theme(axis.text = element_text(family = "Times New Roman"),  # Set font for axis text
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12, family = "Times New Roman"),  # Set font and size for x-axis text
        axis.text.y = element_text(size = 12, family = "Times New Roman"),  # Set font and size for y-axis text
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16, family = "Times New Roman"),  # Set font and size for title
        axis.title = element_text(family = "Times New Roman"),  # Set font for axis titles
        axis.title.x = element_text(margin = margin(t = 10), size = 14, family = "Times New Roman"),  # Add space above the x-axis label and set size
        axis.title.y = element_text(margin = margin(r = 10), size = 12, family = "Times New Roman")) # Add space to the right of the y-axis label and set size



