###################################

#Chapter 4: VIX Futures Term Structure and other characteristics

#Fabian Welti 
#07.02.2024
#updated 30.03.2024

##################################


#Packages ####
library(knitr)
library(kableExtra)
library(xts)
library(ggplot2)
library(zoo)
library(dplyr)
library(TTR)
library(broom)
library(moments)
library(tibble)
library(tidyr)
library(dplyr)
library(readxl)
options(scipen = 999)




#Create new data set ####
vix_futures_06_24_2 <- vix_futures_06_24
# clean data 
vix_futures_06_24_2 <- vix_futures_06_24_2 %>% # Remove rows where the value in the "Settle" column is 0
  filter(Settle != 0)


#Calculate Days to Maturity for each contract ####
vix_futures_06_24_2 <- vix_futures_06_24_2 %>%
  mutate(DaysToMat = as.numeric(ExpirationDate - Date))
# order by Days to Maturity 
vix_futures_06_24_2 <- vix_futures_06_24_2[order(vix_futures_06_24_2$DaysToMat), ]



#Create synthetic constant Maturity Futures ####
# Define the linear interpolation function
linear_interpolation <- function(df, target_maturity) {
  # Subset the data frame to contracts with maturity greater than the target
  df_above <- subset(df, DaysToMat >= target_maturity)
  # Subset the data frame to contracts with maturity less than the target
  df_below <- subset(df, DaysToMat < target_maturity)
  
  # Find the nearest contracts above and below the target maturity
  contract_above <- df_above[which.min(df_above$DaysToMat), ]
  contract_below <- df_below[which.max(df_below$DaysToMat), ]
  
  # Perform linear interpolation
  tau1 <- contract_below$DaysToMat
  tau2 <- contract_above$DaysToMat
  Ft1 <- contract_below$Settle
  Ft2 <- contract_above$Settle
  
  Ft <- ((tau2 - target_maturity) * Ft1 + (target_maturity - tau1) * Ft2) / (tau2 - tau1)
  
  return(Ft)
}

# Define the target maturities
target_maturities <- c(30, 60, 90, 120, 150, 180)

# Create an empty list to store results
constant_maturity_list <- list()

# Loop over each trading day
for (date in unique(vix_futures_06_24_2$Date)) {
  # Filter contracts traded on the current date
  df_day <- subset(vix_futures_06_24_2, Date == date)
  
  # Sort by days to maturity
  df_day <- df_day[order(df_day$DaysToMat), ]
  
  # Calculate the constant maturity prices for each target maturity
  constant_maturity_prices <- sapply(target_maturities, function(m) {
    linear_interpolation(df_day, m)
  })
  
  # Store the results in the list with the date as the name
  constant_maturity_list[[as.character(date)]] <- constant_maturity_prices
}



# Clean and convert the data
# Calculate the lengths of all elements in constant_maturity_list
elements_lengths <- lengths(constant_maturity_list)

# Find names of the elements with lengths matching the length of target_maturities
correct_length_names <- names(elements_lengths[elements_lengths == length(target_maturities)])

# Subset the original list to include only elements with the correct length
constant_maturity_list_correct <- constant_maturity_list[correct_length_names]

# Convert the corrected list to a data frame
constant_maturity_df <- do.call(rbind, constant_maturity_list_correct)


# Convert the matrix to a data frame
constant_maturity_df <- as.data.frame(constant_maturity_df)
constant_maturity_df <- sapply(constant_maturity_df, function(x) sapply(x, `[`, 1))
constant_maturity_df <- as.data.frame(constant_maturity_df)



# Define maturities corresponding to the V columns
maturities <- target_maturities

# Perform the pivot_longer operation
constant_maturity_long <- pivot_longer(
  constant_maturity_df,
  cols = starts_with("V"),
  names_to = "Maturity",
  values_to = "Price",
  names_prefix = "V",
  names_transform = list(Maturity = function(x) maturities[as.integer(x)])
)


# Convert the 'Price' column to numeric 
constant_maturity_long$Price <- as.numeric(as.character(constant_maturity_long$Price))



# Compute the average
average_term_structure <- constant_maturity_long %>%
  group_by(Maturity) %>%
  summarise(AveragePrice = mean(Price, na.rm = TRUE))


# Add mean spot VIX index level for the same time period 
  # Import the data
setwd("~/Documents/Data BA Thesis ")
vix_data <- read_excel("VIX_History 1990-2024.xlsx", skip = 1)

colnames(vix_data) <- c("Date", "Open", "High", "Low", "Close", "Returns")

vix_data$Date <- as.Date(vix_data$Date, format = "%m/%d/%Y") #converting the Date column 

vix_data$Returns <- log(vix_data$Close/lag(vix_data$Close))
vix_data <- na.omit(vix_data) # Remove NA values 


  # add the mean
vix_data_06_24 <- subset(vix_data, Date >= as.Date("2005-05-23") & Date <= as.Date("2024-02-02"))
vix_closing_mean_06_24 <-mean(vix_data_06_24$Close, na.rm = TRUE)

spot_vix_row <- data.frame(Maturity = "Spot VIX", AveragePrice = vix_closing_mean_06_24)

average_term_structure <- rbind(spot_vix_row, average_term_structure)





#Create the Plot for the Term Structure ####
# Convert Maturity to a factor and specify the levels to ensure the correct order
average_term_structure$Maturity <- factor(
  average_term_structure$Maturity,
  levels = c("Spot VIX", "30", "60", "90", "120", "150", "180")
)

# Create the plot with updated styles and larger axis text sizes
term_structure_plot <- ggplot(average_term_structure, aes(x = Maturity, y = AveragePrice)) +
  geom_line(group = 1) +  # Ensure that the line is continuous
  geom_point() +  # Add points for all maturities
  geom_text(
    aes(label = round(AveragePrice, 2)), 
    nudge_y = 0.5, check_overlap = TRUE
  ) +  # Add labels for the points
  scale_x_discrete(  # Use discrete scale for x-axis
    breaks = levels(average_term_structure$Maturity),
    labels = levels(average_term_structure$Maturity)
  ) +
  labs(title = "VIX Futures Term Structure", x = "Days to Maturity", y = "Value") +
  theme_minimal() +  # Use a minimal theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", family = "Times New Roman"),  # Center and bold the title in Times New Roman
    axis.title.x = element_text(margin = margin(t = 10), family = "Times New Roman"),  # X-axis title in Times New Roman
    axis.title.y = element_text(margin = margin(r = 10), family = "Times New Roman"),  # Y-axis title in Times New Roman
    axis.text.x = element_text(family = "Times New Roman", size = 12),  # Increase X-axis text size
    axis.text.y = element_text(family = "Times New Roman", size = 12),  # Increase Y-axis text size
    panel.border = element_rect(colour = "black", fill=NA, size=1)  # Add border around the plot
  )

# Print the plot
print(term_structure_plot)




#Create Summary Statistics for VIX Level and constant maturity VIX futures Level####
# Summary Statistics for VIX Futures Levels
# Calculate summary statistics for each maturity level (vfl = vix forward level) 
summary_stats_levels_vfl <- constant_maturity_long %>%
  group_by(Maturity) %>%
  summarise(
    N_Obs = n(),
    Mean = mean(Price, na.rm = TRUE),
    Median = median(Price, na.rm = TRUE),
    StdDev = sd(Price, na.rm = TRUE),
    Skewness = skewness(Price, na.rm = TRUE),
    Kurtosis = kurtosis(Price, na.rm = TRUE),
    Min= min(Price, na.rm = TRUE),
    Max = max(Price, na.rm = TRUE)
  )

print(summary_stats_levels_vfl)

# do the same thing for the spot vix level over the same time period (vsl = vix spot level)
summary_stats_level_vsl <- vix_data_06_24 %>%
  summarise(
    N_Obs = n(),
    Mean = mean(Close, na.rm = TRUE),
    Median = median(Close, na.rm = TRUE),
    StdDev = sd(Close, na.rm = TRUE),
    Skewness = skewness(Close, na.rm = TRUE),
    Kurtosis = kurtosis(Close, na.rm = TRUE),
    Min = min(Close, na.rm = TRUE),
    Max = max(Close, na.rm = TRUE)
  )
summary_stats_level_vsl <- mutate(summary_stats_level_vsl, Maturity = "Spot VIX") #add this column to make it easier to merge the data sets 



# combine data sets
combined_summary_stats_futures <- rbind(summary_stats_levels_vfl, summary_stats_level_vsl)
combined_summary_stats_futures <- combined_summary_stats_futures[c("Maturity", names(combined_summary_stats_futures)[!names(combined_summary_stats_futures) %in% "Maturity"])]


# Get the desired format
# make the data long so that everything except Maturity is in one column
long_data_vix_futures <- pivot_longer(
  combined_summary_stats_futures,
  cols = -Maturity,
  names_to = "Statistic",
  values_to = "Value"
)
# pivot it wider to get the desired format
combined_summary_stats_futures <- pivot_wider(
  long_data_vix_futures,
  names_from = Maturity,
  values_from = Value
)

# Delete first row 
combined_summary_stats_futures <- combined_summary_stats_futures[-1, ]


# Rename the column headers
names(combined_summary_stats_futures)[2:7] <- paste0("VX", c("30", "60", "90", "120", "150", "180"))
# Ensure 'Spot VIX' is the second column
spot_vix_col <- combined_summary_stats_futures[["Spot VIX"]]
combined_summary_stats_futures <- combined_summary_stats_futures[-which(names(combined_summary_stats_futures) == "Spot VIX")]
combined_summary_stats_futures <- cbind(combined_summary_stats_futures[1], Spot_VIX = spot_vix_col, combined_summary_stats_futures[-1])
# Round the values to two decimal places
combined_summary_stats_futures[-1] <- round(combined_summary_stats_futures[-1], 2)




#Plot Summary Statistics Table for Levels ####
table_summary_stats_futures <- kable(combined_summary_stats_futures, format = "html", align = 'c', caption = "Levels") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F) %>%
  column_spec(1, bold = TRUE) %>%  # Make the first column bold 
  

print(table_summary_stats_futures)

  
  
  
  
  


#Create Summary Statistics for VIX Returns and constant maturity VIX futures Returns####

# create new dataset to work on 
constant_maturity_df_dates <- constant_maturity_df
# Convert row names to a column in the dataframe
constant_maturity_df_dates <- rownames_to_column(constant_maturity_df_dates, var = "Date")
# Assuming the "Date" column contains Julian dates and the origin is "1970-01-01"
constant_maturity_df_dates$Date <- as.Date(as.numeric(constant_maturity_df_dates$Date), origin = "1970-01-01")
# Order the dataframe by the "Date" column in ascending order
constant_maturity_df_dates <- constant_maturity_df_dates[order(constant_maturity_df_dates$Date), ]
# Keep only rows with dates on or after "2005-12-22" to get rid of missing values which are created through the creation of the synthetic contracts
constant_maturity_df_dates <- subset(constant_maturity_df_dates, Date >= as.Date("2005-12-22") & Date <= as.Date("2024-01-12"))
# Remove rows with any NA values in constant_maturity_df_dates
constant_maturity_df_dates <- na.omit(constant_maturity_df_dates)
# Convert columns V1-V6 to numeric
constant_maturity_df_dates$V1 <- as.numeric(as.character(constant_maturity_df_dates$V1))
constant_maturity_df_dates$V2 <- as.numeric(as.character(constant_maturity_df_dates$V2))
constant_maturity_df_dates$V3 <- as.numeric(as.character(constant_maturity_df_dates$V3))
constant_maturity_df_dates$V4 <- as.numeric(as.character(constant_maturity_df_dates$V4))
constant_maturity_df_dates$V5 <- as.numeric(as.character(constant_maturity_df_dates$V5))
constant_maturity_df_dates$V6 <- as.numeric(as.character(constant_maturity_df_dates$V6))

# Calculate log returns for each numeric column and add as new columns
constant_maturity_df_dates$VX30 <- c(NA, diff(log(constant_maturity_df_dates$V1)))
constant_maturity_df_dates$VX60 <- c(NA, diff(log(constant_maturity_df_dates$V2)))
constant_maturity_df_dates$VX90 <- c(NA, diff(log(constant_maturity_df_dates$V3)))
constant_maturity_df_dates$VX120 <- c(NA, diff(log(constant_maturity_df_dates$V4)))
constant_maturity_df_dates$VX150 <- c(NA, diff(log(constant_maturity_df_dates$V5)))
constant_maturity_df_dates$VX180 <- c(NA, diff(log(constant_maturity_df_dates$V6)))

# Function to calculate all the statistics
calculate_statistics_vfr <- function(x) {
  return(c(
    Mean = mean(x, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    StdDev = sd(x, na.rm = TRUE),
    Skewness = skewness(x, na.rm = TRUE),
    Kurtosis = kurtosis(x, na.rm = TRUE),
    Min = min(x, na.rm = TRUE),
    Max = max(x, na.rm = TRUE)
  ))
}
# Apply the function to each log return column and store the results in a data frame
summary_statistics <- sapply(constant_maturity_df_dates[8:13], calculate_statistics_vfr) # Adjust the column indices accordingly
# Convert the results to a data frame for a better view
summary_stats_returns_vfr <- as.data.frame(summary_statistics) # (vfr = vix forward return)



# Add the VIX data
vix_data_06_24_returns <- vix_data_06_24
vix_data_06_24_returns <- subset(vix_data_06_24_returns, Date >= as.Date("2005-12-22"))

# Calculate summary statistics for the 'Returns' column in 'vix_data_06_24_returns'
calculate_statistics_vsl <- vix_data_06_24_returns %>%
  summarise(
    Mean = mean(Returns, na.rm = TRUE),
    Median = median(Returns, na.rm = TRUE),
    StdDev = sd(Returns, na.rm = TRUE),
    Skewness = skewness(Returns, na.rm = TRUE),
    Kurtosis = kurtosis(Returns, na.rm = TRUE),
    Min = min(Returns, na.rm = TRUE),
    Max = max(Returns, na.rm = TRUE)
  )
# Add a column 'Maturity' with the value 'Spot VIX'
calculate_statistics_vsl <- mutate(calculate_statistics_vsl, Maturity = "Spot VIX")
# Clean the dataset
calculate_statistics_vsl_long <- pivot_longer(
  data = calculate_statistics_vsl, 
  cols = -Maturity, # This selects all columns except for Maturity
  names_to = "Statistic", 
  values_to = "Spot VIX"
)

calculate_statistics_vsl_long <- calculate_statistics_vsl_long %>% 
  select(-Maturity)  # This excludes the Maturity column

calculate_statistics_vsl_long <- calculate_statistics_vsl_long %>%
  column_to_rownames(var = "Statistic")



# Merge the two statistics in one dataset
# If calculate_statistics_vsl_long doesn't have 'Statistic' as a column, create it from rownames
if(!"Statistic" %in% colnames(calculate_statistics_vsl_long)) {
  calculate_statistics_vsl_long <- tibble::rownames_to_column(calculate_statistics_vsl_long, var = "Statistic")
}
# Do the same for summary_stats_returns_vfr
if(!"Statistic" %in% colnames(summary_stats_returns_vfr)) {
  summary_stats_returns_vfr <- tibble::rownames_to_column(summary_stats_returns_vfr, var = "Statistic")
}
# Now merge the datasets
combined_summary_stats_futures_returns <- merge(calculate_statistics_vsl_long, summary_stats_returns_vfr, by = "Statistic", all = TRUE)
# Clean this new dataset 
# Define the desired order for the "Statistic" column
desired_order <- c("Mean", "Median", "StdDev", "Skewness", "Kurtosis", "Min", "Max")
# Reorder the dataset based on the desired order
combined_summary_stats_futures_returns <- combined_summary_stats_futures_returns[match(desired_order, combined_summary_stats_futures_returns$Statistic), ]
# Convert specified rows to percentage format 
combined_summary_stats_futures_returns <- combined_summary_stats_futures_returns %>%
  mutate(across(where(is.numeric), ~ ifelse(Statistic %in% c("Mean", "Median", "StdDev", "Min", "Max"),
                                            . * 100, 
                                            .)))
# Round all values (including the percentages) to four decimal places
combined_summary_stats_futures_returns <- combined_summary_stats_futures_returns %>%
  mutate(across(where(is.numeric), ~ round(., 4)))
# For percentage rows, add the "%" sign after rounding
combined_summary_stats_futures_returns <- combined_summary_stats_futures_returns %>%
  mutate(across(where(is.numeric), ~ ifelse(Statistic %in% c("Mean", "Median", "StdDev", "Min", "Max"),
                                            paste0(., "%"), 
                                            .)))

names(combined_summary_stats_futures_returns)[1] <- ""



print(combined_summary_stats_futures_returns)


























#Correlation VIX futures and VIX index ####
# Create new dataset based on the returns calculated in the last step 
constant_maturity_df_dates_correlation <- constant_maturity_df_dates
  # Only keep returns 
constant_maturity_df_dates_correlation <- constant_maturity_df_dates_correlation %>%
  select(-V1, -V2, -V3, -V4, -V5, -V6)
  # add returns from spot VIX Index 
    # Perform a left join to match the "Returns" based on the "Date" column
constant_maturity_df_dates_correlation <- left_join(constant_maturity_df_dates_correlation, 
                                                    vix_data_06_24_returns[, c("Date", "Returns")], 
                                                    by = "Date")



# Calculate the actual correlation and create a dataset
  # Initialize a vector to store correlation values
correlations <- c()
  # List of VX column names
vx_columns <- c("VX30", "VX60", "VX90", "VX120", "VX150", "VX180")
  # Calculate the correlation for each VX column with Returns
for (vx_col in vx_columns) {
  corr_value <- cor(constant_maturity_df_dates_correlation[[vx_col]], 
                    constant_maturity_df_dates_correlation$Returns, 
                    use = "complete.obs")
  correlations <- c(correlations, corr_value)
}
  # Create a new data frame with the correlation values
correlation_df <- data.frame(VX_Column = vx_columns, Correlation = correlations)
  # Turn the 'VX_Column' into column names and 'Correlation' into values
transposed_correlation_df <- t(correlation_df[-1])
colnames(transposed_correlation_df) <- correlation_df$VX_Column
  # Convert the matrix to a data frame, if needed
transposed_correlation_df <- as.data.frame(transposed_correlation_df)
  # If you need to make the row names into a proper column
transposed_correlation_df <- tibble::rownames_to_column(transposed_correlation_df, var = "Metric")
  # Rename the value in the 'Metric' column
transposed_correlation_df$Metric <- "Corr. with Spot VIX"
  # Round, multiply, and format all other columns
transposed_correlation_df <- transposed_correlation_df %>%
  mutate(across(-Metric, ~ paste0(format(round(.x * 100, 2), nsmall = 2), "%")))
  # delete the first column name
names(transposed_correlation_df)[1] <- ""
# Print the updated dataset
print(transposed_correlation_df)








#Time Series comparison of the VIX and the VIX Futures ####

# create new dataset to work on as well as clean and format it appropriatly
constant_maturity_df_2 <- constant_maturity_df
# Convert row names to a column in the dataframe
constant_maturity_df_2 <- rownames_to_column(constant_maturity_df_2, var = "Date")

# Assuming the "Date" column contains Julian dates and the origin is "1970-01-01"
constant_maturity_df_2$Date <- as.Date(as.numeric(constant_maturity_df_2$Date), origin = "1970-01-01")


# combine with VIX data 
combined_vix_and_future <- merge(vix_data_06_24, constant_maturity_df_2, by= "Date", all = TRUE)
combined_vix_and_future <- combined_vix_and_future %>%
  rename(
    VX30 = V1,
    VX60 = V2,
    VX90 = V3,
    VX120 = V4,
    VX150 = V5,
    VX180 = V6
  )

#Plot the Time Series comparison of the VIX and the VIX Futures ####
# Set font to Times New Roman for the entire plot
par(family="Times New Roman")

# Define the range for y-axis
y_range_combined_vix_and_future <- range(c(combined_vix_and_future$VX30, combined_vix_and_future$VX60, 
                                           combined_vix_and_future$VX90, combined_vix_and_future$VX120, 
                                           combined_vix_and_future$VX150, combined_vix_and_future$VX180, 
                                           combined_vix_and_future$Close), na.rm = TRUE)

# Plot the first line to set up the plot area with suppressed x-axis
plot(combined_vix_and_future$Date, combined_vix_and_future$VX30, type = 'l', 
     col = "blue", xlab = "", ylab = "Value", ylim = y_range_combined_vix_and_future,
     main = "VIX Futures & VIX Index Levels", xaxt = "n")

# Add the rest of the lines for futures
lines(combined_vix_and_future$Date, combined_vix_and_future$VX60, col = "red")
lines(combined_vix_and_future$Date, combined_vix_and_future$VX90, col = "green")
lines(combined_vix_and_future$Date, combined_vix_and_future$VX120, col = "purple")
lines(combined_vix_and_future$Date, combined_vix_and_future$VX150, col = "orange")
lines(combined_vix_and_future$Date, combined_vix_and_future$VX180, col = "brown")

# Add the line for the Spot VIX
lines(combined_vix_and_future$Date, combined_vix_and_future$Close, col = "black")

# Generate a sequence of dates for each year within the data range
start_year <- format(min(combined_vix_and_future$Date, na.rm = TRUE), "%Y")
end_year <- format(max(combined_vix_and_future$Date, na.rm = TRUE), "%Y")
yearly_dates <- seq(as.Date(paste(start_year, "-01-01", sep="")), 
                    as.Date(paste(end_year, "-01-01", sep="")), by="year")

# Add custom x-axis with annual ticks
axis(1, at = yearly_dates, labels = format(yearly_dates, "%Y"))

# Add a legend to the plot
legend("topleft", legend = c("VX30", "VX60", "VX90", "VX120", "VX150", "VX180", "Spot VIX"),
       col = c("blue", "red", "green", "purple", "orange", "brown", "black"), lty = 1, bty = "n", cex = 0.8)




#Zoomed Time Series comparison of the VIX and the VIX Futures ####
# filter the data set to only include rows between 2017-01-01 and 2018-12-31
combined_vix_and_future_zoomed <- combined_vix_and_future[
  combined_vix_and_future$Date >= as.Date("2017-01-01") & 
    combined_vix_and_future$Date <= as.Date("2018-09-29"), ]



#Plot the zoomed Time Series comparison of the VIX and the VIX Futures ####
# Set font to Times New Roman for the entire plot
par(family="Times New Roman")

# Define the range for y-axis
y_range_combined_vix_and_future_zoomed <- range(c(combined_vix_and_future_zoomed$VX30, combined_vix_and_future_zoomed$VX60, 
                                                  combined_vix_and_future_zoomed$VX90, combined_vix_and_future_zoomed$VX120, 
                                                  combined_vix_and_future_zoomed$VX150, combined_vix_and_future_zoomed$VX180, 
                                                  combined_vix_and_future_zoomed$Close), na.rm = TRUE)

# Plot the first line to set up the plot area with suppressed x-axis
plot(combined_vix_and_future_zoomed$Date, combined_vix_and_future_zoomed$VX30, type = 'l', 
     col = "blue", xlab = "", ylab = "Value", ylim = y_range_combined_vix_and_future_zoomed,
     main = "VIX Futures & VIX Index Levels", xaxt = "n")

# Add the rest of the lines for futures
lines(combined_vix_and_future_zoomed$Date, combined_vix_and_future_zoomed$VX60, col = "red")
lines(combined_vix_and_future_zoomed$Date, combined_vix_and_future_zoomed$VX90, col = "green")
lines(combined_vix_and_future_zoomed$Date, combined_vix_and_future_zoomed$VX120, col = "purple")
lines(combined_vix_and_future_zoomed$Date, combined_vix_and_future_zoomed$VX150, col = "orange")
lines(combined_vix_and_future_zoomed$Date, combined_vix_and_future_zoomed$VX180, col = "brown")

# Add the line for the Spot VIX
lines(combined_vix_and_future_zoomed$Date, combined_vix_and_future_zoomed$Close, col = "black")

# Generate a sequence of dates for each month within the data range, extending by two months
start_date <- min(combined_vix_and_future_zoomed$Date, na.rm = TRUE)
end_date <- max(combined_vix_and_future_zoomed$Date, na.rm = TRUE)
monthly_dates <- seq(start_date, end_date, by="month")

# Extend the monthly_dates by adding one more month at the end
extended_monthly_dates <- c(monthly_dates, max(monthly_dates) + as.difftime(30, units = "days"))

# Initialize labels with empty strings and assign labels for every second month, excluding the second-last one
labels <- rep("", length(extended_monthly_dates))
labels[seq(2, length(labels) - 2, 2)] <- format(extended_monthly_dates[seq(2, length(labels) - 2, 2)], "%b %Y")

# Ensure only the last date is labeled
labels[length(labels)] <- format(extended_monthly_dates[length(extended_monthly_dates)], "%b %Y")

# Add custom x-axis with monthly ticks but labels for every second month, excluding the second-last date, and including the last date
axis(1, at = extended_monthly_dates, labels = labels, las=2, cex.axis = 0.8)

# Add a legend to the plot
legend("topleft", legend = c("VX30", "VX60", "VX90", "VX120", "VX150", "VX180", "Spot VIX"),
       col = c("blue", "red", "green", "purple", "orange", "brown", "black"), lty = 1, bty = "n", cex = 0.8)







#Percent of Days VIX Futures are at a Premium to VIX Index ####
# Calculate the percentage where each 'VX' column is greater than the 'Close' column 
# Overall:
percentages_futures_greater <- sapply(combined_vix_and_future[, c("VX30", "VX60", "VX90", "VX120", "VX150", "VX180")], function(vx_column) {
  sum(vx_column > combined_vix_and_future$Close, na.rm = TRUE) / sum(!is.na(vx_column) & !is.na(combined_vix_and_future$Close)) * 100
}, USE.NAMES = TRUE)

# Convert to dataframe 
percentages_futures_greater_df <- as.data.frame(t(percentages_futures_greater))

# Add the 'Year' column with a value that represents the total
percentages_futures_greater_df$Year <- "Total"


# Yearly:  
# Extract the year from the 'Date' column
combined_vix_and_future$Year <- format(combined_vix_and_future$Date, "%Y")

# Initialize a dataframe to hold the percentages for each year (vvf = vix versus futures)
yearly_percentages_vvf <- data.frame()

# Calculate the percentages for each year
for(year in unique(combined_vix_and_future$Year)) {
  # Subset the data for the year
  year_data <- subset(combined_vix_and_future, Year == year)
  
  # Calculate the percentages for the subset
  percentages <- sapply(year_data[, c("VX30", "VX60", "VX90", "VX120", "VX150", "VX180")], function(vx_column) {
    sum(vx_column > year_data$Close, na.rm = TRUE) / sum(!is.na(vx_column) & !is.na(year_data$Close)) * 100
  }, USE.NAMES = TRUE)
  
  # Combine the percentages into a single row data frame with the year as a column
  year_percentages_vvf_df <- data.frame(Year = year, t(percentages))
  
  # Bind the row to the yearly_percentages dataframe
  yearly_percentages_vvf <- rbind(yearly_percentages_vvf, year_percentages_vvf_df)
}

# delete year 2005 due to lack of deta
yearly_percentages_vvf <- yearly_percentages_vvf[-1, ]
# delete year 2024 due to lack of data
yearly_percentages_vvf <- yearly_percentages_vvf[-nrow(yearly_percentages_vvf), ]




# Combine yearly and overall: 
# Bind the total percentages as the last row of yearly_percentages_vvf
yearly_percentages_vvf <- rbind(yearly_percentages_vvf, percentages_futures_greater_df)


# Change col names
names(yearly_percentages_vvf)[names(yearly_percentages_vvf) == "VX30"] <- "VX30>VIX"
names(yearly_percentages_vvf)[names(yearly_percentages_vvf) == "VX60"] <- "VX60>VIX"
names(yearly_percentages_vvf)[names(yearly_percentages_vvf) == "VX90"] <- "VX90>VIX"
names(yearly_percentages_vvf)[names(yearly_percentages_vvf) == "VX120"] <- "VX120>VIX"
names(yearly_percentages_vvf)[names(yearly_percentages_vvf) == "VX150"] <- "VX150>VIX"
names(yearly_percentages_vvf)[names(yearly_percentages_vvf) == "VX180"] <- "VX180>VIX"


# Percentage levels 
yearly_percentages_vvf <- yearly_percentages_vvf %>%
  mutate(across(2:7, ~ paste0(format(round(.x, 2), nsmall = 2), "%")))




#Plot a Table of the percentage where each 'VX' column is greater than the 'Close' column ####
table_yearly_percentages_vvf <- kable(yearly_percentages_vvf, format = "html", 
                                      caption = "Percentage of VIX Futures greater than Spot VIX levels",
                                      row.names = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)

# Print the table to display it
print(table_yearly_percentages_vvf)


















#The VIX Futures Basis by Volatility Regime ####

# Create new column in Data assigning each Date to a Volatility Regime based on the spot VIX Level 
combined_vix_and_future <- combined_vix_and_future %>%
  mutate(Volatility_Regime = case_when(
    Close <= 20 ~ "VIX<=20",
    Close > 20 & Close <= 30 ~ "20<VIX<=30",
    Close > 30 & Close <= 40 ~ "30<VIX<=40",
    Close > 40 & Close <= 50 ~ "40<VIX<=50",
    Close > 50 ~ "VIX>50"
  ))


# Find the average VIX and VIX Futures Levels for the VIX Regimes
# ensure that all the relevant columns are of numeric type
combined_vix_and_future <- combined_vix_and_future %>%
  mutate(across(c(Close, VX30, VX60, VX90, VX120, VX150, VX180), as.numeric))

# Calculate the averages for each volatility regime
average_values_per_vixregime <- combined_vix_and_future %>%
  group_by(Volatility_Regime) %>%
  summarise(
    Average_Close = mean(Close, na.rm = TRUE),
    Average_VX30 = mean(VX30, na.rm = TRUE),
    Average_VX60 = mean(VX60, na.rm = TRUE),
    Average_VX90 = mean(VX90, na.rm = TRUE),
    Average_VX120 = mean(VX120, na.rm = TRUE),
    Average_VX150 = mean(VX150, na.rm = TRUE),
    Average_VX180 = mean(VX180, na.rm = TRUE)
  )

# Calculate the overall averages across all volatility regimes
overall_averages_vixregime <- combined_vix_and_future %>%
  summarise(
    Average_Close = mean(Close, na.rm = TRUE),
    Average_VX30 = mean(VX30, na.rm = TRUE),
    Average_VX60 = mean(VX60, na.rm = TRUE),
    Average_VX90 = mean(VX90, na.rm = TRUE),
    Average_VX120 = mean(VX120, na.rm = TRUE),
    Average_VX150 = mean(VX150, na.rm = TRUE),
    Average_VX180 = mean(VX180, na.rm = TRUE)
  )

# Add a label for the new row
overall_averages_vixregime <- overall_averages_vixregime %>% mutate(Volatility_Regime = "All Observations")

# Bind the overall averages as a new row to the average_values_per_vixregime dataset
average_values_per_vixregime <- bind_rows(average_values_per_vixregime, overall_averages_vixregime)

# arrange the data 
# Remove the row where Volatility_Regime is NA
average_values_per_vixregime <- filter(average_values_per_vixregime, !is.na(Volatility_Regime))
# manually set "VIX<=20" as the first row and then follow with the rest in ascending order, you can do:
average_values_per_vixregime <- average_values_per_vixregime %>%
  arrange(desc(Volatility_Regime == "VIX<=20")) # This will bring "VIX<=20" to the top



# Calculate the Futures Basis
# Calculate the basis for each VIX futures column
average_values_per_vixregime <- average_values_per_vixregime %>%
  mutate(
    Basis_VX30 = Average_VX30 - Average_Close, 
    Basis_VX60 = Average_VX60 - Average_Close,
    Basis_VX90 = Average_VX90 - Average_Close,
    Basis_VX120 = Average_VX120 - Average_Close,
    Basis_VX150 = Average_VX150 - Average_Close,
    Basis_VX180 = Average_VX180 - Average_Close,
  )

# Calculate the Futures Basis as a % of the VIX 
average_values_per_vixregime <- average_values_per_vixregime %>%
  mutate(
    Basis_VX30_Percent = (Average_VX30 - Average_Close) / Average_Close * 100,
    Basis_VX60_Percent = (Average_VX60 - Average_Close) / Average_Close * 100,
    Basis_VX90_Percent = (Average_VX90 - Average_Close) / Average_Close * 100,
    Basis_VX120_Percent = (Average_VX120 - Average_Close) / Average_Close * 100,
    Basis_VX150_Percent = (Average_VX150 - Average_Close) / Average_Close * 100,
    Basis_VX180_Percent = (Average_VX180 - Average_Close) / Average_Close * 100
  )




# Calculate the % times of Contango
# Calculate the contango percentages for each Volatility Regime
contango_percentages_per_vixregime <- combined_vix_and_future %>%
  group_by(Volatility_Regime) %>%
  summarise(
    Percent_VX30_gt_Close = mean(VX30 > Close, na.rm = TRUE) * 100,
    Percent_VX60_gt_Close = mean(VX60 > Close, na.rm = TRUE) * 100,
    Percent_VX90_gt_Close = mean(VX90 > Close, na.rm = TRUE) * 100,
    Percent_VX120_gt_Close = mean(VX120 > Close, na.rm = TRUE) * 100,
    Percent_VX150_gt_Close = mean(VX150 > Close, na.rm = TRUE) * 100,
    Percent_VX180_gt_Close = mean(VX180 > Close, na.rm = TRUE) * 100
  ) %>%
  ungroup() # Ungrouping so that it can be easily bound with the overall percentages

# Calculate the overall percentages across all observations
contango_overall_percentages <- combined_vix_and_future %>%
  summarise(
    Percent_VX30_gt_Close = mean(VX30 > Close, na.rm = TRUE) * 100,
    Percent_VX60_gt_Close = mean(VX60 > Close, na.rm = TRUE) * 100,
    Percent_VX90_gt_Close = mean(VX90 > Close, na.rm = TRUE) * 100,
    Percent_VX120_gt_Close = mean(VX120 > Close, na.rm = TRUE) * 100,
    Percent_VX150_gt_Close = mean(VX150 > Close, na.rm = TRUE) * 100,
    Percent_VX180_gt_Close = mean(VX180 > Close, na.rm = TRUE) * 100
  ) %>%
  mutate(Volatility_Regime = "All Observations")

# Add the contango percentages to the existing average_values_per_vixregime
average_values_per_vixregime <- average_values_per_vixregime %>%
  left_join(contango_percentages_per_vixregime, by = "Volatility_Regime")
# Remove the last column
contango_overall_percentages <- contango_overall_percentages[, -ncol(contango_overall_percentages)]
# Ensure column names match between the two data frames (they should be the same as per your screenshots)
column_names <- c("Percent_VX30_gt_Close", "Percent_VX60_gt_Close", "Percent_VX90_gt_Close", 
                  "Percent_VX120_gt_Close", "Percent_VX150_gt_Close", "Percent_VX180_gt_Close")
# Replace the NA values in row 6 of 'average_values_per_vix_regime' with the values from 'contango_overall_percentages'
average_values_per_vixregime[6, column_names] <- contango_overall_percentages[1, column_names]









#Create Table for VIX Futures Basis by Volatility Regime ####
# Create a new dataset with selected columns
table_average_values_per_vixregime <- average_values_per_vixregime %>%
  select(Volatility_Regime, Average_Close, Basis_VX30, Basis_VX30_Percent, Percent_VX30_gt_Close, Basis_VX60, Basis_VX60_Percent, Percent_VX60_gt_Close, Basis_VX90, Basis_VX90_Percent, Percent_VX90_gt_Close, Basis_VX120, Basis_VX120_Percent, Percent_VX120_gt_Close)
# rename the columns 
table_average_values_per_vixregime <- table_average_values_per_vixregime %>%
  rename(
    `VIX Level` = Average_Close,
    `Basis 30 Day Futures` = Basis_VX30,
    `Basis 30 Day Futures % of VIX` = Basis_VX30_Percent,
    `% in Contango 30 Day` = Percent_VX30_gt_Close,
    `Basis 60 Day Futures` = Basis_VX60,
    `Basis 60 Day Futures % of VIX` = Basis_VX60_Percent,
    `% in Contango 60 Day` = Percent_VX60_gt_Close,
    `Basis 90 Day Futures` = Basis_VX90,
    `Basis 90 Day Futures % of VIX` = Basis_VX90_Percent,
    `% in Contango 90 Day` = Percent_VX90_gt_Close,
    `Basis 120 Day Futures` = Basis_VX120,
    `Basis 120 Day Futures % of VIX` = Basis_VX120_Percent,
    `% in Contango 120 Day` = Percent_VX120_gt_Close,
  )

# Count the number of observations 
# Count the number of observations for each Volatility_Regime
count_per_vixregime <- combined_vix_and_future %>%
  group_by(Volatility_Regime) %>%
  summarise(Count = n())
# Count the total number of observations
total_count_vix_and_futures <- nrow(combined_vix_and_future)
# Add the total count as a new row in the count_per_vixregime dataset
count_per_vixregime <- count_per_vixregime %>%
  ungroup() %>%
  bind_rows(data.frame(Volatility_Regime = "All Observations", Count = total_count_vix_and_futures))
# Remove the 6th row
count_per_vixregime <- count_per_vixregime[-6, ]
# change the order
desired_top_row_count_per_vixregime <- 4
count_per_vixregime <- count_per_vixregime[c(desired_top_row_count_per_vixregime, 1:(desired_top_row_count_per_vixregime-1), (desired_top_row_count_per_vixregime+1):nrow(count_per_vixregime)), ]


# Put combine the datasets so it has the number of observations in the table_average_values_per_vixregime 
table_average_values_per_vixregime$Count <- count_per_vixregime$Count

# rearrange the order 
column_order_vixregime <- c("Volatility_Regime", "Count", setdiff(names(table_average_values_per_vixregime), c("Volatility_Regime", "Count")))
table_average_values_per_vixregime <- table_average_values_per_vixregime[, column_order_vixregime]

# rename Column 
table_average_values_per_vixregime <- table_average_values_per_vixregime %>%
  rename(`N. Obs.` = Count)


# Make the Table:
table_vixfutures_basis <- kable(table_average_values_per_vixregime, format = "html", align = 'c', caption = "Average Values per VIX Regime") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F) %>%
  column_spec(1, bold = TRUE, border_right = TRUE) %>%
  column_spec(2, width = "50px") # Adjust the width as needed
print(table_vixfutures_basis)





#Create separate Table for Basis only ####
  # Create and rename the columns in the new dataset
table_basis_per_vix_regime <- table_average_values_per_vixregime %>%
  select(
    `Volatility Regime` = Volatility_Regime, 
    `N. Obs.` = `N. Obs.`, 
    `VIX Level` = `VIX Level`, 
    `30 Day Futures Basis` = `Basis 30 Day Futures`, 
    `30 Day Futures Basis as % of VIX` = `Basis 30 Day Futures % of VIX`, 
    `60 Day Futures Basis` = `Basis 60 Day Futures`, 
    `60 Day Futures Basis as % of VIX` = `Basis 60 Day Futures % of VIX`, 
    `90 Day Futures Basis` = `Basis 90 Day Futures`, 
    `90 Day Futures Basis as % of VIX` = `Basis 90 Day Futures % of VIX`, 
    `120 Day Futures Basis` = `Basis 120 Day Futures`, 
    `120 Day Futures Basis as % of VIX` = `Basis 120 Day Futures % of VIX`
  )

  # Round all numerical columns to 2 decimal places, excluding non-numeric columns
table_basis_per_vix_regime <- table_basis_per_vix_regime %>%
  mutate(across(where(is.numeric), ~round(.x, 2)))




#Create separate Table for Contango only ####
# Create and rename the columns in the new dataset
table_contango_per_vix_regime <- table_average_values_per_vixregime %>%
  select(
    `Volatility Regime` = Volatility_Regime, 
    `N. Obs.` = `N. Obs.`, 
    `VIX Level` = `VIX Level`, 
    `30 Day Futures` = `% in Contango 30 Day`, 
    `60 Day Futures` = `% in Contango 60 Day`, 
    `90 Day Futures` = `% in Contango 90 Day`, 
    `120 Day Futures` = `% in Contango 120 Day`, 
  )

# Round all numerical columns to 2 decimal places, excluding non-numeric columns
table_contango_per_vix_regime <- table_contango_per_vix_regime %>%
  mutate(across(where(is.numeric), ~round(.x, 2)))


# Add a % sign to all values in specific columns 
table_contango_per_vix_regime <- table_contango_per_vix_regime %>%
  mutate_at(vars(`30 Day Futures`, `60 Day Futures`, `90 Day Futures`, `120 Day Futures`), 
            ~sprintf("%.2f%%", .))





