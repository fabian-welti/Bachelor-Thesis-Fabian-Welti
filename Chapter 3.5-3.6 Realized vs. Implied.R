###################################

# Chapter 3.5: Realized vs. Implied

#Fabian Welti 
#21.01.2024

##################################

#Packages ####
library(xts)
library(ggplot2)
library(zoo)
library(dplyr)
library(TTR)
library(broom)



#Step 1: Calculate realized monthly volatility for SPX ####
  # convert to xts
spx_xts <- xts(spx_data$Returns, order.by = as.Date(spx_data$Date))
  # Calculate the standard deviation of daily returns for each month - essentially capturing realized volatility for each month
monthly_realized_std_dev_spx <- apply.monthly(spx_xts, function(x) sd(x, na.rm = TRUE))
  # Annualize the standard deviation
annualized_std_dev_spx <- monthly_realized_std_dev_spx * sqrt(252) * 100  # Annualize using sqrt of 252 trading days
  # Create a new xts object with values only on the last day of each month
monthly_annualized_std_dev <- na.omit(merge(annualized_std_dev_spx, spx_xts, join = "right"))
monthly_annualized_std_dev[is.na(monthly_annualized_std_dev[,2]), 1]

print(monthly_annualized_std_dev)




#Step 2: Amend th VIX data so it can be compared to the realized volatility  ####
  # Convert the vix_data dataframe to an xts time-series object
vix_xts <- xts(vix_data[, -1], order.by = as.Date(vix_data$Date))
  # Extract the end-of-month closing prices for the VIX
eom_vix_prices <- apply.monthly(vix_xts$Close, last)
  # Shift the 'Close' values down by one row as the VIX index is forward looking and to compare it with the realized volatility which is backward looking
shifted_values <- coredata(eom_vix_prices)
shifted_values <- c(NA, shifted_values[-length(shifted_values)])  # Add NA at the start, remove the last value
# Update the original eom_vix_prices with the shifted values
eom_vix_prices_shifted <- xts(shifted_values, order.by = index(eom_vix_prices))
colnames(eom_vix_prices_shifted) <- "Close"
# Print the end-of-month VIX closing prices
print(eom_vix_prices)
print(eom_vix_prices_shifted)





#Step 3: Amend data so we can compare the smoothed difference between realized and implied volatility####
  # Convert eom_vix_prices to a data frame
df_vix_prices_shifted <- data.frame(Date = index(eom_vix_prices_shifted), VIX_Close = eom_vix_prices_shifted$Close)
  # Convert monthly_annualized_std_dev to a data frame
df_spx_std_dev <- data.frame(Date = index(monthly_annualized_std_dev), SPX_Annualized_Std_Dev = monthly_annualized_std_dev$annualized_std_dev_spx)
  # Merge the shifted VIX data with the SPX data
merged_df_clean <- merge(df_vix_prices_shifted, df_spx_std_dev, by = "Date", all = TRUE)
  # Convert Date to a Date 
merged_df_clean$Date <- as.Date(merged_df_clean$Date)
  # Convert rest of the columns to numeric 
merged_df_clean$Close <- as.numeric(merged_df_clean$Close)
merged_df_clean$annualized_std_dev_spx <- as.numeric(merged_df_clean$annualized_std_dev_spx)
  # Calculate the smoothed difference between VIX and Realized std. dev. 
window_size <- 5 # 5-day rolling mean
merged_df_clean$Difference <- merged_df_clean$Close - merged_df_clean$annualized_std_dev_spx
  # Fill NA values by carrying forward the last observation
merged_df_clean$Difference <- na.locf(merged_df_clean$Difference, na.rm = FALSE)
merged_df_clean$SmoothedDifference <- rollmean(merged_df_clean$Difference, window_size, align = "right", na.pad = TRUE)



#Step 4: Generate the Plot ####
# Set font to Times New Roman for the entire plot
par(family="Times New Roman")

# Initial plot with x-axis turned off (to manually add later)
plot(merged_df_clean$Date, merged_df_clean$annualized_std_dev_spx, type = "l", col = "red", 
     xlab = "", ylab = "Volatility (%)", 
     ylim = c(-20, max(merged_df_clean$annualized_std_dev_spx, na.rm = TRUE)), 
     main = "S&P 500 Standard Deviation and VIX Level", xaxt = "n")

# Adding additional lines
lines(merged_df_clean$Date, merged_df_clean$Close, col = "darkgreen", lwd = 1.2)
lines(merged_df_clean$Date, merged_df_clean$SmoothedDifference, col = "blue", lwd = 1.2)
abline(h = 0, col = "black", lwd = 1)

# Adding legend
legend("topleft", legend = c("SPX Realized Volatility", "VIX Level", "Smoothed Difference"), 
       col = c("red", "darkgreen", "blue"), lty = 1, bty = "n", cex = 0.8)

# Manually adding the x-axis with custom tick marks and labels
axis_dates <- as.Date(c("1990-01-01", "2000-01-01", "2010-01-01", "2020-01-01", "2024-01-01"))
axis(1, at = axis_dates, labels = format(axis_dates, "%Y"))



#Step 5: Calculate the correlation ####
correlation_vix_realized <- cor(merged_df_clean$Close, merged_df_clean$annualized_std_dev_spx, use = "complete.obs")

print(correlation_vix_realized)


#Step 6: Calculate the average Levels ####
mean_eom_vix_prices_shifted <- mean(merged_df_clean$Close, na.rm = TRUE)
print(mean_eom_vix_prices_shifted)

mean_ralized_monthly_sd <- mean(merged_df_clean$annualized_std_dev_spx, na.rm = TRUE)
print(mean_ralized_monthly_sd)




#VIX vs. Realized Volatility (1) ####
# Convert data frames to xts objects for time-series manipulation 
spx_xts_2 <- xts(spx_data$Returns, order.by = as.Date(spx_data$Date))
vix_xts_2 <- xts(vix_data$Close, order.by = as.Date(vix_data$Date))

# Calculate the realized volatility (Rt) as the rolling sd of the past 21 trading days
realized_vol_spx <- runSD(spx_xts_2, n = 21, sample = FALSE) * sqrt(252) * 100

# Manually shift the VIX series forward by 20 days so the same time frame is compared with the realized vola which is looking at the past 21 days
  # Create a vector of NAs to prepend
na_vector <- rep(NA, 20)
  # Create the shifted VIX series
vix_shifted_data <- c(na_vector, coredata(vix_xts_2)[1:(length(vix_xts_2) - 20)])
  # Create a new xts object for the shifted VIX series
vix_shifted <- xts(vix_shifted_data, order.by = index(vix_xts_2))

# Merge shifted VIX with realized vol
combined_vix_realized <- merge(vix_shifted, realized_vol_spx, join = "inner")

# Divide into Quartiles and Calculate Averages
combined_df_vix_realized <- data.frame(Date = index(combined_vix_realized), VIX = coredata(combined_vix_realized[, 1]), RealizedVol = coredata(combined_vix_realized[, 2]))
combined_df_vix_realized$VixQuartile <- ntile(combined_df_vix_realized$vix_shifted, 4)

quartile_averages_vix_realized <- combined_df_vix_realized %>%
  group_by(VixQuartile) %>%
  summarize(AvgVIX = mean(vix_shifted, na.rm = TRUE),
            AvgRealizedVol = mean(realized_vol_spx, na.rm = TRUE))

# Calculate the difference for each quantile 
quartile_averages_vix_realized <- quartile_averages_vix_realized %>%
  mutate(Difference = AvgVIX - AvgRealizedVol)


# Calculate the min and max for the VIX within each quartile and create the Interquartile Range
quartile_ranges_vix <- combined_df_vix_realized %>%
  group_by(VixQuartile) %>%
  summarize(
    InterquartileRange = paste(min(vix_shifted, na.rm = TRUE), max(vix_shifted, na.rm = TRUE), sep = " - ")
  )

# Merge the Interquartile Range data with the quartile averages data
quartile_averages_vix_realized <- merge(quartile_averages_vix_realized, quartile_ranges_vix, by = "VixQuartile")

# Clean dataset
quartile_averages_vix_realized <- quartile_averages_vix_realized[-nrow(quartile_averages_vix_realized), ]
quartile_averages_vix_realized <- data.frame(lapply(quartile_averages_vix_realized, function(x) {
  if(is.numeric(x)) round(x, 2) else x
}))


# The resulting dataframe will only have the Interquartile Range for the VIX values in each quartile
# Display the updated dataframe
print(quartile_averages_vix_realized)






#Vix vs. realized Volatility (2) ####
# Calculate the forward Volatility (Ft) as the rolling sd of the next 21 trading days
# Since we want to calculate forward volatility starting from today, we need to create a loop to calculate the standard deviation from t+1 to t+21 for each day
forward_vol_data <- rep(NA, length(spx_xts_2))

# Calculate rolling standard deviation for each day
for (i in 1:(length(spx_xts_2) - 21)) {
  forward_vol_data[i] <- sd(coredata(spx_xts_2)[i:(i+20)], na.rm = TRUE)
}

# Convert to xts object
forward_vol_spx <- xts(forward_vol_data, order.by = index(spx_xts_2))

# Annualized by multiplying by the square root of the number of trading days in a year
forward_vol_spx <- forward_vol_spx * sqrt(252) * 100

# Merge VIX with forward vol
combined_vix_forward <- merge(vix_xts_2, forward_vol_spx, join = "inner")

# Convert to data frame for further manipulation
combined_df_vix_forward <- data.frame(Date = index(combined_vix_forward), VIX = coredata(combined_vix_forward[, 1]), ForwardVol = coredata(combined_vix_forward[, 2]))

# Divide into Quartiles based on the VIX values
combined_df_vix_forward$VixQuartile <- ntile(combined_df_vix_forward$vix_xts_2, 4)

# Calculate averages for each quartile
quartile_averages_vix_forward <- combined_df_vix_forward %>%
  group_by(VixQuartile) %>%
  summarize(AvgVIX = mean(vix_xts_2, na.rm = TRUE),
            AvgForwardVol = mean(forward_vol_spx, na.rm = TRUE))

# Calculate the overall average difference
overall_avg_difference <- mean(quartile_averages_vix_forward$AvgVIX - quartile_averages_vix_forward$AvgForwardVol, na.rm = TRUE)

# Calculate the difference for each quartile and the deviation from the overall average
quartile_averages_vix_forward <- quartile_averages_vix_forward %>%
  mutate(Difference = AvgVIX - AvgForwardVol,
         DeviationFromAvg = Difference - overall_avg_difference)

# Display the updated dataframe
print(quartile_averages_vix_forward)

# Add a column for the deviation from the overall average difference
combined_df_vix_forward <- combined_df_vix_forward %>%
  mutate(DeviationFromAvg = (vix_xts_2 - forward_vol_spx) - overall_avg_difference)

# Perform t-test for each quartile to test if the average deviation is significantly different from zero
quartile_t_tests <- combined_df_vix_forward %>%
  group_by(VixQuartile) %>%
  do(tidy(t.test(.$DeviationFromAvg)))

# Add significance stars based on p-values
quartile_t_tests <- quartile_t_tests %>%
  mutate(Significance = case_when(
    p.value < 0.01 ~ "**",
    p.value < 0.05 ~ "*",
    TRUE ~ ""
  ))

# Combine with quartile averages
quartile_averages_vix_forward <- merge(quartile_averages_vix_forward, quartile_t_tests, by = "VixQuartile")

# Select and rename the columns for a cleaner table
final_table_vix_forward <- quartile_averages_vix_forward %>%
  select(VixQuartile, AvgVIX, AvgForwardVol, Difference, DeviationFromAvg, p.value, Significance) %>%
  rename(PValue = p.value)

final_table_vix_forward$PValue <- NULL
final_table_vix_forward$Significance <- NULL

final_table_vix_forward <- data.frame(lapply(final_table_vix_forward, function(x) {
  if(is.numeric(x)) round(x, 2) else x
}))

  
# Append significance levels bakc in ** to the first row, * to the second row, and ** to the third and fourth rows of 'DeviationFromAvg'
final_table_vix_forward$DeviationFromAvg[1] <- paste0(final_table_vix_forward$DeviationFromAvg[1], "**")
final_table_vix_forward$DeviationFromAvg[2] <- paste0(final_table_vix_forward$DeviationFromAvg[2], "*")
final_table_vix_forward$DeviationFromAvg[3] <- paste0(final_table_vix_forward$DeviationFromAvg[3], "**")
final_table_vix_forward$DeviationFromAvg[4] <- paste0(final_table_vix_forward$DeviationFromAvg[4], "**")



# Display the final table with significance levels
print(final_table_vix_forward)

