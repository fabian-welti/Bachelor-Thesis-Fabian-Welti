###################################

#Chapter 3.1-3.4 - VIX Index Characteristics 

#Fabian Welti 
#13.01.2024

##################################

#WD ####
setwd("~/Documents/Data BA Thesis ")

#Packages ####
library(readxl)
library(dplyr)
library(moments)
library(psy)
library(ggplot2)
library(knitr)
library(kableExtra)
library(lubridate)
library(zoo)
library(vrtest)
library(xts)




#Step 1: load and clean historical S&P 500 and VIX data ####
spx_data <- read_excel("^SPX 1990-12.01.2024.xlsx", skip = 1)
vix_data <- read_excel("VIX_History 1990-2024.xlsx", skip = 1)

colnames(vix_data) <- c("Date", "Open", "High", "Low", "Close", "Returns")

spx_data$Date <- as.Date(spx_data$Date, format = "%Y-%m-%d") #converting the Date column 
vix_data$Date <- as.Date(vix_data$Date, format = "%m/%d/%Y") #converting the Date column 



#Step 2: Calculate the daily Returns for SPX and VIX ####
spx_data$Returns <- log(spx_data$Close/ lag(spx_data$Close))
spx_data <- na.omit(spx_data) # Remove NA values

vix_data$Returns <- log(vix_data$Close/lag(vix_data$Close))
vix_data <- na.omit(vix_data) # Remove NA values 


#Step 3: Compute summary Statistics ####
spx_mean <- mean(spx_data$Returns)
spx_median <- median(spx_data$Returns)
spx_sd <- sd(spx_data$Returns)
spx_skewness <- skewness(spx_data$Returns)
spx_kurtosis <- kurtosis(spx_data$Returns)
spx_min_return <- min(spx_data$Returns)
spx_max_return <- max(spx_data$Returns)

vix_mean <- mean(vix_data$Returns)
vix_median <- median(vix_data$Returns)
vix_sd <- sd(vix_data$Returns)
vix_skewness <- skewness(vix_data$Returns)
vix_kurtosis <- kurtosis(vix_data$Returns)
vix_min_return <- min(vix_data$Returns)
vix_max_return <- max(vix_data$Returns)


spx_summary <- data.frame(
  Index = "SPX",
  Mean = spx_mean,
  Median = spx_median,
  Std.Dev = spx_sd,
  Min = spx_min_return,
  Max = spx_max_return,
  Skewness = spx_skewness,
  Kurtosis = spx_kurtosis
)
 
 
 vix_summary <- data.frame(
   Index = "VIX",
   Mean = vix_mean,
   Median = vix_median,
   Std.Dev = vix_sd,
   Min = vix_min_return,
   Max = vix_max_return,
   Skewness = vix_skewness,
   Kurtosis = vix_kurtosis
 )

 

 
#Step 4: Create summary stats table ####
 # combine the Data frames to make one table 
 combined_stats <- rbind(spx_summary, vix_summary)
 combined_stats[, -1] <- round(combined_stats[, -1], 6)
 combined_stats[, -1] <- sapply(combined_stats[, -1], function(x) format(x, scientific = FALSE))
 print(combined_stats)

 
# formatted Table
 # Use kable to create an HTML table and save it
 kable(combined_stats, "html", escape = FALSE) %>%
   kable_styling(bootstrap_options = c("striped", "hover"), 
                 full_width = F,
                 font_size = 12,
                 html_font = "Times New Roman") %>%
   column_spec(1, bold = TRUE) %>%
   save_kable(file = "spx_vix_stats_table.html")
 
 
#Step 5: Plot a chart S&P 500 Index vs. VIX Index ####
 # Set font to Times New Roman
 par(family="Times New Roman")
 
 # Adjust margins: bottom, left, top, right
 par(mar = c(5, 4, 4, 5) + 0.1)
 
 # Plot the VIX data with suppressed x-axis
 plot(vix_data$Date, vix_data$Close, type = "l", col = "darkgreen",
      xlab = "", ylab = "VIX Level",
      ylim = range(vix_data$Close), xaxt="n")
 
 # Overlay the SPX data
 par(new = TRUE)
 plot(spx_data$Date, spx_data$Close, type = "l", col = "red", lwd = 2,
      xlab = "", ylab = "", axes = FALSE,
      ylim = range(spx_data$Close), xaxt="n")
 
 # Custom x-axis ticks and labels
 axis_dates <- seq(as.Date("1990-01-01"), as.Date("2020-01-01"), by="10 years")
 axis_dates <- c(axis_dates, as.Date("2024-01-01"))  # Adding 2024 explicitly
 axis_labels <- format(axis_dates, "%Y")  # Format as years
 
 # Add the custom x-axis
 axis(1, at = axis_dates, labels = axis_labels)
 
 # Add a secondary y-axis for SPX
 axis(side = 4)
 mtext("SPX Level", side = 4, line = 3)
 
 # Add legend
 legend("topleft", legend = c("VIX", "SPX"), col = c("darkgreen", "red"), lty = 1)
 
 # Add title
 title(main = "VIX Index & S&P 500 Level")
 
 
 
 
#Step 6: Shorter time frame Plot ####
 # Subset data for the zoomed time frame
 spx_data_zoomed <- subset(spx_data, Date >= as.Date("2009-04-01") & Date <= as.Date("2011-01-30"))
 vix_data_zoomed <- subset(vix_data, Date >= as.Date("2009-04-01") & Date <= as.Date("2011-01-30"))
 
 # Adjust margins: bottom, left, top, right
 par(mar = c(5, 4, 4, 5) + 0.1)
 
 # Plot the VIX data with the x-axis suppressed for custom ticks
 plot(vix_data_zoomed$Date, vix_data_zoomed$Close, type = "l", col = "darkgreen",
      xlab = "", ylab = "VIX Level",
      ylim = range(vix_data_zoomed$Close), xaxt = "n")
 
 # Generate a sequence of dates every three months within the zoomed time frame
 three_month_seq <- seq(from = as.Date("2009-04-01"), to = as.Date("2011-01-30"), by = "3 months")
 
 # Add the custom x-axis with three-month intervals
 axis(1, at = three_month_seq, labels = format(three_month_seq, "%Y-%m"))
 
 # Overlay the SPX data
 par(new = TRUE)
 plot(spx_data_zoomed$Date, spx_data_zoomed$Close, type = "l", col = "red", lwd = 2,
      xlab = "", ylab = "", axes = FALSE,
      ylim = range(spx_data_zoomed$Close))
 
 # Add a secondary y-axis for SPX
 axis(side = 4)
 mtext("SPX Level", side = 4, line = 3)
 
 # Add legend
 legend("topleft", legend = c("VIX", "SPX"), col = c("darkgreen", "red"), lty = 1)
 
 # Add title
 title(main = "VIX Index & S&P 500 Level Zoomed")
 
 

#Step 7: Calculate the correlation between the SPX Index and VIX Index Returns #### 
# merge the deta frames by Date
 merged_spx_vix <- merge(spx_data,vix_data, by = "Date", suffixes = c("_spx", "_vix"))
# calculate the correlation 
 correlation_spx_vix <- cor(merged_spx_vix$Returns_spx, merged_spx_vix$Returns_vix, use = "complete.obs")
 print(correlation_spx_vix)
 

 
#Step 8: % of Days of opposite moves ####
# Calculate the % of days of opposite moves
  # Create a logical vector that is TRUE when SPX and VIX move in opposite directions
 opposite_moves <- (merged_spx_vix$Returns_spx > 0 & merged_spx_vix$Returns_vix < 0) | 
   (merged_spx_vix$Returns_spx < 0 & merged_spx_vix$Returns_vix > 0)
 
# Calculate the percentage of days where they move in opposite directions
 percent_opposite <- mean(opposite_moves) * 100

 percent_opposite


 
 
 
#Step 9: Regression for asymmetry based on Whaley (2009) ####
 merged_spx_vix$RSPX_down <- ifelse(merged_spx_vix$Returns_spx < 0, merged_spx_vix$Returns_spx, 0)

# Regression model
 model_spx_vix <- lm(Returns_vix ~ Returns_spx + RSPX_down, data = merged_spx_vix)
 
 summary(model_spx_vix)
 
 coefficients(model_spx_vix)
 
 


#Step 10: Worst performing Days SPX vs VIX index performance on these days ####
# TOP 10
  # identify 10 worst performing days in SPX
 worst_days_spx_10 <- spx_data[order(spx_data$Returns), ][1:10, ]
 
  # find the corresponding VIX returns 
 vix_returns_on_worst_days_10 <- merge(worst_days_spx_10, vix_data, by = "Date", suffixes = c("_spx", "_vix"))

  # view the results
 worst_days_spx_10
 vix_returns_on_worst_days_10

  # create table
    #Select and rename the columns for the table
 table_data <- vix_returns_on_worst_days_10[, c("Date", "Returns_spx", "Returns_vix")]
 names(table_data) <- c("Date", "% Performance SPX", "% Performance VIX")
 
  # Convert returns to percentage format for better readability
 table_data$`% Performance SPX` <- round(table_data$`% Performance SPX` * 100, 2)
 table_data$`% Performance VIX` <- round(table_data$`% Performance VIX` * 100, 2)

  # View the table
 table_data
 
  # save as a CSV file
 write.csv(table_data, "worst_days_performance.csv", row.names = FALSE)
 
 
 
 
# Top 500 worst days
 worst_days_spx_500 <- spx_data[order(spx_data$Returns), ][1:500, ]
 
 # find the corresponding VIX returns 
 vix_returns_on_worst_days_500 <- merge(worst_days_spx_500, vix_data, by = "Date", suffixes = c("_spx", "_vix"))
 days_vix_greater_500 <- abs(vix_returns_on_worst_days_500$Returns_vix) > abs(vix_returns_on_worst_days_500$Returns_spx)
 num_days_vix_greater_500 <- sum(days_vix_greater_500)
 num_days_vix_greater_500

 # Calculate the percentage
 percentage_vix_greater <- (num_days_vix_greater_500 / 500) * 100
 
 # Print the result
 percentage_vix_greater
 
 # Identify the least worst day among the 500 worst
 least_worst_day_500 <- worst_days_spx_500[nrow(worst_days_spx_500), ]
 
 # Calculate the percentile rank
 percentile_rank_500 <- (sum(spx_data$Returns <= least_worst_day_500$Returns) / nrow(spx_data)) * 100
 
 # Print the results
 least_worst_day_500
 percentile_rank_500 #calculates the proportion of days that have returns less than or equal to the return
 
 

 
# Top 1000 worst days
 worst_days_spx_1000 <- spx_data[order(spx_data$Returns), ][1:1000, ]
 
 #find the corresponding VIX returns 
 vix_returns_on_worst_days_1000 <- merge(worst_days_spx_1000, vix_data, by = "Date", suffixes = c("_spx", "_vix"))
 days_vix_greater_1000 <- abs(vix_returns_on_worst_days_1000$Returns_vix) > abs(vix_returns_on_worst_days_1000$Returns_spx)
 num_days_vix_greater_1000 <- sum(days_vix_greater_1000)
 num_days_vix_greater_1000
 
 # Calculate the percentage
 percentage_vix_greater <- (num_days_vix_greater_1000 / 1000) * 100
 
 # Print the result
 percentage_vix_greater
 
 # Identify the least worst day among the 1000 worst
 least_worst_day_1000 <- worst_days_spx_1000[nrow(worst_days_spx_1000), ]
 
 # Calculate the percentile rank
 percentile_rank_1000 <- (sum(spx_data$Returns <= least_worst_day_1000$Returns) / nrow(spx_data)) * 100
 
 # Print the results
 least_worst_day_1000
 percentile_rank_1000 #calculates the proportion of days that have returns less than or equal to the return
 


 
 
 
#Step 11: Scatter Plot - Rolling 30 day % change in SPX vs. VIX ####
 # Assuming that 'Returns' column is in decimal form (e.g., 0.01 for 1%)
 spx_data$Rolling_Returns <- rollapply(spx_data$Returns + 1, 30, prod, fill = NA) - 1
 vix_data$Rolling_Returns <- rollapply(vix_data$Returns + 1, 30, prod, fill = NA) - 1
 
 # Convert to percentages
 spx_data$Rolling_Returns <- spx_data$Rolling_Returns * 100
 vix_data$Rolling_Returns <- vix_data$Rolling_Returns * 100
 
 
 # Assume that 'Date' columns are in both datasets and are of Date type
 combined_data_rolling <- merge(spx_data, vix_data, by = "Date")
 

 # Plot
 ggplot(combined_data_rolling, aes(x = Rolling_Returns.x, y = Rolling_Returns.y)) +
   geom_point(alpha = 0.5) +
   geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red") +
   geom_vline(xintercept = 0, color = "black", linetype = "solid") +
   geom_hline(yintercept = 0, color = "black", linetype = "solid") +
   labs(x = "S&P 500 Rolling 30-day % Change", 
        y = "VIX Rolling 30-day % Change", 
        title = "Rolling 30-Day Percentage Change in S&P 500 vs. VIX") +
   scale_x_continuous(limits = c(-50, 50), breaks = seq(-50, 50, by = 10)) +
   theme_minimal() +
   theme(
     plot.title = element_text(hjust = 0.5, face = "bold", family = "Times New Roman"),  # Center and bold the title
     axis.title.x = element_text(margin = margin(t = 10), family = "Times New Roman"),  # Add space above the x-axis label
     axis.title.y = element_text(margin = margin(r = 10), family = "Times New Roman"),  # Y-axis title in Times New Roman
     axis.text.x = element_text(family = "Times New Roman"),  # X-axis text in Times New Roman
     axis.text.y = element_text(family = "Times New Roman"),  # Y-axis text in Times New Roman
     panel.border = element_rect(colour = "black", fill=NA, size=1)  # Add border around the plot
   )
 
 
 
#Step 12: Variance Ratio Test ####
 # Define lags for 1 week and 1 month in terms of trading days 
 # 1 week = 5 trading days, 1 month = 21 trading days
 kvec <- c(5, 21)
 
 # Perform the Lo and MacKinlay variance ratio test 
 vrt_vix_results <- Lo.Mac(vix_data$Returns, kvec)
 
 print(vrt_vix_results)
 
 p_values <- 2 * (1 - pnorm(abs(vrt_vix_results$Stats)))
 
 # Create a data frame with the results and the p-values
 results <- data.frame(Statistics = vrt_vix_results$Stats, P_Values = p_values)
 
 # Print the results data frame
 print(results)
 
 
 
 
 #confirm by looking at annualized volatility of different time horizons
 vix_sd_daily <- sd(vix_data$Returns)
 vix_sd_daily_annualised <- vix_sd_daily * sqrt(252)
 
 # Convert daily return into weekly and monthly returns
 vix_xts <- xts(vix_data$Returns, order.by = as.Date(vix_data$Date))
 
 # Calculate weekly returns
 weekly_returns_vix <- period.apply(vix_xts, endpoints(vix_xts, "weeks"), sum)
 
 # Calculate monthly returns
 monthly_returns_vix <- period.apply(vix_xts, endpoints(vix_xts, "months"), sum)
 
 vix_sd_weekly_annualises <- sd(weekly_returns_vix) * sqrt(52)
 vix_sd_monthly_annualised <- sd(monthly_returns_vix) * sqrt(12)
 
 vix_sd_daily_annualised
 vix_sd_weekly_annualises
 vix_sd_monthly_annualised
 
 