###################################

#Contango Backwardation Trigger final
#Compare close with close

#Fabian Welti 
#created: 29.02.2024
#amended: 27.04.2024

##################################



#WD ####
setwd("/Users/lucaswelti/Documents/Data BA Thesis /Treasury ")

#Packages ####
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(readxl)
library(tibble)
library(PerformanceAnalytics)
library(readxl)
options(scipen = 999)



#Step 1: Create a new Dataset to work on which includes the VIX Index Data, the VIX futures and the Risk-Free Rate ####
# remove all the additional rows of futures data
contback_data <- subset(vix_futures_06_24, Date >= as.Date("2006-01-03") & Date <= as.Date("2024-01-12"))
# read in the data for three month Treasury Bills 
treasury_data <- read.csv("Treasury.csv") 
# clean the dataset
treasury_data <- subset(treasury_data, DATE >= as.Date("2006-01-03") & DATE <= as.Date("2024-01-12")) 
treasury_data <- treasury_data %>%
  rename(Date = DATE,
         RFR = DTB3)
treasury_data$Date <- as.Date(treasury_data$Date, format = "%Y-%m-%d")
treasury_data <- treasury_data[treasury_data$RFR != ".", ]
contback_data <- subset(contback_data, select = -c(High, Low, `Total Volume`, EFP, Change, `Open Interest`, `Futures expiration month`, `Expiration Year`, `Expiration month and year`))
# Merge the Datasets to one and clean and order
contback_data <- merge(treasury_data, contback_data, by="Date", all.x = TRUE)
contback_data$RFR <- as.numeric(contback_data$RFR)
contback_data <- contback_data[order(contback_data$Date, contback_data$ExpirationDate), ] #order the data
# read in the data for the vix index for the same time period
vix_data_contback_data <- read_excel("VIX_History 1990-2024.xlsx", skip = 1) # Adjust path as necessary
vix_data_contback_data$Date <- as.Date(vix_data_contback_data$Date, format = "%m/%d/%Y")
vix_data_contback_data <- subset(vix_data_contback_data, Date >= as.Date("2006-01-03"))

# combine the datasets for the final dataset
contback_data <- merge(contback_data, vix_data_contback_data, by = "Date", suffixes = c("_futures", "_index"))
# Convert the RFR from percentage to a daily rate by dividing by 360
contback_data$RFR <- contback_data$RFR / 36000 # Dividing by 100 to get rate and by 360 for daily rate





#Step 2: filter out front month contracts ####
# get second month contract on expiration days, otherwise get the front month
contback_data <- contback_data %>%
  group_by(Date) %>%
  arrange(Date, ExpirationDate) %>% # sort by Date and then by ExpirationDate within each group
  mutate(is_expiration = Date == ExpirationDate) %>%
  filter(!is_expiration | row_number() > 1) %>% # Exclude rows that are expiration rows or select the second row if it is an expiration day
  slice(1) %>% # Select the first row, which is now the front month contract unless it's the expiration day
  select(-is_expiration) %>% # Remove the helper column
  ungroup() # Remove the grouping

# skip rows in which Open prices are not there
contback_data <- subset(contback_data, Open_futures != 0)




#Step 3: Create new columns to help identify the expiration dates and the term structure in order to identify when the contracts have to be rolled and closed ####
# Step 3.1: Create a new column 'IsExpirationNextDay' which looks if the we close the current front month contract 
contback_data <- contback_data %>%
  mutate(IsExpirationNextDay = ExpirationDate - Date == 1)


# Step 3.2: find the rebalnce day on which the new position is opened 
contback_data <- contback_data %>%
  mutate(RebalanceDay = if_else(lag(IsExpirationNextDay, default = FALSE) == TRUE, TRUE, FALSE)) 
# manually override the first we can sell the on the first obsevation of the dataset
contback_data$RebalanceDay[1] <- TRUE


# Step 3.3: Term Structure
contback_data <- contback_data %>%
  mutate(TermStructure = if_else(lag(Close_futures) > lag(Close_index), "Contango", "Backwardation"))


# Manually set the TermStructure for the first row to "Contango"
contback_data$TermStructure[1] <- "Contango"





#Step 4: Open and Close Logic and Calculate the daily PnL ####
# Step 4.1: Initialize columns for current position and daily P&L
contback_data <- contback_data %>%
  mutate(
    CurrentPosition = 0,  # 0 for no position, -1 for short, +1 for long
    DailyPnL = 0  # Initialize daily P&L to 0
  )

# Step 4.2: Loop through the dataset to manage positions and calculate P&L
for(i in 1:nrow(contback_data)) {
  # Opening and managing positions
  if(i == 1 || contback_data$RebalanceDay[i]) {
    # On the first day or RebalanceDay, open position based on TermStructure
    contback_data$CurrentPosition[i] <- if_else(contback_data$TermStructure[i] == "Contango", -1, 1)
  } else {
    # From the second row onwards, check for TermStructure change
    if(contback_data$TermStructure[i] != contback_data$TermStructure[i - 1]) {
      # If TermStructure changes, switch position
      contback_data$CurrentPosition[i] <- if_else(contback_data$TermStructure[i] == "Contango", -1, 1)
    } else {
      # If TermStructure remains the same, hold the previous position
      contback_data$CurrentPosition[i] <- contback_data$CurrentPosition[i - 1]
    }
  }
  
  # Calculating daily P&L
  if(i < nrow(contback_data)) {
    # For all days except the last, calculate P&L based on the next day's Open_futures
    contback_data$DailyPnL[i] <- contback_data$CurrentPosition[i] * (contback_data$Open_futures[i + 1] - contback_data$Open_futures[i])
  } else {
    # For the last row, calculate P&L based on the Settle price (edge case handling)
    contback_data$DailyPnL[i] <- contback_data$CurrentPosition[i] * (contback_data$Settle[i] - contback_data$Open_futures[i])
  }
  
  # Handle expiration days
  if(contback_data$IsExpirationNextDay[i]) {
    # On expiration days, calculate P&L based on the Settle price
    contback_data$DailyPnL[i] <- contback_data$CurrentPosition[i] * (contback_data$Settle[i] - contback_data$Open_futures[i])
  }
}



#Step 5: Initialize Values ####
# Initialize the margin account with $100,000
initial_margin_account <- 1000000
# Set the initial vega notional to 1
initial_vega_notional <- 10 # Since we already accounted for short and long positions in the daily PnL calculation can set it to 1 and not -1 



#Step 6: Margin Account Calculation and Vega Notional Rebalancing ####
# Initialize the Trade column as FALSE for all rows
contback_data$Trade <- FALSE

# Step 6.1: Initialize the MarginAccount and VN on the first day
contback_data$MarginAccount[1] <- initial_margin_account
contback_data$VN[1] <- initial_vega_notional

# Step 6.2: Loop through the dataframe starting from the second row
for (i in 2:nrow(contback_data)) {
  
  # Calculate the new MarginAccount value
  Rt_1 <- contback_data$RFR[i - 1]  # Previous day's risk-free rate
  MAt_1 <- contback_data$MarginAccount[i - 1]  # Previous day's margin account
  DailyPnL <- contback_data$DailyPnL[i] * 1000  # Adjust for contract multiplier
  
  # Check if TermStructure has changed from the previous day
  TermStructureChange <- contback_data$TermStructure[i] != contback_data$TermStructure[i - 1]
  
  # Rebalance day logic
  if(contback_data$RebalanceDay[i]) {
    # Calculate VN based on MAt_1, rounding to the nearest positive number
    contback_data$VN[i] <- ifelse(MAt_1 < 100000, 1, floor(MAt_1 / 100000))
    
    # Apply trading costs and bid-ask spread due to rebalancing
    contback_data$MarginAccount[i] <- (1 + Rt_1) * MAt_1 + contback_data$VN[i] * DailyPnL - abs(contback_data$VN[i]) * 2.32 - abs(contback_data$VN[i]) * 0.05 * 1000
    
    # Indicate that a trade was made due to rebalancing
    contback_data$Trade[i] <- TRUE
  } else {
    # Carry over the VN from the previous day and update MarginAccount normally
    contback_data$VN[i] <- contback_data$VN[i - 1]
    contback_data$MarginAccount[i] <- (1 + Rt_1) * MAt_1 + contback_data$VN[i] * DailyPnL
  }
  
  # Apply trading costs and bid-ask spread on TermStructureChange
  if (TermStructureChange) {
    contback_data$MarginAccount[i] <- contback_data$MarginAccount[i] - abs(contback_data$VN[i]) * 2.32 - abs(contback_data$VN[i]) * 0.05 * 1000
    
    # Indicate that a trade was made due to term structure change
    contback_data$Trade[i] <- TRUE
  }
}








#Calculate the margin requirement ####
# Import the data
margin_requirement_3 <- read_excel("margin requirements vix futures contback1.xlsx")

# Prepare the margin requirement data by converting Date, selecting needed columns, and arranging by Date
margin_requirement_3 <- margin_requirement_3 %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%  # Ensure the date format matches your dataset
  select(Date, IM, MM) %>%
  arrange(Date)

# Merge the margin requirement data with contback_data
contback_data <- contback_data %>%
  left_join(margin_requirement_3, by = "Date") %>%
  fill(IM, MM, .direction = "down") %>%
  mutate(IM = replace_na(IM, 17625),  # Assuming 17625 is a default or missing value placeholder for IM
         MM = replace_na(MM, 11100))  # Assuming 11100 is for MM

# Add new columns for IB IM and IB MM with conditional logic based on TermStructure
contback_data <- contback_data %>%
  mutate(
    `IB IM` = case_when(
      TermStructure == "Contango" ~ IM * 4.7,
      TermStructure == "Backwardation" ~ IM * 1.8,
      TRUE ~ IM  # Default case if there are other values or NA
    ),
    `IB MM` = case_when(
      TermStructure == "Contango" ~ MM * 3.7,
      TermStructure == "Backwardation" ~ MM * 1.8,
      TRUE ~ MM  # Default case
    )
  )



# Calcultate the actual Margin Requirements each day 
contback_data <- contback_data %>%
  mutate(margin_requirement_3 = ifelse(Trade,
                                       VN * `IB IM`,
                                       VN * `IB MM`))





#Import VDP as benchmark ####
VPD_data <- read_excel("VPD_DATA.xlsx")

contback_data <- contback_data %>%
  left_join(VPD_data, by = "Date")


# Calculate daily PnL for "Last Price"
contback_data <- contback_data %>%
  mutate(daily_pnl_lastprice = `Last Price` - lag(`Last Price`))
# Set the first value of 'daily_pnl_lastprice' to 0
contback_data$daily_pnl_lastprice[1] <- 0
# Replace NA values in 'daily_pnl_lastprice' with 0
contback_data$daily_pnl_lastprice <- replace_na(contback_data$daily_pnl_lastprice, 0)

# Calculate the initial contract size based on "Last Price"
x_lastprice <- round(1000000 / contback_data$`Last Price`[1], digits = 0)

# Initialize the 'VPD' column with the first value as the initial investment
contback_data$VPD <- rep(0, nrow(contback_data))
contback_data$VPD[1] <- 1000000  # Set the initial value for the first row

# Loop through the dataset starting from the second row to calculate VPD
for (i in 2:nrow(contback_data)) {
  contback_data$VPD[i] <- x_lastprice * contback_data$daily_pnl_lastprice[i] + contback_data$VPD[i - 1]
}




#Import SPX data as a benchmark ####
# Read in the SPX data and prepare it
spx_data <- read_excel("^SPX 1990-12.01.2024.xlsx", skip = 1) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  rename(Close_SPX = Close) %>%
  select(Date, Close_SPX)  # Select only Date and Close_SPX for merging

# Merge only the Close_SPX column
contback_data <- contback_data %>%
  left_join(spx_data, by = "Date")


# Calculate daily PnL for SPX
contback_data <- contback_data %>%
  mutate(daily_pnl_spx = Close_SPX - lag(Close_SPX))
# Set the first value of 'daily_pnl_spx' to 0
contback_data$daily_pnl_spx[1] <- 0
# Replace NA values in 'daily_pnl_spx' with 0
contback_data$daily_pnl_spx <- replace_na(contback_data$daily_pnl_spx, 0)

# Calculate the development of 100k 
# Calculate the initial contract size 'x' using the correct column name "PUT Index"
x <- round(1000000 / contback_data$Close_SPX[1], digits = 0)

# Initialize the 'SPX' column with the first value as the initial investment
contback_data$SPX <- rep(0, nrow(contback_data))
contback_data$SPX[1] <- 1000000  # Set the initial value for the first row

# Loop through the dataset starting from the second row
for (i in 2:nrow(contback_data)) {
  contback_data$SPX[i] <- x * contback_data$daily_pnl_spx[i] + contback_data$SPX[i - 1]
}




#Plot the results ####
# Calculate the upper limit for y-axis
upper_limit <- max(c(max(contback_data$MarginAccount, na.rm = TRUE), 
                     max(contback_data$margin_requirement_3, na.rm = TRUE),
                     max(contback_data$VPD, na.rm = TRUE),
                     max(contback_data$SPX, na.rm = TRUE)))

# Define y-axis range starting from 0 to the upper limit
y_range <- c(0, upper_limit)

# Set plot font family
par(family="Times New Roman")

# Set up the plot with the first line to establish the plot area
plot(contback_data$Date, contback_data$MarginAccount, type = 'l', 
     col = "darkblue", ylim = y_range, xaxt = 'n',    # Disable default x-axis
     xlab = "", ylab = "Development of $1'000'000", 
     main = "Performance ContBack", lwd = 2)  # Increase line width with lwd

# Add the remaining series with lines
lines(contback_data$Date, contback_data$margin_requirement_2, col = "deepskyblue4", lwd = 2)
lines(contback_data$Date, contback_data$VPD, col = "red", lwd = 2)
lines(contback_data$Date, contback_data$SPX, col = "black", lwd = 2)

# Custom x-axis labels for each year
year_ticks <- seq(from=min(contback_data$Date, na.rm=TRUE),
                  to=max(contback_data$Date, na.rm=TRUE), by="years")
axis(1, at=year_ticks, labels=format(year_ticks, "%Y"), cex.axis=0.8, las=2)

# Add light gridlines
abline(h=seq(from=0, to=upper_limit, by=(upper_limit)/5), 
       col="grey", lty="dotted", lwd=0.5)
abline(v=year_ticks, col="grey", lty="dotted", lwd=0.5)

# Add a legend without a box 
legend("topleft", legend = c("ContBack", "Margin Requirement ContBack", "VPD Index", "S&P 500"),
       col = c("darkblue", "deepskyblue4", "red", "black"), lty = 1, bty = "n", cex = 0.8, lwd=c(2,2,2,2))








#Descriptive statistics ContBack ####
# Step 1: Calculate Simple Returns
contback_data <- contback_data %>%
  mutate(Returns = (MarginAccount - lag(MarginAccount)) / lag(MarginAccount))

# Replace the first NA value in Returns with 0 
contback_data$Returns[1] <- 0  


# Step 2: Generate Summary Statistics
summary_stats_contback <- contback_data %>%
  summarise(N.Obs = n(),
            Mean = mean(Returns, na.rm = TRUE),
            Median = median(Returns, na.rm = TRUE),
            S.D. = sd(Returns, na.rm = TRUE),
            Min = min(Returns, na.rm = TRUE),
            Max = max(Returns, na.rm = TRUE))

# Step 3: Calculate Max Drawdown
# Initialize variables to track the peak and maximum drawdown
peak = contback_data$MarginAccount[1]
max_drawdown = 0

# Loop through the MarginAccount to find the maximum drawdown
for (value in contback_data$MarginAccount) {
  # Update the peak if the current value is higher than the current peak
  if (value > peak) {
    peak = value
  }
  
  # Calculate the drawdown from the current peak
  drawdown = (peak - value) / peak
  
  # Update the maximum drawdown if the current drawdown is larger
  if (drawdown > max_drawdown) {
    max_drawdown = drawdown
  }
}

# max_drawdown now holds the maximum drawdown percentage
max_drawdown


# Step 4: Compute Skewness and Kurtosis
skewness <- skewness(contback_data$Returns, na.rm = TRUE)
kurtosis <- kurtosis(contback_data$Returns, na.rm = TRUE)


# Step 5: Calculate Cumulative Return
cumulative_return <- last(contback_data$MarginAccount) / first(contback_data$MarginAccount) - 1


# Step 6: Annualize Returns and Volatility
annualized_return <- (1 + mean(contback_data$Returns, na.rm = TRUE)) ^ 252 - 1  # Assuming 252 trading days in a year
annualized_volatility <- sd(contback_data$Returns, na.rm = TRUE) * sqrt(252)


# Step 7: Calculate the Sharpe Ratio
# Calculate the Daily Excess Returns
contback_data$ExcessReturns <- contback_data$Returns - contback_data$RFR

# Calculate the Mean Daily Excess Retun and Standard Deviation for the Daily Returns 
mean_daily_excess_return_contback <- mean(contback_data$ExcessReturns, na.rm = TRUE)
sd_daily_excess_returns_contback <- sd(contback_data$Returns, na.rm = TRUE)

# Compute the Daily Sharpe Ratio
daily_sharpe_ratio_contback <- mean_daily_excess_return_contback / sd_daily_excess_returns_contback

# Annualize the Sharpe Ratio
annualized_sharpe_ratio_contback <- daily_sharpe_ratio_contback * sqrt(252)

annualized_sharpe_ratio_contback


# Step 8: Calculate the Sortino Ratio
# Calculate the Downside Deviation
squared_negative_excess_returns <- ifelse(contback_data$ExcessReturns < 0, 
                                          contback_data$ExcessReturns^2, 
                                          0)
mean_squared_negative_excess_returns <- mean(squared_negative_excess_returns, na.rm = TRUE)
downside_deviation <- sqrt(mean_squared_negative_excess_returns)

# Calculate the Daily Sortino Ratio
daily_sortino_ratio <- mean_daily_excess_return_contback / downside_deviation

# Annualize the Sortino Ratio
annualized_sortino_ratio <- daily_sortino_ratio * sqrt(252)

annualized_sortino_ratio


#Step 9: Calculate the Calmar Ratio
# Calculate the Annualized Mean of Daily Excess Returns
annualized_mean_daily_excess_return <- mean(contback_data$ExcessReturns, na.rm = TRUE) * 252

# Compute the Calmar Ratio using the Annualized Mean of Daily Excess Returns
calmar_ratio_contback <- annualized_mean_daily_excess_return / max_drawdown

# The result is the Calmar Ratio for your strategy
calmar_ratio_contback







# Combine all the results into a single tibble
results_contback <- tibble(
  Statistics = c("Mean", "Median", "S.D.", "Min", "Max", "Max Drawdown", "Skewness", "Kurtosis", "Cumulative Return", "Annualized Return", "Annualized Volatility", "Annualizes Sharpe Ratio","Annualized Sortino Ratio", "Calmar Ratio"),
  ContBack = c(summary_stats_contback$Mean, summary_stats_contback$Median, summary_stats_contback$S.D., summary_stats_contback$Min, summary_stats_contback$Max, max_drawdown, 
               skewness, kurtosis, cumulative_return, annualized_return, annualized_volatility, annualized_sharpe_ratio_contback, annualized_sortino_ratio, calmar_ratio_contback)
)

# make the dataset visually nicer
# Convert tibble to data frame if it is not already
results_contback <- as.data.frame(results_contback)

# Set the row names and remove the 'Statistics' column
rownames(results_contback) <- results_contback$Statistics
results_contback <-results_contback[-1]

# Round the numbers in the ContBack column to 2 decimal places
results_contback$ContBack <- round(results_contback$ContBack, 4)


# Print the results to check
print(results_contback)













#Descriptive statistics VPD Index####
# Step 1: Calculate Simple Returns_VPD
contback_data <- contback_data %>%
  mutate(Returns_VPD = (VPD - lag(VPD)) / lag(VPD))

# Replace the first NA value in Returns_VPD with 0 
contback_data$Returns_VPD[1] <- 0  


# Step 2: Generate Summary Statistics
summary_stats_vpd <- contback_data %>%
  summarise(N.Obs = n(),
            Mean = mean(Returns_VPD, na.rm = TRUE),
            Median = median(Returns_VPD, na.rm = TRUE),
            S.D. = sd(Returns_VPD, na.rm = TRUE),
            Min = min(Returns_VPD, na.rm = TRUE),
            Max = max(Returns_VPD, na.rm = TRUE))


# Step 3: Calculate Max Drawdown
# Initialize variables to track the peak and maximum drawdown
peak = contback_data$VPD[1]
max_drawdown = 0

# Loop through the VPD to find the maximum drawdown
for (value in contback_data$VPD) {
  # Update the peak if the current value is higher than the current peak
  if (value > peak) {
    peak = value
  }
  
  # Calculate the drawdown from the current peak
  drawdown = (peak - value) / peak
  
  # Update the maximum drawdown if the current drawdown is larger
  if (drawdown > max_drawdown) {
    max_drawdown = drawdown
  }
}

# max_drawdown now holds the maximum drawdown percentage
max_drawdown


# Step 4: Compute Skewness and Kurtosis
skewness <- skewness(contback_data$Returns_VPD, na.rm = TRUE)
kurtosis <- kurtosis(contback_data$Returns_VPD, na.rm = TRUE)


# Step 5: Calculate Cumulative Return
cumulative_return <- last(contback_data$VPD) / first(contback_data$VPD) - 1


# Step 6: Annualize Returns_VPD and Volatility
annualized_return <- (1 + mean(contback_data$Returns_VPD, na.rm = TRUE)) ^ 252 - 1  # Assuming 252 trading days in a year
annualized_volatility <- sd(contback_data$Returns_VPD, na.rm = TRUE) * sqrt(252)


# Step 7: Calculate the Sharpe Ratio
# Calculate the Daily Excess Returns_VPD
contback_data$ExcessReturns_VPD <- contback_data$Returns_VPD - contback_data$RFR

# Calculate the Mean Daily Excess Retun and Standard Deviation for the Daily Returns_VPD 
mean_daily_excess_return_vpd <- mean(contback_data$ExcessReturns_VPD, na.rm = TRUE)
sd_daily_excess_Returns_VPD <- sd(contback_data$Returns_VPD, na.rm = TRUE)

# Compute the Daily Sharpe Ratio
daily_sharpe_ratio <- mean_daily_excess_return_vpd / sd_daily_excess_Returns_VPD

# Annualize the Sharpe Ratio
annualized_sharpe_ratio <- daily_sharpe_ratio * sqrt(252)

annualized_sharpe_ratio


# Step 8: Calculate the Sortino Ratio
# Calculate the Downside Deviation
squared_negative_excess_Returns_VPD <- ifelse(contback_data$ExcessReturns_VPD < 0, 
                                              contback_data$ExcessReturns_VPD^2, 
                                              0)
mean_squared_negative_excess_Returns_VPD <- mean(squared_negative_excess_Returns_VPD, na.rm = TRUE)
downside_deviation <- sqrt(mean_squared_negative_excess_Returns_VPD)

# Calculate the Daily Sortino Ratio
daily_sortino_ratio <- mean_daily_excess_return_vpd / downside_deviation

# Annualize the Sortino Ratio
annualized_sortino_ratio <- daily_sortino_ratio * sqrt(252)

annualized_sortino_ratio


#Step 9: Calculate the Calmar Ratio
# Calculate the Annualized Mean of Daily Excess Returns_VPD
annualized_mean_daily_excess_return <- mean(contback_data$ExcessReturns_VPD, na.rm = TRUE) * 252

# Compute the Calmar Ratio using the Annualized Mean of Daily Excess Returns_VPD
calmar_ratio_vpd <- annualized_mean_daily_excess_return / max_drawdown

# The result is the Calmar Ratio for your strategy
calmar_ratio_vpd





# Combine all the results into a single tibble
results_vpd <- tibble(
  Statistics = c("Mean", "Median", "S.D.", "Min", "Max", "Max Drawdown", "Skewness", "Kurtosis", "Cumulative Return", "Annualized Return", "Annualized Volatility", "Annualized Sharpe Ratio", "Annualized Sortino Ratio", "Calmar Ratio"),
  VPD_Index = c(summary_stats_vpd$Mean, summary_stats_vpd$Median, summary_stats_vpd$S.D., summary_stats_vpd$Min, summary_stats_vpd$Max, 
                max_drawdown, skewness, kurtosis, cumulative_return, annualized_return, annualized_volatility, annualized_sharpe_ratio, annualized_sortino_ratio, calmar_ratio_vpd)
)

# make the dataset visually nicer
# Convert tibble to data frame if it is not already
results_vpd <- as.data.frame(results_vpd)

# Set the row names and remove the 'Statistics' column
rownames(results_vpd) <- results_vpd$Statistics
results_vpd <-results_vpd[-1]

# Round the numbers in the VPD_Index column to 2 decimal places
results_vpd$VPD_Index <- round(results_vpd$VPD_Index, 4)

# Print the results to check
print(results_vpd)

#Descriptive statistics SPX ####
# Step 1: Calculate Simple Returns_SPX
contback_data <- contback_data %>%
  mutate(Returns_SPX = (SPX - lag(SPX)) / lag(SPX))

# Replace the first NA value in Returns_SPX with 0 
contback_data$Returns_SPX[1] <- 0  


# Step 2: Generate Summary Statistics
summary_stats_spx <- contback_data %>%
  summarise(N.Obs = n(),
            Mean = mean(Returns_SPX, na.rm = TRUE),
            Median = median(Returns_SPX, na.rm = TRUE),
            S.D. = sd(Returns_SPX, na.rm = TRUE),
            Min = min(Returns_SPX, na.rm = TRUE),
            Max = max(Returns_SPX, na.rm = TRUE))


# Step 3: Calculate Max Drawdown
# Initialize variables to track the peak and maximum drawdown
peak = contback_data$SPX[1]
max_drawdown = 0

# Loop through the SPX to find the maximum drawdown
for (value in contback_data$SPX) {
  # Update the peak if the current value is higher than the current peak
  if (value > peak) {
    peak = value
  }
  
  # Calculate the drawdown from the current peak
  drawdown = (peak - value) / peak
  
  # Update the maximum drawdown if the current drawdown is larger
  if (drawdown > max_drawdown) {
    max_drawdown = drawdown
  }
}

# max_drawdown now holds the maximum drawdown percentage
max_drawdown


# Step 4: Compute Skewness and Kurtosis
skewness <- skewness(contback_data$Returns_SPX, na.rm = TRUE)
kurtosis <- kurtosis(contback_data$Returns_SPX, na.rm = TRUE)


# Step 5: Calculate Cumulative Return
cumulative_return <- last(contback_data$SPX) / first(contback_data$SPX) - 1


# Step 6: Annualize Returns_SPX and Volatility
annualized_return <- (1 + mean(contback_data$Returns_SPX, na.rm = TRUE)) ^ 252 - 1  # Assuming 252 trading days in a year
annualized_volatility <- sd(contback_data$Returns_SPX, na.rm = TRUE) * sqrt(252)


# Step 7: Calculate the Sharpe Ratio
# Calculate the Daily Excess Returns_SPX
contback_data$ExcessReturns_SPX <- contback_data$Returns_SPX - contback_data$RFR

# Calculate the Mean Daily Excess Retun and Standard Deviation for the Daily Returns_SPX 
mean_daily_excess_return_spx <- mean(contback_data$ExcessReturns_SPX, na.rm = TRUE)
sd_daily_excess_Returns_SPX <- sd(contback_data$Returns_SPX, na.rm = TRUE)

# Compute the Daily Sharpe Ratio
daily_sharpe_ratio <- mean_daily_excess_return_spx / sd_daily_excess_Returns_SPX

# Annualize the Sharpe Ratio
annualized_sharpe_ratio <- daily_sharpe_ratio * sqrt(252)

annualized_sharpe_ratio


# Step 8: Calculate the Sortino Ratio
# Calculate the Downside Deviation
squared_negative_excess_Returns_SPX <- ifelse(contback_data$ExcessReturns_SPX < 0, 
                                              contback_data$ExcessReturns_SPX^2, 
                                              0)
mean_squared_negative_excess_Returns_SPX <- mean(squared_negative_excess_Returns_SPX, na.rm = TRUE)
downside_deviation <- sqrt(mean_squared_negative_excess_Returns_SPX)

# Calculate the Daily Sortino Ratio
daily_sortino_ratio <- mean_daily_excess_return_spx / downside_deviation

# Annualize the Sortino Ratio
annualized_sortino_ratio <- daily_sortino_ratio * sqrt(252)

annualized_sortino_ratio


#Step 9: Calculate the Calmar Ratio
# Calculate the Annualized Mean of Daily Excess Returns_SPX
annualized_mean_daily_excess_return <- mean(contback_data$ExcessReturns_SPX, na.rm = TRUE) * 252

# Compute the Calmar Ratio using the Annualized Mean of Daily Excess Returns_SPX
calmar_ratio_spx <- annualized_mean_daily_excess_return / max_drawdown

# The result is the Calmar Ratio for your strategy
calmar_ratio_spx





# Combine all the results into a single tibble
results_spx <- tibble(
  Statistics = c("Mean", "Median", "S.D.", "Min", "Max", "Max Drawdown", "Skewness", "Kurtosis", "Cumulative Return", "Annualized Return", "Annualized Volatility", "Annualized Sharpe Ratio", "Annualized Sortino Ratio", "Calmar Ratio"),
  SPX = c(summary_stats_spx$Mean,summary_stats_spx$Median, summary_stats_spx$S.D., summary_stats_spx$Min, summary_stats_spx$Max, 
          max_drawdown, skewness, kurtosis, cumulative_return, annualized_return, annualized_volatility, annualized_sharpe_ratio, annualized_sortino_ratio, calmar_ratio_spx)
)

# make the dataset visually nicer
# Convert tibble to data frame if it is not already
results_spx <- as.data.frame(results_spx)

# Set the row names and remove the 'Statistics' column
rownames(results_spx) <- results_spx$Statistics
results_spx <-results_spx[-1]

# Round the numbers in the SPX column to 2 decimal places
results_spx$SPX <- round(results_spx$SPX, 4)

# Print the results to check
print(results_spx)







#Alpha and Beta calculation ####
# Strategy: 
# Step 1: Run the regression to calculate Beta
model_contback <- lm(Returns ~ Returns_SPX, data = contback_data)
beta_contback <- coef(model_contback)["Returns_SPX"]  # Extract the beta coefficient
# Step 2: Calculate Alpha
# The RFR is already in the dataframe and adjusted for daily use.
annualized_market_return <- mean(contback_data$Returns_SPX, na.rm = TRUE) * 252  # Annualize SPX returns
# Annualize strategy returns for comparison
annualized_strategy_return <- mean(contback_data$Returns, na.rm = TRUE) * 252
# Annualize the mean daily RFR
annualized_rfr <- mean(contback_data$RFR, na.rm = TRUE) * 360
# Calculate alpha using the CAPM formula
alpha_contback <- annualized_strategy_return - (annualized_rfr + beta_contback * (annualized_market_return - annualized_rfr))

# Output the results
cat("Beta: ", beta_contback, "\n")
cat("Alpha: ", alpha_contback, "\n")

# Add to other stats
new_rows <- data.frame(ContBack = c(alpha_contback, beta_contback))
rownames(new_rows) <- c("Alpha", "Beta")

# Combine with the original results
# Use rbind to append the data, this assumes results_contback is a data frame with ContBack_75 as its column
results_contback <- rbind(results_contback, new_rows)

# Round the ContBack column
results_contback$ContBack <- round(results_contback$ContBack, 4)

# Print the dataframe to check
print(results_contback)




# SPX
alpha_SPX <- 0
beta_SPX <- 1

# Add to other stats
new_rows <- data.frame(SPX = c(alpha_SPX, beta_SPX))
rownames(new_rows) <- c("Alpha", "Beta")

# Combine with the original results
# Use rbind to append the data, this assumes results_ContBack is a data frame with SPX as its column
results_spx <- rbind(results_spx, new_rows)

# Round the SPX column
results_spx$SPX <- round(results_spx$SPX, 4)

# Print the dataframe to check
print(results_spx)




# VPD Index
# Step 1: Run the regression to calculate Beta
model_vpdindex <- lm(Returns_VPD ~ Returns_SPX, data = contback_data)
beta_vpdindex <- coef(model_vpdindex)["Returns_SPX"]  # Extract the beta coefficient

# Step 2: Calculate Alpha
# The RFR is already in the dataframe and adjusted for daily use.
annualized_market_return <- mean(contback_data$Returns_SPX, na.rm = TRUE) * 252  # Annualize SPX returns
# Annualize strategy returns for comparison
annualized_putindex_return <- mean(contback_data$Returns_VPD, na.rm = TRUE) * 252
# Annualize the mean daily RFR
annualized_rfr <- mean(contback_data$RFR, na.rm = TRUE) * 360
# Calculate alpha using the CAPM formula
alpha_putindex <- annualized_putindex_return - (annualized_rfr + beta_vpdindex * (annualized_market_return - annualized_rfr))

# Output the results
cat("Beta: ", beta_vpdindex, "\n")
cat("Alpha: ", alpha_putindex, "\n")

# Add to other stats
new_rows <- data.frame(VPD_Index = c(alpha_putindex, beta_vpdindex))
rownames(new_rows) <- c("Alpha", "Beta")

# Combine with the original results
# Use rbind to append the data, this assumes results_ContBack is a data frame with VPD_Index as its column
results_vpd <- rbind(results_vpd, new_rows)

# Round the VPD_Index column
results_vpd$VPD_Index <- round(results_vpd$VPD_Index, 4)

# Print the dataframe to check
print(results_vpd)


#All Descriptive Stats Table ####
# Combine the data frames by columns
combined_results <- cbind(results_contback, results_vpd, results_spx)

# Rename the columns 
colnames(combined_results) <- c("ContBack", "VPD Index", "S&P 500")

# Print the combined results to check
print(combined_results)




