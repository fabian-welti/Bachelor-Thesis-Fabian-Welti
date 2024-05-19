###################################

#ShortFutures1
#with margin account, spreads and cost

#Fabian Welti 
#created: 17.02.2024
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


#Create new Datasets and structure it to work on ####
# Step 1: Import and clean the data
# remove all the additional rows of futures data
futures_data_shortfutures1 <- subset(vix_futures_06_24, Date >= as.Date("2006-01-03")) 
# read in the data for three month Treasury Bills 
treasury_data <- read.csv("Treasury.csv") 
# clean the dataset
treasury_data <- subset(treasury_data, DATE >= as.Date("2006-01-03") & DATE <= as.Date("2024-02-02")) #same dates as futues_data_shortfutures1
treasury_data <- treasury_data %>%
  rename(Date = DATE,
         RFR = DTB3)
treasury_data$Date <- as.Date(treasury_data$Date, format = "%Y-%m-%d")
treasury_data <- treasury_data[treasury_data$RFR != ".", ]
futures_data_shortfutures1 <- subset(futures_data_shortfutures1, select = -c(High, Low, `Total Volume`, EFP, Change, `Open Interest`, `Futures expiration month`, `Expiration Year`, `Expiration month and year`))
# Merge the Datasets to one and clean and order
futures_data_shortfutures1 <- merge(treasury_data, futures_data_shortfutures1, by="Date", all.x = TRUE)
futures_data_shortfutures1$RFR <- as.numeric(futures_data_shortfutures1$RFR)
futures_data_shortfutures1 <- futures_data_shortfutures1[order(futures_data_shortfutures1$Date, futures_data_shortfutures1$ExpirationDate), ] #order the data


# Step 2: filter out front month contracts
# get second month contract on expiration days, otherwise get the front month
futures_data_shortfutures1 <- futures_data_shortfutures1 %>%
  group_by(Date) %>%
  arrange(Date, ExpirationDate) %>% # sort by Date and then by ExpirationDate within each group
  mutate(is_expiration = Date == ExpirationDate) %>%
  filter(!is_expiration | row_number() > 1) %>% # Exclude rows that are expiration rows or select the second row if it is an expiration day
  slice(1) %>% # Select the first row, which is now the front month contract unless it's the expiration day
  select(-is_expiration) %>% # Remove the helper column
  ungroup() # Remove the grouping


# Step 3: Convert the RFR from percentage to a daily rate by dividing by 360
futures_data_shortfutures1$RFR <- futures_data_shortfutures1$RFR / 36000 # Dividing by 100 to get rate and by 360 for daily rate







#Implement the trading strategy by using multiple sub steps to keep track of the results of the code ####
# Step 1: Create a new column 'IsExpirationNextDay' which looks if the we close the current front month contract 
futures_data_shortfutures1 <- futures_data_shortfutures1 %>%
  mutate(IsExpirationNextDay = ExpirationDate - Date == 1)


# Step 2: find the rebalnce day on which the new position is opened 
futures_data_shortfutures1 <- futures_data_shortfutures1 %>%
  mutate(RebalanceDay = if_else(lag(IsExpirationNextDay, default = FALSE) == TRUE, TRUE, FALSE)) 

# manually override the first we can sell the on the first obsevation of the dataset
futures_data_shortfutures1$RebalanceDay[1] <- TRUE


# Step 3: new column only looking at the PnL of the roll only
futures_data_shortfutures1 <- futures_data_shortfutures1 %>%
  mutate(DailyPnL = if_else(RebalanceDay,
                            Settle - Open, # TRUE case: use Settle - Open
                            Settle - lag(Settle, default = first(Settle)))) # FALSE case: use Settle - previous Settle



# Step 4: Initialize Values 
# Initialize the margin account with $100,000
initial_margin_account <- 1000000

# Set the initial vega notional to -1
initial_vega_notional <- -10 # Since we are short selling 1 VIX futures contract per $100,000 of capital



# Step 5: Calculate how the value of the Margin Account changes over time, which calculates the actual PnL of the strategy, 
# and simultaneously update the vega notional (VN) on rebalance days

# Initialize the MarginAccount and VN on the first day
futures_data_shortfutures1$MarginAccount[1] <- initial_margin_account
futures_data_shortfutures1$VN[1] <- initial_vega_notional

# Loop through the dataframe starting from the second row
for (i in 2:nrow(futures_data_shortfutures1)) {
  
  # Calculate the new MarginAccount value using the formula
  Rt_1 <- futures_data_shortfutures1$RFR[i - 1]  # Use RFR directly as it's already in decimal form
  MAt_1 <- futures_data_shortfutures1$MarginAccount[i - 1]
  DailyPnL <- futures_data_shortfutures1$DailyPnL[i] * 1000 # Adjust for contract multiplier
  
  # Update MarginAccount and VN based on whether it's a rebalance day or not
  if (futures_data_shortfutures1$RebalanceDay[i]) {
    futures_data_shortfutures1$VN[i] <- if (MAt_1 < 100000) {
      -1
    } else {
      -floor(abs(MAt_1 / 100000))
    }
    futures_data_shortfutures1$MarginAccount[i] <- (1 + Rt_1) * MAt_1 + futures_data_shortfutures1$VN[i] * DailyPnL
  } else {
    futures_data_shortfutures1$VN[i] <- futures_data_shortfutures1$VN[i - 1]
    futures_data_shortfutures1$MarginAccount[i] <- (1 + Rt_1) * MAt_1 + futures_data_shortfutures1$VN[i] * DailyPnL
  }
  
  # Apply trading costs on Rebalance and Expiration Days
  if (futures_data_shortfutures1$RebalanceDay[i] || futures_data_shortfutures1$IsExpirationNextDay[i]) {
    futures_data_shortfutures1$MarginAccount[i] <- futures_data_shortfutures1$MarginAccount[i] - abs(futures_data_shortfutures1$VN[i]) * 2.32
    # Apply bid-ask spread cost on opening and closing of positions
    futures_data_shortfutures1$MarginAccount[i] <- futures_data_shortfutures1$MarginAccount[i] - abs(futures_data_shortfutures1$VN[i]) * 0.05 * 1000
  }
}




#Calculate the margin requirement ####
# Import the data
margin_requirement <- read_excel("margin requirements vix futures shortfutures1.xlsx")

margin_requirement <- margin_requirement %>%
  mutate(Date = as.Date(Date)) %>%
  select(Date, `IB IM`, `IB MM`) %>%
  arrange(Date)


# Merge
futures_data_shortfutures1 <- futures_data_shortfutures1 %>%
  left_join(margin_requirement, by = "Date") %>%
  fill(`IB IM`, `IB MM`, .direction = "down") %>%
  mutate(`IB IM` = replace_na(`IB IM`, 17625),
         `IB MM` = replace_na(`IB MM`, 11100))


# Calcultate the actual Margin Requirements each day 
futures_data_shortfutures1 <- futures_data_shortfutures1 %>%
  mutate(Margin_Requirement = ifelse(RebalanceDay,
                                     -1 * VN * `IB IM`,
                                     -1 * VN * `IB MM`))





#Import VDP as benchmark ####
VPD_data <- read_excel("VPD_DATA.xlsx")

futures_data_shortfutures1 <- futures_data_shortfutures1 %>%
  left_join(VPD_data, by = "Date")


# Calculate daily PnL for "Last Price"
futures_data_shortfutures1 <- futures_data_shortfutures1 %>%
  mutate(daily_pnl_lastprice = `Last Price` - lag(`Last Price`))
# Set the first value of 'daily_pnl_lastprice' to 0
futures_data_shortfutures1$daily_pnl_lastprice[1] <- 0
# Replace NA values in 'daily_pnl_lastprice' with 0
futures_data_shortfutures1$daily_pnl_lastprice <- replace_na(futures_data_shortfutures1$daily_pnl_lastprice, 0)

# Calculate the initial contract size based on "Last Price"
x_lastprice <- round(1000000 / futures_data_shortfutures1$`Last Price`[1], digits = 0)

# Initialize the 'VPD' column with the first value as the initial investment
futures_data_shortfutures1$VPD <- rep(0, nrow(futures_data_shortfutures1))
futures_data_shortfutures1$VPD[1] <- 1000000  # Set the initial value for the first row

# Loop through the dataset starting from the second row to calculate VPD
for (i in 2:nrow(futures_data_shortfutures1)) {
  futures_data_shortfutures1$VPD[i] <- x_lastprice * futures_data_shortfutures1$daily_pnl_lastprice[i] + futures_data_shortfutures1$VPD[i - 1]
}







#Import SPX data as a benchmark ####
# Read in the SPX data and prepare it
spx_data <- read_excel("^SPX 1990-12.01.2024.xlsx", skip = 1) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  rename(Close_SPX = Close) %>%
  select(Date, Close_SPX)  # Select only Date and Close_SPX for merging

# Merge only the Close_SPX column
futures_data_shortfutures1 <- futures_data_shortfutures1 %>%
  left_join(spx_data, by = "Date")


# Calculate daily PnL for SPX
futures_data_shortfutures1 <- futures_data_shortfutures1 %>%
  mutate(daily_pnl_spx = Close_SPX - lag(Close_SPX))
# Set the first value of 'daily_pnl_spx' to 0
futures_data_shortfutures1$daily_pnl_spx[1] <- 0
# Replace NA values in 'daily_pnl_spx' with 0
futures_data_shortfutures1$daily_pnl_spx <- replace_na(futures_data_shortfutures1$daily_pnl_spx, 0)

# Calculate the development of 100k 
# Calculate the initial contract size 'x' using the correct column name "PUT Index"
x <- round(1000000 / futures_data_shortfutures1$Close_SPX[1], digits = 0)

# Initialize the 'SPX' column with the first value as the initial investment
futures_data_shortfutures1$SPX <- rep(0, nrow(futures_data_shortfutures1))
futures_data_shortfutures1$SPX[1] <- 1000000  # Set the initial value for the first row

# Loop through the dataset starting from the second row
for (i in 2:nrow(futures_data_shortfutures1)) {
  futures_data_shortfutures1$SPX[i] <- x * futures_data_shortfutures1$daily_pnl_spx[i] + futures_data_shortfutures1$SPX[i - 1]
}







#Plot the results ####
# Calculate the upper limit for y-axis
  upper_limit <- max(c(max(futures_data_shortfutures1$MarginAccount, na.rm = TRUE), 
                       max(futures_data_shortfutures1$Margin_Requirement, na.rm = TRUE),
                       max(futures_data_shortfutures1$VPD, na.rm = TRUE),
                       max(futures_data_shortfutures1$SPX, na.rm = TRUE)))

# Define y-axis range starting from 0 to the upper limit
y_range <- c(0, upper_limit)

# Set plot font family
par(family="Times New Roman")

# Set up the plot with the first line to establish the plot area
plot(futures_data_shortfutures1$Date, futures_data_shortfutures1$MarginAccount, type = 'l', 
     col = "darkblue", ylim = y_range, xaxt = 'n',    # Disable default x-axis
     xlab = "", ylab = "Development of $1'000'000", 
     main = "Performance ShortFutures1", lwd = 2)  # Increase line width with lwd

# Add the remaining series with lines
lines(futures_data_shortfutures1$Date, futures_data_shortfutures1$Margin_Requirement, col = "deepskyblue4", lwd = 2)
lines(futures_data_shortfutures1$Date, futures_data_shortfutures1$VPD, col = "red", lwd = 2)
lines(futures_data_shortfutures1$Date, futures_data_shortfutures1$SPX, col = "black", lwd = 2)

# Custom x-axis labels for each year
year_ticks <- seq(from=min(futures_data_shortfutures1$Date, na.rm=TRUE),
                  to=max(futures_data_shortfutures1$Date, na.rm=TRUE), by="years")
axis(1, at=year_ticks, labels=format(year_ticks, "%Y"), cex.axis=0.8, las=2)

# Add light gridlines
abline(h=seq(from=0, to=upper_limit, by=(upper_limit)/5), 
       col="grey", lty="dotted", lwd=0.5)
abline(v=year_ticks, col="grey", lty="dotted", lwd=0.5)

# Add a legend without a box 
legend("topleft", legend = c("ShortFutures1", "Margin Requirement ShortFutures1", "VPD Index", "S&P 500"),
       col = c("darkblue", "deepskyblue4", "red", "black"), lty = 1, bty = "n", cex = 0.8, lwd=c(2,2,2,2))



#Descriptive statistics ShortFutures1####

# Step 1: Calculate Simple Returns
futures_data_shortfutures1 <- futures_data_shortfutures1 %>%
  mutate(Returns = (MarginAccount - lag(MarginAccount)) / lag(MarginAccount))

# Replace the first NA value in Returns with 0 
futures_data_shortfutures1$Returns[1] <- 0  


# Step 2: Generate Summary Statistics
summary_stats_shortfutures1 <- futures_data_shortfutures1 %>%
  summarise(N.Obs = n(),
            Mean = mean(Returns, na.rm = TRUE),
            Median = median(Returns, na.rm = TRUE),
            S.D. = sd(Returns, na.rm = TRUE),
            Min = min(Returns, na.rm = TRUE),
            Max = max(Returns, na.rm = TRUE))


# Step 3: Calculate Max Drawdown
# Initialize variables to track the peak and maximum drawdown
peak = futures_data_shortfutures1$MarginAccount[1]
max_drawdown = 0

# Loop through the MarginAccount to find the maximum drawdown
for (value in futures_data_shortfutures1$MarginAccount) {
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
skewness <- skewness(futures_data_shortfutures1$Returns, na.rm = TRUE)
kurtosis <- kurtosis(futures_data_shortfutures1$Returns, na.rm = TRUE)


# Step 5: Calculate Cumulative Return
cumulative_return <- last(futures_data_shortfutures1$MarginAccount) / first(futures_data_shortfutures1$MarginAccount) - 1


# Step 6: Annualize Returns and Volatility
annualized_return <- (1 + mean(futures_data_shortfutures1$Returns, na.rm = TRUE)) ^ 252 - 1  # Assuming 252 trading days in a year
annualized_volatility <- sd(futures_data_shortfutures1$Returns, na.rm = TRUE) * sqrt(252)


# Step 7: Calculate the Sharpe Ratio
# Calculate the Daily Excess Returns
futures_data_shortfutures1$ExcessReturns <- futures_data_shortfutures1$Returns - futures_data_shortfutures1$RFR

# Calculate the Mean Daily Excess Retun and Standard Deviation for the Daily Returns 
mean_daily_excess_return_shortfutures1 <- mean(futures_data_shortfutures1$ExcessReturns, na.rm = TRUE)
sd_daily_excess_returns_shortfutures1 <- sd(futures_data_shortfutures1$Returns, na.rm = TRUE)

# Compute the Daily Sharpe Ratio
daily_sharpe_ratio_shortfutures1 <- mean_daily_excess_return_shortfutures1 / sd_daily_excess_returns_shortfutures1

# Annualize the Sharpe Ratio
annualized_sharpe_ratio_shortfutures1 <- daily_sharpe_ratio_shortfutures1 * sqrt(252)

annualized_sharpe_ratio_shortfutures1


# Step 8: Calculate the Sortino Ratio
# Calculate the Downside Deviation
squared_negative_excess_returns <- ifelse(futures_data_shortfutures1$ExcessReturns < 0, 
                                          futures_data_shortfutures1$ExcessReturns^2, 
                                          0)
mean_squared_negative_excess_returns <- mean(squared_negative_excess_returns, na.rm = TRUE)
downside_deviation <- sqrt(mean_squared_negative_excess_returns)

# Calculate the Daily Sortino Ratio
daily_sortino_ratio <- mean_daily_excess_return_shortfutures1 / downside_deviation

# Annualize the Sortino Ratio
annualized_sortino_ratio <- daily_sortino_ratio * sqrt(252)

annualized_sortino_ratio


#Step 9: Calculate the Calmar Ratio
# Calculate the Annualized Mean of Daily Excess Returns
annualized_mean_daily_excess_return <- mean(futures_data_shortfutures1$ExcessReturns, na.rm = TRUE) * 252

# Compute the Calmar Ratio using the Annualized Mean of Daily Excess Returns
calmar_ratio_shortfutures1 <- annualized_mean_daily_excess_return / max_drawdown

# The result is the Calmar Ratio for your strategy
calmar_ratio_shortfutures1





# Combine all the results into a single tibble
results_shortfutures1 <- tibble(
  Statistics = c("Mean", "Median", "S.D.", "Min", "Max", "Max Drawdown", "Skewness", "Kurtosis", "Cumulative Return", "Annualized Return", "Annualized Volatility", "Annualized Sharpe Ratio", "Annualized Sortino Ratio", "Calmar Ratio"),
  ShortFutures1 = c(summary_stats_shortfutures1$Mean, summary_stats_shortfutures1$Median, summary_stats_shortfutures1$S.D., summary_stats_shortfutures1$Min, summary_stats_shortfutures1$Max, 
                    max_drawdown, skewness, kurtosis, cumulative_return, annualized_return, annualized_volatility, annualized_sharpe_ratio_shortfutures1, annualized_sortino_ratio, calmar_ratio_shortfutures1)
)

# make the dataset visually nicer
# Convert tibble to data frame if it is not already
results_shortfutures1 <- as.data.frame(results_shortfutures1)

# Set the row names and remove the 'Statistics' column
rownames(results_shortfutures1) <- results_shortfutures1$Statistics
results_shortfutures1 <-results_shortfutures1[-1]

# Round the numbers in the ShortFutures1 column to 2 decimal places
results_shortfutures1$ShortFutures1 <- round(results_shortfutures1$ShortFutures1, 4)

# Print the results to check
print(results_shortfutures1)







#Descriptive statistics VPD Index####
# Step 1: Calculate Simple Returns_VPD
futures_data_shortfutures1 <- futures_data_shortfutures1 %>%
  mutate(Returns_VPD = (VPD - lag(VPD)) / lag(VPD))

# Replace the first NA value in Returns_VPD with 0 
futures_data_shortfutures1$Returns_VPD[1] <- 0  


# Step 2: Generate Summary Statistics
summary_stats_vpd <- futures_data_shortfutures1 %>%
  summarise(N.Obs = n(),
            Mean = mean(Returns_VPD, na.rm = TRUE),
            Median = median(Returns_VPD, na.rm = TRUE),
            S.D. = sd(Returns_VPD, na.rm = TRUE),
            Min = min(Returns_VPD, na.rm = TRUE),
            Max = max(Returns_VPD, na.rm = TRUE))


# Step 3: Calculate Max Drawdown
# Initialize variables to track the peak and maximum drawdown
peak = futures_data_shortfutures1$VPD[1]
max_drawdown = 0

# Loop through the VPD to find the maximum drawdown
for (value in futures_data_shortfutures1$VPD) {
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
skewness <- skewness(futures_data_shortfutures1$Returns_VPD, na.rm = TRUE)
kurtosis <- kurtosis(futures_data_shortfutures1$Returns_VPD, na.rm = TRUE)


# Step 5: Calculate Cumulative Return
cumulative_return <- last(futures_data_shortfutures1$VPD) / first(futures_data_shortfutures1$VPD) - 1


# Step 6: Annualize Returns_VPD and Volatility
annualized_return <- (1 + mean(futures_data_shortfutures1$Returns_VPD, na.rm = TRUE)) ^ 252 - 1  # Assuming 252 trading days in a year
annualized_volatility <- sd(futures_data_shortfutures1$Returns_VPD, na.rm = TRUE) * sqrt(252)


# Step 7: Calculate the Sharpe Ratio
# Calculate the Daily Excess Returns_VPD
futures_data_shortfutures1$ExcessReturns_VPD <- futures_data_shortfutures1$Returns_VPD - futures_data_shortfutures1$RFR

# Calculate the Mean Daily Excess Retun and Standard Deviation for the Daily Returns_VPD 
mean_daily_excess_return_vpd <- mean(futures_data_shortfutures1$ExcessReturns_VPD, na.rm = TRUE)
sd_daily_excess_Returns_VPD <- sd(futures_data_shortfutures1$Returns_VPD, na.rm = TRUE)

# Compute the Daily Sharpe Ratio
daily_sharpe_ratio <- mean_daily_excess_return_vpd / sd_daily_excess_Returns_VPD

# Annualize the Sharpe Ratio
annualized_sharpe_ratio <- daily_sharpe_ratio * sqrt(252)

annualized_sharpe_ratio


# Step 8: Calculate the Sortino Ratio
# Calculate the Downside Deviation
squared_negative_excess_Returns_VPD <- ifelse(futures_data_shortfutures1$ExcessReturns_VPD < 0, 
                                              futures_data_shortfutures1$ExcessReturns_VPD^2, 
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
annualized_mean_daily_excess_return <- mean(futures_data_shortfutures1$ExcessReturns_VPD, na.rm = TRUE) * 252

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
futures_data_shortfutures1 <- futures_data_shortfutures1 %>%
  mutate(Returns_SPX = (SPX - lag(SPX)) / lag(SPX))

# Replace the first NA value in Returns_SPX with 0 
futures_data_shortfutures1$Returns_SPX[1] <- 0  


# Step 2: Generate Summary Statistics
summary_stats_spx <- futures_data_shortfutures1 %>%
  summarise(N.Obs = n(),
            Mean = mean(Returns_SPX, na.rm = TRUE),
            Median = median(Returns_SPX, na.rm = TRUE),
            S.D. = sd(Returns_SPX, na.rm = TRUE),
            Min = min(Returns_SPX, na.rm = TRUE),
            Max = max(Returns_SPX, na.rm = TRUE))


# Step 3: Calculate Max Drawdown
# Initialize variables to track the peak and maximum drawdown
peak = futures_data_shortfutures1$SPX[1]
max_drawdown = 0

# Loop through the SPX to find the maximum drawdown
for (value in futures_data_shortfutures1$SPX) {
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
skewness <- skewness(futures_data_shortfutures1$Returns_SPX, na.rm = TRUE)
kurtosis <- kurtosis(futures_data_shortfutures1$Returns_SPX, na.rm = TRUE)


# Step 5: Calculate Cumulative Return
cumulative_return <- last(futures_data_shortfutures1$SPX) / first(futures_data_shortfutures1$SPX) - 1


# Step 6: Annualize Returns_SPX and Volatility
annualized_return <- (1 + mean(futures_data_shortfutures1$Returns_SPX, na.rm = TRUE)) ^ 252 - 1  # Assuming 252 trading days in a year
annualized_volatility <- sd(futures_data_shortfutures1$Returns_SPX, na.rm = TRUE) * sqrt(252)


# Step 7: Calculate the Sharpe Ratio
# Calculate the Daily Excess Returns_SPX
futures_data_shortfutures1$ExcessReturns_SPX <- futures_data_shortfutures1$Returns_SPX - futures_data_shortfutures1$RFR

# Calculate the Mean Daily Excess Retun and Standard Deviation for the Daily Returns_SPX 
mean_daily_excess_return_spx <- mean(futures_data_shortfutures1$ExcessReturns_SPX, na.rm = TRUE)
sd_daily_excess_Returns_SPX <- sd(futures_data_shortfutures1$Returns_SPX, na.rm = TRUE)

# Compute the Daily Sharpe Ratio
daily_sharpe_ratio <- mean_daily_excess_return_spx / sd_daily_excess_Returns_SPX

# Annualize the Sharpe Ratio
annualized_sharpe_ratio <- daily_sharpe_ratio * sqrt(252)

annualized_sharpe_ratio


# Step 8: Calculate the Sortino Ratio
# Calculate the Downside Deviation
squared_negative_excess_Returns_SPX <- ifelse(futures_data_shortfutures1$ExcessReturns_SPX < 0, 
                                              futures_data_shortfutures1$ExcessReturns_SPX^2, 
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
annualized_mean_daily_excess_return <- mean(futures_data_shortfutures1$ExcessReturns_SPX, na.rm = TRUE) * 252

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
model_shortfutures1 <- lm(Returns ~ Returns_SPX, data = futures_data_shortfutures1)
beta_shortfutures1 <- coef(model_shortfutures1)["Returns_SPX"]  # Extract the beta coefficient
# Step 2: Calculate Alpha
# The RFR is already in the dataframe and adjusted for daily use.
annualized_market_return <- mean(futures_data_shortfutures1$Returns_SPX, na.rm = TRUE) * 252  # Annualize SPX returns
# Annualize strategy returns for comparison
annualized_strategy_return <- mean(futures_data_shortfutures1$Returns, na.rm = TRUE) * 252
# Annualize the mean daily RFR
annualized_rfr <- mean(futures_data_shortfutures1$RFR, na.rm = TRUE) * 360
# Calculate alpha using the CAPM formula
alpha_shortfutures1 <- annualized_strategy_return - (annualized_rfr + beta_shortfutures1 * (annualized_market_return - annualized_rfr))

# Output the results
cat("Beta: ", beta_shortfutures1, "\n")
cat("Alpha: ", alpha_shortfutures1, "\n")

# Add to other stats
new_rows <- data.frame(ShortFutures1 = c(alpha_shortfutures1, beta_shortfutures1))
rownames(new_rows) <- c("Alpha", "Beta")

# Combine with the original results
# Use rbind to append the data, this assumes results_shortfutures1 is a data frame with ShortFutures1_75 as its column
results_shortfutures1 <- rbind(results_shortfutures1, new_rows)

# Round the ShortFutures1 column
results_shortfutures1$ShortFutures1 <- round(results_shortfutures1$ShortFutures1, 4)

# Print the dataframe to check
print(results_shortfutures1)




# SPX
alpha_SPX <- 0
beta_SPX <- 1

# Add to other stats
new_rows <- data.frame(SPX = c(alpha_SPX, beta_SPX))
rownames(new_rows) <- c("Alpha", "Beta")

# Combine with the original results
# Use rbind to append the data, this assumes results_shortfutures1 is a data frame with SPX as its column
results_spx <- rbind(results_spx, new_rows)

# Round the SPX column
results_spx$SPX <- round(results_spx$SPX, 4)

# Print the dataframe to check
print(results_spx)




# VPD Index
# Step 1: Run the regression to calculate Beta
model_vpdindex <- lm(Returns_VPD ~ Returns_SPX, data = futures_data_shortfutures1)
beta_vpdindex <- coef(model_vpdindex)["Returns_SPX"]  # Extract the beta coefficient

# Step 2: Calculate Alpha
# The RFR is already in the dataframe and adjusted for daily use.
annualized_market_return <- mean(futures_data_shortfutures1$Returns_SPX, na.rm = TRUE) * 252  # Annualize SPX returns
# Annualize strategy returns for comparison
annualized_putindex_return <- mean(futures_data_shortfutures1$Returns_VPD, na.rm = TRUE) * 252
# Annualize the mean daily RFR
annualized_rfr <- mean(futures_data_shortfutures1$RFR, na.rm = TRUE) * 360
# Calculate alpha using the CAPM formula
alpha_putindex <- annualized_putindex_return - (annualized_rfr + beta_vpdindex * (annualized_market_return - annualized_rfr))

# Output the results
cat("Beta: ", beta_vpdindex, "\n")
cat("Alpha: ", alpha_putindex, "\n")

# Add to other stats
new_rows <- data.frame(VPD_Index = c(alpha_putindex, beta_vpdindex))
rownames(new_rows) <- c("Alpha", "Beta")

# Combine with the original results
# Use rbind to append the data, this assumes results_shortfutures1 is a data frame with VPD_Index as its column
results_vpd <- rbind(results_vpd, new_rows)

# Round the VPD_Index column
results_vpd$VPD_Index <- round(results_vpd$VPD_Index, 4)

# Print the dataframe to check
print(results_vpd)



#All Descriptive Stats Table ####
# Combine the data frames by columns
combined_results <- cbind(results_shortfutures1, results_vpd, results_spx)

# Rename the columns 
colnames(combined_results) <- c("ShortFutures1", "VPD Index", "S&P 500")

# Print the combined results to check
print(combined_results)



#Distribution Plot####
# Extract mean and standard deviation from the summary statistics
mean_returns <- summary_stats_shortfutures1$Mean
sd_returns <- summary_stats_shortfutures1$S.D.

# Create a histogram of simple returns
p <- ggplot(futures_data_shortfutures1, aes(x = Returns)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.001, fill = "blue", alpha = 0.7) +
  geom_density(color = "blue", size = 1) +  # Add a density plot for comparison
  stat_function(fun = dnorm, args = list(mean = mean_returns, sd = sd_returns), color = "red", size = 1) + # Overlay the normal distribution
  labs(title = "Histogram of Simple Returns from ShortFront1 Strategy vs. Normal Distribution",
       x = "Simple Returns", y = "Density") +
  theme_minimal() +
  theme(text = element_text(size = 12)) # Customize text size for better readability

# Print the plot
print(p)












