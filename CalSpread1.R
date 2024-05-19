###################################

#Calendar Spread 1

#Fabian Welti 
#created: 06.03.2024
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
calspread1 <- subset(vix_futures_06_24, Date >= as.Date("2006-01-03") & Date <= as.Date("2024-01-12"))
calspread1 <- subset(calspread1, select = -c(High, Low, `Total Volume`, EFP, Change, `Open Interest`, `Futures expiration month`, `Expiration Year`, `Expiration month and year`))
# read in the data for three month Treasury Bills 
treasury_data <- read.csv("Treasury.csv") 
# clean the dataset
treasury_data <- subset(treasury_data, DATE >= as.Date("2006-01-03") & DATE <= as.Date("2024-02-02")) 
treasury_data <- treasury_data %>%
  rename(Date = DATE,
         RFR = DTB3)
treasury_data$Date <- as.Date(treasury_data$Date, format = "%Y-%m-%d")
treasury_data <- treasury_data[treasury_data$RFR != ".", ]
# Merge the Datasets to one and clean and order
calspread1 <- merge(treasury_data, calspread1, by="Date", all.x = TRUE)
calspread1$RFR <- as.numeric(calspread1$RFR)
calspread1 <- calspread1[order(calspread1$Date, calspread1$ExpirationDate), ] #order the data

# read in the data for the vix index for the same time period
vix_data_calspread1 <- read_excel("VIX_History 1990-2024.xlsx", skip = 1) # Adjust path as necessary
vix_data_calspread1$Date <- as.Date(vix_data_calspread1$Date, format = "%m/%d/%Y")
vix_data_calspread1 <- subset(vix_data_calspread1, Date >= as.Date("2006-01-03"))

# combine the datasets for the final dataset
calspread1 <- merge(calspread1, vix_data_calspread1, by = "Date", suffixes = c("_futures", "_index"))
# Convert the RFR from percentage to a daily rate by dividing by 360
calspread1$RFR <- calspread1$RFR / 36000 # Dividing by 100 to get rate and by 360 for daily rate





#Step 2: Filter to keep front and second month contracts on non-expiration days,and second and third month contracts on expiration days ####
calspread1 <- calspread1 %>%
  arrange(Date, ExpirationDate) %>% 
  group_by(Date) %>%
  # Identify the expiring contracts
  mutate(expiration_flag = as.integer(Date == ExpirationDate)) %>%
  # Rank contracts by ExpirationDate within each Date
  mutate(contract_rank = rank(ExpirationDate)) %>%
  # Logic to shift the rank on expiration days
  mutate(adjusted_rank = ifelse(any(expiration_flag == 1) & expiration_flag == 0, contract_rank - 1, contract_rank)) %>%
  # Filter out the expiring contract and keep the subsequent two
  filter(!(expiration_flag == 1), adjusted_rank <= 2) %>%
  # Remove helper columns
  select(-expiration_flag, -contract_rank, -adjusted_rank) %>%
  ungroup()


# Clean some data in calendar year 2006 since data for second and third month is not complete
# Dates to remove
dates_to_remove <- as.Date(c("2006-01-18", "2006-01-19", "2006-02-15", "2006-02-16", "2006-04-19", "2006-04-20", 
                             "2006-05-17", "2006-05-18", "2006-07-19", "2006-07-20", "2006-07-21", 
                             "2006-08-16", "2006-08-17", "2007-11-21", "2006-01-20", "2006-01-23", "2006-02-17",
                             "2006-02-21", "2006-04-21", "2006-05-19", "2006-08-18" ))

# Filtering out the specified dates
calspread1 <- calspread1 %>%
  filter(!as.Date(Date) %in% dates_to_remove)




#Step 3: Create new columns to help identify the expiration dates and rebalance days ####
# Step 3.1: Create a new column 'IsExpirationNextDay' which looks if the we close the current calendar spread 
calspread1 <- calspread1 %>%
  mutate(IsExpirationNextDay = as.logical(ave(ExpirationDate - Date == 1, Date, FUN = max))) %>%
  ungroup()  

# Step 3.2: find the rebalnce day on which the new position in the new calendar spread is opened 
calspread1 <- calspread1 %>%
  mutate(
    # Step 1 & 2: Find sequences of consecutive TRUEs in IsExpirationNextDay
    ConsecutiveExpiration = IsExpirationNextDay & lead(IsExpirationNextDay, default = FALSE),
    # Step 3: Identify the end of a sequence
    EndOfSequence = !lead(IsExpirationNextDay, default = FALSE) & IsExpirationNextDay,
    # Step 4: Mark RebalanceDay as TRUE for the two rows after the end of a sequence
    RebalanceDay = if_else(
      lag(EndOfSequence, default = FALSE) | lag(EndOfSequence, n = 2, default = FALSE), 
      TRUE, 
      FALSE
    )
  ) %>%
  select(-ConsecutiveExpiration, -EndOfSequence) # Cleanup helper columns


  # manually override the value in the first row - open spread on the first observation of the dataset
calspread1$RebalanceDay[1] <- TRUE
calspread1$RebalanceDay[2] <- TRUE







#Step 4: new column calculating the PnL of the roll for each leg of the spread ####
calspread1 <- calspread1 %>%
  group_by(Futures) %>% # Group by contract type to separate front and second month contracts
  mutate(
    Prev_Settle = lag(Settle, n = 1), # Get the Settle price from 1 day ago (two rows up)
    DailyPnL = if_else(
      RebalanceDay,
      Settle - Open_futures, # On rebalance days, use Settle - Open
      Settle - Prev_Settle # On non-rebalance days, use Settle - previous Settle (from 1 day ago)
    )
  ) %>%
  ungroup() # Ungroup the data for further operations or analysis





#Step 5: Calculate the PnL for the complete spread as well as the development of the margin account ####
# Step 5.1: Calculate the combined daily PnL of both legs of the spread
Results_calspread1 <- calspread1 %>%
  group_by(Date) %>%
  summarise(
    roll_pnl= (-first(DailyPnL) + last(DailyPnL)) * 1000, #account for the contract multiplier of VX futures
    RFR = first(RFR), # Get the risk-free rate from the first row in the group
    RebalanceDay = first(RebalanceDay)
  ) %>%
  ungroup()



# Step 5.2: Calculate the development of the margin account and the position rebalancing 
Results_calspread1 <- Results_calspread1 %>%
  mutate(
    NextRebalance = lead(RebalanceDay, default = FALSE)  # Predict if the next day is a RebalanceDay
  )

# Initialize margin account and position size
Mt <- 1000000  # Initial margin account total
ps <- Mt / 50000  # Initial position size based on margin account

# Iterate through each row in the dataset
for (i in 1:nrow(Results_calspread1)) {
  if (i == 1) {
    Results_calspread1$MarginAccount[i] <- Mt
    Results_calspread1$PositionSize[i] <- ps
  } else {
    # Calculate Margin Account before applying costs
    Mt <- (1 + Results_calspread1$RFR[i-1]) * Results_calspread1$MarginAccount[i-1] +
      Results_calspread1$PositionSize[i-1] * Results_calspread1$roll_pnl[i]
    
    # Update Position Size on Rebalance Days 
    if (Results_calspread1$RebalanceDay[i]) {
      ps <- floor(Mt / 50000)
    }
    
    Results_calspread1$PositionSize[i] <- ps  # Assign the updated position size
    
    # Apply trading costs on Rebalance Days
    if (Results_calspread1$RebalanceDay[i]) {
      Mt <- Mt - 2.32 * ps
    }
    
    # Apply spread costs if RebalanceDay is today or tomorrow
    if (Results_calspread1$RebalanceDay[i] || Results_calspread1$NextRebalance[i]) {
      Mt <- Mt - 0.05 * 1000 * ps
    }
    
    # Assign the updated Margin Account after all deductions
    Results_calspread1$MarginAccount[i] <- Mt
  }
}










#Calculate the margin requirement ####
# Import the data
margin_requirement_4 <- read_excel("margin requirements vix futures calspread1.xlsx")

margin_requirement_4 <- margin_requirement_4 %>%
  mutate(Date = as.Date(Date)) %>%
  select(Date, `IB IM`, `IB MM`) %>%
  arrange(Date)


# Merge
Results_calspread1 <- Results_calspread1 %>%
  left_join(margin_requirement_4, by = "Date") %>%
  fill(`IB IM`, `IB MM`, .direction = "down") %>%
  mutate(`IB IM` = replace_na(`IB IM`, 2700),
         `IB MM` = replace_na(`IB MM`, 2160))


# Calcultate the actual Margin Requirements each day 
Results_calspread1 <- Results_calspread1 %>%
  mutate(margin_requirement_4 = ifelse(RebalanceDay,
                                       PositionSize * `IB IM`,
                                       PositionSize * `IB MM`))


#Import VDP as benchmark ####
VPD_data <- read_excel("VPD_DATA.xlsx")

Results_calspread1 <- Results_calspread1 %>%
  left_join(VPD_data, by = "Date")


# Calculate daily PnL for "Last Price"
Results_calspread1 <- Results_calspread1 %>%
  mutate(daily_pnl_lastprice = `Last Price` - lag(`Last Price`))
# Set the first value of 'daily_pnl_lastprice' to 0
Results_calspread1$daily_pnl_lastprice[1] <- 0
# Replace NA values in 'daily_pnl_lastprice' with 0
Results_calspread1$daily_pnl_lastprice <- replace_na(Results_calspread1$daily_pnl_lastprice, 0)

# Calculate the initial contract size based on "Last Price"
x_lastprice <- round(1000000 / Results_calspread1$`Last Price`[1], digits = 0)

# Initialize the 'VPD' column with the first value as the initial investment
Results_calspread1$VPD <- rep(0, nrow(Results_calspread1))
Results_calspread1$VPD[1] <- 1000000  # Set the initial value for the first row

# Loop through the dataset starting from the second row to calculate VPD
for (i in 2:nrow(Results_calspread1)) {
  Results_calspread1$VPD[i] <- x_lastprice * Results_calspread1$daily_pnl_lastprice[i] + Results_calspread1$VPD[i - 1]
}


#Import SPX data as a benchmark ####
# Read in the SPX data and prepare it
spx_data <- read_excel("^SPX 1990-12.01.2024.xlsx", skip = 1) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  rename(Close_SPX = Close) %>%
  select(Date, Close_SPX)  # Select only Date and Close_SPX for merging

# Merge only the Close_SPX column
Results_calspread1 <- Results_calspread1 %>%
  left_join(spx_data, by = "Date")


# Calculate daily PnL for SPX
Results_calspread1 <- Results_calspread1 %>%
  mutate(daily_pnl_spx = Close_SPX - lag(Close_SPX))
# Set the first value of 'daily_pnl_spx' to 0
Results_calspread1$daily_pnl_spx[1] <- 0
# Replace NA values in 'daily_pnl_spx' with 0
Results_calspread1$daily_pnl_spx <- replace_na(Results_calspread1$daily_pnl_spx, 0)

# Calculate the development of 100k 
# Calculate the initial contract size 'x' using the correct column name "PUT Index"
x <- round(1000000 / Results_calspread1$Close_SPX[1], digits = 0)

# Initialize the 'SPX' column with the first value as the initial investment
Results_calspread1$SPX <- rep(0, nrow(Results_calspread1))
Results_calspread1$SPX[1] <- 1000000  # Set the initial value for the first row

# Loop through the dataset starting from the second row
for (i in 2:nrow(Results_calspread1)) {
  Results_calspread1$SPX[i] <- x * Results_calspread1$daily_pnl_spx[i] + Results_calspread1$SPX[i - 1]
}





#Plot the results ####
# Calculate the upper limit for y-axis
upper_limit <- max(c(max(Results_calspread1$MarginAccount, na.rm = TRUE), 
                     max(Results_calspread1$margin_requirement_4, na.rm = TRUE),
                     max(Results_calspread1$VPD, na.rm = TRUE),
                     max(Results_calspread1$SPX, na.rm = TRUE)))

# Define y-axis range starting from 0 to the upper limit
y_range <- c(0, upper_limit)

# Set plot font family
par(family="Times New Roman")

# Set up the plot with the first line to establish the plot area
plot(Results_calspread1$Date, Results_calspread1$MarginAccount, type = 'l', 
     col = "darkblue", ylim = y_range, xaxt = 'n',    # Disable default x-axis
     xlab = "", ylab = "Development of $1'000'000", 
     main = "Performance CalSpread1", lwd = 2)  # Increase line width with lwd

# Add the remaining series with lines
lines(Results_calspread1$Date, Results_calspread1$margin_requirement_4, col = "deepskyblue4", lwd = 2)
lines(Results_calspread1$Date, Results_calspread1$VPD, col = "red", lwd = 2)
lines(Results_calspread1$Date, Results_calspread1$SPX, col = "black", lwd = 2)

# Custom x-axis labels for each year
year_ticks <- seq(from=min(Results_calspread1$Date, na.rm=TRUE),
                  to=max(Results_calspread1$Date, na.rm=TRUE), by="years")
axis(1, at=year_ticks, labels=format(year_ticks, "%Y"), cex.axis=0.8, las=2)

# Add light gridlines
abline(h=seq(from=0, to=upper_limit, by=(upper_limit)/5), 
       col="grey", lty="dotted", lwd=0.5)
abline(v=year_ticks, col="grey", lty="dotted", lwd=0.5)

# Add a legend without a box 
legend("topleft", legend = c("CalSpread1", "Margin Requirement CalSpread1", "VPD Index", "S&P 500"),
       col = c("darkblue", "deepskyblue4", "red", "black"), lty = 1, bty = "n", cex = 0.8, lwd=c(2,2,2,2))










#Descriptive Statistics CalSpread1####
# Step 1: Calculate Simple Returns
Results_calspread1 <- Results_calspread1 %>%
  mutate(Returns = (MarginAccount - lag(MarginAccount)) / lag(MarginAccount))

  # Replace the first NA value in Returns with 0 or leave as NA depending on your preference
Results_calspread1$Returns[1] <- 0  # or NA if you prefer to keep it as NA


# Step 2: Generate Summary Statistics
summary_stats_calspread1 <- Results_calspread1 %>%
  summarise(N.Obs = n(),
            Mean = mean(Returns, na.rm = TRUE),
            Median = median(Returns, na.rm = TRUE),
            S.D. = sd(Returns, na.rm = TRUE),
            Min = min(Returns, na.rm = TRUE),
            Max = max(Returns, na.rm = TRUE))


# Step 3: Calculate Max Drawdown
# Initialize variables to track the peak and maximum drawdown
peak = Results_calspread1$MarginAccount[1]
max_drawdown = 0

# Loop through the MarginAccount to find the maximum drawdown
for (value in Results_calspread1$MarginAccount) {
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
skewness <- skewness(Results_calspread1$Returns, na.rm = TRUE)
kurtosis <- kurtosis(Results_calspread1$Returns, na.rm = TRUE)


# Step 5: Calculate Cumulative Return
cumulative_return <- last(Results_calspread1$MarginAccount) / first(Results_calspread1$MarginAccount) - 1


# Step 6: Annualize Returns and Volatility
annualized_return <- (1 + mean(Results_calspread1$Returns, na.rm = TRUE)) ^ 252 - 1  # Assuming 252 trading days in a year
annualized_volatility <- sd(Results_calspread1$Returns, na.rm = TRUE) * sqrt(252)



# Step 7: Calculate the Sharpe Ratio
  # Calculate the Daily Excess Returns
Results_calspread1$ExcessReturns <- Results_calspread1$Returns - Results_calspread1$RFR

  # Calculate the Mean Daily Excess Retun and Standard Deviation for the Daily Returns 
mean_daily_excess_return_calspread1 <- mean(Results_calspread1$ExcessReturns, na.rm = TRUE)
sd_daily_excess_returns_calspread1 <- sd(Results_calspread1$Returns, na.rm = TRUE)

  # Compute the Daily Sharpe Ratio
daily_sharpe_ratio_calspread1 <- mean_daily_excess_return_calspread1 / sd_daily_excess_returns_calspread1

  # Annualize the Sharpe Ratio
annualized_sharpe_ratio_calspread1 <- daily_sharpe_ratio_calspread1 * sqrt(252)

annualized_sharpe_ratio_calspread1


# Step 8: Calculate the Sortino Ratio
  # Calculate the Downside Deviation
squared_negative_excess_returns <- ifelse(Results_calspread1$ExcessReturns < 0, 
                                          Results_calspread1$ExcessReturns^2, 
                                          0)
mean_squared_negative_excess_returns <- mean(squared_negative_excess_returns, na.rm = TRUE)
downside_deviation <- sqrt(mean_squared_negative_excess_returns)

  # Calculate the Daily Sortino Ratio
daily_sortino_ratio <- mean_daily_excess_return_calspread1 / downside_deviation

  # Annualize the Sortino Ratio
annualized_sortino_ratio <- daily_sortino_ratio * sqrt(252)

annualized_sortino_ratio


#Step 9: Calculate the Calmar Ratio
  # Calculate the Annualized Mean of Daily Excess Returns
annualized_mean_daily_excess_return <- mean(Results_calspread1$ExcessReturns, na.rm = TRUE) * 252

  # Compute the Calmar Ratio using the Annualized Mean of Daily Excess Returns
calmar_ratio_calspread1 <- annualized_mean_daily_excess_return / max_drawdown

  # The result is the Calmar Ratio for your strategy
calmar_ratio_calspread1



# Combine all the results into a single tibble
descrip_stats_results_calspread1 <- tibble(
  Statistics = c("Mean", "Median", "S.D.", "Min", "Max", "Max Drawdown", "Skewness", "Kurtosis", "Cumulative Return", "Annualized Return", "Annualized Volatility", "Annualized Sharpe Ratio", "Annualized Sortino Ratio", "Calmar Ratio"),
  CalSpread1 = c(summary_stats_calspread1$Mean, summary_stats_calspread1$Median, summary_stats_calspread1$S.D., summary_stats_calspread1$Min, summary_stats_calspread1$Max, 
                    max_drawdown, skewness, kurtosis, cumulative_return, annualized_return, annualized_volatility, annualized_sharpe_ratio_calspread1, annualized_sortino_ratio, calmar_ratio_calspread1)
)

# make the dataset visually nicer
# Convert tibble to data frame if it is not already
descrip_stats_results_calspread1 <- as.data.frame(descrip_stats_results_calspread1)

# Set the row names and remove the 'Statistics' column
rownames(descrip_stats_results_calspread1) <- descrip_stats_results_calspread1$Statistics
descrip_stats_results_calspread1 <-descrip_stats_results_calspread1[-1]

# Round the numbers in the CalSpread1 column to 2 decimal places
descrip_stats_results_calspread1$CalSpread1 <- round(descrip_stats_results_calspread1$CalSpread1, 4)

# Print the results to check
print(descrip_stats_results_calspread1)



#Descriptive statistics VPD Index####
# Step 1: Calculate Simple Returns_VPD
Results_calspread1 <- Results_calspread1 %>%
  mutate(Returns_VPD = (VPD - lag(VPD)) / lag(VPD))

# Replace the first NA value in Returns_VPD with 0 or leave as NA depending on your preference
Results_calspread1$Returns_VPD[1] <- 0  # or NA if you prefer to keep it as NA


# Step 2: Generate Summary Statistics
summary_stats_vpd <- Results_calspread1 %>%
  summarise(N.Obs = n(),
            Mean = mean(Returns_VPD, na.rm = TRUE),
            Median = median(Returns_VPD, na.rm = TRUE),
            S.D. = sd(Returns_VPD, na.rm = TRUE),
            Min = min(Returns_VPD, na.rm = TRUE),
            Max = max(Returns_VPD, na.rm = TRUE))


# Step 3: Calculate Max Drawdown
# Initialize variables to track the peak and maximum drawdown
peak = Results_calspread1$VPD[1]
max_drawdown = 0

# Loop through the VPD to find the maximum drawdown
for (value in Results_calspread1$VPD) {
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
skewness <- skewness(Results_calspread1$Returns_VPD, na.rm = TRUE)
kurtosis <- kurtosis(Results_calspread1$Returns_VPD, na.rm = TRUE)


# Step 5: Calculate Cumulative Return
cumulative_return <- last(Results_calspread1$VPD) / first(Results_calspread1$VPD) - 1


# Step 6: Annualize Returns_VPD and Volatility
annualized_return <- (1 + mean(Results_calspread1$Returns_VPD, na.rm = TRUE)) ^ 252 - 1  # Assuming 252 trading days in a year
annualized_volatility <- sd(Results_calspread1$Returns_VPD, na.rm = TRUE) * sqrt(252)


# Step 7: Calculate the Sharpe Ratio
# Calculate the Daily Excess Returns_VPD
Results_calspread1$ExcessReturns_VPD <- Results_calspread1$Returns_VPD - Results_calspread1$RFR

# Calculate the Mean Daily Excess Retun and Standard Deviation for the Daily Returns_VPD 
mean_daily_excess_return_vpd <- mean(Results_calspread1$ExcessReturns_VPD, na.rm = TRUE)
sd_daily_excess_Returns_VPD <- sd(Results_calspread1$Returns_VPD, na.rm = TRUE)

# Compute the Daily Sharpe Ratio
daily_sharpe_ratio <- mean_daily_excess_return_vpd / sd_daily_excess_Returns_VPD

# Annualize the Sharpe Ratio
annualized_sharpe_ratio <- daily_sharpe_ratio * sqrt(252)

annualized_sharpe_ratio


# Step 8: Calculate the Sortino Ratio
# Calculate the Downside Deviation
squared_negative_excess_Returns_VPD <- ifelse(Results_calspread1$ExcessReturns_VPD < 0, 
                                              Results_calspread1$ExcessReturns_VPD^2, 
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
annualized_mean_daily_excess_return <- mean(Results_calspread1$ExcessReturns_VPD, na.rm = TRUE) * 252

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
Results_calspread1 <- Results_calspread1 %>%
  mutate(Returns_SPX = (SPX - lag(SPX)) / lag(SPX))

# Replace the first NA value in Returns_SPX with 0 or leave as NA depending on your preference
Results_calspread1$Returns_SPX[1] <- 0  # or NA if you prefer to keep it as NA


# Step 2: Generate Summary Statistics
summary_stats_spx <- Results_calspread1 %>%
  summarise(N.Obs = n(),
            Mean = mean(Returns_SPX, na.rm = TRUE),
            Median = median(Returns_SPX, na.rm = TRUE),
            S.D. = sd(Returns_SPX, na.rm = TRUE),
            Min = min(Returns_SPX, na.rm = TRUE),
            Max = max(Returns_SPX, na.rm = TRUE))


# Step 3: Calculate Max Drawdown
# Initialize variables to track the peak and maximum drawdown
peak = Results_calspread1$SPX[1]
max_drawdown = 0

# Loop through the SPX to find the maximum drawdown
for (value in Results_calspread1$SPX) {
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
skewness <- skewness(Results_calspread1$Returns_SPX, na.rm = TRUE)
kurtosis <- kurtosis(Results_calspread1$Returns_SPX, na.rm = TRUE)


# Step 5: Calculate Cumulative Return
cumulative_return <- last(Results_calspread1$SPX) / first(Results_calspread1$SPX) - 1


# Step 6: Annualize Returns_SPX and Volatility
annualized_return <- (1 + mean(Results_calspread1$Returns_SPX, na.rm = TRUE)) ^ 252 - 1  # Assuming 252 trading days in a year
annualized_volatility <- sd(Results_calspread1$Returns_SPX, na.rm = TRUE) * sqrt(252)


# Step 7: Calculate the Sharpe Ratio
# Calculate the Daily Excess Returns_SPX
Results_calspread1$ExcessReturns_SPX <- Results_calspread1$Returns_SPX - Results_calspread1$RFR

# Calculate the Mean Daily Excess Retun and Standard Deviation for the Daily Returns_SPX 
mean_daily_excess_return_spx <- mean(Results_calspread1$ExcessReturns_SPX, na.rm = TRUE)
sd_daily_excess_Returns_SPX <- sd(Results_calspread1$Returns_SPX, na.rm = TRUE)

# Compute the Daily Sharpe Ratio
daily_sharpe_ratio <- mean_daily_excess_return_spx / sd_daily_excess_Returns_SPX

# Annualize the Sharpe Ratio
annualized_sharpe_ratio <- daily_sharpe_ratio * sqrt(252)

annualized_sharpe_ratio


# Step 8: Calculate the Sortino Ratio
# Calculate the Downside Deviation
squared_negative_excess_Returns_SPX <- ifelse(Results_calspread1$ExcessReturns_SPX < 0, 
                                              Results_calspread1$ExcessReturns_SPX^2, 
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
annualized_mean_daily_excess_return <- mean(Results_calspread1$ExcessReturns_SPX, na.rm = TRUE) * 252

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
model_calspread1 <- lm(Returns ~ Returns_SPX, data = Results_calspread1)
beta_calspread1 <- coef(model_calspread1)["Returns_SPX"]  # Extract the beta coefficient
# Step 2: Calculate Alpha
# The RFR is already in the dataframe and adjusted for daily use.
annualized_market_return <- mean(Results_calspread1$Returns_SPX, na.rm = TRUE) * 252  # Annualize SPX returns
# Annualize strategy returns for comparison
annualized_strategy_return <- mean(Results_calspread1$Returns, na.rm = TRUE) * 252
# Annualize the mean daily RFR
annualized_rfr <- mean(Results_calspread1$RFR, na.rm = TRUE) * 360
# Calculate alpha using the CAPM formula
alpha_calspread1 <- annualized_strategy_return - (annualized_rfr + beta_calspread1 * (annualized_market_return - annualized_rfr))

# Output the results
cat("Beta: ", beta_calspread1, "\n")
cat("Alpha: ", alpha_calspread1, "\n")

# Add to other stats
new_rows <- data.frame(CalSpread1 = c(alpha_calspread1, beta_calspread1))
rownames(new_rows) <- c("Alpha", "Beta")

# Combine with the original results
# Use rbind to append the data, this assumes descrip_stats_results_calspread1 is a data frame with CalSpread1_75 as its column
descrip_stats_results_calspread1 <- rbind(descrip_stats_results_calspread1, new_rows)

# Round the CalSpread1 column
descrip_stats_results_calspread1$CalSpread1 <- round(descrip_stats_results_calspread1$CalSpread1, 4)

# Print the dataframe to check
print(descrip_stats_results_calspread1)




# SPX
alpha_SPX <- 0
beta_SPX <- 1

# Add to other stats
new_rows <- data.frame(SPX = c(alpha_SPX, beta_SPX))
rownames(new_rows) <- c("Alpha", "Beta")

# Combine with the original results
# Use rbind to append the data, this assumes Results_calspread1 is a data frame with SPX as its column
results_spx <- rbind(results_spx, new_rows)

# Round the SPX column
results_spx$SPX <- round(results_spx$SPX, 4)

# Print the dataframe to check
print(results_spx)




# VPD Index
# Step 1: Run the regression to calculate Beta
model_vpdindex <- lm(Returns_VPD ~ Returns_SPX, data = Results_calspread1)
beta_vpdindex <- coef(model_vpdindex)["Returns_SPX"]  # Extract the beta coefficient

# Step 2: Calculate Alpha
# The RFR is already in the dataframe and adjusted for daily use.
annualized_market_return <- mean(Results_calspread1$Returns_SPX, na.rm = TRUE) * 252  # Annualize SPX returns
# Annualize strategy returns for comparison
annualized_putindex_return <- mean(Results_calspread1$Returns_VPD, na.rm = TRUE) * 252
# Annualize the mean daily RFR
annualized_rfr <- mean(Results_calspread1$RFR, na.rm = TRUE) * 360
# Calculate alpha using the CAPM formula
alpha_putindex <- annualized_putindex_return - (annualized_rfr + beta_vpdindex * (annualized_market_return - annualized_rfr))

# Output the results
cat("Beta: ", beta_vpdindex, "\n")
cat("Alpha: ", alpha_putindex, "\n")

# Add to other stats
new_rows <- data.frame(VPD_Index = c(alpha_putindex, beta_vpdindex))
rownames(new_rows) <- c("Alpha", "Beta")

# Combine with the original results
# Use rbind to append the data, this assumes Results_calspread1 is a data frame with VPD_Index as its column
results_vpd <- rbind(results_vpd, new_rows)

# Round the VPD_Index column
results_vpd$VPD_Index <- round(results_vpd$VPD_Index, 4)

# Print the dataframe to check
print(results_vpd)

#All Descriptive Stats Table ####
# Combine the data frames by columns
combined_results <- cbind(descrip_stats_results_calspread1, results_vpd, results_spx)

# Rename the columns to reflect the source tibble if needed
colnames(combined_results) <- c("CalSpread1", "VPD Index", "S&P 500")

# Print the combined results to check
print(combined_results)




