###################################

#Short 1 

#Fabian Welti 
#20.03.2024

##################################

#WD ####
setwd("/Users/lucaswelti/Documents/Data BA Thesis /Options")

#Packages ####
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(readxl)
library(tibble)
library(PerformanceAnalytics)
library(stats)
options(scipen = 999)



#Read in the data ####
# Overall dataset - containing options with max 30 days to maturity 
# Read the CSV file into a dataset 
file_name <- "jap4rpqoavcoxozr.csv"
Options_data <- read_csv(file_name)

#Dataset for Short1 strategy ####
# filter out only put options 
short1_data <- Options_data %>% 
  filter(cp_flag != "C")



#Create Dateset for all the expiration dates of SPX options since 08.01.1996-30.03.2023 ####
# Define the start and end dates
start_date <- as.Date("1996-01-08")
end_date <- as.Date("2023-03-30")

# Generate sequence of dates from start to end
all_dates <- seq.Date(start_date, end_date, by = "day")

# Filter for Fridays
fridays <- all_dates[weekdays(all_dates) == "Friday"]

# Initialize an empty vector to store the third Fridays
third_fridays <- as.Date(character())

# Loop through each year and month to find the third Fridays
for (year in unique(year(fridays))) {
  for (month in 1:12) {
    # Get all the Fridays for the particular year and month
    month_fridays <- fridays[year(fridays) == year & month(fridays) == month]
    
    # If there are at least three Fridays in that month, take the third one
    if (length(month_fridays) >= 3) {
      third_fridays <- c(third_fridays, month_fridays[3])
    }
  }
}

# Create a data frame to hold the dates
expdate <- data.frame(Date = third_fridays)

# Ensure the 'Date' column is of type Date
expdate$Date <- as.Date(expdate$Date)








#Filter out the monthly expiration contracts ####
# Add one day to each date in the 'expdate' to account for the following day
next_days <- expdate$Date + 1
# Combine the original third Friday dates with the next days into one vector
all_relevant_dates <- unique(c(expdate$Date, next_days))
# Filter 'short1_data' to keep only rows where 'exdate' matches one of the all_relevant_dates
short1_data <- short1_data %>%
  filter(as.Date(exdate, format = "%Y%m%d") %in% all_relevant_dates)



#Combine S&P 500 prices with options dataset ####
spx_data <- read_excel("^SPX 1990-12.01.2024.xlsx", skip = 1)
spx_data$Date <- as.Date(spx_data$Date, format = "%Y-%m-%d") #converting the Date column 

spx_data_selected <- select(spx_data, Date, Open, Close)
short1_data <- left_join(short1_data, spx_data_selected, by = c("date"="Date"))



#Calculate the absolute difference from Strike to S&P Close ####
short1_data$abs_diff <- abs(short1_data$Close - (short1_data$strike_price)/1000)



#Clean the dateset ####
# Filter the dataset to exclude rows where 'date' is equal to 'exdate' since want to close posiotion before
short1_data <- short1_data %>%
  filter(date != exdate)





#Filter out the ATM contracts ####
# Step 1: Find days where position is closed
# Add a new column 'next_exdate' that contains the exdate of the next row
short1_data <- short1_data %>%
  mutate(next_exdate = lead(exdate))

# Compare 'exdate' with 'next_exdate' to determine if the position should be closed
# Assign TRUE if exdate is different from next_exdate (position should be closed)
# The last row will automatically get NA for 'next_exdate', so replace NA with TRUE to close the position
short1_data <- short1_data %>%
  mutate(close_position = if_else(exdate != next_exdate, TRUE, FALSE)) %>%
  mutate(close_position = replace_na(close_position, TRUE))

# Remove the temporary 'next_exdate' column
short1_data <- select(short1_data, -next_exdate)

# Identify all unique dates where close_position is TRUE
dates_to_mark <- unique(short1_data$date[short1_data$close_position == TRUE])

# Update close_position to TRUE for all rows that have a matching date in dates_to_mark
short1_data <- short1_data %>%
  mutate(close_position = if_else(date %in% dates_to_mark, TRUE, close_position))


# Step 2: Find days where position is opened
# Add a new column 'prev_exdate' that contains the exdate of the previous row
short1_data <- short1_data %>%
  mutate(prev_exdate = lag(exdate))

# Compare 'exdate' with 'prev_exdate' to determine if a new position starts
# Assign TRUE if exdate is different from prev_exdate (indicating a new position)
# The first row will automatically get NA for 'prev_exdate', so replace NA with TRUE to indicate a new position
short1_data <- short1_data %>%
  mutate(new_position = if_else(exdate != prev_exdate, TRUE, FALSE)) %>%
  mutate(new_position = replace_na(new_position, TRUE))

# Remove the temporary 'prev_exdate' column
short1_data <- select(short1_data, -prev_exdate)

# Identify all unique dates where new_position is TRUE
dates_with_new_position <- unique(short1_data$date[short1_data$new_position == TRUE])

# Update new_position to TRUE for all rows that have a matching date in dates_with_new_position
short1_data <- short1_data %>%
  mutate(new_position = if_else(date %in% dates_with_new_position, TRUE, new_position))



# Step 3: Find the ATM contract for days where new position is opened
# Add an identifier for each row to assist in marking rows to keep
short1_data <- short1_data %>%
  mutate(row_id = row_number())

# Identify the row with the smallest abs_diff for each date where new_position is TRUE
min_abs_diff_rows <- short1_data %>%
  filter(new_position == TRUE) %>%
  group_by(date) %>%
  slice_min(abs_diff, n = 1) %>%
  ungroup() %>%
  pull(row_id)  # Get the row IDs of the rows with the minimum abs_diff

# Mark rows to keep: rows with the smallest abs_diff in their date group where new_position is TRUE, and all rows where new_position is FALSE
short1_data <- short1_data %>%
  mutate(keep = row_id %in% min_abs_diff_rows | new_position == FALSE)

# Filter the dataset to keep only the marked rows
short1_data_filtered <- short1_data %>%
  filter(keep) %>%
  select(-row_id, -keep)  # Remove the helper columns




# Step 4: Find the ATM contract for normal days
# Step 4.1: Add helper columns to track the last 'strike_price' and 'optionid' where 'new_position' is TRUE
short1_data_filtered <- short1_data_filtered %>%
  mutate(last_new_strike = ifelse(new_position == TRUE, strike_price, NA),
         last_new_optionid = ifelse(new_position == TRUE, optionid, NA)) %>%
  fill(last_new_strike, .direction = "down") %>%
  fill(last_new_optionid, .direction = "down")

# Step 4.2: Filter out rows where both 'close_position' and 'new_position' are FALSE
# and either 'strike_price' or 'optionid' doesn't match their respective last values
short1_data_filtered <- short1_data_filtered %>%
  filter(!(close_position == FALSE & new_position == FALSE & 
             (strike_price != last_new_strike | optionid != last_new_optionid)))

# Step 4.3: Remove the helper columns
short1_data_filtered <- select(short1_data_filtered, -last_new_strike, -last_new_optionid)





# Step 5: Find the ATM contract for days where contract is closed
# Step 5.1: Create helper columns for the previous 'strike_price' and 'optionid'
# Initialize them with NA and then fill in the values from rows where both 'close_position' and 'new_position' are FALSE
short1_data_filtered <- short1_data_filtered %>%
  mutate(prev_strike_price = ifelse(close_position == FALSE & new_position == FALSE, strike_price, NA),
         prev_optionid = ifelse(close_position == FALSE & new_position == FALSE, optionid, NA)) %>%
  fill(prev_strike_price, .direction = "down") %>%
  fill(prev_optionid, .direction = "down")

# Step 5.2: Apply the logic for keeping rows based on 'close_position', 'strike_price', and 'optionid'
# Mark rows to keep when 'close_position' is TRUE and both 'strike_price' and 'optionid' match their previous values
short1_data_filtered <- short1_data_filtered %>%
  mutate(keep = ifelse(close_position == TRUE,
                       (strike_price == prev_strike_price & optionid == prev_optionid),
                       TRUE))

# Step 5.3: Filter the dataset to keep only the marked rows and remove helper columns
short1_data_filtered <- short1_data_filtered %>%
  filter(keep) %>%
  select(-prev_strike_price, -prev_optionid, -keep)




#New open position logic as the dateset is now cleaned ####
# Step 1: Add a column for the previous 'exdate'
short1_data_filtered <- short1_data_filtered %>%
  mutate(prev_exdate = lag(exdate))
# Step 2: Update 'new_position' to TRUE when 'exdate' changes from the previous row
short1_data_filtered <- short1_data_filtered %>%
  mutate(new_position = ifelse(exdate != prev_exdate | row_number() == 1, TRUE, new_position)) #ensure that the first row will be an entry
# Step 3: Remove the helper 'prev_exdate' column
short1_data_filtered <- select(short1_data_filtered, -prev_exdate)




#New close position logic as the dateset is now cleaned ####
# Step 1: Add a column for the 'new_position' of the next row
short1_data_filtered <- short1_data_filtered %>%
  mutate(next_new_position = lead(new_position))
# Step 2: Update 'close_position' to TRUE when the next row's 'new_position' is TRUE
short1_data_filtered <- short1_data_filtered %>%
  mutate(close_position = ifelse(next_new_position == TRUE, TRUE, close_position))
# Step 3: Remove the helper 'next_new_position' column
short1_data_filtered <- select(short1_data_filtered, -next_new_position)
# Step 4: Close position last row
short1_data_filtered <- short1_data_filtered %>%
  mutate(close_position = replace(close_position, n(), TRUE))



#Clean the dataset
# Filter out rows where both 'close_position' and 'new_position' are TRUE
short1_data_filtered <- short1_data_filtered %>%
  filter(!(close_position == TRUE & new_position == TRUE))

# View the result to confirm the rows have been removed
head(short1_data_filtered)








#Implement the Strategy ####
#Step 1: Add Treasury Data to Dataset
# Import Data
treasury_data <- read.csv("Treasury.csv") 
# Clean Data
# Convert the 'DATE' column in treasury_data to Date type
treasury_data <- treasury_data %>%
  mutate(DATE = as.Date(DATE, format = "%Y-%m-%d"))  # Adjust the format string as necessary
# Rename the DTB3 column to RFR in treasury_data before joining
treasury_data <- treasury_data %>%
  rename(RFR = DTB3)
# Convert the RFR column to numeric and replace NA values with 0
treasury_data <- treasury_data %>%
  mutate(RFR = as.numeric(RFR),
         RFR = ifelse(is.na(RFR), 0, RFR))
#Convert the RFR from percentage to a daily rate by dividing by 360
treasury_data$RFR <- treasury_data$RFR / 36000 # Dividing by 100 to get rate and by 360 for daily rate
# Merge
# Perform a left join to merge the RFR values from treasury_data into short1_data_filtered
short1_data_filtered <- short1_data_filtered %>%
  left_join(treasury_data, by = c("date" = "DATE"))



#Step 2: Midpoint price
# Calculate the midpoint price for a short put
short1_data_filtered <- short1_data_filtered %>%
  mutate(midpoint_price = ifelse(close_position == TRUE,
                                 (1/3) * best_bid + (2/3) *best_offer,
                                 (2/3) * best_bid + (1/3) * best_offer))


#Step 3: Calculate the daily PnL of selling the ATM put for each month
short1_data_filtered <- short1_data_filtered %>%
  mutate(daily_pnl = ifelse(new_position == TRUE, 
                            0, 
                            -100 * (midpoint_price - lag(midpoint_price))))






# Step 4: Implement the strategy over the data (adjusted contract sizes)
# Initialize MarginAccount for the first row and N_contracts based on the initial MarginAccount
short1_data_filtered$MarginAccount[1] <- 1000000
# Initial N_contracts based on the adjusted formula for the first row's MarginAccount and strike_price
short1_data_filtered$N_contracts[1] <- floor((short1_data_filtered$MarginAccount[1] / (0.75 * short1_data_filtered$strike_price[1])) * 10)

# Define the cost per contract per trade
cost_per_contract <- 1.10

# Loop through the dataset starting from the second row
for (i in 2:nrow(short1_data_filtered)) {
  # Update N_contracts for rows where new_position is TRUE using the new formula
  if (short1_data_filtered$new_position[i] == TRUE) {
    short1_data_filtered$N_contracts[i] <- floor((short1_data_filtered$MarginAccount[i-1] / (0.75 * short1_data_filtered$strike_price[i])) * 10)
  } else {
    # Carry forward the N_contracts value from the previous row if new_position is not TRUE
    short1_data_filtered$N_contracts[i] <- short1_data_filtered$N_contracts[i-1]
  }
  
  # Calculate the cost of opening or closing positions
  if (short1_data_filtered$new_position[i] == TRUE || short1_data_filtered$close_position[i] == TRUE) {
    trade_cost <- short1_data_filtered$N_contracts[i] * cost_per_contract
  } else {
    trade_cost <- 0
  }
  
  # Calculate MarginAccount for each row accounting for the transaction costs
  short1_data_filtered$MarginAccount[i] <- (1 + short1_data_filtered$RFR[i-1]) * 
    short1_data_filtered$MarginAccount[i-1] + 
    short1_data_filtered$N_contracts[i] * short1_data_filtered$daily_pnl[i] - trade_cost  # Subtracting the trade cost here
}

# Close position last row
short1_data_filtered <- short1_data_filtered %>%
  mutate(close_position = replace(close_position, n(), TRUE))








#Calculate the Margin Requirement ####
# Add the Margin_Requirement calculation to the dataset
short1_data_filtered <- short1_data_filtered %>%
  mutate(
    # Calculate the Out of the Money Amount, ensuring it's non-negative
    Out_of_the_Money_Amount = pmax(Close - (strike_price / 1000), 0),
    
    # Calculate the two components of the margin requirement
    Margin_Component_1 = (0.15 * Close - Out_of_the_Money_Amount),
    Margin_Component_2 = (0.10 * (strike_price / 1000)),
    
    # Determine the maximum of the two margin components and add the Put Price
    Margin_Requirement = 100*(N_contracts*(midpoint_price + pmax(Margin_Component_1, Margin_Component_2)))
  )

# View the updated dataset to verify the new Margin_Requirement column
head(short1_data_filtered)






#Put Index ####
# Step 1: Import the PUT index data and calculate daily PnL
# Import Put Index data
put_data <- read_excel("Put Index.xlsx")
# Calculate daily PnL for Put Index
put_data <- put_data %>%
  mutate(daily_pnl_put = `PUT Index` - lag(`PUT Index`))


# Step 2: Merge the Datesets
short1_data_filtered <- left_join(short1_data_filtered, put_data, by = c("date"="Date"))
# Replace NA values in 'daily_pnl_put' with 0
short1_data_filtered$daily_pnl_put <- replace_na(short1_data_filtered$daily_pnl_put, 0)


# Step 3: Calculate the development of 100k 
# Calculate the initial contract size 'x' using the correct column name "PUT Index"
x <- round(1000000 / short1_data_filtered$`PUT Index`[1], digits = 0)

# Initialize the 'Put_Strategy' column with the first value as the initial investment
short1_data_filtered$Put_Strategy <- rep(0, nrow(short1_data_filtered))
short1_data_filtered$Put_Strategy[1] <- 1000000  # Set the initial value for the first row

# Loop through the dataset starting from the second row
for (i in 2:nrow(short1_data_filtered)) {
  short1_data_filtered$Put_Strategy[i] <- x * short1_data_filtered$daily_pnl_put[i] + short1_data_filtered$Put_Strategy[i - 1]
}


#Development of Investment in S&P 500 ####
# Step 1: Calculate daily PnL for SPX
short1_data_filtered <- short1_data_filtered %>%
  mutate(daily_pnl_spx = Close - lag(Close))
# Set the first value of 'daily_pnl_spx' to 0
short1_data_filtered$daily_pnl_spx[1] <- 0
# Replace NA values in 'daily_pnl_spx' with 0
short1_data_filtered$daily_pnl_spx <- replace_na(short1_data_filtered$daily_pnl_spx, 0)

# Step 2: Calculate the development of 100k 
# Calculate the initial contract size 'x' using the correct column name "PUT Index"
x <- round(1000000 / short1_data_filtered$Close[1], digits = 0)

# Initialize the 'SPX' column with the first value as the initial investment
short1_data_filtered$SPX <- rep(0, nrow(short1_data_filtered))
short1_data_filtered$SPX[1] <- 1000000  # Set the initial value for the first row

# Loop through the dataset starting from the second row
for (i in 2:nrow(short1_data_filtered)) {
  short1_data_filtered$SPX[i] <- x * short1_data_filtered$daily_pnl_spx[i] + short1_data_filtered$SPX[i - 1]
}








#Plot the results ####
# Calculate the upper limit for y-axis based on the maximum values of the plotted series
upper_limit <- max(c(max(short1_data_filtered$MarginAccount, na.rm = TRUE), 
                     max(short1_data_filtered$Put_Strategy, na.rm = TRUE),
                     max(short1_data_filtered$SPX, na.rm = TRUE),
                     max(short1_data_filtered$Margin_Requirement, na.rm = TRUE)))

# Define y-axis range starting from 0 to the upper limit
y_range <- c(0, upper_limit)


par(family="Times New Roman")

# Set up the plot with the first line to establish the plot area
plot(short1_data_filtered$date, short1_data_filtered$MarginAccount, type = 'l', 
     col = "darkblue", ylim = y_range, xaxt = 'n',    # Disable default x-axis
     xlab = "", ylab = "Development of $1'000'000", 
     main = "Short1 Performance", lwd = 2)  # Increase line width with lwd

# Add the remaining series with increased line width
lines(short1_data_filtered$date, short1_data_filtered$Put_Strategy, col = "red", lwd = 2)
lines(short1_data_filtered$date, short1_data_filtered$SPX, col = "black", lwd = 2)
lines(short1_data_filtered$date, short1_data_filtered$Margin_Requirement, col = "deepskyblue4") 

# Custom x-axis labels for each year
year_ticks <- seq(from=min(short1_data_filtered$date, na.rm=TRUE),
                  to=max(short1_data_filtered$date, na.rm=TRUE), by="years")
axis(1, at=year_ticks, labels=format(year_ticks, "%Y"), cex.axis=0.8, las=2)

# Add light gridlines
abline(h=seq(from=min(y_range, na.rm = TRUE), to=max(y_range, na.rm = TRUE), 
             by=(max(y_range, na.rm = TRUE)-min(y_range, na.rm = TRUE))/5), 
       col="grey", lty="dotted", lwd=0.5)

abline(v=year_ticks, col="grey", lty="dotted", lwd=0.5)

# Add a legend without a box ('n' means no box around the legend)
legend("topleft", legend = c("Short1", "PUT", "S&P 500", "Margin Requirement Short 1"),
       col = c("darkblue", "red", "black", "deepskyblue4"), lty = 1, bty = "n", cex = 0.8, lwd=c(2,2,2,1))





#Descriptive statistics Short1 ####

# Step 1: Calculate Simple Returns
short1_data_filtered <- short1_data_filtered %>%
  mutate(Returns = (MarginAccount - lag(MarginAccount)) / lag(MarginAccount))

# Replace the first NA value in Returns with 0 
short1_data_filtered$Returns[1] <- 0  


# Step 2: Generate Summary Statistics
summary_stats_short1 <- short1_data_filtered %>%
  summarise(N.Obs = n(),
            Mean = mean(Returns, na.rm = TRUE),
            Median = median(Returns, na.rm = TRUE),
            S.D. = sd(Returns, na.rm = TRUE),
            Min = min(Returns, na.rm = TRUE),
            Max = max(Returns, na.rm = TRUE))


# Step 3: Calculate Max Drawdown
# Initialize variables to track the peak and maximum drawdown
peak = short1_data_filtered$MarginAccount[1]
max_drawdown = 0

for (value in short1_data_filtered$MarginAccount) {
  # Skip the iteration if the value is NA
  if (is.na(value)) {
    next
  }
  
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
skewness <- skewness(short1_data_filtered$Returns, na.rm = TRUE)
kurtosis <- kurtosis(short1_data_filtered$Returns, na.rm = TRUE)


# Step 5: Calculate Cumulative Return
cumulative_return <- last(short1_data_filtered$MarginAccount) / first(short1_data_filtered$MarginAccount) - 1


# Step 6: Annualize Returns and Volatility
annualized_return <- (1 + mean(short1_data_filtered$Returns, na.rm = TRUE)) ^ 252 - 1  # Assuming 252 trading days in a year
annualized_volatility <- sd(short1_data_filtered$Returns, na.rm = TRUE) * sqrt(252)


# Step 7: Calculate the Sharpe Ratio
# Calculate the Daily Excess Returns
short1_data_filtered$ExcessReturns <- short1_data_filtered$Returns - short1_data_filtered$RFR

# Calculate the Mean Daily Excess Retun and Standard Deviation for the Daily Returns 
mean_daily_excess_return_short1 <- mean(short1_data_filtered$ExcessReturns, na.rm = TRUE)
sd_daily_excess_returns_short1 <- sd(short1_data_filtered$Returns, na.rm = TRUE)

# Compute the Daily Sharpe Ratio
daily_sharpe_ratio_short1 <- mean_daily_excess_return_short1 / sd_daily_excess_returns_short1

# Annualize the Sharpe Ratio
annualized_sharpe_ratio_short1 <- daily_sharpe_ratio_short1 * sqrt(252)

annualized_sharpe_ratio_short1


# Step 8: Calculate the Sortino Ratio
# Calculate the Downside Deviation
squared_negative_excess_returns <- ifelse(short1_data_filtered$ExcessReturns < 0, 
                                          short1_data_filtered$ExcessReturns^2, 
                                          0)
mean_squared_negative_excess_returns <- mean(squared_negative_excess_returns, na.rm = TRUE)
downside_deviation <- sqrt(mean_squared_negative_excess_returns)

# Calculate the Daily Sortino Ratio
daily_sortino_ratio <- mean_daily_excess_return_short1 / downside_deviation

# Annualize the Sortino Ratio
annualized_sortino_ratio <- daily_sortino_ratio * sqrt(252)

annualized_sortino_ratio


#Step 9: Calculate the Calmar Ratio
# Calculate the Annualized Mean of Daily Excess Returns
annualized_mean_daily_excess_return <- mean(short1_data_filtered$ExcessReturns, na.rm = TRUE) * 252

# Compute the Calmar Ratio using the Annualized Mean of Daily Excess Returns
calmar_ratio_short1 <- annualized_mean_daily_excess_return / max_drawdown

# The result is the Calmar Ratio for your strategy
calmar_ratio_short1


#Step 10: Number of Trades
number_true_close_position <- sum(short1_data_filtered$close_position, na.rm = TRUE)
number_true_new_position <- sum(short1_data_filtered$new_position, na.rm = TRUE)

# Calculate the overall total and name it N. Trades
n_trades <- number_true_close_position + number_true_new_position

# Print out N. Trades
cat("N. Trades:", n_trades, "\n")




# Combine all the results into a single tibble
results_short1 <- tibble(
  Statistics = c("Mean", "Median", "S.D.", "Min", "Max", "Max Drawdown", "Skewness", "Kurtosis", "Cumulative Return", "Annualized Return", "Annualized Volatility", "Annualized Sharpe Ratio", "Annualized Sortino Ratio", "Calmar Ratio"),
  Short1 = c(summary_stats_short1$Mean, summary_stats_short1$Median, summary_stats_short1$S.D., summary_stats_short1$Min, summary_stats_short1$Max, 
                max_drawdown, skewness, kurtosis, cumulative_return, annualized_return, annualized_volatility, annualized_sharpe_ratio_short1, annualized_sortino_ratio, calmar_ratio_short1)
)

# make the dataset visually nicer
# Convert tibble to data frame if it is not already
results_short1 <- as.data.frame(results_short1)

# Set the row names and remove the 'Statistics' column
rownames(results_short1) <- results_short1$Statistics
results_short1 <-results_short1[-1]

# Round the numbers in the Short1 column to 2 decimal places
results_short1$Short1 <- round(results_short1$Short1, 4)

# Print the results to check
print(results_short1)




#Descriptive statistics Put Index####
# Step 1: Calculate Simple Returns_Put
short1_data_filtered <- short1_data_filtered %>%
  mutate(Returns_Put = (Put_Strategy - lag(Put_Strategy)) / lag(Put_Strategy))

# Replace the first NA value in Returns_Put with 0 
short1_data_filtered$Returns_Put[1] <- 0  


# Step 2: Generate Summary Statistics
summary_stats_put <- short1_data_filtered %>%
  summarise(N.Obs = n(),
            Mean = mean(Returns_Put, na.rm = TRUE),
            Median = median(Returns_Put, na.rm = TRUE),
            S.D. = sd(Returns_Put, na.rm = TRUE),
            Min = min(Returns_Put, na.rm = TRUE),
            Max = max(Returns_Put, na.rm = TRUE))


# Step 3: Calculate Max Drawdown
# Initialize variables to track the peak and maximum drawdown
peak = short1_data_filtered$Put_Strategy[1]
max_drawdown = 0

# Loop through the Put_Strategy to find the maximum drawdown
for (value in short1_data_filtered$Put_Strategy) {
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
skewness <- skewness(short1_data_filtered$Returns_Put, na.rm = TRUE)
kurtosis <- kurtosis(short1_data_filtered$Returns_Put, na.rm = TRUE)


# Step 5: Calculate Cumulative Return
cumulative_return <- last(short1_data_filtered$Put_Strategy) / first(short1_data_filtered$Put_Strategy) - 1


# Step 6: Annualize Returns_Put and Volatility
annualized_return <- (1 + mean(short1_data_filtered$Returns_Put, na.rm = TRUE)) ^ 252 - 1  # Assuming 252 trading days in a year
annualized_volatility <- sd(short1_data_filtered$Returns_Put, na.rm = TRUE) * sqrt(252)


# Step 7: Calculate the Sharpe Ratio
# Calculate the Daily Excess Returns_Put
short1_data_filtered$ExcessReturns_Put <- short1_data_filtered$Returns_Put - short1_data_filtered$RFR

# Calculate the Mean Daily Excess Retun and Standard Deviation for the Daily Returns_Put 
mean_daily_excess_return_put <- mean(short1_data_filtered$ExcessReturns_Put, na.rm = TRUE)
sd_daily_excess_Returns_Put <- sd(short1_data_filtered$Returns_Put, na.rm = TRUE)

# Compute the Daily Sharpe Ratio
daily_sharpe_ratio <- mean_daily_excess_return_put / sd_daily_excess_Returns_Put

# Annualize the Sharpe Ratio
annualized_sharpe_ratio <- daily_sharpe_ratio * sqrt(252)

annualized_sharpe_ratio


# Step 8: Calculate the Sortino Ratio
# Calculate the Downside Deviation
squared_negative_excess_Returns_Put <- ifelse(short1_data_filtered$ExcessReturns_Put < 0, 
                                              short1_data_filtered$ExcessReturns_Put^2, 
                                              0)
mean_squared_negative_excess_Returns_Put <- mean(squared_negative_excess_Returns_Put, na.rm = TRUE)
downside_deviation <- sqrt(mean_squared_negative_excess_Returns_Put)

# Calculate the Daily Sortino Ratio
daily_sortino_ratio <- mean_daily_excess_return_put / downside_deviation

# Annualize the Sortino Ratio
annualized_sortino_ratio <- daily_sortino_ratio * sqrt(252)

annualized_sortino_ratio


#Step 9: Calculate the Calmar Ratio
# Calculate the Annualized Mean of Daily Excess Returns_Put
annualized_mean_daily_excess_return <- mean(short1_data_filtered$ExcessReturns_Put, na.rm = TRUE) * 252

# Compute the Calmar Ratio using the Annualized Mean of Daily Excess Returns_Put
calmar_ratio_put <- annualized_mean_daily_excess_return / max_drawdown

# The result is the Calmar Ratio for your strategy
calmar_ratio_put





# Combine all the results into a single tibble
results_put <- tibble(
  Statistics = c("Mean", "Median", "S.D.", "Min", "Max", "Max Drawdown", "Skewness", "Kurtosis", "Cumulative Return", "Annualized Return", "Annualized Volatility", "Annualized Sharpe Ratio", "Annualized Sortino Ratio", "Calmar Ratio"),
  Put_Index = c(summary_stats_put$Mean, summary_stats_put$Median, summary_stats_put$S.D., summary_stats_put$Min, summary_stats_put$Max, 
                max_drawdown, skewness, kurtosis, cumulative_return, annualized_return, annualized_volatility, annualized_sharpe_ratio, annualized_sortino_ratio, calmar_ratio_put)
)

# make the dataset visually nicer
# Convert tibble to data frame if it is not already
results_put <- as.data.frame(results_put)

# Set the row names and remove the 'Statistics' column
rownames(results_put) <- results_put$Statistics
results_put <-results_put[-1]

# Round the numbers in the Put_Index column to 2 decimal places
results_put$Put_Index <- round(results_put$Put_Index, 4)

# Print the results to check
print(results_put)




#Descriptive statistics SPX ####
# Step 1: Calculate Simple Returns_SPX
short1_data_filtered <- short1_data_filtered %>%
  mutate(Returns_SPX = (SPX - lag(SPX)) / lag(SPX))

# Replace the first NA value in Returns_SPX with 0 
short1_data_filtered$Returns_SPX[1] <- 0  


# Step 2: Generate Summary Statistics
summary_stats_spx <- short1_data_filtered %>%
  summarise(N.Obs = n(),
            Mean = mean(Returns_SPX, na.rm = TRUE),
            Median = median(Returns_SPX, na.rm = TRUE),
            S.D. = sd(Returns_SPX, na.rm = TRUE),
            Min = min(Returns_SPX, na.rm = TRUE),
            Max = max(Returns_SPX, na.rm = TRUE))


# Step 3: Calculate Max Drawdown
# Initialize variables to track the peak and maximum drawdown
peak = short1_data_filtered$SPX[1]
max_drawdown = 0

# Loop through the SPX to find the maximum drawdown
for (value in short1_data_filtered$SPX) {
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
skewness <- skewness(short1_data_filtered$Returns_SPX, na.rm = TRUE)
kurtosis <- kurtosis(short1_data_filtered$Returns_SPX, na.rm = TRUE)


# Step 5: Calculate Cumulative Return
cumulative_return <- last(short1_data_filtered$SPX) / first(short1_data_filtered$SPX) - 1


# Step 6: Annualize Returns_SPX and Volatility
annualized_return <- (1 + mean(short1_data_filtered$Returns_SPX, na.rm = TRUE)) ^ 252 - 1  # Assuming 252 trading days in a year
annualized_volatility <- sd(short1_data_filtered$Returns_SPX, na.rm = TRUE) * sqrt(252)


# Step 7: Calculate the Sharpe Ratio
# Calculate the Daily Excess Returns_SPX
short1_data_filtered$ExcessReturns_SPX <- short1_data_filtered$Returns_SPX - short1_data_filtered$RFR

# Calculate the Mean Daily Excess Retun and Standard Deviation for the Daily Returns_SPX 
mean_daily_excess_return_spx <- mean(short1_data_filtered$ExcessReturns_SPX, na.rm = TRUE)
sd_daily_excess_Returns_SPX <- sd(short1_data_filtered$Returns_SPX, na.rm = TRUE)

# Compute the Daily Sharpe Ratio
daily_sharpe_ratio <- mean_daily_excess_return_spx / sd_daily_excess_Returns_SPX

# Annualize the Sharpe Ratio
annualized_sharpe_ratio <- daily_sharpe_ratio * sqrt(252)

annualized_sharpe_ratio


# Step 8: Calculate the Sortino Ratio
# Calculate the Downside Deviation
squared_negative_excess_Returns_SPX <- ifelse(short1_data_filtered$ExcessReturns_SPX < 0, 
                                              short1_data_filtered$ExcessReturns_SPX^2, 
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
annualized_mean_daily_excess_return <- mean(short1_data_filtered$ExcessReturns_SPX, na.rm = TRUE) * 252

# Compute the Calmar Ratio using the Annualized Mean of Daily Excess Returns_SPX
calmar_ratio_spx <- annualized_mean_daily_excess_return / max_drawdown

# The result is the Calmar Ratio for your strategy
calmar_ratio_spx





# Combine all the results into a single tibble
results_spx <- tibble(
  Statistics = c("Mean", "Median", "S.D.", "Min", "Max", "Max Drawdown", "Skewness", "Kurtosis", "Cumulative Return", "Annualized Return", "Annualized Volatility", "Annualized Sharpe Ratio", "Annualized Sortino Ratio", "Calmar Ratio"),
  SPX = c(summary_stats_spx$Mean, summary_stats_spx$Median, summary_stats_spx$S.D., summary_stats_spx$Min, summary_stats_spx$Max, 
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
model_short1 <- lm(Returns ~ Returns_SPX, data = short1_data_filtered)
beta_short1 <- coef(model_short1)["Returns_SPX"]  # Extract the beta coefficient
  # Step 2: Calculate Alpha
annualized_market_return <- mean(short1_data_filtered$Returns_SPX, na.rm = TRUE) * 252  # Annualize SPX returns
  # Annualize strategy returns for comparison
annualized_strategy_return <- mean(short1_data_filtered$Returns, na.rm = TRUE) * 252
  # Annualize the mean daily RFR
annualized_rfr <- mean(short1_data_filtered$RFR, na.rm = TRUE) * 360
  # Calculate alpha using the CAPM formula
alpha_short1 <- annualized_strategy_return - (annualized_rfr + beta_short1 * (annualized_market_return - annualized_rfr))

  # Output the results
cat("Beta: ", beta_short1, "\n")
cat("Alpha: ", alpha_short1, "\n")

  # Add to other stats
new_rows <- data.frame(Short1 = c(alpha_short1, beta_short1))
rownames(new_rows) <- c("Alpha", "Beta")

    # Combine with the original results
results_short1 <- rbind(results_short1, new_rows)

    # Round the Short1 column
results_short1$Short1 <- round(results_short1$Short1, 4)

# Print the dataframe to check
print(results_short1)




# SPX
alpha_SPX <- 0
beta_SPX <- 1

  # Add to other stats
new_rows <- data.frame(SPX = c(alpha_SPX, beta_SPX))
rownames(new_rows) <- c("Alpha", "Beta")

    # Combine with the original results
results_spx <- rbind(results_spx, new_rows)

    # Round the SPX column
results_spx$SPX <- round(results_spx$SPX, 4)

# Print the dataframe to check
print(results_spx)




# PUT Index
  # Step 1: Run the regression to calculate Beta
model_putindex <- lm(Returns_Put ~ Returns_SPX, data = short1_data_filtered)
beta_putindex <- coef(model_putindex)["Returns_SPX"]  # Extract the beta coefficient

  # Step 2: Calculate Alpha
  # The RFR is already in the dataframe and adjusted for daily use.
annualized_market_return <- mean(short1_data_filtered$Returns_SPX, na.rm = TRUE) * 252  # Annualize SPX returns
  # Annualize strategy returns for comparison
annualized_putindex_return <- mean(short1_data_filtered$Returns_Put, na.rm = TRUE) * 252
  # Annualize the mean daily RFR
annualized_rfr <- mean(short1_data_filtered$RFR, na.rm = TRUE) * 360
  # Calculate alpha using the CAPM formula
alpha_putindex <- annualized_putindex_return - (annualized_rfr + beta_putindex * (annualized_market_return - annualized_rfr))

  # Output the results
cat("Beta: ", beta_putindex, "\n")
cat("Alpha: ", alpha_putindex, "\n")

  # Add to other stats
new_rows <- data.frame(Put_Index = c(alpha_putindex, beta_putindex))
rownames(new_rows) <- c("Alpha", "Beta")

# Combine with the original results
# Use rbind to append the data
results_put <- rbind(results_put, new_rows)

# Round the Put_Index column
results_put$Put_Index <- round(results_put$Put_Index, 4)

# Print the dataframe to check
print(results_put)



#All Descriptive Stats Table ####
# Combine the data frames by columns
combined_results <- cbind(results_short1, results_put, results_spx)

# Rename the columns to reflect the source tibble if needed
colnames(combined_results) <- c("Short1", "Put Index", "S&P 500")

# Print the combined results to check
print(combined_results)