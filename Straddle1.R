###################################

#Straddle1

#Fabian Welti 
#10.03.2024

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
options(scipen = 999)


#Read in the data ####
# Overall dataset - containing options with max 30 days to maturity 
# Read the CSV file into a dataset 
file_name <- "jap4rpqoavcoxozr.csv"
Options_data <- read_csv(file_name)


#Create individual Dataset for each leg of the strategy ####
# filter out only put options 
straddle1_put <- Options_data %>% 
  filter(cp_flag != "C")

#filter out the call options
straddle1_call <- Options_data %>% 
  filter(cp_flag != "P")



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
# Filter both put and call dataset to keep only rows where 'exdate' matches one of the all_relevant_dates
  # put data
straddle1_put <- straddle1_put %>%
  filter(as.Date(exdate, format = "%Y%m%d") %in% all_relevant_dates)
  # call data
straddle1_call <- straddle1_call %>%
  filter(as.Date(exdate, format = "%Y%m%d") %in% all_relevant_dates)





#Combine S&P 500 prices with options dataset ####
spx_data <- read_excel("^SPX 1990-12.01.2024.xlsx", skip = 1)
spx_data$Date <- as.Date(spx_data$Date, format = "%Y-%m-%d") #converting the Date column 

spx_data_selected <- select(spx_data, Date, Open, Close)
# merge with put data
straddle1_put <- left_join(straddle1_put, spx_data_selected, by = c("date"="Date"))
# merge with put data
straddle1_call <- left_join(straddle1_call, spx_data_selected, by = c("date"="Date"))


#Calculate the absolute difference from Strike to S&P Close ####
# Put 
straddle1_put$abs_diff <- abs(straddle1_put$Close - (straddle1_put$strike_price)/1000)
# Call
straddle1_call$abs_diff <- abs(straddle1_call$Close - (straddle1_call$strike_price)/1000)



#Clean the datesets ####
# Filter the dataset to exclude rows where 'date' is equal to 'exdate' since want to close posiotion before
straddle1_put <- straddle1_put %>%
  filter(date != exdate)
straddle1_call <- straddle1_call %>%
  filter(date != exdate)



#Filter out the ATM contracts for the Put dataset####
# Step 1: Find days where position is closed
# Add a new column 'next_exdate' that contains the exdate of the next row
straddle1_put <- straddle1_put %>%
  mutate(next_exdate = lead(exdate))

# Compare 'exdate' with 'next_exdate' to determine if the position should be closed
# Assign TRUE if exdate is different from next_exdate (position should be closed)
# The last row will automatically get NA for 'next_exdate', so replace NA with TRUE to close the position
straddle1_put <- straddle1_put %>%
  mutate(close_position = if_else(exdate != next_exdate, TRUE, FALSE)) %>%
  mutate(close_position = replace_na(close_position, TRUE))

# Remove the temporary 'next_exdate' column
straddle1_put <- select(straddle1_put, -next_exdate)

# Identify all unique dates where close_position is TRUE
dates_to_mark <- unique(straddle1_put$date[straddle1_put$close_position == TRUE])

# Update close_position to TRUE for all rows that have a matching date in dates_to_mark
straddle1_put <- straddle1_put %>%
  mutate(close_position = if_else(date %in% dates_to_mark, TRUE, close_position))



# Step 2: Find days where position is opened
# Add a new column 'prev_exdate' that contains the exdate of the previous row
straddle1_put <- straddle1_put %>%
  mutate(prev_exdate = lag(exdate))

# Compare 'exdate' with 'prev_exdate' to determine if a new position starts
# Assign TRUE if exdate is different from prev_exdate (indicating a new position)
# The first row will automatically get NA for 'prev_exdate', so replace NA with TRUE to indicate a new position
straddle1_put <- straddle1_put %>%
  mutate(new_position = if_else(exdate != prev_exdate, TRUE, FALSE)) %>%
  mutate(new_position = replace_na(new_position, TRUE))

# Remove the temporary 'prev_exdate' column
straddle1_put <- select(straddle1_put, -prev_exdate)

# Identify all unique dates where new_position is TRUE
dates_with_new_position <- unique(straddle1_put$date[straddle1_put$new_position == TRUE])

# Update new_position to TRUE for all rows that have a matching date in dates_with_new_position
straddle1_put <- straddle1_put %>%
  mutate(new_position = if_else(date %in% dates_with_new_position, TRUE, new_position))



# Step 3: Find the ATM contract for days where new position is opened
# Add an identifier for each row to assist in marking rows to keep
straddle1_put <- straddle1_put %>%
  mutate(row_id = row_number())

# Identify the row with the smallest abs_diff for each date where new_position is TRUE
min_abs_diff_rows <- straddle1_put %>%
  filter(new_position == TRUE) %>%
  group_by(date) %>%
  slice_min(abs_diff, n = 1) %>%
  ungroup() %>%
  pull(row_id)  # Get the row IDs of the rows with the minimum abs_diff

# Mark rows to keep: rows with the smallest abs_diff in their date group where new_position is TRUE, and all rows where new_position is FALSE
straddle1_put <- straddle1_put %>%
  mutate(keep = row_id %in% min_abs_diff_rows | new_position == FALSE)

# Filter the dataset to keep only the marked rows
straddle1_put_filtered <- straddle1_put %>%
  filter(keep) %>%
  select(-row_id, -keep)  # Remove the helper columns



# Step 4: Find the ATM contract for normal days
# Step 4.1: Add helper columns to track the last 'strike_price' and 'optionid' where 'new_position' is TRUE
straddle1_put_filtered <- straddle1_put_filtered %>%
  mutate(last_new_strike = ifelse(new_position == TRUE, strike_price, NA),
         last_new_optionid = ifelse(new_position == TRUE, optionid, NA)) %>%
  fill(last_new_strike, .direction = "down") %>%
  fill(last_new_optionid, .direction = "down")

# Step 4.2: Filter out rows where both 'close_position' and 'new_position' are FALSE
# and either 'strike_price' or 'optionid' doesn't match their respective last values
straddle1_put_filtered <- straddle1_put_filtered %>%
  filter(!(close_position == FALSE & new_position == FALSE & 
             (strike_price != last_new_strike | optionid != last_new_optionid)))

# Step 4.3: Remove the helper columns
straddle1_put_filtered <- select(straddle1_put_filtered, -last_new_strike, -last_new_optionid)



# Step 5: Find the ATM contract for days where contract is closed
# Step 5.1: Create helper columns for the previous 'strike_price' and 'optionid'
# Initialize them with NA and then fill in the values from rows where both 'close_position' and 'new_position' are FALSE
straddle1_put_filtered <- straddle1_put_filtered %>%
  mutate(prev_strike_price = ifelse(close_position == FALSE & new_position == FALSE, strike_price, NA),
         prev_optionid = ifelse(close_position == FALSE & new_position == FALSE, optionid, NA)) %>%
  fill(prev_strike_price, .direction = "down") %>%
  fill(prev_optionid, .direction = "down")

# Step 5.2: Apply the logic for keeping rows based on 'close_position', 'strike_price', and 'optionid'
# Mark rows to keep when 'close_position' is TRUE and both 'strike_price' and 'optionid' match their previous values
straddle1_put_filtered <- straddle1_put_filtered %>%
  mutate(keep = ifelse(close_position == TRUE,
                       (strike_price == prev_strike_price & optionid == prev_optionid),
                       TRUE))

# Step 5.3: Filter the dataset to keep only the marked rows and remove helper columns
straddle1_put_filtered <- straddle1_put_filtered %>%
  filter(keep) %>%
  select(-prev_strike_price, -prev_optionid, -keep)



#Filter out the ATM contracts for the Call dataset ####
# Step 1: Find days where position is closed
# Add a new column 'next_exdate' that contains the exdate of the next row
straddle1_call <- straddle1_call %>%
  mutate(next_exdate = lead(exdate))

# Compare 'exdate' with 'next_exdate' to determine if the position should be closed
# Assign TRUE if exdate is different from next_exdate (position should be closed)
# The last row will automatically get NA for 'next_exdate', so replace NA with TRUE to close the position
straddle1_call <- straddle1_call %>%
  mutate(close_position = if_else(exdate != next_exdate, TRUE, FALSE)) %>%
  mutate(close_position = replace_na(close_position, TRUE))

# Remove the temporary 'next_exdate' column
straddle1_call <- select(straddle1_call, -next_exdate)

# Identify all unique dates where close_position is TRUE
dates_to_mark <- unique(straddle1_call$date[straddle1_call$close_position == TRUE])

# Update close_position to TRUE for all rows that have a matching date in dates_to_mark
straddle1_call <- straddle1_call %>%
  mutate(close_position = if_else(date %in% dates_to_mark, TRUE, close_position))


# Step 2: Find days where position is opened
# Add a new column 'prev_exdate' that contains the exdate of the previous row
straddle1_call <- straddle1_call %>%
  mutate(prev_exdate = lag(exdate))

# Compare 'exdate' with 'prev_exdate' to determine if a new position starts
# Assign TRUE if exdate is different from prev_exdate (indicating a new position)
# The first row will automatically get NA for 'prev_exdate', so replace NA with TRUE to indicate a new position
straddle1_call <- straddle1_call %>%
  mutate(new_position = if_else(exdate != prev_exdate, TRUE, FALSE)) %>%
  mutate(new_position = replace_na(new_position, TRUE))

# Remove the temporary 'prev_exdate' column
straddle1_call <- select(straddle1_call, -prev_exdate)

# Identify all unique dates where new_position is TRUE
dates_with_new_position <- unique(straddle1_call$date[straddle1_call$new_position == TRUE])

# Update new_position to TRUE for all rows that have a matching date in dates_with_new_position
straddle1_call <- straddle1_call %>%
  mutate(new_position = if_else(date %in% dates_with_new_position, TRUE, new_position))



# Step 3: Find the ATM contract for days where new position is opened
# Add an identifier for each row to assist in marking rows to keep
straddle1_call <- straddle1_call %>%
  mutate(row_id = row_number())

# Identify the row with the smallest abs_diff for each date where new_position is TRUE
min_abs_diff_rows <- straddle1_call %>%
  filter(new_position == TRUE) %>%
  group_by(date) %>%
  slice_min(abs_diff, n = 1) %>%
  ungroup() %>%
  pull(row_id)  # Get the row IDs of the rows with the minimum abs_diff

# Mark rows to keep: rows with the smallest abs_diff in their date group where new_position is TRUE, and all rows where new_position is FALSE
straddle1_call <- straddle1_call %>%
  mutate(keep = row_id %in% min_abs_diff_rows | new_position == FALSE)

# Filter the dataset to keep only the marked rows
straddle1_call_filtered <- straddle1_call %>%
  filter(keep) %>%
  select(-row_id, -keep)  # Remove the helper columns




# Step 4: Find the ATM contract for normal days
# Step 4.1: Add helper columns to track the last 'strike_price' and 'optionid' where 'new_position' is TRUE
straddle1_call_filtered <- straddle1_call_filtered %>%
  mutate(last_new_strike = ifelse(new_position == TRUE, strike_price, NA),
         last_new_optionid = ifelse(new_position == TRUE, optionid, NA)) %>%
  fill(last_new_strike, .direction = "down") %>%
  fill(last_new_optionid, .direction = "down")

# Step 4.2: Filter out rows where both 'close_position' and 'new_position' are FALSE
# and either 'strike_price' or 'optionid' doesn't match their respective last values
straddle1_call_filtered <- straddle1_call_filtered %>%
  filter(!(close_position == FALSE & new_position == FALSE & 
             (strike_price != last_new_strike | optionid != last_new_optionid)))

# Step 4.3: Remove the helper columns
straddle1_call_filtered <- select(straddle1_call_filtered, -last_new_strike, -last_new_optionid)




# Step 5: Find the ATM contract for days where contract is closed
# Step 5.1: Create helper columns for the previous 'strike_price' and 'optionid'
# Initialize them with NA and then fill in the values from rows where both 'close_position' and 'new_position' are FALSE
straddle1_call_filtered <- straddle1_call_filtered %>%
  mutate(prev_strike_price = ifelse(close_position == FALSE & new_position == FALSE, strike_price, NA),
         prev_optionid = ifelse(close_position == FALSE & new_position == FALSE, optionid, NA)) %>%
  fill(prev_strike_price, .direction = "down") %>%
  fill(prev_optionid, .direction = "down")

# Step 5.2: Apply the logic for keeping rows based on 'close_position', 'strike_price', and 'optionid'
# Mark rows to keep when 'close_position' is TRUE and both 'strike_price' and 'optionid' match their previous values
straddle1_call_filtered <- straddle1_call_filtered %>%
  mutate(keep = ifelse(close_position == TRUE,
                       (strike_price == prev_strike_price & optionid == prev_optionid),
                       TRUE))

# Step 5.3: Filter the dataset to keep only the marked rows and remove helper columns
straddle1_call_filtered <- straddle1_call_filtered %>%
  filter(keep) %>%
  select(-prev_strike_price, -prev_optionid, -keep)

#New open position logic as the datesets are now cleaned ####
#Put: 
# Step 1: Add a column for the previous 'exdate'
straddle1_put_filtered <- straddle1_put_filtered %>%
  mutate(prev_exdate = lag(exdate))
# Step 2: Update 'new_position' to TRUE when 'exdate' changes from the previous row
straddle1_put_filtered <- straddle1_put_filtered %>%
  mutate(new_position = ifelse(exdate != prev_exdate | row_number() == 1, TRUE, new_position)) #ensure that the first row will be an entry
# Step 3: Remove the helper 'prev_exdate' column
straddle1_put_filtered <- select(straddle1_put_filtered, -prev_exdate)

#Call: 
# Step 1: Add a column for the previous 'exdate'
straddle1_call_filtered <- straddle1_call_filtered %>%
  mutate(prev_exdate = lag(exdate))
# Step 2: Update 'new_position' to TRUE when 'exdate' changes from the previous row
straddle1_call_filtered <- straddle1_call_filtered %>%
  mutate(new_position = ifelse(exdate != prev_exdate | row_number() == 1, TRUE, new_position)) #ensure that the first row will be an entry
# Step 3: Remove the helper 'prev_exdate' column
straddle1_call_filtered <- select(straddle1_call_filtered, -prev_exdate)



#New close position logic as the datesets are now cleaned ####
#Put
# Step 1: Add a column for the 'new_position' of the next row
straddle1_put_filtered <- straddle1_put_filtered %>%
  mutate(next_new_position = lead(new_position))
# Step 2: Update 'close_position' to TRUE when the next row's 'new_position' is TRUE
straddle1_put_filtered <- straddle1_put_filtered %>%
  mutate(close_position = ifelse(next_new_position == TRUE, TRUE, close_position))
# Step 3: Remove the helper 'next_new_position' column
straddle1_put_filtered <- select(straddle1_put_filtered, -next_new_position)


#Call:
# Step 1: Add a column for the 'new_position' of the next row
straddle1_call_filtered <- straddle1_call_filtered %>%
  mutate(next_new_position = lead(new_position))
# Step 2: Update 'close_position' to TRUE when the next row's 'new_position' is TRUE
straddle1_call_filtered <- straddle1_call_filtered %>%
  mutate(close_position = ifelse(next_new_position == TRUE, TRUE, close_position))
# Step 3: Remove the helper 'next_new_position' column
straddle1_call_filtered <- select(straddle1_call_filtered, -next_new_position)



#Clean the dataset
# Filter out rows where both 'close_position' and 'new_position' are TRUE
straddle1_put_filtered <- straddle1_put_filtered %>%
  filter(!(close_position == TRUE & new_position == TRUE))

straddle1_call_filtered <- straddle1_call_filtered %>%
  filter(!(close_position == TRUE & new_position == TRUE))




#Implement the strategy ####
#first we will calculate the daily PnL for each leg of the straddle seperatly and than combine it in one dataset 
#to calculate the development of the Margin Account over time 

#Step 1: Midpoint price
# Calculate the midpoint price as the average of 'best_bid' and 'best_offer'
# Put:
straddle1_put_filtered <- straddle1_put_filtered %>%
  mutate(midpoint_price = ifelse(close_position == TRUE,
                                 (1/3) * best_bid + (2/3) * best_offer,
                                 (2/3) * best_bid + (1/3) * best_offer))

# Call:
straddle1_call_filtered <- straddle1_call_filtered %>%
  mutate(midpoint_price = ifelse(close_position == TRUE,
                                 (1/3) * best_bid + (2/3) * best_offer,
                                 (2/3) * best_bid + (1/3) * best_offer))




#Step 2: Calculate the daily PnL of selling the ATM put or call for each month
# Put:
straddle1_put_filtered <- straddle1_put_filtered %>%
  mutate(daily_pnl = ifelse(new_position == TRUE, 
                            0, 
                            -100 * (midpoint_price - lag(midpoint_price))))

# Call:
straddle1_call_filtered <- straddle1_call_filtered %>%
  mutate(daily_pnl = ifelse(new_position == TRUE, 
                            0, 
                            -100 * (midpoint_price - lag(midpoint_price))))





#Step 3: Merge the datasets 
# Step 3.1: Merge daily pnl
  # Rename 'daily_pnl' in both datasets for clarity and include 'strike_price'
straddle1_put_filtered <- straddle1_put_filtered %>%
  rename(daily_pnl_put = daily_pnl,
         midpoint_price_put = midpoint_price)

straddle1_call_filtered <- straddle1_call_filtered %>%
  rename(daily_pnl_call = daily_pnl,
         midpoint_price_call = midpoint_price)

  # Perform a left join to create the new dataset 'Straddle1' and include the 'strike_price' columns
Straddle1 <- straddle1_put_filtered %>%
  select(date, exdate, daily_pnl_put, close_position, new_position, strike_price, Close, midpoint_price_put) %>%
  left_join(straddle1_call_filtered %>% select(date, daily_pnl_call,midpoint_price_call),
            by = "date")

  # Fill NA values with 0 in 'daily_pnl_call' and 'daily_pnl_put'
Straddle1 <- Straddle1 %>%
  mutate(daily_pnl_call = coalesce(daily_pnl_call, 0),
         daily_pnl_put = coalesce(daily_pnl_put, 0))



# Step 3.2: Calculate the daily PnL combining both legs
Straddle1 <- Straddle1 %>%
  mutate(daily_pnl = daily_pnl_put + daily_pnl_call)



# Step 3.3: Import Risk Free Rate and merge into the dataset
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
Straddle1 <- Straddle1 %>%
  left_join(treasury_data, by = c("date" = "DATE"))



# Step 4: Implement the strategy over the data (adjusted contract sizes)
# Initialize MarginAccount for the first row and N_contracts based on the initial MarginAccount
Straddle1$MarginAccount[1] <- 1000000
# Initial N_contracts based on the adjusted formula for the first row's MarginAccount and strike_price
Straddle1$N_contracts[1] <- floor((Straddle1$MarginAccount[1] / (0.75 * Straddle1$strike_price[1])) * 10)

# Define the cost per contract per trade
cost_per_contract <- 1.10

# Loop through the dataset starting from the second row
for (i in 2:nrow(Straddle1)) {
  # Update N_contracts for rows where new_position is TRUE using the new formula
  if (Straddle1$new_position[i] == TRUE) {
    Straddle1$N_contracts[i] <- floor((Straddle1$MarginAccount[i-1] / (0.75 * Straddle1$strike_price[i])) * 10)
  } else {
    # Carry forward the N_contracts value from the previous row if new_position is not TRUE
    Straddle1$N_contracts[i] <- Straddle1$N_contracts[i-1]
  }
  
  # Calculate the cost of opening or closing positions
  # Ensure that neither new_position nor close_position is NA before evaluating them
  if (!is.na(Straddle1$new_position[i]) && !is.na(Straddle1$close_position[i])) {
    if (Straddle1$new_position[i] == TRUE || Straddle1$close_position[i] == TRUE) {
      trade_cost <- Straddle1$N_contracts[i] * cost_per_contract
    } else {
      trade_cost <- 0
    }
  } else {
    trade_cost <- 0  # Default to 0 if there is an NA
  }
  
  # Calculate MarginAccount for each row accounting for the transaction costs
  Straddle1$MarginAccount[i] <- (1 + Straddle1$RFR[i-1]) * 
    Straddle1$MarginAccount[i-1] + 
    Straddle1$N_contracts[i] * Straddle1$daily_pnl[i] - trade_cost  # Subtracting the trade cost here
}





#Calculate the Margin Requirement of the Strategy ####
# Calculate Initial Margin for puts (IM_P) and calls (IM_C)
Straddle1 <- Straddle1 %>%
  mutate(
    Out_of_the_Money_Amount_P = pmax(0, Close - (strike_price / 1000)),
    Out_of_the_Money_Amount_C = pmax(0, (strike_price / 1000) - Close),
    IM_P = midpoint_price_put + pmax((0.15 * Close - Out_of_the_Money_Amount_P), (0.10 * (strike_price / 1000))),
    IM_C = midpoint_price_call + pmax((0.15 * Close - Out_of_the_Money_Amount_C), (0.10 * (strike_price / 1000)))
  )

# Determine Margin_Requirement for each row
Straddle1 <- Straddle1 %>%
  mutate(
    Margin_Requirement = ifelse(IM_P > IM_C, 
                                IM_P + midpoint_price_call, 
                                IM_C + midpoint_price_put)
  )

# Clean up by removing intermediary columns if no longer needed
Straddle1 <- select(Straddle1, -Out_of_the_Money_Amount_P, -Out_of_the_Money_Amount_C, -IM_P, -IM_C)

# Multiply by number of contracts
Straddle1 <- Straddle1 %>%
  mutate(
    Margin_Requirement = Margin_Requirement * N_contracts * 100
  )



#Put Index ####
# Step 1: Import the PUT index data and calculate daily PnL
# Import Put Index data
put_data <- read_excel("Put Index.xlsx")
# Calculate daily PnL for Put Index
put_data <- put_data %>%
  mutate(daily_pnl_put_index = `PUT Index` - lag(`PUT Index`))


# Step 2: Merge the Datesets
Straddle1 <- left_join(Straddle1, put_data, by = c("date"="Date"))
# Replace NA values in 'daily_pnl_put_index' with 0
Straddle1$daily_pnl_put_index <- replace_na(Straddle1$daily_pnl_put_index, 0)


# Step 3: Calculate the development of 100k 
# Calculate the initial contract size 'x' using the correct column name "PUT Index"
x <- round(1000000 / Straddle1$`PUT Index`[1], digits = 0)

# Initialize the 'Put_Strategy' column with the first value as the initial investment
Straddle1$Put_Strategy <- rep(0, nrow(Straddle1))
Straddle1$Put_Strategy[1] <- 1000000  # Set the initial value for the first row

# Loop through the dataset starting from the second row
for (i in 2:nrow(Straddle1)) {
  Straddle1$Put_Strategy[i] <- x * Straddle1$daily_pnl_put_index[i] + Straddle1$Put_Strategy[i - 1]
}


#Development of Investment in S&P 500 ####
# Step 1: Calculate daily PnL for SPX
Straddle1 <- Straddle1 %>%
  mutate(daily_pnl_spx = Close - lag(Close))
# Set the first value of 'daily_pnl_spx' to 0
Straddle1$daily_pnl_spx[1] <- 0
# Replace NA values in 'daily_pnl_spx' with 0
Straddle1$daily_pnl_spx <- replace_na(Straddle1$daily_pnl_spx, 0)

# Step 2: Calculate the development of 100k 
# Calculate the initial contract size 'x' using the correct column name "PUT Index"
x <- round(1000000 / Straddle1$Close[1], digits = 0)

# Initialize the 'SPX' column with the first value as the initial investment
Straddle1$SPX <- rep(0, nrow(Straddle1))
Straddle1$SPX[1] <- 1000000  # Set the initial value for the first row

# Loop through the dataset starting from the second row
for (i in 2:nrow(Straddle1)) {
  Straddle1$SPX[i] <- x * Straddle1$daily_pnl_spx[i] + Straddle1$SPX[i - 1]
}




#Plot the results ####
# Calculate the upper limit for y-axis based on the maximum values of the plotted series
upper_limit <- max(c(max(Straddle1$MarginAccount, na.rm = TRUE), 
                     max(Straddle1$Put_Strategy, na.rm = TRUE),
                     max(Straddle1$SPX, na.rm = TRUE),
                     max(Straddle1$Margin_Requirement, na.rm = TRUE)))

# Define y-axis range starting from 0 to the upper limit
y_range <- c(0, upper_limit)

par(family="Times New Roman")

# Set up the plot with the first line to establish the plot area
plot(Straddle1$date, Straddle1$MarginAccount, type = 'l', 
     col = "darkblue", ylim = y_range, xaxt = 'n', # Disable default x-axis
     xlab = "", ylab = "Development of $1'000'000", 
     main = "Straddle1 Performance", lwd = 2) # Increase line width with lwd

# Add the remaining series with increased line width
lines(Straddle1$date, Straddle1$Put_Strategy, col = "red", lwd = 2)
lines(Straddle1$date, Straddle1$SPX, col = "black", lwd = 2)
lines(Straddle1$date, Straddle1$Margin_Requirement, col = "deepskyblue4", lwd = 1) 

# Custom x-axis labels for each year
year_ticks <- seq(from=min(Straddle1$date, na.rm=TRUE),
                  to=max(Straddle1$date, na.rm=TRUE), by="years")
axis(1, at=year_ticks, labels=format(year_ticks, "%Y"), cex.axis=0.8, las=2)

# Add light gridlines
abline(h=seq(from=0, to=upper_limit, by=upper_limit/5), 
       col="grey", lty="dotted", lwd=0.5)

abline(v=year_ticks, col="grey", lty="dotted", lwd=0.5)

# Add a legend without a box 
legend("topleft", legend = c("Straddle1", "PUT", "S&P 500", "Margin Requirement Straddle1"),
       col = c("darkblue", "red", "black", "deepskyblue4"), lty = 1, bty = "n", cex = 0.8, lwd=c(2,2,2,2))


#Descriptive statistics Straddle1 ####
# Step 1: Calculate Simple Returns
Straddle1 <- Straddle1 %>%
  mutate(Returns = (MarginAccount - lag(MarginAccount)) / lag(MarginAccount))

# Replace the first NA value in Returns with 0 
Straddle1$Returns[1] <- 0  


# Step 2: Generate Summary Statistics
summary_stats_straddle1 <- Straddle1 %>%
  summarise(N.Obs = n(),
            Mean = mean(Returns, na.rm = TRUE),
            Median = median(Returns, na.rm = TRUE),
            S.D. = sd(Returns, na.rm = TRUE),
            Min = min(Returns, na.rm = TRUE),
            Max = max(Returns, na.rm = TRUE))


# Step 3: Calculate Max Drawdown
# Initialize variables to track the peak and maximum drawdown
peak = Straddle1$MarginAccount[1]
max_drawdown = 0

# Loop through the MarginAccount to find the maximum drawdown
for (value in Straddle1$MarginAccount) {
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
skewness <- skewness(Straddle1$Returns, na.rm = TRUE)
kurtosis <- kurtosis(Straddle1$Returns, na.rm = TRUE)


# Step 5: Calculate Cumulative Return
cumulative_return <- last(Straddle1$MarginAccount) / first(Straddle1$MarginAccount) - 1


# Step 6: Annualize Returns and Volatility
annualized_return <- (1 + mean(Straddle1$Returns, na.rm = TRUE)) ^ 252 - 1  # Assuming 252 trading days in a year
annualized_volatility <- sd(Straddle1$Returns, na.rm = TRUE) * sqrt(252)


# Step 7: Calculate the Sharpe Ratio
# Calculate the Daily Excess Returns
Straddle1$ExcessReturns <- Straddle1$Returns - Straddle1$RFR

# Calculate the Mean Daily Excess Retun and Standard Deviation for the Daily Returns 
mean_daily_excess_return_straddle1 <- mean(Straddle1$ExcessReturns, na.rm = TRUE)
sd_daily_excess_returns_straddle1 <- sd(Straddle1$Returns, na.rm = TRUE)

# Compute the Daily Sharpe Ratio
daily_sharpe_ratio_straddle1 <- mean_daily_excess_return_straddle1 / sd_daily_excess_returns_straddle1

# Annualize the Sharpe Ratio
annualized_sharpe_ratio_straddle1 <- daily_sharpe_ratio_straddle1 * sqrt(252)

annualized_sharpe_ratio_straddle1


# Step 8: Calculate the Sortino Ratio
# Calculate the Downside Deviation
squared_negative_excess_returns <- ifelse(Straddle1$ExcessReturns < 0, 
                                          Straddle1$ExcessReturns^2, 
                                          0)
mean_squared_negative_excess_returns <- mean(squared_negative_excess_returns, na.rm = TRUE)
downside_deviation <- sqrt(mean_squared_negative_excess_returns)

# Calculate the Daily Sortino Ratio
daily_sortino_ratio <- mean_daily_excess_return_straddle1 / downside_deviation

# Annualize the Sortino Ratio
annualized_sortino_ratio <- daily_sortino_ratio * sqrt(252)

annualized_sortino_ratio


#Step 9: Calculate the Calmar Ratio
# Calculate the Annualized Mean of Daily Excess Returns
annualized_mean_daily_excess_return <- mean(Straddle1$ExcessReturns, na.rm = TRUE) * 252

# Compute the Calmar Ratio using the Annualized Mean of Daily Excess Returns
calmar_ratio_straddle1 <- annualized_mean_daily_excess_return / max_drawdown

# The result is the Calmar Ratio for your strategy
calmar_ratio_straddle1





# Combine all the results into a single tibble
results_straddle1 <- tibble(
  Statistics = c("Mean", "Median", "S.D.", "Min", "Max", "Max Drawdown", "Skewness", "Kurtosis", "Cumulative Return", "Annualized Return", "Annualized Volatility", "Annualized Sharpe Ratio", "Annualized Sortino Ratio", "Calmar Ratio"),
  Straddle1 = c(summary_stats_straddle1$Mean, summary_stats_straddle1$Median, summary_stats_straddle1$S.D., summary_stats_straddle1$Min, summary_stats_straddle1$Max, 
                max_drawdown, skewness, kurtosis, cumulative_return, annualized_return, annualized_volatility, annualized_sharpe_ratio_straddle1, annualized_sortino_ratio, calmar_ratio_straddle1)
)

# make the dataset visually nicer
# Convert tibble to data frame if it is not already
results_straddle1 <- as.data.frame(results_straddle1)

# Set the row names and remove the 'Statistics' column
rownames(results_straddle1) <- results_straddle1$Statistics
results_straddle1 <-results_straddle1[-1]

# Round the numbers in the Straddle1 column to 2 decimal places
results_straddle1$Straddle1 <- round(results_straddle1$Straddle1, 4)

# Print the results to check
print(results_straddle1)


#Descriptive statistics Put Index####
# Step 1: Calculate Simple Returns_Put
Straddle1 <- Straddle1 %>%
  mutate(Returns_Put = (Put_Strategy - lag(Put_Strategy)) / lag(Put_Strategy))

# Replace the first NA value in Returns_Put with 0 
Straddle1$Returns_Put[1] <- 0  


# Step 2: Generate Summary Statistics
summary_stats_put <- Straddle1 %>%
  summarise(N.Obs = n(),
            Mean = mean(Returns_Put, na.rm = TRUE),
            Median = median(Returns_Put, na.rm = TRUE),
            S.D. = sd(Returns_Put, na.rm = TRUE),
            Min = min(Returns_Put, na.rm = TRUE),
            Max = max(Returns_Put, na.rm = TRUE))


# Step 3: Calculate Max Drawdown
# Initialize variables to track the peak and maximum drawdown
peak = Straddle1$Put_Strategy[1]
max_drawdown = 0

# Loop through the Put_Strategy to find the maximum drawdown
for (value in Straddle1$Put_Strategy) {
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
skewness <- skewness(Straddle1$Returns_Put, na.rm = TRUE)
kurtosis <- kurtosis(Straddle1$Returns_Put, na.rm = TRUE)


# Step 5: Calculate Cumulative Return
cumulative_return <- last(Straddle1$Put_Strategy) / first(Straddle1$Put_Strategy) - 1


# Step 6: Annualize Returns_Put and Volatility
annualized_return <- (1 + mean(Straddle1$Returns_Put, na.rm = TRUE)) ^ 252 - 1  # Assuming 252 trading days in a year
annualized_volatility <- sd(Straddle1$Returns_Put, na.rm = TRUE) * sqrt(252)


# Step 7: Calculate the Sharpe Ratio
# Calculate the Daily Excess Returns_Put
Straddle1$ExcessReturns_Put <- Straddle1$Returns_Put - Straddle1$RFR

# Calculate the Mean Daily Excess Retun and Standard Deviation for the Daily Returns_Put 
mean_daily_excess_return_put <- mean(Straddle1$ExcessReturns_Put, na.rm = TRUE)
sd_daily_excess_Returns_Put <- sd(Straddle1$Returns_Put, na.rm = TRUE)

# Compute the Daily Sharpe Ratio
daily_sharpe_ratio <- mean_daily_excess_return_put / sd_daily_excess_Returns_Put

# Annualize the Sharpe Ratio
annualized_sharpe_ratio <- daily_sharpe_ratio * sqrt(252)

annualized_sharpe_ratio


# Step 8: Calculate the Sortino Ratio
# Calculate the Downside Deviation
squared_negative_excess_Returns_Put <- ifelse(Straddle1$ExcessReturns_Put < 0, 
                                              Straddle1$ExcessReturns_Put^2, 
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
annualized_mean_daily_excess_return <- mean(Straddle1$ExcessReturns_Put, na.rm = TRUE) * 252

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
Straddle1 <- Straddle1 %>%
  mutate(Returns_SPX = (SPX - lag(SPX)) / lag(SPX))

# Replace the first NA value in Returns_SPX with 0 
Straddle1$Returns_SPX[1] <- 0 


# Step 2: Generate Summary Statistics
summary_stats_spx <- Straddle1 %>%
  summarise(N.Obs = n(),
            Mean = mean(Returns_SPX, na.rm = TRUE),
            Median = median(Returns_SPX, na.rm = TRUE),
            S.D. = sd(Returns_SPX, na.rm = TRUE),
            Min = min(Returns_SPX, na.rm = TRUE),
            Max = max(Returns_SPX, na.rm = TRUE))


# Step 3: Calculate Max Drawdown
# Initialize variables to track the peak and maximum drawdown
peak = Straddle1$SPX[1]
max_drawdown = 0

# Loop through the SPX to find the maximum drawdown
for (value in Straddle1$SPX) {
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
skewness <- skewness(Straddle1$Returns_SPX, na.rm = TRUE)
kurtosis <- kurtosis(Straddle1$Returns_SPX, na.rm = TRUE)


# Step 5: Calculate Cumulative Return
cumulative_return <- last(Straddle1$SPX) / first(Straddle1$SPX) - 1


# Step 6: Annualize Returns_SPX and Volatility
annualized_return <- (1 + mean(Straddle1$Returns_SPX, na.rm = TRUE)) ^ 252 - 1  # Assuming 252 trading days in a year
annualized_volatility <- sd(Straddle1$Returns_SPX, na.rm = TRUE) * sqrt(252)


# Step 7: Calculate the Sharpe Ratio
# Calculate the Daily Excess Returns_SPX
Straddle1$ExcessReturns_SPX <- Straddle1$Returns_SPX - Straddle1$RFR

# Calculate the Mean Daily Excess Retun and Standard Deviation for the Daily Returns_SPX 
mean_daily_excess_return_spx <- mean(Straddle1$ExcessReturns_SPX, na.rm = TRUE)
sd_daily_excess_Returns_SPX <- sd(Straddle1$Returns_SPX, na.rm = TRUE)

# Compute the Daily Sharpe Ratio
daily_sharpe_ratio <- mean_daily_excess_return_spx / sd_daily_excess_Returns_SPX

# Annualize the Sharpe Ratio
annualized_sharpe_ratio <- daily_sharpe_ratio * sqrt(252)

annualized_sharpe_ratio


# Step 8: Calculate the Sortino Ratio
# Calculate the Downside Deviation
squared_negative_excess_Returns_SPX <- ifelse(Straddle1$ExcessReturns_SPX < 0, 
                                              Straddle1$ExcessReturns_SPX^2, 
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
annualized_mean_daily_excess_return <- mean(Straddle1$ExcessReturns_SPX, na.rm = TRUE) * 252

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
model_straddle1 <- lm(Returns ~ Returns_SPX, data = Straddle1)
beta_straddle1 <- coef(model_straddle1)["Returns_SPX"]  # Extract the beta coefficient
# Step 2: Calculate Alpha
# The RFR is already in the dataframe and adjusted for daily use.
annualized_market_return <- mean(Straddle1$Returns_SPX, na.rm = TRUE) * 252  # Annualize SPX returns
# Annualize strategy returns for comparison
annualized_strategy_return <- mean(Straddle1$Returns, na.rm = TRUE) * 252
# Annualize the mean daily RFR
annualized_rfr <- mean(Straddle1$RFR, na.rm = TRUE) * 360
# Calculate alpha using the CAPM formula
alpha_Straddle1 <- annualized_strategy_return - (annualized_rfr + beta_straddle1 * (annualized_market_return - annualized_rfr))

# Output the results
cat("Beta: ", beta_straddle1, "\n")
cat("Alpha: ", alpha_Straddle1, "\n")

# Add to other stats
new_rows <- data.frame(Straddle1 = c(alpha_Straddle1, beta_straddle1))
rownames(new_rows) <- c("Alpha", "Beta")

# Combine with the original results
# Use rbind to append the data
results_straddle1 <- rbind(results_straddle1, new_rows)

# Round the Straddle1 column
results_straddle1$Straddle1 <- round(results_straddle1$Straddle1, 4)

# Print the dataframe to check
print(results_straddle1)




# SPX
alpha_SPX <- 0
beta_SPX <- 1

# Add to other stats
new_rows <- data.frame(SPX = c(alpha_SPX, beta_SPX))
rownames(new_rows) <- c("Alpha", "Beta")

# Combine with the original results
# Use rbind to append the data
results_spx <- rbind(results_spx, new_rows)

# Round the SPX column
results_spx$SPX <- round(results_spx$SPX, 4)

# Print the dataframe to check
print(results_spx)




# PUT Index
# Step 1: Run the regression to calculate Beta
model_putindex <- lm(Returns_Put ~ Returns_SPX, data = Straddle1)
beta_putindex <- coef(model_putindex)["Returns_SPX"]  # Extract the beta coefficient

# Step 2: Calculate Alpha
# The RFR is already in the dataframe and adjusted for daily use.
annualized_market_return <- mean(Straddle1$Returns_SPX, na.rm = TRUE) * 252  # Annualize SPX returns
# Annualize strategy returns for comparison
annualized_putindex_return <- mean(Straddle1$Returns_Put, na.rm = TRUE) * 252
# Annualize the mean daily RFR
annualized_rfr <- mean(Straddle1$RFR, na.rm = TRUE) * 360
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
combined_results <- cbind(results_straddle1, results_put, results_spx)

# Rename the columns 
colnames(combined_results) <- c("Straddle1", "Put Index", "S&P 500")

# Print the combined results to check
print(combined_results)

