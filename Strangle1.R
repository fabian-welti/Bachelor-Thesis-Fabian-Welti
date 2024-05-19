###################################

#Strangle1
#2% OTM

#Fabian Welti 
#13.03.2024

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
strangle1_put <- Options_data %>% 
  filter(cp_flag != "C")

#filter out the call options
strangle1_call <- Options_data %>% 
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
strangle1_put <- strangle1_put %>%
  filter(as.Date(exdate, format = "%Y%m%d") %in% all_relevant_dates)
# call data
strangle1_call <- strangle1_call %>%
  filter(as.Date(exdate, format = "%Y%m%d") %in% all_relevant_dates)










#Combine S&P 500 prices with options dataset ####
spx_data <- read_excel("^SPX 1990-12.01.2024.xlsx", skip = 1)
spx_data$Date <- as.Date(spx_data$Date, format = "%Y-%m-%d") #converting the Date column 

spx_data_selected <- select(spx_data, Date, Open, Close)
# merge with put data
strangle1_put <- left_join(strangle1_put, spx_data_selected, by = c("date"="Date"))
# merge with put data
strangle1_call <- left_join(strangle1_call, spx_data_selected, by = c("date"="Date"))



#Calculate the absolute difference from Strike to S&P Close ####
# Put 
strangle1_put$abs_diff <- abs(strangle1_put$Close - (strangle1_put$strike_price)/1000)
# Call
strangle1_call$abs_diff <- abs(strangle1_call$Close - (strangle1_call$strike_price)/1000)


#Clean the datesets ####
# Filter the dataset to exclude rows where 'date' is equal to 'exdate' since want to close posiotion before
strangle1_put <- strangle1_put %>%
  filter(date != exdate)
strangle1_call <- strangle1_call %>%
  filter(date != exdate)


#Filter out the 2% OTM contracts for the Put dataset####
# Step 1: Find days where position is closed
# Add a new column 'next_exdate' that contains the exdate of the next row
strangle1_put <- strangle1_put %>%
  mutate(next_exdate = lead(exdate))

# Compare 'exdate' with 'next_exdate' to determine if the position should be closed
# Assign TRUE if exdate is different from next_exdate (position should be closed)
# The last row will automatically get NA for 'next_exdate', so replace NA with TRUE to close the position
strangle1_put <- strangle1_put %>%
  mutate(close_position = if_else(exdate != next_exdate, TRUE, FALSE)) %>%
  mutate(close_position = replace_na(close_position, TRUE))

# Remove the temporary 'next_exdate' column
strangle1_put <- select(strangle1_put, -next_exdate)

# Identify all unique dates where close_position is TRUE
dates_to_mark <- unique(strangle1_put$date[strangle1_put$close_position == TRUE])

# Update close_position to TRUE for all rows that have a matching date in dates_to_mark
strangle1_put <- strangle1_put %>%
  mutate(close_position = if_else(date %in% dates_to_mark, TRUE, close_position))



# Step 2: Find days where position is opened
# Add a new column 'prev_exdate' that contains the exdate of the previous row
strangle1_put <- strangle1_put %>%
  mutate(prev_exdate = lag(exdate))

# Compare 'exdate' with 'prev_exdate' to determine if a new position starts
# Assign TRUE if exdate is different from prev_exdate (indicating a new position)
# The first row will automatically get NA for 'prev_exdate', so replace NA with TRUE to indicate a new position
strangle1_put <- strangle1_put %>%
  mutate(new_position = if_else(exdate != prev_exdate, TRUE, FALSE)) %>%
  mutate(new_position = replace_na(new_position, TRUE))

# Remove the temporary 'prev_exdate' column
strangle1_put <- select(strangle1_put, -prev_exdate)

# Identify all unique dates where new_position is TRUE
dates_with_new_position <- unique(strangle1_put$date[strangle1_put$new_position == TRUE])

# Update new_position to TRUE for all rows that have a matching date in dates_with_new_position
strangle1_put <- strangle1_put %>%
  mutate(new_position = if_else(date %in% dates_with_new_position, TRUE, new_position))




# Step 3: Adjust the code for finding OTM put options
strangle1_put <- strangle1_put %>%
  mutate(row_id = row_number(),  # Add an identifier for each row
         target_otm_strike = Close * 0.98,  # Calculate the target OTM strike price
         abs_diff_otm = abs(target_otm_strike - (strike_price/1000)))  # Calculate abs difference adjusted for strike price scaling

# Identify the row with the smallest abs_diff_otm for each date where new_position is TRUE
min_abs_diff_otm_rows <- strangle1_put %>%
  filter(new_position == TRUE) %>%
  group_by(date) %>%
  slice_min(abs_diff_otm, n = 1) %>%
  ungroup() %>%
  pull(row_id)  # Get the row IDs of the rows with the minimum abs_diff_otm

# Mark rows to keep: rows with the smallest abs_diff_otm in their date group where new_position is TRUE, and all rows where new_position is FALSE
strangle1_put <- strangle1_put %>%
  mutate(keep = row_id %in% min_abs_diff_otm_rows | new_position == FALSE)

# Filter the dataset to keep only the marked rows and remove helper columns
strangle1_put_filtered <- strangle1_put %>%
  filter(keep) %>%
  select(-row_id, -keep)  # Clean up the dataset by removing helper columns




# Step 4: Manage the OTM put contracts
# Step 4.1: Add helper columns to track the last 'strike_price' and 'optionid' where 'new_position' is TRUE
strangle1_put_filtered <- strangle1_put_filtered %>%
  mutate(last_new_strike = ifelse(new_position == TRUE, strike_price, NA),
         last_new_optionid = ifelse(new_position == TRUE, optionid, NA)) %>%
  fill(last_new_strike, .direction = "down") %>%
  fill(last_new_optionid, .direction = "down")

# Step 4.2: Filter out rows where both 'close_position' and 'new_position' are FALSE
# and either 'strike_price' or 'optionid' doesn't match their respective last values
strangle1_put_filtered <- strangle1_put_filtered %>%
  filter(!(close_position == FALSE & new_position == FALSE & 
             (strike_price != last_new_strike | optionid != last_new_optionid)))

# Step 4.3: Remove the helper columns
strangle1_put_filtered <- select(strangle1_put_filtered, -last_new_strike, -last_new_optionid)




# Step 5: Find the OTM put contract for days where the contract is closed
# Step 5.1: Create helper columns for the previous 'strike_price' and 'optionid'
# Initialize them with NA and then fill in the values from rows where both 'close_position' and 'new_position' are FALSE
strangle1_put_filtered <- strangle1_put_filtered %>%
  mutate(prev_strike_price = ifelse(close_position == FALSE & new_position == FALSE, strike_price, NA),
         prev_optionid = ifelse(close_position == FALSE & new_position == FALSE, optionid, NA)) %>%
  fill(prev_strike_price, .direction = "down") %>%
  fill(prev_optionid, .direction = "down")

# Step 5.2: Apply the logic for keeping rows based on 'close_position', 'strike_price', and 'optionid'
# Mark rows to keep when 'close_position' is TRUE and both 'strike_price' and 'optionid' match their previous values
strangle1_put_filtered <- strangle1_put_filtered %>%
  mutate(keep = ifelse(close_position == TRUE,
                       (strike_price == prev_strike_price & optionid == prev_optionid),
                       TRUE))

# Step 5.3: Filter the dataset to keep only the marked rows and remove helper columns
strangle1_put_filtered <- strangle1_put_filtered %>%
  filter(keep) %>%
  select(-prev_strike_price, -prev_optionid, -keep)





#Filter out the 2% OTM contracts for the Call dataset####
# Step 1: Find days where position is closed
# Add a new column 'next_exdate' that contains the exdate of the next row
strangle1_call <- strangle1_call %>%
  mutate(next_exdate = lead(exdate))

# Compare 'exdate' with 'next_exdate' to determine if the position should be closed
# Assign TRUE if exdate is different from next_exdate (position should be closed)
# The last row will automatically get NA for 'next_exdate', so replace NA with TRUE to close the position
strangle1_call <- strangle1_call %>%
  mutate(close_position = if_else(exdate != next_exdate, TRUE, FALSE)) %>%
  mutate(close_position = replace_na(close_position, TRUE))

# Remove the temporary 'next_exdate' column
strangle1_call <- select(strangle1_call, -next_exdate)

# Identify all unique dates where close_position is TRUE
dates_to_mark <- unique(strangle1_call$date[strangle1_call$close_position == TRUE])

# Update close_position to TRUE for all rows that have a matching date in dates_to_mark
strangle1_call <- strangle1_call %>%
  mutate(close_position = if_else(date %in% dates_to_mark, TRUE, close_position))



# Step 2: Find days where position is opened
# Add a new column 'prev_exdate' that contains the exdate of the previous row
strangle1_call <- strangle1_call %>%
  mutate(prev_exdate = lag(exdate))

# Compare 'exdate' with 'prev_exdate' to determine if a new position starts
# Assign TRUE if exdate is different from prev_exdate (indicating a new position)
# The first row will automatically get NA for 'prev_exdate', so replace NA with TRUE to indicate a new position
strangle1_call <- strangle1_call %>%
  mutate(new_position = if_else(exdate != prev_exdate, TRUE, FALSE)) %>%
  mutate(new_position = replace_na(new_position, TRUE))

# Remove the temporary 'prev_exdate' column
strangle1_call <- select(strangle1_call, -prev_exdate)

# Identify all unique dates where new_position is TRUE
dates_with_new_position <- unique(strangle1_call$date[strangle1_call$new_position == TRUE])

# Update new_position to TRUE for all rows that have a matching date in dates_with_new_position
strangle1_call <- strangle1_call %>%
  mutate(new_position = if_else(date %in% dates_with_new_position, TRUE, new_position))




# Step 3: Adjust the code for finding OTM call options
strangle1_call <- strangle1_call %>%
  mutate(row_id = row_number(),  # Add an identifier for each row
         target_otm_strike = Close * 1.02,  # Calculate the target OTM strike price
         abs_diff_otm = abs(target_otm_strike - (strike_price/1000)))  # Calculate abs difference adjusted for strike price scaling

# Identify the row with the smallest abs_diff_otm for each date where new_position is TRUE
min_abs_diff_otm_rows <- strangle1_call %>%
  filter(new_position == TRUE) %>%
  group_by(date) %>%
  slice_min(abs_diff_otm, n = 1) %>%
  ungroup() %>%
  pull(row_id)  # Get the row IDs of the rows with the minimum abs_diff_otm

# Mark rows to keep: rows with the smallest abs_diff_otm in their date group where new_position is TRUE, and all rows where new_position is FALSE
strangle1_call <- strangle1_call %>%
  mutate(keep = row_id %in% min_abs_diff_otm_rows | new_position == FALSE)

# Filter the dataset to keep only the marked rows and remove helper columns
strangle1_call_filtered <- strangle1_call %>%
  filter(keep) %>%
  select(-row_id, -keep)  # Clean up the dataset by removing helper columns




# Step 4: Manage the OTM call contracts
# Step 4.1: Add helper columns to track the last 'strike_price' and 'optionid' where 'new_position' is TRUE
strangle1_call_filtered <- strangle1_call_filtered %>%
  mutate(last_new_strike = ifelse(new_position == TRUE, strike_price, NA),
         last_new_optionid = ifelse(new_position == TRUE, optionid, NA)) %>%
  fill(last_new_strike, .direction = "down") %>%
  fill(last_new_optionid, .direction = "down")

# Step 4.2: Filter out rows where both 'close_position' and 'new_position' are FALSE
# and either 'strike_price' or 'optionid' doesn't match their respective last values
strangle1_call_filtered <- strangle1_call_filtered %>%
  filter(!(close_position == FALSE & new_position == FALSE & 
             (strike_price != last_new_strike | optionid != last_new_optionid)))

# Step 4.3: Remove the helper columns
strangle1_call_filtered <- select(strangle1_call_filtered, -last_new_strike, -last_new_optionid)




# Step 5: Find the OTM call contract for days where the contract is closed
# Step 5.1: Create helper columns for the previous 'strike_price' and 'optionid'
# Initialize them with NA and then fill in the values from rows where both 'close_position' and 'new_position' are FALSE
strangle1_call_filtered <- strangle1_call_filtered %>%
  mutate(prev_strike_price = ifelse(close_position == FALSE & new_position == FALSE, strike_price, NA),
         prev_optionid = ifelse(close_position == FALSE & new_position == FALSE, optionid, NA)) %>%
  fill(prev_strike_price, .direction = "down") %>%
  fill(prev_optionid, .direction = "down")

# Step 5.2: Apply the logic for keeping rows based on 'close_position', 'strike_price', and 'optionid'
# Mark rows to keep when 'close_position' is TRUE and both 'strike_price' and 'optionid' match their previous values
strangle1_call_filtered <- strangle1_call_filtered %>%
  mutate(keep = ifelse(close_position == TRUE,
                       (strike_price == prev_strike_price & optionid == prev_optionid),
                       TRUE))

# Step 5.3: Filter the dataset to keep only the marked rows and remove helper columns
strangle1_call_filtered <- strangle1_call_filtered %>%
  filter(keep) %>%
  select(-prev_strike_price, -prev_optionid, -keep)






#New open position logic as the datesets are now cleaned ####
#Put: 
# Step 1: Add a column for the previous 'exdate'
strangle1_put_filtered <- strangle1_put_filtered %>%
  mutate(prev_exdate = lag(exdate))
# Step 2: Update 'new_position' to TRUE when 'exdate' changes from the previous row
strangle1_put_filtered <- strangle1_put_filtered %>%
  mutate(new_position = ifelse(exdate != prev_exdate | row_number() == 1, TRUE, new_position)) #ensure that the first row will be an entry
# Step 3: Remove the helper 'prev_exdate' column
strangle1_put_filtered <- select(strangle1_put_filtered, -prev_exdate)

#Call: 
# Step 1: Add a column for the previous 'exdate'
strangle1_call_filtered <- strangle1_call_filtered %>%
  mutate(prev_exdate = lag(exdate))
# Step 2: Update 'new_position' to TRUE when 'exdate' changes from the previous row
strangle1_call_filtered <- strangle1_call_filtered %>%
  mutate(new_position = ifelse(exdate != prev_exdate | row_number() == 1, TRUE, new_position)) #ensure that the first row will be an entry
# Step 3: Remove the helper 'prev_exdate' column
strangle1_call_filtered <- select(strangle1_call_filtered, -prev_exdate)







#New close position logic as the datesets are now cleaned ####
#Put
# Step 1: Add a column for the 'new_position' of the next row
strangle1_put_filtered <- strangle1_put_filtered %>%
  mutate(next_new_position = lead(new_position))
# Step 2: Update 'close_position' to TRUE when the next row's 'new_position' is TRUE
strangle1_put_filtered <- strangle1_put_filtered %>%
  mutate(close_position = ifelse(next_new_position == TRUE, TRUE, close_position))
# Step 3: Remove the helper 'next_new_position' column
strangle1_put_filtered <- select(strangle1_put_filtered, -next_new_position)


#Call:
# Step 1: Add a column for the 'new_position' of the next row
strangle1_call_filtered <- strangle1_call_filtered %>%
  mutate(next_new_position = lead(new_position))
# Step 2: Update 'close_position' to TRUE when the next row's 'new_position' is TRUE
strangle1_call_filtered <- strangle1_call_filtered %>%
  mutate(close_position = ifelse(next_new_position == TRUE, TRUE, close_position))
# Step 3: Remove the helper 'next_new_position' column
strangle1_call_filtered <- select(strangle1_call_filtered, -next_new_position)



#Clean the dataset
# Filter out rows where both 'close_position' and 'new_position' are TRUE
strangle1_put_filtered <- strangle1_put_filtered %>%
  filter(!(close_position == TRUE & new_position == TRUE))

strangle1_call_filtered <- strangle1_call_filtered %>%
  filter(!(close_position == TRUE & new_position == TRUE))




#Implement the strategy ####
#first we will calculate the daily PnL for each leg of the straddle seperatly and than combine it in one dataset 
#to calculate the development of the Margin Account over time 

#Step 1: Midpoint price
# Calculate the midpoint price as the average of 'best_bid' and 'best_offer'
# Put:
strangle1_put_filtered <- strangle1_put_filtered %>%
  mutate(midpoint_price = ifelse(close_position == TRUE,
                                 (1/3) * best_bid + (2/3) * best_offer,
                                 (2/3) * best_bid + (1/3) * best_offer))

# Call:
strangle1_call_filtered <- strangle1_call_filtered %>%
  mutate(midpoint_price = ifelse(close_position == TRUE,
                                 (1/3) * best_bid + (2/3) * best_offer,
                                 (2/3) * best_bid + (1/3) * best_offer))


#Step 2: Calculate the daily PnL of selling the OTM put or call for each month
# Put:
strangle1_put_filtered <- strangle1_put_filtered %>%
  mutate(daily_pnl = ifelse(new_position == TRUE, 
                            0, 
                            -100 * (midpoint_price - lag(midpoint_price))))

# Call:
strangle1_call_filtered <- strangle1_call_filtered %>%
  mutate(daily_pnl = ifelse(new_position == TRUE, 
                            0, 
                            -100 * (midpoint_price - lag(midpoint_price))))





#Step 3: Merge the datasets 
# Step 3.1: Merge daily pnl
# Rename 'daily_pnl' in both datasets for clarity and include 'strike_price'
strangle1_put_filtered <- strangle1_put_filtered %>%
  rename(daily_pnl_put = daily_pnl,
         strike_price_put = strike_price,
         midpoint_price_put = midpoint_price)

strangle1_call_filtered <- strangle1_call_filtered %>%
  rename(daily_pnl_call = daily_pnl,
         strike_price_call = strike_price,
         midpoint_price_call = midpoint_price)

# Perform a left join to create the new dataset 'Strangle1' and include the 'strike_price' columns
Strangle1 <- strangle1_put_filtered %>%
  select(date, exdate, daily_pnl_put, close_position, new_position, strike_price_put, Close, midpoint_price_put) %>%
  left_join(strangle1_call_filtered %>% select(date, daily_pnl_call, strike_price_call, midpoint_price_call),
            by = "date")

# Fill NA values with 0 in 'daily_pnl_call' and 'daily_pnl_put'
Strangle1 <- Strangle1 %>%
  mutate(daily_pnl_call = coalesce(daily_pnl_call, 0),
         daily_pnl_put = coalesce(daily_pnl_put, 0))

# Clean data
Strangle1$strike_price_call[1765] <- 1055000
Strangle1$strike_price_call[1881] <- 1130000
Strangle1$strike_price_call[2496] <- 1430000

# Step 3.2: Calculate the daily PnL combining both legs
Strangle1 <- Strangle1 %>%
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
Strangle1 <- Strangle1 %>%
  left_join(treasury_data, by = c("date" = "DATE"))


# Step 4: Implement the strategy over the data (adjusted contract sizes)
# Initialize MarginAccount for the first row and N_contracts based on the initial MarginAccount
Strangle1$MarginAccount[1] <- 1000000  # Initial MarginAccount
# Initial N_contracts based on the adjusted formula for the first row's average strike price
Strangle1$N_contracts[1] <- floor((Strangle1$MarginAccount[1] / (0.75 * ((Strangle1$strike_price_put[1] + Strangle1$strike_price_call[1]) / 2))) * 10)

# Define the doubled cost per contract per trade
cost_per_contract <- 2.20

# Loop through the dataset starting from the second row
for (i in 2:nrow(Strangle1)) {
  # Update N_contracts for rows where new_position is TRUE using the new average strike price formula
  if (Strangle1$new_position[i] == TRUE) {
    Strangle1$N_contracts[i] <- floor((Strangle1$MarginAccount[i-1] / (0.75 * ((Strangle1$strike_price_put[i] + Strangle1$strike_price_call[i]) / 2))) * 10)
  } else {
    # Carry forward the N_contracts value from the previous row if new_position is not TRUE
    Strangle1$N_contracts[i] <- Strangle1$N_contracts[i-1]
  }
  
  # Calculate the cost of opening or closing positions
  # Ensure that neither new_position nor close_position is NA before evaluating them
  if (!is.na(Strangle1$new_position[i]) && !is.na(Strangle1$close_position[i])) {
    if (Strangle1$new_position[i] == TRUE || Strangle1$close_position[i] == TRUE) {
      trade_cost <- Strangle1$N_contracts[i] * cost_per_contract
    } else {
      trade_cost <- 0
    }
  } else {
    trade_cost <- 0  # Default to 0 if there is an NA
  }
  
  # Calculate MarginAccount for each row accounting for the transaction costs
  Strangle1$MarginAccount[i] <- (1 + Strangle1$RFR[i-1]) * Strangle1$MarginAccount[i-1] + 
    Strangle1$N_contracts[i] * Strangle1$daily_pnl[i] - trade_cost  # Subtracting the trade cost here
}





#Calculate the Margin Requirement of the Strategy ####
#Calculate the Margin Requirement of the Strategy
# Calculate Initial Margin for puts (IM_P) and calls (IM_C)
Strangle1 <- Strangle1 %>%
  mutate(
    Out_of_the_Money_Amount_P = pmax(0, Close - (strike_price_put / 1000)),
    Out_of_the_Money_Amount_C = pmax(0, (strike_price_call / 1000) - Close),
    IM_P = midpoint_price_put + pmax((0.15 * Close - Out_of_the_Money_Amount_P), (0.10 * (strike_price_put / 1000))),
    IM_C = midpoint_price_call + pmax((0.15 * Close - Out_of_the_Money_Amount_C), (0.10 * (strike_price_call / 1000)))
  )

# Determine Margin_Requirement for each row
Strangle1 <- Strangle1 %>%
  mutate(
    Margin_Requirement = ifelse(IM_P > IM_C, 
                                IM_P + midpoint_price_call, 
                                IM_C + midpoint_price_put)
  )

# Clean up by removing intermediary columns if no longer needed
Strangle1 <- select(Strangle1, -Out_of_the_Money_Amount_P, -Out_of_the_Money_Amount_C, -IM_P, -IM_C)

# Multiply by number of contracts
Strangle1 <- Strangle1 %>%
  mutate(
    Margin_Requirement = Margin_Requirement * N_contracts *100
  )


#Put Index ####
# Step 1: Import the PUT index data and calculate daily PnL
# Import Put Index data
put_data <- read_excel("Put Index.xlsx")
# Calculate daily PnL for Put Index
put_data <- put_data %>%
  mutate(daily_pnl_put_index = `PUT Index` - lag(`PUT Index`))


# Step 2: Merge the Datesets
Strangle1 <- left_join(Strangle1, put_data, by = c("date"="Date"))
# Replace NA values in 'daily_pnl_put_index' with 0
Strangle1$daily_pnl_put_index <- replace_na(Strangle1$daily_pnl_put_index, 0)


# Step 3: Calculate the development of 100k 
# Calculate the initial contract size 'x' using the correct column name "PUT Index"
x <- round(1000000 / Strangle1$`PUT Index`[1], digits = 0)

# Initialize the 'Put_Strategy' column with the first value as the initial investment
Strangle1$Put_Strategy <- rep(0, nrow(Strangle1))
Strangle1$Put_Strategy[1] <- 1000000  # Set the initial value for the first row

# Loop through the dataset starting from the second row
for (i in 2:nrow(Strangle1)) {
  Strangle1$Put_Strategy[i] <- x * Strangle1$daily_pnl_put_index[i] + Strangle1$Put_Strategy[i - 1]
}

#Development of Investment in S&P 500 ####
# Step 1: Calculate daily PnL for SPX
Strangle1 <- Strangle1 %>%
  mutate(daily_pnl_spx = Close - lag(Close))
# Set the first value of 'daily_pnl_spx' to 0
Strangle1$daily_pnl_spx[1] <- 0
# Replace NA values in 'daily_pnl_spx' with 0
Strangle1$daily_pnl_spx <- replace_na(Strangle1$daily_pnl_spx, 0)

# Step 2: Calculate the development of 100k 
# Calculate the initial contract size 'x' using the correct column name "PUT Index"
x <- round(1000000 / Strangle1$Close[1], digits = 0)

# Initialize the 'SPX' column with the first value as the initial investment
Strangle1$SPX <- rep(0, nrow(Strangle1))
Strangle1$SPX[1] <- 1000000  # Set the initial value for the first row

# Loop through the dataset starting from the second row
for (i in 2:nrow(Strangle1)) {
  Strangle1$SPX[i] <- x * Strangle1$daily_pnl_spx[i] + Strangle1$SPX[i - 1]
}


#Plot the results ####
# Calculate the upper limit for y-axis based on the maximum values of the plotted series
upper_limit <- max(c(max(Strangle1$MarginAccount, na.rm = TRUE), 
                     max(Strangle1$Put_Strategy, na.rm = TRUE),
                     max(Strangle1$SPX, na.rm = TRUE),
                     max(Strangle1$Margin_Requirement, na.rm = TRUE)))

# Define y-axis range starting from 0 to the upper limit
y_range <- c(0, upper_limit)

par(family="Times New Roman")

# Set up the plot with the first line to establish the plot area
plot(Strangle1$date, Strangle1$MarginAccount, type = 'l', 
     col = "darkblue", ylim = y_range, xaxt = 'n', # Disable default x-axis
     xlab = "", ylab = "Development of $1'000'000", 
     main = "Strangle1 Performance", lwd = 2) # Increase line width with lwd

# Add the remaining series with increased line width
lines(Strangle1$date, Strangle1$Put_Strategy, col = "red", lwd = 2)
lines(Strangle1$date, Strangle1$SPX, col = "black", lwd = 2)
lines(Strangle1$date, Strangle1$Margin_Requirement, col = "deepskyblue4", lwd = 1) 

# Custom x-axis labels for each year
year_ticks <- seq(from=min(Strangle1$date, na.rm=TRUE),
                  to=max(Strangle1$date, na.rm=TRUE), by="years")
axis(1, at=year_ticks, labels=format(year_ticks, "%Y"), cex.axis=0.8, las=2)

# Add light gridlines
abline(h=seq(from=0, to=upper_limit, by=upper_limit/5), 
       col="grey", lty="dotted", lwd=0.5)

abline(v=year_ticks, col="grey", lty="dotted", lwd=0.5)

# Add a legend without a box ('n' means no box around the legend)
legend("topleft", legend = c("Strangle1", "PUT", "S&P 500", "Margin Requirement Strangle1"),
       col = c("darkblue", "red", "black", "deepskyblue4"), lty = 1, bty = "n", cex = 0.8, lwd=c(2,2,2,2))


#Descriptive statistics Strangle1 ####
# Step 1: Calculate Simple Returns
Strangle1 <- Strangle1 %>%
  mutate(Returns = (MarginAccount - lag(MarginAccount)) / lag(MarginAccount))

# Replace the first NA value in Returns with 0 
Strangle1$Returns[1] <- 0  


# Step 2: Generate Summary Statistics
summary_stats_Strangle1 <- Strangle1 %>%
  summarise(N.Obs = n(),
            Mean = mean(Returns, na.rm = TRUE),
            Median = median(Returns, na.rm = TRUE),
            S.D. = sd(Returns, na.rm = TRUE),
            Min = min(Returns, na.rm = TRUE),
            Max = max(Returns, na.rm = TRUE))


# Step 3: Calculate Max Drawdown
# Initialize variables to track the peak and maximum drawdown
peak = Strangle1$MarginAccount[1]
max_drawdown = 0

# Loop through the MarginAccount to find the maximum drawdown
for (value in Strangle1$MarginAccount) {
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
skewness <- skewness(Strangle1$Returns, na.rm = TRUE)
kurtosis <- kurtosis(Strangle1$Returns, na.rm = TRUE)


# Step 5: Calculate Cumulative Return
cumulative_return <- last(Strangle1$MarginAccount) / first(Strangle1$MarginAccount) - 1


# Step 6: Annualize Returns and Volatility
annualized_return <- (1 + mean(Strangle1$Returns, na.rm = TRUE)) ^ 252 - 1  # Assuming 252 trading days in a year
annualized_volatility <- sd(Strangle1$Returns, na.rm = TRUE) * sqrt(252)


# Step 7: Calculate the Sharpe Ratio
# Calculate the Daily Excess Returns
Strangle1$ExcessReturns <- Strangle1$Returns - Strangle1$RFR

# Calculate the Mean Daily Excess Retun and Standard Deviation for the Daily Returns 
mean_daily_excess_return_Strangle1 <- mean(Strangle1$ExcessReturns, na.rm = TRUE)
sd_daily_excess_returns_Strangle1 <- sd(Strangle1$Returns, na.rm = TRUE)

# Compute the Daily Sharpe Ratio
daily_sharpe_ratio_Strangle1 <- mean_daily_excess_return_Strangle1 / sd_daily_excess_returns_Strangle1

# Annualize the Sharpe Ratio
annualized_sharpe_ratio_Strangle1 <- daily_sharpe_ratio_Strangle1 * sqrt(252)

annualized_sharpe_ratio_Strangle1


# Step 8: Calculate the Sortino Ratio
# Calculate the Downside Deviation
squared_negative_excess_returns <- ifelse(Strangle1$ExcessReturns < 0, 
                                          Strangle1$ExcessReturns^2, 
                                          0)
mean_squared_negative_excess_returns <- mean(squared_negative_excess_returns, na.rm = TRUE)
downside_deviation <- sqrt(mean_squared_negative_excess_returns)

# Calculate the Daily Sortino Ratio
daily_sortino_ratio <- mean_daily_excess_return_Strangle1 / downside_deviation

# Annualize the Sortino Ratio
annualized_sortino_ratio <- daily_sortino_ratio * sqrt(252)

annualized_sortino_ratio


#Step 9: Calculate the Calmar Ratio
# Calculate the Annualized Mean of Daily Excess Returns
annualized_mean_daily_excess_return <- mean(Strangle1$ExcessReturns, na.rm = TRUE) * 252

# Compute the Calmar Ratio using the Annualized Mean of Daily Excess Returns
calmar_ratio_Strangle1 <- annualized_mean_daily_excess_return / max_drawdown

# The result is the Calmar Ratio for your strategy
calmar_ratio_Strangle1





# Combine all the results into a single tibble
results_Strangle1 <- tibble(
  Statistics = c("Mean", "Median", "S.D.", "Min", "Max", "Max Drawdown", "Skewness", "Kurtosis", "Cumulative Return", "Annualized Return", "Annualized Volatility", "Annualized Sharpe Ratio", "Annualized Sortino Ratio", "Calmar Ratio"),
  Strangle1 = c(summary_stats_Strangle1$Mean, summary_stats_Strangle1$Median, summary_stats_Strangle1$S.D., summary_stats_Strangle1$Min, summary_stats_Strangle1$Max, 
                max_drawdown, skewness, kurtosis, cumulative_return, annualized_return, annualized_volatility, annualized_sharpe_ratio_Strangle1, annualized_sortino_ratio, calmar_ratio_Strangle1)
)

# make the dataset visually nicer
# Convert tibble to data frame if it is not already
results_Strangle1 <- as.data.frame(results_Strangle1)

# Set the row names and remove the 'Statistics' column
rownames(results_Strangle1) <- results_Strangle1$Statistics
results_Strangle1 <-results_Strangle1[-1]

# Round the numbers in the Strangle1 column to 2 decimal places
results_Strangle1$Strangle1 <- round(results_Strangle1$Strangle1, 4)

# Print the results to check
print(results_Strangle1)
#Descriptive statistics Put Index####
# Step 1: Calculate Simple Returns_Put
Strangle1 <- Strangle1 %>%
  mutate(Returns_Put = (Put_Strategy - lag(Put_Strategy)) / lag(Put_Strategy))

# Replace the first NA value in Returns_Put with 0 
Strangle1$Returns_Put[1] <- 0  


# Step 2: Generate Summary Statistics
summary_stats_put <- Strangle1 %>%
  summarise(N.Obs = n(),
            Mean = mean(Returns_Put, na.rm = TRUE),
            Median = median(Returns_Put, na.rm = TRUE),
            S.D. = sd(Returns_Put, na.rm = TRUE),
            Min = min(Returns_Put, na.rm = TRUE),
            Max = max(Returns_Put, na.rm = TRUE))


# Step 3: Calculate Max Drawdown
# Initialize variables to track the peak and maximum drawdown
peak = Strangle1$Put_Strategy[1]
max_drawdown = 0

# Loop through the Put_Strategy to find the maximum drawdown
for (value in Strangle1$Put_Strategy) {
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
skewness <- skewness(Strangle1$Returns_Put, na.rm = TRUE)
kurtosis <- kurtosis(Strangle1$Returns_Put, na.rm = TRUE)


# Step 5: Calculate Cumulative Return
cumulative_return <- last(Strangle1$Put_Strategy) / first(Strangle1$Put_Strategy) - 1


# Step 6: Annualize Returns_Put and Volatility
annualized_return <- (1 + mean(Strangle1$Returns_Put, na.rm = TRUE)) ^ 252 - 1  # Assuming 252 trading days in a year
annualized_volatility <- sd(Strangle1$Returns_Put, na.rm = TRUE) * sqrt(252)


# Step 7: Calculate the Sharpe Ratio
# Calculate the Daily Excess Returns_Put
Strangle1$ExcessReturns_Put <- Strangle1$Returns_Put - Strangle1$RFR

# Calculate the Mean Daily Excess Retun and Standard Deviation for the Daily Returns_Put 
mean_daily_excess_return_put <- mean(Strangle1$ExcessReturns_Put, na.rm = TRUE)
sd_daily_excess_Returns_Put <- sd(Strangle1$Returns_Put, na.rm = TRUE)

# Compute the Daily Sharpe Ratio
daily_sharpe_ratio <- mean_daily_excess_return_put / sd_daily_excess_Returns_Put

# Annualize the Sharpe Ratio
annualized_sharpe_ratio <- daily_sharpe_ratio * sqrt(252)

annualized_sharpe_ratio


# Step 8: Calculate the Sortino Ratio
# Calculate the Downside Deviation
squared_negative_excess_Returns_Put <- ifelse(Strangle1$ExcessReturns_Put < 0, 
                                              Strangle1$ExcessReturns_Put^2, 
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
annualized_mean_daily_excess_return <- mean(Strangle1$ExcessReturns_Put, na.rm = TRUE) * 252

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
Strangle1 <- Strangle1 %>%
  mutate(Returns_SPX = (SPX - lag(SPX)) / lag(SPX))

# Replace the first NA value in Returns_SPX with 0 
Strangle1$Returns_SPX[1] <- 0  


# Step 2: Generate Summary Statistics
summary_stats_spx <- Strangle1 %>%
  summarise(N.Obs = n(),
            Mean = mean(Returns_SPX, na.rm = TRUE),
            Median = median(Returns_SPX, na.rm = TRUE),
            S.D. = sd(Returns_SPX, na.rm = TRUE),
            Min = min(Returns_SPX, na.rm = TRUE),
            Max = max(Returns_SPX, na.rm = TRUE))


# Step 3: Calculate Max Drawdown
# Initialize variables to track the peak and maximum drawdown
peak = Strangle1$SPX[1]
max_drawdown = 0

# Loop through the SPX to find the maximum drawdown
for (value in Strangle1$SPX) {
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
skewness <- skewness(Strangle1$Returns_SPX, na.rm = TRUE)
kurtosis <- kurtosis(Strangle1$Returns_SPX, na.rm = TRUE)


# Step 5: Calculate Cumulative Return
cumulative_return <- last(Strangle1$SPX) / first(Strangle1$SPX) - 1


# Step 6: Annualize Returns_SPX and Volatility
annualized_return <- (1 + mean(Strangle1$Returns_SPX, na.rm = TRUE)) ^ 252 - 1  # Assuming 252 trading days in a year
annualized_volatility <- sd(Strangle1$Returns_SPX, na.rm = TRUE) * sqrt(252)


# Step 7: Calculate the Sharpe Ratio
# Calculate the Daily Excess Returns_SPX
Strangle1$ExcessReturns_SPX <- Strangle1$Returns_SPX - Strangle1$RFR

# Calculate the Mean Daily Excess Retun and Standard Deviation for the Daily Returns_SPX 
mean_daily_excess_return_spx <- mean(Strangle1$ExcessReturns_SPX, na.rm = TRUE)
sd_daily_excess_Returns_SPX <- sd(Strangle1$Returns_SPX, na.rm = TRUE)

# Compute the Daily Sharpe Ratio
daily_sharpe_ratio <- mean_daily_excess_return_spx / sd_daily_excess_Returns_SPX

# Annualize the Sharpe Ratio
annualized_sharpe_ratio <- daily_sharpe_ratio * sqrt(252)

annualized_sharpe_ratio


# Step 8: Calculate the Sortino Ratio
# Calculate the Downside Deviation
squared_negative_excess_Returns_SPX <- ifelse(Strangle1$ExcessReturns_SPX < 0, 
                                              Strangle1$ExcessReturns_SPX^2, 
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
annualized_mean_daily_excess_return <- mean(Strangle1$ExcessReturns_SPX, na.rm = TRUE) * 252

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
model_strangle1 <- lm(Returns ~ Returns_SPX, data = Strangle1)
beta_strangle1 <- coef(model_strangle1)["Returns_SPX"]  # Extract the beta coefficient
# Step 2: Calculate Alpha
# The RFR is already in the dataframe and adjusted for daily use.
annualized_market_return <- mean(Strangle1$Returns_SPX, na.rm = TRUE) * 252  # Annualize SPX returns
# Annualize strategy returns for comparison
annualized_strategy_return <- mean(Strangle1$Returns, na.rm = TRUE) * 252
# Annualize the mean daily RFR
annualized_rfr <- mean(Strangle1$RFR, na.rm = TRUE) * 360
# Calculate alpha using the CAPM formula
alpha_Strangle1 <- annualized_strategy_return - (annualized_rfr + beta_strangle1 * (annualized_market_return - annualized_rfr))

# Output the results
cat("Beta: ", beta_strangle1, "\n")
cat("Alpha: ", alpha_Strangle1, "\n")

# Add to other stats
new_rows <- data.frame(Strangle1 = c(alpha_Strangle1, beta_strangle1))
rownames(new_rows) <- c("Alpha", "Beta")

# Combine with the original results
# Use rbind to append the data
results_Strangle1 <- rbind(results_Strangle1, new_rows)

# Round the Strangle1 column
results_Strangle1$Strangle1 <- round(results_Strangle1$Strangle1, 4)

# Print the dataframe to check
print(results_Strangle1)




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
model_putindex <- lm(Returns_Put ~ Returns_SPX, data = Strangle1)
beta_putindex <- coef(model_putindex)["Returns_SPX"]  # Extract the beta coefficient

# Step 2: Calculate Alpha
# The RFR is already in the dataframe and adjusted for daily use.
annualized_market_return <- mean(Strangle1$Returns_SPX, na.rm = TRUE) * 252  # Annualize SPX returns
# Annualize strategy returns for comparison
annualized_putindex_return <- mean(Strangle1$Returns_Put, na.rm = TRUE) * 252
# Annualize the mean daily RFR
annualized_rfr <- mean(Strangle1$RFR, na.rm = TRUE) * 360
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
combined_results <- cbind(results_Strangle1, results_put, results_spx)

# Rename the columns 
colnames(combined_results) <- c("Strangle1", "Put Index", "S&P 500")

# Print the combined results to check
print(combined_results)



