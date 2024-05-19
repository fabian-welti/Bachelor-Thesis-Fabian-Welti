###################################

#Strangle2
#4% OTM

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
strangle2_put <- Options_data %>% 
  filter(cp_flag != "C")

#filter out the call options
strangle2_call <- Options_data %>% 
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
strangle2_put <- strangle2_put %>%
  filter(as.Date(exdate, format = "%Y%m%d") %in% all_relevant_dates)
# call data
strangle2_call <- strangle2_call %>%
  filter(as.Date(exdate, format = "%Y%m%d") %in% all_relevant_dates)










#Combine S&P 500 prices with options dataset ####
spx_data <- read_excel("^SPX 1990-12.01.2024.xlsx", skip = 1)
spx_data$Date <- as.Date(spx_data$Date, format = "%Y-%m-%d") #converting the Date column 

spx_data_selected <- select(spx_data, Date, Open, Close)
# merge with put data
strangle2_put <- left_join(strangle2_put, spx_data_selected, by = c("date"="Date"))
# merge with put data
strangle2_call <- left_join(strangle2_call, spx_data_selected, by = c("date"="Date"))



#Calculate the absolute difference from Strike to S&P Close ####
# Put 
strangle2_put$abs_diff <- abs(strangle2_put$Close - (strangle2_put$strike_price)/1000)
# Call
strangle2_call$abs_diff <- abs(strangle2_call$Close - (strangle2_call$strike_price)/1000)


#Clean the datesets ####
# Filter the dataset to exclude rows where 'date' is equal to 'exdate' since want to close posiotion before
strangle2_put <- strangle2_put %>%
  filter(date != exdate)
strangle2_call <- strangle2_call %>%
  filter(date != exdate)


#Filter out the 4% OTM contracts for the Put dataset####
# Step 1: Find days where position is closed
# Add a new column 'next_exdate' that contains the exdate of the next row
strangle2_put <- strangle2_put %>%
  mutate(next_exdate = lead(exdate))

# Compare 'exdate' with 'next_exdate' to determine if the position should be closed
# Assign TRUE if exdate is different from next_exdate (position should be closed)
# The last row will automatically get NA for 'next_exdate', so replace NA with TRUE to close the position
strangle2_put <- strangle2_put %>%
  mutate(close_position = if_else(exdate != next_exdate, TRUE, FALSE)) %>%
  mutate(close_position = replace_na(close_position, TRUE))

# Remove the temporary 'next_exdate' column
strangle2_put <- select(strangle2_put, -next_exdate)

# Identify all unique dates where close_position is TRUE
dates_to_mark <- unique(strangle2_put$date[strangle2_put$close_position == TRUE])

# Update close_position to TRUE for all rows that have a matching date in dates_to_mark
strangle2_put <- strangle2_put %>%
  mutate(close_position = if_else(date %in% dates_to_mark, TRUE, close_position))



# Step 2: Find days where position is opened
# Add a new column 'prev_exdate' that contains the exdate of the previous row
strangle2_put <- strangle2_put %>%
  mutate(prev_exdate = lag(exdate))

# Compare 'exdate' with 'prev_exdate' to determine if a new position starts
# Assign TRUE if exdate is different from prev_exdate (indicating a new position)
# The first row will automatically get NA for 'prev_exdate', so replace NA with TRUE to indicate a new position
strangle2_put <- strangle2_put %>%
  mutate(new_position = if_else(exdate != prev_exdate, TRUE, FALSE)) %>%
  mutate(new_position = replace_na(new_position, TRUE))

# Remove the temporary 'prev_exdate' column
strangle2_put <- select(strangle2_put, -prev_exdate)

# Identify all unique dates where new_position is TRUE
dates_with_new_position <- unique(strangle2_put$date[strangle2_put$new_position == TRUE])

# Update new_position to TRUE for all rows that have a matching date in dates_with_new_position
strangle2_put <- strangle2_put %>%
  mutate(new_position = if_else(date %in% dates_with_new_position, TRUE, new_position))




# Step 3: Adjust the code for finding OTM put options
strangle2_put <- strangle2_put %>%
  mutate(row_id = row_number(),  # Add an identifier for each row
         target_otm_strike = Close * 0.96,  # Calculate the target OTM strike price
         abs_diff_otm = abs(target_otm_strike - (strike_price/1000)))  # Calculate abs difference adjusted for strike price scaling

# Identify the row with the smallest abs_diff_otm for each date where new_position is TRUE
min_abs_diff_otm_rows <- strangle2_put %>%
  filter(new_position == TRUE) %>%
  group_by(date) %>%
  slice_min(abs_diff_otm, n = 1) %>%
  ungroup() %>%
  pull(row_id)  # Get the row IDs of the rows with the minimum abs_diff_otm

# Mark rows to keep: rows with the smallest abs_diff_otm in their date group where new_position is TRUE, and all rows where new_position is FALSE
strangle2_put <- strangle2_put %>%
  mutate(keep = row_id %in% min_abs_diff_otm_rows | new_position == FALSE)

# Filter the dataset to keep only the marked rows and remove helper columns
strangle2_put_filtered <- strangle2_put %>%
  filter(keep) %>%
  select(-row_id, -keep)  # Clean up the dataset by removing helper columns




# Step 4: Manage the OTM put contracts
# Step 4.1: Add helper columns to track the last 'strike_price' and 'optionid' where 'new_position' is TRUE
strangle2_put_filtered <- strangle2_put_filtered %>%
  mutate(last_new_strike = ifelse(new_position == TRUE, strike_price, NA),
         last_new_optionid = ifelse(new_position == TRUE, optionid, NA)) %>%
  fill(last_new_strike, .direction = "down") %>%
  fill(last_new_optionid, .direction = "down")

# Step 4.2: Filter out rows where both 'close_position' and 'new_position' are FALSE
# and either 'strike_price' or 'optionid' doesn't match their respective last values
strangle2_put_filtered <- strangle2_put_filtered %>%
  filter(!(close_position == FALSE & new_position == FALSE & 
             (strike_price != last_new_strike | optionid != last_new_optionid)))

# Step 4.3: Remove the helper columns
strangle2_put_filtered <- select(strangle2_put_filtered, -last_new_strike, -last_new_optionid)




# Step 5: Find the OTM put contract for days where the contract is closed
# Step 5.1: Create helper columns for the previous 'strike_price' and 'optionid'
# Initialize them with NA and then fill in the values from rows where both 'close_position' and 'new_position' are FALSE
strangle2_put_filtered <- strangle2_put_filtered %>%
  mutate(prev_strike_price = ifelse(close_position == FALSE & new_position == FALSE, strike_price, NA),
         prev_optionid = ifelse(close_position == FALSE & new_position == FALSE, optionid, NA)) %>%
  fill(prev_strike_price, .direction = "down") %>%
  fill(prev_optionid, .direction = "down")

# Step 5.2: Apply the logic for keeping rows based on 'close_position', 'strike_price', and 'optionid'
# Mark rows to keep when 'close_position' is TRUE and both 'strike_price' and 'optionid' match their previous values
strangle2_put_filtered <- strangle2_put_filtered %>%
  mutate(keep = ifelse(close_position == TRUE,
                       (strike_price == prev_strike_price & optionid == prev_optionid),
                       TRUE))

# Step 5.3: Filter the dataset to keep only the marked rows and remove helper columns
strangle2_put_filtered <- strangle2_put_filtered %>%
  filter(keep) %>%
  select(-prev_strike_price, -prev_optionid, -keep)





#Filter out the 4% OTM contracts for the Call dataset####
# Step 1: Find days where position is closed
# Add a new column 'next_exdate' that contains the exdate of the next row
strangle2_call <- strangle2_call %>%
  mutate(next_exdate = lead(exdate))

# Compare 'exdate' with 'next_exdate' to determine if the position should be closed
# Assign TRUE if exdate is different from next_exdate (position should be closed)
# The last row will automatically get NA for 'next_exdate', so replace NA with TRUE to close the position
strangle2_call <- strangle2_call %>%
  mutate(close_position = if_else(exdate != next_exdate, TRUE, FALSE)) %>%
  mutate(close_position = replace_na(close_position, TRUE))

# Remove the temporary 'next_exdate' column
strangle2_call <- select(strangle2_call, -next_exdate)

# Identify all unique dates where close_position is TRUE
dates_to_mark <- unique(strangle2_call$date[strangle2_call$close_position == TRUE])

# Update close_position to TRUE for all rows that have a matching date in dates_to_mark
strangle2_call <- strangle2_call %>%
  mutate(close_position = if_else(date %in% dates_to_mark, TRUE, close_position))



# Step 2: Find days where position is opened
# Add a new column 'prev_exdate' that contains the exdate of the previous row
strangle2_call <- strangle2_call %>%
  mutate(prev_exdate = lag(exdate))

# Compare 'exdate' with 'prev_exdate' to determine if a new position starts
# Assign TRUE if exdate is different from prev_exdate (indicating a new position)
# The first row will automatically get NA for 'prev_exdate', so replace NA with TRUE to indicate a new position
strangle2_call <- strangle2_call %>%
  mutate(new_position = if_else(exdate != prev_exdate, TRUE, FALSE)) %>%
  mutate(new_position = replace_na(new_position, TRUE))

# Remove the temporary 'prev_exdate' column
strangle2_call <- select(strangle2_call, -prev_exdate)

# Identify all unique dates where new_position is TRUE
dates_with_new_position <- unique(strangle2_call$date[strangle2_call$new_position == TRUE])

# Update new_position to TRUE for all rows that have a matching date in dates_with_new_position
strangle2_call <- strangle2_call %>%
  mutate(new_position = if_else(date %in% dates_with_new_position, TRUE, new_position))




# Step 3: Adjust the code for finding OTM call options
strangle2_call <- strangle2_call %>%
  mutate(row_id = row_number(),  # Add an identifier for each row
         target_otm_strike = Close * 1.04,  # Calculate the target OTM strike price
         abs_diff_otm = abs(target_otm_strike - (strike_price/1000)))  # Calculate abs difference adjusted for strike price scaling

# Identify the row with the smallest abs_diff_otm for each date where new_position is TRUE
min_abs_diff_otm_rows <- strangle2_call %>%
  filter(new_position == TRUE) %>%
  group_by(date) %>%
  slice_min(abs_diff_otm, n = 1) %>%
  ungroup() %>%
  pull(row_id)  # Get the row IDs of the rows with the minimum abs_diff_otm

# Mark rows to keep: rows with the smallest abs_diff_otm in their date group where new_position is TRUE, and all rows where new_position is FALSE
strangle2_call <- strangle2_call %>%
  mutate(keep = row_id %in% min_abs_diff_otm_rows | new_position == FALSE)

# Filter the dataset to keep only the marked rows and remove helper columns
strangle2_call_filtered <- strangle2_call %>%
  filter(keep) %>%
  select(-row_id, -keep)  # Clean up the dataset by removing helper columns




# Step 4: Manage the OTM call contracts
# Step 4.1: Add helper columns to track the last 'strike_price' and 'optionid' where 'new_position' is TRUE
strangle2_call_filtered <- strangle2_call_filtered %>%
  mutate(last_new_strike = ifelse(new_position == TRUE, strike_price, NA),
         last_new_optionid = ifelse(new_position == TRUE, optionid, NA)) %>%
  fill(last_new_strike, .direction = "down") %>%
  fill(last_new_optionid, .direction = "down")

# Step 4.2: Filter out rows where both 'close_position' and 'new_position' are FALSE
# and either 'strike_price' or 'optionid' doesn't match their respective last values
strangle2_call_filtered <- strangle2_call_filtered %>%
  filter(!(close_position == FALSE & new_position == FALSE & 
             (strike_price != last_new_strike | optionid != last_new_optionid)))

# Step 4.3: Remove the helper columns
strangle2_call_filtered <- select(strangle2_call_filtered, -last_new_strike, -last_new_optionid)




# Step 5: Find the OTM call contract for days where the contract is closed
# Step 5.1: Create helper columns for the previous 'strike_price' and 'optionid'
# Initialize them with NA and then fill in the values from rows where both 'close_position' and 'new_position' are FALSE
strangle2_call_filtered <- strangle2_call_filtered %>%
  mutate(prev_strike_price = ifelse(close_position == FALSE & new_position == FALSE, strike_price, NA),
         prev_optionid = ifelse(close_position == FALSE & new_position == FALSE, optionid, NA)) %>%
  fill(prev_strike_price, .direction = "down") %>%
  fill(prev_optionid, .direction = "down")

# Step 5.2: Apply the logic for keeping rows based on 'close_position', 'strike_price', and 'optionid'
# Mark rows to keep when 'close_position' is TRUE and both 'strike_price' and 'optionid' match their previous values
strangle2_call_filtered <- strangle2_call_filtered %>%
  mutate(keep = ifelse(close_position == TRUE,
                       (strike_price == prev_strike_price & optionid == prev_optionid),
                       TRUE))

# Step 5.3: Filter the dataset to keep only the marked rows and remove helper columns
strangle2_call_filtered <- strangle2_call_filtered %>%
  filter(keep) %>%
  select(-prev_strike_price, -prev_optionid, -keep)






#New open position logic as the datesets are now cleaned ####
#Put: 
# Step 1: Add a column for the previous 'exdate'
strangle2_put_filtered <- strangle2_put_filtered %>%
  mutate(prev_exdate = lag(exdate))
# Step 2: Update 'new_position' to TRUE when 'exdate' changes from the previous row
strangle2_put_filtered <- strangle2_put_filtered %>%
  mutate(new_position = ifelse(exdate != prev_exdate | row_number() == 1, TRUE, new_position)) #ensure that the first row will be an entry
# Step 3: Remove the helper 'prev_exdate' column
strangle2_put_filtered <- select(strangle2_put_filtered, -prev_exdate)

#Call: 
# Step 1: Add a column for the previous 'exdate'
strangle2_call_filtered <- strangle2_call_filtered %>%
  mutate(prev_exdate = lag(exdate))
# Step 2: Update 'new_position' to TRUE when 'exdate' changes from the previous row
strangle2_call_filtered <- strangle2_call_filtered %>%
  mutate(new_position = ifelse(exdate != prev_exdate | row_number() == 1, TRUE, new_position)) #ensure that the first row will be an entry
# Step 3: Remove the helper 'prev_exdate' column
strangle2_call_filtered <- select(strangle2_call_filtered, -prev_exdate)







#New close position logic as the datesets are now cleaned ####
#Put
# Step 1: Add a column for the 'new_position' of the next row
strangle2_put_filtered <- strangle2_put_filtered %>%
  mutate(next_new_position = lead(new_position))
# Step 2: Update 'close_position' to TRUE when the next row's 'new_position' is TRUE
strangle2_put_filtered <- strangle2_put_filtered %>%
  mutate(close_position = ifelse(next_new_position == TRUE, TRUE, close_position))
# Step 3: Remove the helper 'next_new_position' column
strangle2_put_filtered <- select(strangle2_put_filtered, -next_new_position)


#Call:
# Step 1: Add a column for the 'new_position' of the next row
strangle2_call_filtered <- strangle2_call_filtered %>%
  mutate(next_new_position = lead(new_position))
# Step 2: Update 'close_position' to TRUE when the next row's 'new_position' is TRUE
strangle2_call_filtered <- strangle2_call_filtered %>%
  mutate(close_position = ifelse(next_new_position == TRUE, TRUE, close_position))
# Step 3: Remove the helper 'next_new_position' column
strangle2_call_filtered <- select(strangle2_call_filtered, -next_new_position)



#Clean the dataset
# Filter out rows where both 'close_position' and 'new_position' are TRUE
strangle2_put_filtered <- strangle2_put_filtered %>%
  filter(!(close_position == TRUE & new_position == TRUE))

strangle2_call_filtered <- strangle2_call_filtered %>%
  filter(!(close_position == TRUE & new_position == TRUE))




#Implement the strategy ####
#first we will calculate the daily PnL for each leg of the straddle seperatly and than combine it in one dataset 
#to calculate the development of the Margin Account over time 

#Step 1: Midpoint price
# Calculate the midpoint price as the average of 'best_bid' and 'best_offer'
# Put:
strangle2_put_filtered <- strangle2_put_filtered %>%
  mutate(midpoint_price = ifelse(close_position == TRUE,
                                 (1/3) * best_bid + (2/3) * best_offer,
                                 (2/3) * best_bid + (1/3) * best_offer))

# Call:
strangle2_call_filtered <- strangle2_call_filtered %>%
  mutate(midpoint_price = ifelse(close_position == TRUE,
                                 (1/3) * best_bid + (2/3) * best_offer,
                                 (2/3) * best_bid + (1/3) * best_offer))


#Step 2: Calculate the daily PnL of selling the OTM put or call for each month
# Put:
strangle2_put_filtered <- strangle2_put_filtered %>%
  mutate(daily_pnl = ifelse(new_position == TRUE, 
                            0, 
                            -100 * (midpoint_price - lag(midpoint_price))))

# Call:
strangle2_call_filtered <- strangle2_call_filtered %>%
  mutate(daily_pnl = ifelse(new_position == TRUE, 
                            0, 
                            -100 * (midpoint_price - lag(midpoint_price))))





#Step 3: Merge the datasets 
# Step 3.1: Merge daily pnl
# Rename 'daily_pnl' in both datasets for clarity and include 'strike_price'
strangle2_put_filtered <- strangle2_put_filtered %>%
  rename(daily_pnl_put = daily_pnl,
         strike_price_put = strike_price)

strangle2_call_filtered <- strangle2_call_filtered %>%
  rename(daily_pnl_call = daily_pnl,
         strike_price_call = strike_price)

# Perform a left join to create the new dataset 'Strangle2' and include the 'strike_price' columns
Strangle2 <- strangle2_put_filtered %>%
  select(date, exdate, daily_pnl_put, close_position, new_position, strike_price_put, Close) %>%
  left_join(strangle2_call_filtered %>% select(date, daily_pnl_call, strike_price_call),
            by = "date")

# Fill NA values with 0 in 'daily_pnl_call' and 'daily_pnl_put'
Strangle2 <- Strangle2 %>%
  mutate(daily_pnl_call = coalesce(daily_pnl_call, 0),
         daily_pnl_put = coalesce(daily_pnl_put, 0))

# Clean data
Strangle2$strike_price_call[1744] <- 1075000
Strangle2$strike_price_call[1860] <- 1150000
Strangle2$strike_price_call[2475] <- 1455000


# Step 3.2: Calculate the daily PnL combining both legs
Strangle2 <- Strangle2 %>%
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
Strangle2 <- Strangle2 %>%
  left_join(treasury_data, by = c("date" = "DATE"))


#Step 4: Implement the strategy over the data (adjusted contract sizes)
# Initialize MarginAccount for the first row and N_contracts based on the initial MarginAccount
Strangle2$MarginAccount[1] <- 1000000  # Initial MarginAccount
# Initial N_contracts based on the adjusted formula for the first row's average strike price
Strangle2$N_contracts[1] <- floor((Strangle2$MarginAccount[1] / (0.75 * ((Strangle2$strike_price_put[1] + Strangle2$strike_price_call[1]) / 2))) * 10)

# Loop through the dataset starting from the second row
for (i in 2:nrow(Strangle2)) {
  # Update N_contracts for rows where new_position is TRUE using the new average strike price formula
  if (Strangle2$new_position[i] == TRUE) {
    Strangle2$N_contracts[i] <- floor((Strangle2$MarginAccount[i-1] / (0.75 * ((Strangle2$strike_price_put[i] + Strangle2$strike_price_call[i]) / 2))) * 10)
  } else {
    # Carry forward the N_contracts value from the previous row if new_position is not TRUE
    Strangle2$N_contracts[i] <- Strangle2$N_contracts[i-1]
  }
  
  # Calculate MarginAccount for each row
  Strangle2$MarginAccount[i] <- (1 + Strangle2$RFR[i-1]) * Strangle2$MarginAccount[i-1] + 
    Strangle2$N_contracts[i] * Strangle2$daily_pnl[i]  # Assuming daily_pnl is already per contract
}



