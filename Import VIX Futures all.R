###################################

#VIX Futures Merge Import All

#Fabian Welti 
#04.02.2024

##################################
#Packages ####
library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(timeDate)





#### 06-13 ####
#WD ####
setwd("/Users/lucaswelti/Documents/Data BA Thesis /Futures/2006-2013")


#Combine the individual files for the monthly VIX futures ####
# List all CSV files in WD
csv_files_06_13 <- list.files(pattern = "^CFE_[FGHJKMNQUVXZ]\\d{2}_VX\\.csv$")
print(csv_files_06_13)

# Read and combine the files into one data frame
all_data_06_13 <- lapply(csv_files_06_13, read_csv)

# Combine all data frames into one
combined_data_06_13 <- bind_rows(all_data_06_13)
combined_data_06_13 <- combined_data_06_13 %>%
  rename(Date = `Trade Date`) #rename column

# Rescale the Values before 26 March 2007
  # Convert the 'Date' column to a Date type 
combined_data_06_13$Date <- as.Date(combined_data_06_13$Date,format = "%m/%d/%Y")
  # Set the rescale date
rescale_date <- as.Date("2007-03-26")
  # Rescale the data if the 'Date' is before the 'rescale_date'
combined_data_06_13 <- combined_data_06_13 %>%
  mutate(across(c(Open, High, Low, Close, Settle, Change), 
                ~ if_else(Date < rescale_date, . / 10, .)))


# Check the structure and summary of the combined data frame
str(combined_data_06_13)
summary(combined_data_06_13)

# Write the combined data frame to a CSV file
write_csv(combined_data_06_13, "combined_vix_futures_06_13.csv")




#### 14-24 ####
#WD ####
setwd("/Users/lucaswelti/Documents/Data BA Thesis /Futures/2014-2024/Monthly ")


#Combine the individual files for the monthly VIX futures ####
# Define a vector with the year suffixes according to the provided mapping
year_suffixes_14_24 <- c("14" = "2014", "5" = "2015", "6" = "2016", "7" = "2017", 
                         "8" = "2018", "9" = "2019", "0" = "2020", "1" = "2021", 
                         "2" = "2022", "3" = "2023", "4" = "2024")

# Generate filenames for all possible combinations
file_patterns_14_24 <- paste0("CFE_VX_[FGHJKMNQUVXZ](", paste(names(year_suffixes_14_24), collapse = "|"), ")(-\\d+)?\\.csv")

# List all CSV files that match the generated patterns
csv_files_14_24 <- list.files(pattern = file_patterns_14_24)
print(csv_files_14_24)

# Read and combine the files into one data frame
all_data_14_24 <- lapply(csv_files_14_24, function(file_name) {
  # Extract the year code from the file name
  year_code <- str_extract(file_name, paste(names(year_suffixes_14_24), collapse = "|"))
  
  # Read the CSV file
  data_14_24 <- read_csv(file_name)
  
  # Add a Year column based on the extracted year code
  data_14_24$Year <- year_suffixes_14_24[year_code]
  
  return(data_14_24)
})

# Combine all
combined_data_14_24 <- bind_rows(all_data_14_24)
combined_data_14_24 <- combined_data_14_24 %>%
  rename(Date = `Trade Date`)  # Rename 'Trade Date' to 'Date'
combined_data_14_24$Date <- as.Date(combined_data_14_24$Date, format = "%Y-%m-%d") 


# Check the structure and summary of the combined data frame
str(combined_data_14_24)
summary(combined_data_14_24)

# Write the combined data frame to a CSV file
write_csv(combined_data_06_13, "combined_vix_futures_14_24.csv")







#### Merge all ####
#Merge all the data for VIX futures from 2006-2024 ####
vix_futures_06_24 <- bind_rows(combined_data_06_13, combined_data_14_24)

vix_futures_06_24 <- vix_futures_06_24 %>%
  select(-ncol(vix_futures_06_24)) #remove the last column 

# arrange by ascending order of date
vix_futures_06_24 <- vix_futures_06_24 %>%
  arrange(Date)

# safe as a CSV file 
write.csv(vix_futures_06_24, "vix futures from 06 until 24", row.names = FALSE)




#Manipulate the Data ####
# new column with futures expiration month
vix_futures_06_24 <- vix_futures_06_24 %>%
  mutate(`Futures expiration month` = str_extract(Futures, "\\(([^)]+)\\)")) # The regular expression "\\(([^)]+)\\)" matches any character between parentheses

# new column with the futures expiration year 
vix_futures_06_24 <- vix_futures_06_24 %>%
  mutate(`Expiration Year` = str_extract(`Futures expiration month`, "\\d{2}(?=\\))"))

# in Futures expiration month only keep the month
vix_futures_06_24 <- vix_futures_06_24 %>%
  mutate(`Futures expiration month` = str_extract(`Futures expiration month`, "[A-Za-z]+"))

# Change the abbreviation of the month name to the full name 
vix_futures_06_24 <- vix_futures_06_24 %>%
  mutate(`Futures expiration month` = case_when(
    `Futures expiration month` == "Jan" ~ "01",
    `Futures expiration month` == "Feb" ~ "02",
    `Futures expiration month` == "Mar" ~ "03",
    `Futures expiration month` == "Apr" ~ "04",
    `Futures expiration month` == "May" ~ "05",
    `Futures expiration month` == "Jun" ~ "06",
    `Futures expiration month` == "Jul" ~ "07",
    `Futures expiration month` == "Aug" ~ "08",
    `Futures expiration month` == "Sep" ~ "09",
    `Futures expiration month` == "Oct" ~ "10",
    `Futures expiration month` == "Nov" ~ "11",
    `Futures expiration month` == "Dec" ~ "12",
    TRUE ~ as.character(`Futures expiration month`)  # Default case to keep the original value
  ))

# Change the abbreviation of the year name to the full name 
vix_futures_06_24 <- vix_futures_06_24 %>%
  mutate(`Expiration Year` = case_when(
    `Expiration Year` == "06" ~ "2006",
    `Expiration Year` == "07" ~ "2007",
    `Expiration Year` == "08" ~ "2008",
    `Expiration Year` == "09" ~ "2009",
    `Expiration Year` == "10" ~ "2010",
    `Expiration Year` == "11" ~ "2011",
    `Expiration Year` == "12" ~ "2012",
    `Expiration Year` == "13" ~ "2013",
    `Expiration Year` == "14" ~ "2014",
    `Expiration Year` == "15" ~ "2015",
    `Expiration Year` == "16" ~ "2016",
    `Expiration Year` == "17" ~ "2017",
    `Expiration Year` == "18" ~ "2018",
    `Expiration Year` == "19" ~ "2019",
    `Expiration Year` == "20" ~ "2020",
    `Expiration Year` == "21" ~ "2021",
    `Expiration Year` == "22" ~ "2022",
    `Expiration Year` == "23" ~ "2023",
    `Expiration Year` == "24" ~ "2024",
    TRUE ~ paste0("20", `Expiration Year`)  # For years beyond the range specified
  ))







vix_futures_06_24 <- vix_futures_06_24 %>%
  mutate(`Expiration month and year` = paste(`Futures expiration month`, `Expiration Year`, sep = "-"))






#Calculate the expiration month ####
calculate_expiration_date <- function(date_string) {
  # Parse the month and year from the date string
  parts <- strsplit(date_string, "-")[[1]]
  month <- as.numeric(parts[1])
  year <- as.numeric(parts[2])
  
  # Move to the first day of the next month
  if(month == 12) {
    month <- 1
    year <- year + 1
  } else {
    month <- month + 1
  }
  
  date <- as.Date(paste(year, month, "01", sep = "-"))
  
  # Calculate the third Friday of the next month
  # Add days until it's the first Friday
  while (format(date, "%A") != "Friday") {
    date <- date + 1
  }
  # Then add 14 days to get to the third Friday
  third_friday <- date + 14
  
  # Subtract 30 days to find the expiration date which should be a Wednesday
  expiration_date <- third_friday - 30
  
  # If it's not a business day (Saturday or Sunday), adjust to the previous business day
  while (format(expiration_date, "%u") %in% c("6", "7")) {
    expiration_date <- expiration_date - 1
  }
  
  return(expiration_date)
}

# Apply the function to each row
vix_futures_06_24$ExpirationDate <- as.Date(sapply(vix_futures_06_24$`Expiration month and year`, calculate_expiration_date), origin = "1970-01-01")


# Account manually for some exeptions because of holidays
vix_futures_06_24 <- vix_futures_06_24 %>%
  mutate(ExpirationDate = case_when(
    ExpirationDate == as.Date('2007-11-21') ~ as.Date('2007-11-20'),
    ExpirationDate == as.Date('2024-06-19') ~ as.Date('2024-06-18'),
    TRUE ~ ExpirationDate
  ))






