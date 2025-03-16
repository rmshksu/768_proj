

# This file is just to get the freddata for 
library(fredr)

fredr::fredr_set_key("327bfc1100fbd9841c1887c4a615e480")

state_abbreviations <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", 
                         "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", 
                         "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", 
                         "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", 
                         "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

fred_search <- paste0(state_abbreviations, "NGSP")

all_data <- data.frame() # Initialize an empty data frame

for (series_id in fred_search) {
  tryCatch({
    data <- fredr(series_id)
    if (!is.null(data)) { # Check if data was successfully retrieved
      data$STATE <- substr(series_id, 1, 2) # Add a state column
      all_data <- bind_rows(all_data, data) # Append to the combined data frame
    } else {
      cat(paste("No data found for", series_id, "\n"))
    }
  }, error = function(e) {
    cat(paste("Error retrieving data for", series_id, ":", e$message, "\n"))
  })
}

all_data$date = lubridate::as_date(all_data$date)
all_data$date = lubridate::year(all_data$date)
all_data$YEAR <- all_data$date
all_data = all_data %>% select(-date)


all_data = all_data %>% filter(
  YEAR >= 2014 & YEAR < 2023
)

























