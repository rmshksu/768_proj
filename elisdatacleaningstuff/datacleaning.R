library(tidyverse)
library(lubridate)

# File PATHS
pop.file.path <- "./annual.csv"
overdose.counts.file.path <- "./DOMBS_2020.csv"
fredGDPSTATE.file.path <- "./fredGDPSTATE.R"
fredPCPISTATE.file.path <- "./fredPCPISTATE.R"
fredHOWNSTATE.file.path <- "./fredHOWNSTATE.R"
fredGCT1502STATE.file.path <- "./fredGCT1502STATE.R"
fredTOTLTAXSTATE.file.path <- "./fredTOTLTAXSTATE.R"

# Reading in pop data
pop.data <- read.csv(pop.file.path)

# Reading in overdose data
overdose.counts <- read.csv(overdose.counts.file.path)

# From wide to long format
pop.data = reshape2::melt(
  pop.data, 
  id.vars = c("observation_date")
)


# Getting data in cleaned format
pop.data$STATE = gsub("POP", "", pop.data$variable)
pop.data = pop.data %>% select(-variable)
pop.data$observation_date = lubridate::as_date(pop.data$observation_date)
pop.data$observation_date = lubridate::year(pop.data$observation_date)
pop.data$YEAR <- pop.data$observation_date
pop.data = pop.data %>% select(-observation_date)

# Removing URL variable
overdose.counts = overdose.counts %>% select(-URL)
overdose.counts$YEAR = as.numeric(overdose.counts$YEAR)

overdose.counts = overdose.counts %>% filter(YEAR >= 2014)
pop.data = pop.data %>% filter(
  YEAR >= 2014,
  YEAR < 2024
)

wyoming.pop.data <- data.frame(
  YEAR = seq(2014, 2022), 
  STATE = rep("WY", 9),
  value = c(583.159, 586.389, 585.243, 579.994, 579.054, 580.116, 577.681, 579.636, 581.978)
)

pop.data = bind_rows(pop.data, wyoming.pop.data)

merged_df <- inner_join(
  pop.data, 
  overdose.counts, 
  by = c("YEAR", "STATE"))

merged_df$POP <- merged_df$value
merged_df = merged_df %>% select(-value)


# Now run fredGDPSTATE.R 
source(fredGDPSTATE.file.path)

merged_df = inner_join(
  all_data,
  merged_df,
  by = c("YEAR", "STATE")
)

merged_df = merged_df %>% 
  select(
    STATE,
    YEAR,
    RATE,
    DEATHS,
    POP,
    value
  )
merged_df$GROSSDOMESTICPRODUCT = merged_df$value
merged_df = merged_df %>% select(-value)

# Now run fredPCPISTATE.R
source(fredPCPISTATE.file.path)

merged_df = inner_join(
  all_data,
  merged_df,
  by = c("YEAR", "STATE")
)

merged_df = merged_df %>% 
  select(
    STATE,
    YEAR,
    RATE,
    DEATHS,
    POP,
    GROSSDOMESTICPRODUCT,
    value
  )

merged_df$PERCAPITAPERSONALINCOME = merged_df$value
merged_df = merged_df %>% select(-value)

# Now run fredHOWNSTATE.R
source(fredHOWNSTATE.file.path)

merged_df = inner_join(
  all_data,
  merged_df,
  by = c("YEAR", "STATE")
)

merged_df = merged_df %>% 
  select(
    STATE,
    YEAR,
    RATE,
    DEATHS,
    POP,
    GROSSDOMESTICPRODUCT,
    PERCAPITAPERSONALINCOME,
    value
  )

merged_df$HOMEOWNERSHIPRATE = merged_df$value
merged_df = merged_df %>% select(-value)

# Now run fredGCT1502STATE.R 
source(fredGCT1502STATE.file.path)

merged_df = inner_join(
  all_data,
  merged_df,
  by = c("YEAR", "STATE")
)

merged_df = merged_df %>% 
  select(
    STATE,
    YEAR,
    RATE,
    DEATHS,
    POP,
    GROSSDOMESTICPRODUCT,
    PERCAPITAPERSONALINCOME,
    HOMEOWNERSHIPRATE,
    value
  )

merged_df$PERCENTBACHELORSDEGREEORHIGHER = merged_df$value
merged_df = merged_df %>% select(-value)


source(fredTOTLTAXSTATE.file.path)

merged_df = inner_join(
  all_data,
  merged_df,
  by = c("YEAR", "STATE")
)

merged_df = merged_df %>% 
  select(
    STATE,
    YEAR,
    RATE,
    DEATHS,
    POP,
    GROSSDOMESTICPRODUCT,
    PERCAPITAPERSONALINCOME,
    HOMEOWNERSHIPRATE,
    PERCENTBACHELORSDEGREEORHIGHER,
    value
  )

merged_df$STATEGOVTAXCOLLECTIONSTOTALTAXES = merged_df$value
merged_df = merged_df %>% select(-value)

View(merged_df)


