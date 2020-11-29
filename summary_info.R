library(dplyr)

# loaded data which has information regarding the electricity supply by fuel
# source in WA. This is in raw data -> energy -> YT08; units are megawatt-hour

elec_supply_wa <- read.csv("data/electicity_supply_by_fuel_source_clean.csv",
  stringsAsFactors = FALSE
)

# loaded data which has information regarding Primary energy consumption by
# energy source in WA. This is in raw data -> energy -> YT02; units are Billion
# BTUs.

energy_consum_source <- read.csv(
  "data/primary_energy_consumption_by_energy_source_clean.csv",
  stringsAsFactors = FALSE
)

# loaded data which has information regarding energy consumption per each state
# in 2018. This is in raw data -> Primary Energy Consumption Estimates, 2018.
# converted original s to na's and removed certain columns which I felt weren't
# need, so please refer to the raw data file if needed.

energy_consum_state <- read.csv(
  "data/energy_consumption_per_state_2018_clean.csv",
  stringsAsFactors = FALSE
)

# loaded data which has information regarding the energy expenditures by sector
# in WA. This is in raw data -> energy -> YT03; units are million $ (adjusted
# for inflation)

energy_exp_sector <- read.csv(
  "data/energy_expenditure_by_sector_clean.csv",
  stringsAsFactors = FALSE
)

# loaded data which has information regarding the energy prices in WA. This is
# in raw data -> energy -> YT04; units are Constant $ per Million BTU (adjusted
# for inflation)

energy_price_per_milbtu <- read.csv(
  "data/energy_prices_per_btus_clean.csv",
  stringsAsFactors = FALSE
)

# loaded data which has info regarding the electricity prices by sector. This is
# in raw data -> energy -> YT06; units are cents per kilowatt hour (adjusted for
# inflation)

elec_price_by_sector <- read.csv("data/elec_prices_by_sector_clean.csv",
  stringsAsFactors = FALSE
)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# For all datasets, columns sometimes don't add up and give the total,
# maybe because it's difficult to collect certain data, hence why some totals
# aren't equal to the sums.

summary_test <- function(elec_supply_wa) {
  
  # create a new column called renewable energy supply share of total supply
  
  total_renewable <- (elec_supply_wa$Hydropower + elec_supply_wa$Nuclear
                      + elec_supply_wa$Biomass + elec_supply_wa$Waste
                      + elec_supply_wa$Wind + elec_supply_wa$Other_Renewables)
  elec_supply_wa$renewable_share_wa <- (total_renewable / elec_supply_wa$Total) *
    100
}

# Variable 1: Average annual rate of change of energy source in WA (in progress)

total_renewable <- (elec_supply_wa$Hydropower + elec_supply_wa$Nuclear
                    + elec_supply_wa$Biomass + elec_supply_wa$Waste
                    + elec_supply_wa$Wind + elec_supply_wa$Other_Renewables)
elec_supply_wa$renewable_share_wa <- (total_renewable / elec_supply_wa$Total) *
  100

# Variable 2: The percentage change of energy consumption of any source, given
# input of 2 years(in progress)

# Variable 3: Highest energy supply source in a given year in Washington


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

get_summary_info <- function(elec_supply_wa) {
  ret <- list()
  
  # Create a new column called renewable_share_col_wa which shows the share of
  # renewable electricity supply out of total electricity supply (Washington
  # State)
  
  renewable_share_col_wa <- elec_supply_wa %>%
    mutate(renewable_share = (((Hydropower + Nuclear + Biomass + Waste + Wind
                                + Other_Renewables) / Total) * 100))
  
  # Year and share of the highest renewable electricity supply out of total 
  # electricity supply (Washington State)
  
  ret$ren_elec_supply_wa_max <- renewable_share_col_wa %>%
    filter(renewable_share == max(renewable_share, na.rm = FALSE)) %>%
    summarise(max_share_year = Year,
              Share = renewable_share)
  
  # Year and share of the lowest renewable electricity supply out of total 
  # electricity supply (Washington State)
  
  ret$ren_elec_supply_wa_min <- renewable_share_col_wa %>%
    filter(renewable_share == min(renewable_share, na.rm = FALSE)) %>%
    summarise(min_share_year = Year,
              Share = renewable_share)
  
  # Average renewable electricity supply share of total electricity supply
  # in Washington State
  
  ret$avg_elec_ren_supply_share_wa <- renewable_share_col_wa %>%
    summarise(avg_share = mean(renewable_share))
  ret
}

trial <- get_summary_info(elec_supply_wa)


#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------

#TESTING AND STUFF (energy_consum_state) !!!


