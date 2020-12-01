library(dplyr)

# ------------------------------------------------------------------------------

# loaded data which has information regarding the electricity supply by fuel
# source in WA. This is in raw data -> energy -> YT08; units are megawatt-hour

elec_supply_wa <- read.csv("data/electicity_supply_by_fuel_source_clean.csv",
  stringsAsFactors = FALSE
)

# loaded data which has information regarding energy consumption per each state
# in 2018. This is in raw data -> Primary Energy Consumption Estimates, 2018.
# converted original s to na's and removed certain columns which I felt weren't
# needed, so please refer to the raw data file if needed.

energy_consum_state <- read.csv(
  "data/energy_consumption_per_state_2018_clean.csv",
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------

get_summary_info <- function(elec_supply_wa, energy_consum_state) {
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
    summarise(
      max_share_year = Year,
      Share = renewable_share
    )

  # Year and share of the lowest renewable electricity supply out of total
  # electricity supply (Washington State)

  ret$ren_elec_supply_wa_min <- renewable_share_col_wa %>%
    filter(renewable_share == min(renewable_share, na.rm = FALSE)) %>%
    summarise(
      min_share_year = Year,
      Share = renewable_share
    )

  # Average renewable electricity supply share of total electricity supply
  # in Washington State

  ret$avg_elec_ren_supply_share_wa <- renewable_share_col_wa %>%
    summarise(avg_share = mean(renewable_share))

  # Adding two columns: one which shows the total energy consumption in US, and
  # the other shows the total renewable energy consumption per total
  # consumption

  ren_consum_per_total <- energy_consum_state %>%
    mutate(total_consumption = Total_fossil_fuels + Total_Renewable_Energy) %>%
    mutate(ren_consum_per_total = (Total_Renewable_Energy / total_consumption)
    * 100)

  # US State with the highest renewable energy consumption (in Btus)

  ret$highest_ren_consum_us <- ren_consum_per_total %>%
    filter(Total_Renewable_Energy == max(Total_Renewable_Energy,
      na.rm = FALSE
    )) %>%
    summarize(
      highest_ren_state = State,
      btus = Total_Renewable_Energy
    )

  # US State with the lowest renewable energy consumption (in Btus)

  ret$lowest_ren_consum_us <- ren_consum_per_total %>%
    filter(Total_Renewable_Energy == min(Total_Renewable_Energy,
      na.rm = FALSE
    )) %>%
    summarise(
      lowest_ren_state = State,
      btus = Total_Renewable_Energy
    )

  # US State with highest renewable energy consumption per total energy
  # consumption

  ret$high_ren_cons_per_total <- ren_consum_per_total %>%
    filter(ren_consum_per_total == max(ren_consum_per_total,
      na.rm = FALSE
    )) %>%
    summarize(
      highest_state = State,
      percent = ren_consum_per_total
    )

  # US State with lowest renewable energy consumption per total energy
  # consumption

  ret$low_ren_cons_per_total <- ren_consum_per_total %>%
    filter(ren_consum_per_total == min(ren_consum_per_total,
      na.rm = FALSE
    )) %>%
    summarize(
      lowest_state = State,
      percent = ren_consum_per_total
    )

  # Average US Renewable and Fossil Fuel Energy Consumption (in BTUs/state)

  ret$consum_us <- ren_consum_per_total %>%
    summarise(
      avgbtu_renew_consum = mean(Total_Renewable_Energy,
        na.rm = FALSE
      ),
      avgbtu_fossil_consum = mean(Total_fossil_fuels,
        na.rm = FALSE
      )
    )
  
  # Washington Energy Consumption Statistics
  
  ret$wa_consum_stats <- ren_consum_per_total %>%
    filter(State == "Washington") %>%
    summarise(
      wa_total_consumption = total_consumption,
      wa_ren_consum_per_total = ren_consum_per_total,
      wa_total_ren_consum = Total_Renewable_Energy
      )
  
  ret
}
