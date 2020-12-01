library("kableExtra")

energy_supp_by_src <- read.csv(
  "data/electicity_supply_by_fuel_source_clean.csv",
                               stringsAsFactors = FALSE)
energy_supp_per_state <- read.csv(
  "data/energy_consumption_per_state_2018_clean.csv",
                                  stringsAsFactors = FALSE)
energy_supp_usa <- read.csv(
  "data/primary_energy_consumption_by_energy_source_clean.csv",
                            stringsAsFactors = FALSE)

energy_by_src_wa <- function(dataset) {
  library("dplyr", warn.conflicts = FALSE)
  options(dplyr.summarise.inform = FALSE)
  dataset %>%
    rowwise(Year) %>%
    mutate("Nonrenewable Energy Consumed (MWh)" =
             sum(Coal, Natural_Gas, Nuclear, Petroleum)) %>%
    mutate("Renewable Energy Consumed (MWh)" =
             sum(Hydropower, Biomass, Wind, Other_Renewables, Waste)) %>%
    mutate("Percentage Renewable (%)" =
             round((100 * `Renewable Energy Consumed (MWh)` / Total), 4)) %>%
    mutate("Max Nonrenewable" =
             pmax(Coal, Natural_Gas, Nuclear, Petroleum)) %>%
    mutate("Max Renewable w/ out Hydropower" =
             pmax(Biomass, Wind, Other_Renewables, Waste)) %>%
    mutate("Max Renewable w/ Hydropower" =
             pmax(Hydropower, Biomass, Wind, Other_Renewables, Waste)) %>%
    mutate("Percentage Max Nonrenewable (%)" =
             round((100 * `Max Nonrenewable` / Total), 4)) %>%
    mutate("Percentage Max Renewable (w/ out Hydro.) (%)" =
             round((100 * `Max Renewable w/ out Hydropower` / Total), 4)) %>%
    mutate("Percentage Max Renewable (w/ Hydro.) (%)" =
             round((100 * `Max Renewable w/ Hydropower` / Total), 4)) %>%
    summarise(`Nonrenewable Energy Consumed (MWh)`,
              `Renewable Energy Consumed (MWh)`,
              `Percentage Max Nonrenewable (%)`,
              `Percentage Max Renewable (w/ out Hydro.) (%)`,
              `Percentage Max Renewable (w/ Hydro.) (%)`)
}

# Insert into rmd
# "Table 1: Energy Consumption Percentages in wa 2000 - 2017"
a <- energy_by_src_wa(energy_supp_by_src)

kable(a, digits = 2, format = "html", row.names = FALSE) %>%
  kable_styling(
    bootstrap_options = c("striped"),
    full_width = F,
    font_size = 12,
    position = "left"
  ) %>%
  scroll_box(height = "300px")

# Table 2:
energy_by_src_states <- function(dataset) {
  library("dplyr", warn.conflicts = FALSE)
  options(dplyr.summarise.inform = FALSE)

  dataset %>%
    rowwise(State) %>%
    mutate("Nonrenewable Energy Consumed - Petroleum" =
             Total_Petroleum) %>%
    mutate("Nonrenewable Energy Consumed - Coal + Natural Gas" =
             sum(Coal, tural_Gas, na.rm = TRUE)) %>%
    mutate("Nonrenewable Energy Consumed - Nuclear" =
             Nuclear_Electric_Power) %>%
    mutate("Renewable Energy - Biomass" =
             Total_Biomass) %>%
    mutate("Renewable Energy - Wind, Water, and Sun (WWS)" =
             sum(Hydro_electric_Waste, Geo_thermal, Solar,
                 Wind, na.rm = TRUE)) %>%
    mutate("Percentage Nonrenewable Energy - Petroleum" =
             round((100 * Total_Petroleum / sum(Total_Petroleum,
                                                Coal, Nuclear_Electric_Power,
                    tural_Gas, na.rm = TRUE)), 4)) %>%
    mutate("Percentage Nonrenewable Energy - Coal + Natural Gas" =
             round((100 * `Nonrenewable Energy Consumed - Coal + Natural Gas`
            / sum(Total_Petroleum, Coal,
            Nuclear_Electric_Power, tural_Gas,
                                                na.rm = TRUE)), 4)) %>%
    mutate("Percentage Nonrenewable Energy - Nuclear" =
             round((100 * Nuclear_Electric_Power / sum(Total_Petroleum,
                    Coal, Nuclear_Electric_Power, tural_Gas,
                    na.rm = TRUE)), 4)) %>%
    mutate("Percentage Renewable Energy - Biomass" =
             round((100 * Total_Biomass / Total_Renewable_Energy), 4)) %>%
    mutate("Percentage Renewable Energy - WWS" =
             round((100 *
                    `Renewable Energy - Wind, Water, and Sun (WWS)`
                    / Total_Renewable_Energy), 4)) %>%
    mutate("Percentage Renewable Energy (out of Total Energy Usage) - Biomass" =
             round((100 * Total_Biomass / sum(
             Total_Petroleum, Coal, Nuclear_Electric_Power,
             Total_Renewable_Energy, tural_Gas, na.rm = TRUE)), 4)) %>%
    mutate("Percentage Renewable Energy (out of Total Energy Usage) - WWS" =
             round((100 *
                    `Renewable Energy - Wind, Water, and Sun (WWS)`
                    / sum(Total_Petroleum, Coal,
                             Nuclear_Electric_Power, Total_Renewable_Energy,
                             tural_Gas, na.rm = TRUE)), 4)) %>%
    mutate("Percentage Renewable Energy (out of Total Energy Usage)"
           = sum(
    `Percentage Renewable Energy (out of Total Energy Usage) - WWS`,
    `Percentage Renewable Energy (out of Total Energy Usage) - Biomass`,
                 na.rm = TRUE)) %>%
    summarize(
      `Nonrenewable Energy Consumed - Petroleum`,
      `Nonrenewable Energy Consumed - Coal + Natural Gas`,
      `Nonrenewable Energy Consumed - Nuclear`,
      `Renewable Energy - Biomass`,
      `Renewable Energy - Wind, Water, and Sun (WWS)`,
      `Percentage Renewable Energy (out of Total Energy Usage) - Biomass`,
      `Percentage Renewable Energy (out of Total Energy Usage) - WWS`,
      `Percentage Renewable Energy (out of Total Energy Usage)`,
      `Percentage Nonrenewable Energy - Petroleum`,
      `Percentage Nonrenewable Energy - Coal + Natural Gas`,
      `Percentage Nonrenewable Energy - Nuclear`,
      `Percentage Renewable Energy - Biomass`,
      `Percentage Renewable Energy - WWS`
    )
}

b <- energy_by_src_states(energy_supp_per_state)
kable(b, digits = 2, format = "html", row.names = FALSE) %>%
  kable_styling(
    bootstrap_options = c("striped"),
    full_width = F,
    font_size = 12,
    position = "left"
  ) %>%
  scroll_box(height = "300px")

# Table 3:
energy_by_src_usa <- function(dataset) {
  library("dplyr", warn.conflicts = FALSE)
  options(dplyr.summarise.inform = FALSE)
  dataset %>%
    rowwise(Year) %>%
    mutate("Nonrenewable Energy Consumed - Petroleum" =
             Petroleum) %>%
    mutate("Nonrenewable Energy Consumed - Coal + Natural Gas" =
             sum(Coal, Natural_Gas, na.rm = TRUE)) %>%
    mutate("Nonrenewable Energy Consumed - Nuclear" =
             Nuclear_Electricity) %>%
    mutate("Renewable Energy - Biomass" = Biomass) %>%
    mutate("Renewable Energy - Wind, Water, and Sun (WWS)" =
             sum(Hydro_Electricity, Other_Renewables, na.rm = TRUE)) %>%
    mutate("Percentage Nonrenewable Energy - Petroleum" =
             round((100 * Petroleum / sum(Coal, Nuclear_Electricity,
                  Natural_Gas, Petroleum, na.rm = TRUE)), 4)) %>%
    mutate("Percentage Nonrenewable Energy - Coal + Natural Gas" =
             round((100 * `Nonrenewable Energy Consumed - Coal + Natural Gas`
                    / sum(Coal, Nuclear_Electricity,
                      Natural_Gas, Petroleum, na.rm = TRUE)), 4)) %>%
    mutate("Percentage Nonrenewable Energy - Nuclear" =
             round((100 * Nuclear_Electricity / sum(Coal,
                   Nuclear_Electricity, Natural_Gas, Petroleum,
                   na.rm = TRUE)), 4)) %>%
    mutate("Percentage Renewable Energy - Biomass" = round((100 *
           Biomass / sum(Biomass, Hydro_Electricity, Other_Renewables,
           na.rm = TRUE)), 4)) %>%
    mutate("Percentage Renewable Energy - WWS" = round((100
            * `Renewable Energy - Wind, Water, and Sun (WWS)`
            / sum(Biomass,
                  Hydro_Electricity, Other_Renewables, na.rm = TRUE)), 4)) %>%
    mutate("Percentage Renewable Energy (out of Total Energy Usage) - Biomass"
           = round((100 * Biomass / sum(Coal,
                                                   Nuclear_Electricity,
                               Natural_Gas, Petroleum, Biomass,
                               Hydro_Electricity,
                               Other_Renewables, na.rm = TRUE)), 4)) %>%
    mutate(
    "Percentage Renewable Energy (out of Total Energy Usage) - WWS"
           = round((100 *
                    `Renewable Energy - Wind, Water, and Sun (WWS)`
                           / sum(Coal,
                           Nuclear_Electricity,
                          Natural_Gas, Petroleum, Biomass,
                          Hydro_Electricity, Other_Renewables,
                          na.rm = TRUE)), 4)) %>%
    mutate("Percentage Renewable Energy (out of Total Energy Usage)"
           = sum(
            `Percentage Renewable Energy (out of Total Energy Usage) - WWS`,
            `Percentage Renewable Energy (out of Total Energy Usage) - Biomass`,
                                  na.rm = TRUE)) %>%
    summarize(
      `Nonrenewable Energy Consumed - Petroleum`,
      `Nonrenewable Energy Consumed - Coal + Natural Gas`,
      `Nonrenewable Energy Consumed - Nuclear`,
      `Renewable Energy - Biomass`,
      `Renewable Energy - Wind, Water, and Sun (WWS)`,
      `Percentage Renewable Energy (out of Total Energy Usage) - Biomass`,
      `Percentage Renewable Energy (out of Total Energy Usage) - WWS`,
      `Percentage Renewable Energy (out of Total Energy Usage)`,
      `Percentage Nonrenewable Energy - Petroleum`,
      `Percentage Nonrenewable Energy - Coal + Natural Gas`,
      `Percentage Nonrenewable Energy - Nuclear`,
      `Percentage Renewable Energy - Biomass`,
      `Percentage Renewable Energy - WWS`
    )
}

# Insert into rmd
# Table 3: Energy Consumption Percentages in the usa 2018
c <- energy_by_src_usa(energy_supp_usa)
kable(c, digits = 2, format = "html", row.names = FALSE) %>%
  kable_styling(
    bootstrap_options = c("striped"),
    full_width = F,
    font_size = 12,
    position = "left"
  ) %>%
  scroll_box(height = "300px")
