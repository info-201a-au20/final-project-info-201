library("ggplot2")
rm(list = ls())

energy_per_src_wa <- read.csv("data/electicity_supply_by_fuel_source_clean.csv",
  stringsAsFactors = FALSE)


# table 3. Drawing trendline for R purposes
energy_per_src_wa <- energy_per_src_wa %>%
  mutate(target_2030 = 1) %>%
  rowwise(Year) %>%
  mutate("NE_Consumed" = sum(Coal, Natural_Gas,
                             Nuclear, Petroleum)) %>%
  mutate("RE_Consumed_w_Hydro" = sum(Hydropower, Biomass,
                            Wind, Other_Renewables, Waste)) %>%
  mutate("RE_Consumed_w_o_Hydro" = sum(Biomass, Wind,
                            Other_Renewables, Waste)) %>%
  mutate("NE_Percent" = ((NE_Consumed / Total))) %>%
  mutate("RE_Percent_w_o_Hydro" = (sum(Biomass, Wind,
                                       Other_Renewables, Waste) / Total)) %>%
  mutate("RE_Percent_w_Hydro" = (RE_Consumed_w_Hydro / Total))


  
  colors <- c("Renewable Energy w/ Hydro" = "darkgreen",
           "Nonrenewable Energy" = "red")
  plot_3 <- ggplot(energy_per_src_wa, aes(x = Year)) +
  geom_point(aes(y = RE_Percent_w_Hydro, color =
                   "Renewable Energy w/ Hydro")) +
  geom_smooth(aes(y = RE_Percent_w_Hydro)) +
  geom_point(aes(y = NE_Percent, color =
                   "Nonrenewable Energy")) +
  geom_smooth(aes(y = NE_Percent)) +
  labs(
    x = "Year",
    y = "(%)",
    color = "Energy Types"
  ) +
  scale_color_manual(values = colors) +
  ggtitle("Annual Energy Consumed in WA per Type (%)") +
  scale_x_continuous(limits = c(2001, 2017)) +
  scale_y_continuous(limits = c(0, 1))
