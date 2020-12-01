library("ggplot2")
# Reference plot_3
# 
# The scatter plot precisely tracks the trend of both nonrenewable and renewable energy consumption in WA to see whether sufficients improvements have been made.
# We are doing this to verify whether Washington is on track to achieve 100% carbon-neutral power by 2030.
# The plot depicts an increase in renewable energy sources, however, 
# it flatlines from 2013-2017. Renewable energy points are seen to have a steady increase in 2015-2017. The trend line hints that renewable energy production is increasing, but steadily.

rm(list = ls())

energy_per_src_WA <- read.csv("../data/electicity_supply_by_fuel_source_clean.csv",
                     stringsAsFactors = FALSE)

summary(energy_per_src_WA)

# table 3. Drawing trendline for R purposes 
energy_per_src_WA <- energy_per_src_WA %>%
  mutate(target_2030 = 1) %>%
  rowwise(Year) %>%
  mutate("NE_Consumed" = sum(Coal, Natural_Gas, Nuclear, Petroleum)) %>%
  mutate("RE_Consumed_w_Hydro" = sum(Hydropower, Biomass, Wind, Other_Renewables, Waste)) %>%
  mutate("RE_Consumed_w_o_Hydro" = sum(Biomass, Wind, Other_Renewables, Waste)) %>%
  mutate("NE_Percent" = ((NE_Consumed / Total))) %>%
  mutate("RE_Percent_w_o_Hydro" = ((sum(Biomass, Wind, Other_Renewables, Waste) / Total))) %>%
  mutate("RE_Percent_w_Hydro" = ((RE_Consumed / Total))) %>%
  mutate("Trendline" = predict(lm(RE_Consumed_w_Hydro~Year)))
  

colors <- c("Renewable Energy w/ Hydro" = "darkgreen", "Nonrenewable Energy" = "red")
plot_3 <- ggplot(energy_per_src_WA, aes(x = Year)) + 
  geom_point(aes(y = RE_Percent_w_Hydro, color = "Renewable Energy w/ Hydro")) + 
  geom_smooth(aes(y = RE_Percent_w_Hydro)) + 
  geom_point(aes(y = NE_Percent, color= "Nonrenewable Energy")) +
  geom_smooth(aes(y = NE_Percent)) +
  labs(x = "Year",
     y = "(%)",
     color = "Energy Types") +
  scale_color_manual(values = colors) +
  ggtitle("Annual Energy Consumed in WA per Type (%)") +
  scale_x_continuous(limits=c(2000, 2017)) +
  scale_y_continuous(limits=c(0, 1))

plot(plot_3)