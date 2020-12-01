library("dplyr")
library("leaflet")
library("ggplot2")
library("scales")

energy_consumption_per_state <-
  read.csv("data/energy_consumption_per_state_2018_clean.csv",
           stringsAsFactors = FALSE)

# Chart 1. Grouped Bar plot of Washington vs
# US Average's renewable energy consumption
chart_2 <- function(energy_consumption_per_state) {
  renewable_only <- energy_consumption_per_state %>% select(1, 12:22)
  washington_energy <-
    renewable_only[energy_consumption_per_state$State == "Washington", ]
  wo_washington_energy <-
    renewable_only[renewable_only$State != "Washington", ]
  wo_washington_energy <- wo_washington_energy %>% select(2:11)
  avg_energy <- summarise_all(wo_washington_energy, mean, na.rm = TRUE)
  xaxis <- rep(c(colnames(avg_energy)))
  washington <- as.numeric(as.vector(washington_energy[1, 2:11]))
  average <- as.numeric(as.vector(avg_energy[1, ]))
  values <- c(washington, average)
  type <- c(rep("Washington", 10), rep("US Average", 10))
  data <- data.frame(xaxis, values, type)
  p <- ggplot(data, aes(values, xaxis))
  p <- p + geom_bar(stat = "identity", aes(fill = type), position = "dodge") +
    xlab("Renewable Energy Types") + ylab("Consumption") +
    ggtitle("Comparison of renewable energy
            consumption of Washington vs US Average") +
    theme_bw()
  return(p)
}
