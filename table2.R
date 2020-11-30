install.packages("dplyr")
install.packages("ggplot2")
install.packages("hexbin")
install.packages("scales")
install.packages("reshape")
library("dplyr")
library("leaflet")
library("ggplot2")
library("scales")

rm(list = ls())

energy_per_state <- read.csv("~/uw2020/info201/final-project-info-201/data/energy_consumption_per_state_2018_lat_long.csv",
                     stringsAsFactors = FALSE)
energy_consumption_per_state_all_2018 <- read.csv("~/uw2020/info201/final-project-info-201/data/energy_consumption_per_state_2018_clean.csv",
                     stringsAsFactors = FALSE)

colnames(energy_per_state)[1] <- "State"
summary(energy_per_state)

# table 1. Grouped Bar plot of Washington vs US Average's renewable energy consumption
renewable_only <- energy_consumption_per_state_all_2018 %>% select(1, 12:22)

washington_energy <-
  renewable_only[energy_consumption_per_state_all_2018$State == 'Washington',]

wo_washington_energy <- 
  renewable_only[renewable_only$State != 'Washington',]

wo_washington_energy <- wo_washington_energy %>% select(2:11)

avg_energy <- summarise_all(wo_washington_energy, mean, na.rm=TRUE)

xaxis <- rep(c(colnames(avg_energy)))
washington <- as.numeric(as.vector(washington_energy[1,2:11]))
average <- as.numeric(as.vector(avg_energy[1,]))
values <- c(washington, average)
type <- c(rep("Washington", 10), rep("US Average", 10))
data <- data.frame(xaxis, values, type)

p <- ggplot(data, aes(values, xaxis))
p + geom_bar(stat = "identity", aes(fill = type), position = "dodge") +
  xlab("Renewable Energy Types") + ylab("Consumption") +
  ggtitle("Comparison of renewable energy consumption of Washington vs US Average") +
  theme_bw()


# table 2. Map of renewable energy consumption percentage
map_recp <- leaflet(data = energy_per_state) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    radius = ~ (Total_Renewable_Energy / (Total_fossil_fuel + Total_Renewable_Energy)) * 100,
    lat = ~lat,
    lng = ~long,
    stroke = FALSE,
    fillOpacity = 0.6,
    popup = paste("net energy consumption = ", energy_per_state$Total_fossil_fuel + energy_per_state$Total_Renewable_Energy,
                  "</br> renewable energy consumption percentage = ",
                  label_percent()(energy_per_state$Total_Renewable_Energy /
                                    (energy_per_state$Total_Renewable_Energy + energy_per_state$Total_fossil_fuel))))
