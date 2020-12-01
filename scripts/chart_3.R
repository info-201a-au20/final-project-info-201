install.packages("dplyr")
install.packages("ggplot2")
install.packages("hexbin")
install.packages("scales")
install.packages("reshape")
library("dplyr")
library("leaflet")
library("ggplot2")
library("scales")

energy_per_state <- read.csv("data/energy_consumption_per_state_2018_lat_long.csv",
                             stringsAsFactors = FALSE)
drawMap <- function(energy_per_state){
  colnames(energy_per_state)[1] <- "State"
  # Chart 3. Map of renewable energy consumption percentage
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
  return(map_recp)
}
