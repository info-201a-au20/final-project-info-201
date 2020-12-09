# server.R
library(shiny)
library(plotly)
library(dplyr)
library(ggplot2)
library(leaflet)
library(kableExtra)
# Loading data

energy_per_src_wa <- read.csv("data/electicity_supply_by_fuel_source_clean.csv",
                              stringsAsFactors = FALSE)
energy_supp_by_src <- read.csv(
  "data/electicity_supply_by_fuel_source_clean.csv",
  stringsAsFactors = FALSE)
energy_per_state <-
  read.csv("data/energy_consumption_per_state_2018_lat_long.csv",
           stringsAsFactors = FALSE)
colnames(energy_per_state)[1] <- "State"

energy_by_year <-
  read.csv("data/primary_energy_consumption_by_energy_source_clean.csv",
           stringsAsFactors = FALSE)
#energy_supp_per_state <- read.csv(
#  "data/energy_consumption_per_state_2018_clean.csv",
#  stringsAsFactors = FALSE)
#energy_supp_usa <- read.csv(
#  "data/primary_energy_consumption_by_energy_source_clean.csv",
#  stringsAsFactors = FALSE)

# plot_1 coding

energy_plot <-
  energy_supp_by_src %>%
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
  mutate("RE_Percent_w_Hydro" = (RE_Consumed_w_Hydro / Total)) %>%
  mutate("Perc_Hydro" = Hydropower / Total) %>%
  mutate("Perc_Coal" = Coal / Total) %>%
  mutate("Perc_NaturalGas" = Natural_Gas / Total) %>%
  mutate("Perc_Nuclear" = Nuclear / Total) %>%
  mutate("Perc_Biomass" = Biomass / Total) %>%
  mutate("Perc_Petrol" = Petroleum / Total) %>%
  mutate("Perc_Waste" = Waste / Total) %>%
  mutate("Perc_Wind" = Wind / Total) %>%
  mutate("Perc_OtherRenew" = Other_Renewables / Total) %>%
  ungroup()

energy_chart2 <-
  energy_per_state %>%
  mutate("Coal" = (Coal / Total) * 50) %>%
  mutate("Natural Gas" = (Natural_Gas / Total) * 50) %>%
  mutate("Total Petroleum" = (Total_Petroleum / Total) * 50) %>%
  mutate("Total Fossil Fuel" = (Total_fossil_fuel / Total)  * 20) %>%
  mutate("Nuclear Electric Power" = (Nuclear_Electric_Power / Total) * 50) %>%
  mutate("Hydro Electric Waste" = (Hydro_electric_Waste / Total) * 50) %>%
  mutate("Biodiesel" = (Biodiesel / Total) * 50) %>%
  mutate("Total Biomass" = (Total_Biomass / Total) * 50) %>%
  mutate("Solar" = (Solar / Total) * 50) %>%
  mutate("Wind" = (Wind / Total) * 50) %>%
  mutate("Total Renewable Energy" = (Total_Renewable_Energy / Total) * 50) %>%
  mutate("Total Energy Consumed / 500" = Total / 500) %>%
  mutate(long) %>%
  mutate(lat) %>%
  ungroup()

energy_chart3 <-
  energy_per_state %>%
  mutate("Coal" = (Coal)) %>%
  mutate("Natural Gas" = (Natural_Gas)) %>%
  mutate("Total Petroleum" = (Total_Petroleum)) %>%
  mutate("Total Fossil Fuel" = (Total_fossil_fuel)) %>%
  mutate("Nuclear Electric Power" = (Nuclear_Electric_Power)) %>%
  mutate("Hydro Electric Waste" = (Hydro_electric_Waste)) %>%
  mutate("Biodiesel" = (Biodiesel)) %>%
  mutate("Total Biomass" = (Total_Biomass)) %>%
  mutate("Solar" = (Solar)) %>%
  mutate("Wind" = (Wind)) %>%
  mutate("Total Renewable Energy" = (Total_Renewable_Energy)) %>%
  mutate("Total Energy Consumed" = Total) %>%
  ungroup()
# output

server <- function(input, output) {
  
  output$chart1 <- renderPlotly({
    p <- plot_ly(energy_plot, x = ~ Year, y = ~ get(input$sel_eng1),
                 type = "scatter", mode = "lines+markers",
                 name = gsub("Perc_", "", input$sel_eng1),
                 hovertemplate = paste('<i>Percent</i>: %{y:.2f}',
                                       '<br><b>Year</b>: %{x}<br>'),
                 connectgaps = TRUE) %>%
      add_trace(y = ~ get(input$sel_eng2), type = "scatter",
                mode = "lines+markers",
                name = gsub("Perc_", "", input$sel_eng2),
                connectgaps = TRUE) %>%
      add_trace(y = ~ get(input$sel_eng3), type = "scatter",
                mode = "lines+markers",
                name = gsub("Perc_", "", input$sel_eng3),
                connectgaps = TRUE) %>%
      layout(xaxis = list(range = c(2000, 2018), title = "Years"),
             yaxis = list(range = c(0, 1),
                          title = "Amount of Total Energy Consumption
                          <br> Consumed By Type"),
             title = "Annual Energy Consumption Rates <br> By Source in WA",
             legend = list(title = list(text = "<b>Energy Types</b>"))
      )
    return(p)
  })
  
  output$chart2 <- renderLeaflet({
    
    map_us <- leaflet(data = energy_chart2) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(
        radius = ~get(input$type1),
        lat = ~lat,
        lng = ~long,
        stroke = FALSE,
        fillOpacity = 0.6,
        popup = paste0(input$type1,
                       " takes up </br>",
                       ~get(input$type1),
                       "% of the Total </br>",
                       "energy consumption"
        )
      )
    return(map_us)
  })
  
  output$chart3 <- renderDataTable(
    energy_chart3 %>% select(1, 16:23),
    options = list(
      pageLength = 10,
      class = "display nowrap compact",
      filter = "top",
      lengthMenu = list(c(10, 25, -1), c('10', '25', 'ALL')),
      scrollX = TRUE
    )
  )
  
  output$chart4 <- renderDataTable(
    colors <- c("Renewable Energy w/ Hydro" = "darkgreen",
                "Nonrenewable Energy" = "red"),
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
      scale_y_continuous(limits = c(0, 1)),
    return(plot_3)
  )
}

energy_chart4 <-
  energy_per_src_wa %>%
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
  

