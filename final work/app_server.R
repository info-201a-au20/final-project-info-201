# server.R
library(shiny)
library(plotly)
library(dplyr)
library(ggplot2)

energy_supp_by_src <- read.csv(
  "data/electicity_supply_by_fuel_source_clean.csv",
  stringsAsFactors = FALSE)
#energy_supp_per_state <- read.csv(
#  "data/energy_consumption_per_state_2018_clean.csv",
#  stringsAsFactors = FALSE)
#energy_supp_usa <- read.csv(
#  "data/primary_energy_consumption_by_energy_source_clean.csv",
#  stringsAsFactors = FALSE)

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

server <- function(input, output) {
  
  output$chart1 <- renderPlotly({
    p <- plot_ly(energy_plot, x = ~ Year, y = ~ get(input$sel_eng1),
            type = "scatter", mode = "lines+markers",
            name = gsub("Perc_", "", input$sel_eng1),
            hovertemplate = paste('<i>Percent</i>: %{y:.2f}',
                                  '<br><b>Year</b>: %{x}<br>'),
            connectgaps = TRUE) %>%
      add_trace(y = ~ get(input$sel_eng2), type = "scatter", mode = "lines+markers",
                name = gsub("Perc_", "", input$sel_eng2), connectgaps = TRUE) %>%
      add_trace(y = ~ get(input$sel_eng3), type = "scatter", mode = "lines+markers",
                name = gsub("Perc_", "", input$sel_eng3), connectgaps = TRUE) %>%
      layout(xaxis = list(range = c(2000, 2018), title = "Years"),
             yaxis = list(range = c(0, 1),
                          title = "Amount of Total Energy Consumption <br> Consumed By Type"),
             title = "Annual Energy Consumption Rates <br> By Source in WA",
             legend = list(title = list(text = "<b>Energy Types</b>"))
             )
    return(p)
  })
}