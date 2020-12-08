# Libraries
library(plotly)
library(shiny)

energy_select <- function(id, initial_val) {
  selectInput(
    inputId = id,
    label = "Select Energy Type",
    choices = list("Nonrenewable" = "NE_Percent",
                   "Renewable with Hydropower" = "RE_Percent_w_Hydro",
                   "Renewable without Hydropower" = "RE_Percent_w_o_Hydro",
                   "Biomass" = "Perc_Biomass",
                   "Coal" = "Perc_Coal",
                   "Hydropower" = "Perc_Hydro",
                   "Natural Gas" = "Perc_NaturalGas",
                   "Nuclear" = "Perc_Nuclear",
                   "Petroleum" = "Perc_Petrol",
                   "Waste" = "Perc_Waste",
                   "Wind" = "Perc_Wind",
                   "Other_Renewables" = "Perc_OtherRenew"),
    selected = initial_val
  )
}

intro_p <- mainPanel(
  tags$img(src="../img/Wild_horse_wind_turbines.jpg"),
  tags$p(tags$a(href="https://mahb.stanford.edu/library-item/fossil-fuels-run/",
                "As the world's non-renewable energy sources are quickly depleting,"),
         "tech pioneers and government bodies around the globe are pursuitng
         renewable energy as a means to sustain future energy demands. As global
         warming further presents itself as a universal threat (in addition to
         the finitieness of nonrenewable sources such as petroleum), citizens
         are calling for a quicker transition to a cleaner energy source.
         Moreover, research and think tanks claim that renewable energy not
         only makes people happier, but also richer",
         tags$a(href ="https://www.climatechangenews.com/2016/01/16/renewables-happier-richer-world/",
                "[1]"), "."), 
    
  tags$p("This project takes an objective look at Washington State's transition
         to clean energy and verifies if they have been effective in achieving
         its' goal to capture more renewable energy",
         tags$a('https://www.governor.wa.gov/news-media/state-pursues-new-smart-grid-projects-capture-store-more-solar-and-wind-power',
                "[2]"), "Additionally, this project will inform how much of the
         United States is dependent on nonrenewable resources. Hopefully, the
         inevitable shift towards renewable energy and its potential for
         widespread improvement of welfare will be achieved soon.")
)

intro_facts <- sidebarPanel(
  "Fun Facts About Climate Change",
  helpText("The U.N. warns that humans have 10 years to make a decisive change
           before climate change is completely ",
           tags$a(href="https://www.un.org/press/en/2019/ga12131.doc.htm",
                  "irreversible.")),
  helpText("In 2018, the USA accounted for 15% of the world's CO2 emissions, ", tags$a(href="https://www.ucsusa.org/resources/each-countrys-share-co2-emissions", "releasing 5.41 gigatons into the atmosphere.")),
  helpText("Fact 3 that validates a conspiracy that the moon contains all of the
           resources humans ever need to produce cheese for the rest of time")
)

intro <- tabPanel(
  "Introduction",
  sidebarLayout(
    intro_p,
    intro_facts
  )
)

chart1_plot <- mainPanel(
  plotlyOutput("chart1"),
  tags$p("The scatter plot precisely tracks the trend of both nonrenewable and
         renewable energy consumption in WA to see whether sufficient
         improvements have been made. We are making this chart to verify whether
         Washington is on track to achieve 100% carbon-neutral power by 2030.
         The plot depicts an increase in renewable energy sources, however,
         it flatlines from 2013-2017. Renewable energy points are seen to
         have a steady increase in 2015-2017. The trend line hints that
         renewable energy production is increasing, but only steadily.")
)


chart1_select <- sidebarPanel(
  # select energy type
  "Energy Consumption in WA",
  energy_select("sel_eng1", "Nonrenewable"),
  
  energy_select("sel_eng2", "RE_Percent_w_Hydro"),
  
  energy_select("sel_eng3", "RE_Percent_w_o_Hydro")
)



chart1_panel <- tabPanel(
  "Energy Consumption in WA (2001 - 2017)",
  titlePanel(title = "Percent of Total Energy Consumption in WA by Type
             (2001 - 2017)"),
  sidebarLayout(
    chart1_plot,
    chart1_select
  )
)

ui <- navbarPage(
  "Renewable Energy: WA and Beyond",
  intro,
  chart1_panel
)