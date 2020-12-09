# Libraries
library(plotly)
library(shiny)
library(leaflet)

# plot_1 input function

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

# Introduction Code

intro_p <- mainPanel(
  tags$img(src = "Wild_horse_wind_turbines.jpg", height = "80%", width = "80%"),
  br(),
  br(),
  tags$p("\t",
         tags$a(href = "https://mahb.stanford.edu/library-item/fossil-fuels-run/",
                "As the world's non-renewable energy sources are quickly
                depleting,"),
         "tech pioneers and government bodies around the globe are pursuitng
         renewable energy as a means to sustain future energy demands. As global
         warming further presents itself as a universal threat (in addition to
         the finitieness of nonrenewable sources such as petroleum), citizens
         are calling for a quicker transition to a cleaner energy source.
         Moreover, research and think tanks claim that renewable energy not
         only makes people happier, but also richer",
         tags$a(href = "https://www.climatechangenews.com/2016/01/16/renewables-happier-richer-world/",
                "[1]"), "."),
  
  tags$p("This project takes an objective look at Washington State's transition
         to clean energy and verifies if they have been effective in achieving
         its' goal to capture more renewable energy",
         tags$a(href = "https://www.governor.wa.gov/news-media/state-pursues-new-smart-grid-projects-capture-store-more-solar-and-wind-power",
                "[2]"), ". Additionally, this project will inform how much of
                the United States is dependent on nonrenewable resources.
                Hopefully, the inevitable shift towards renewable energy and
                its potential for widespread improvement of welfare will
         be achieved soon.")
)

intro_facts <- sidebarPanel(
  tags$h4("Fun Facts About Climate Change"),
  tags$ul(
    tags$li(helpText("The U.N. warns that humans have 10 years to make
                     a decisive change before climate change is completely ",
           tags$a(href = "https://www.un.org/press/en/2019/ga12131.doc.htm",
                  "irreversible."))),
    tags$li(helpText("In 2018, the
                     USA accounted for 15% of the world's CO2 emissions, ",
           tags$a(
    href = "https://www.ucsusa.org/resources/each-countrys-share-co2-emissions",
    "releasing 5.41 gigatons into the atmosphere."))),
    tags$li(helpText("Experts are more than 95% confident that
                     global warming is the the result of human activity.
                     Particularly, the surge in consumption of energy, goods,
                     and more accelerated warming to an unprecendented
                     rate",
                     tags$a(href = "https://climate.nasa.gov/evidence/","[3]"
                     ), "."))
  )
)

intro <- tabPanel(
  "Introduction",
  sidebarLayout(
    intro_p,
    intro_facts
  )
)

# Chart 1 or Interaction Page 1 Code

chart1_plot <- mainPanel(
  plotlyOutput("chart1"),
  br(),
  br(),
  tags$p("The scatter plot precisely tracks the trend of both nonrenewable and
         renewable energy consumption in WA. It answers the question whether
         sufficient improvements have been made to become independent of
         nonrenewable energy. This chart verifies that Washington is on track
         to achieve 100% carbon-neutral power by 2030.
         It depicts a trending increase in renewable energy sources, however,
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
  br(),
  sidebarLayout(
    chart1_plot,
    chart1_select
  )
)

# chart 2

energy_type <- function(id, initial_value) {
  selectInput(
    inputId = id,
    label = "Select Energy Type",
    choices = list("Coal",
                   "Natural Gas",
                   "Nuclear Electric Power",
                   "Hydro Electric Waste",
                   "Biodiesel",
                   "Solar",
                   "Wind",
                   "Total Petroleum",
                   "Total Fossil Fuel",
                   "Total Biomass",
                   "Total Renewable Energy"),
    selected = initial_value
  )
}

chart2_plot <- mainPanel(
  leafletOutput("chart2"),
  br(),
  br(),
  tags$p("A map representation of the United States' energy consumption shows
         the visual representation of each State's percentage of each type
         of energy consumption compared to other States. It answers the question
         whether Washington has been using more renewable energy compared to
         other states in America.")
)

chart2_select <- sidebarPanel(
  "Energy Consumption in US",
  energy_type("type1", "Coal")
)

chart2_panel <- tabPanel(
  "Types of Consumed Energy in US",
  titlePanel("Percentage of each Type of Energy Consumed by State"),
  br(),
  sidebarLayout(
    chart2_plot,
    chart2_select
  )
)



# chart 3

energy_type2 <- function(id, initial_value) {
  selectInput(
    inputId = id,
    label = "Select Energy Type",
    choices = list("Coal",
                   "Natural Gas",
                   "Nuclear Electric Power",
                   "Hydro Electric Waste",
                   "Biodiesel",
                   "Solar",
                   "Wind",
                   "Total Petroleum",
                   "Total Fossil Fuel",
                   "Total Biomass",
                   "Total Renewable Energy",
                   "Total Energy Consumed"),
    selected = initial_value
  )
}

chart3_plot <- mainPanel(
  h2("Energy Consumption by State"),
  width = 12,
  fluidRow(
    column(width = 12,
           dataTableOutput('chart3')
    )
  ),
  br(),
  br(),
  tags$p("This table allows to filter and arrange depending on the user's
         interest in category. It allows the user to explicitly search and
         find out which state has been using particular type of energy. The
         user could rank the state's consumption on a type of energy to 
         compare how that state is doing in comparison to another state of 
         interest. For example, Hawaii uses 0.2 of Natural Gas compared to 
         Texas' 4564.1")
)

chart3_panel <- tabPanel(
  "Energy Consumption Percentages",
  mainPanel = 
  "energy",
    chart3_plot
)



# Summary Code

summary <- tabPanel(
  "Summary",
  tags$h2("Main Takeaways"),
  br(),
  tags$h3("1. Washington State is highly dependent on
          hydroelectricity"),
  tags$p("In this graph, one can not only compare the US renewable energy
         consumption of Washington State to that of the United States but notice
         how much hydroelectricity energy Washington State consumes when
         compared to other renewable energy types. It is almost 7 times the next
         highest renewable energy type."),
  tags$img(src = "final_project_pic_one.png", height = "50%", width = "50%"),
  tags$p("Although having such a high amount of hydroelectricity promotes
         sustainability in terms of consuming renewable energy, it also shows
         the lack of diverse consumption of other renewable energy types.
         Washington has virtually no solar energy consumption and has a nuclear
         energy consumption that is almost half of that of the US average. For
         Washington state to attain 100% carbon neutrality by 2030, there might
         be needed an increase of focus towards increasing the amount of energy
         consumption of other renewable energy types, mainly solar, geothermal,
         nuclear energy, etc."),
  br(),
  tags$h3("2. Most US States have a smaller renewable energy
          consumption by total energy consumption percent than Washington
          State"),
  tags$p("In this map, the larger the size of the circle, the larger the amount
         of renewable energy consumption by total consumption percent. By
         viewing this, one can infer that Northwest, Northeast and North-Middle
         of the United States have much bigger circles than that of anywhere
         else in the United States. States near the Gulf of Mexico and Alaska
         have considerably less percent of renewable energy consumption by total
         energy consumption."),
  tags$img(src = "final_project_pic_two.png", height = "50%", width = "50%"),
  tags$p("If the United States intends to move more towards becoming a carbon
         neutral country in the next few decades, it would be a tough task for
         states such as the ones near the Gulf of Mexico to transition towards
         consuming renewable energy, since they must increase their renewable
         energy consumption by a considerable amount to catch up with the
         Northeast and Northwest states such as Washington and Oregon. Hence,
         the government might have to incentivize such states to consume
         renewable energy for US to become a carbon neutral country"),
  br(),
  tags$h3("3. Washington State seems to be on course to be carbon
          neutral by 2030"),
  tags$p("Since 2000, the renewable energy consumption in Washington State has
         been steadily increasing, with nonrenewable energy consumption
         gradually decreasing. However, both trendlines seem to have become
         flat since 2015."),
  tags$img(src = "final_project_pic_three.png", height = "50%", width = "50%"),
  tags$p("This chart is essential to check whether Washington state will be
         carbon neutral by 2030. Although the trendline since 2015 is not
         encouraging, the cumulative effort of increasing renewable energy
         consumption over the last twenty years will certainly play a crucial
         role in terms of allowing Washington state to smoothly transition
         towards a renewable energy future in the coming decade. Moreover, the
         decrease of nonrenewable energy consumption since 2000 is also a sign
         of optimism.")
)

ui <-  tagList(
  includeCSS("style.css"), 
  navbarPage(
    "Renewable Energy: WA and Beyond",
    intro,
    chart1_panel,
    chart2_panel,
    chart3_panel,
    summary
  )
)