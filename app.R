#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(googledrive)
library(googlesheets4)
library(here)
library(plotly)
library(readxl)
library(shiny)
library(simputation)
library(tidyverse)

energy_2019 <- drive_get("energy") %>%
    sheets_read("2019")

energy_2020 <- drive_get("energy") %>%
    sheets_read("2020")

energy <- bind_rows(energy_2019, energy_2020) %>%
    filter(!is.na(electricity)) %>%
    filter(!is.na(gas))

energy <- energy %>%
    impute_lm(gas ~ date) %>%
    impute_lm(electricity ~ date)

energy <- energy %>%
    arrange(date) %>%
    mutate(
      electricity_unit_rate = 15.886, # 2018 - 2019
      gas_unit_rate         = 3.869,  # 2018 - 2019
      electricity_unit_rate = 15.61,  # 2019 - 2020 19.33 standing charge
      gas_unit_rate         = 3.33,   # 2019 - 2020 19.23 standing charge
      electricity_unit_rate = 13.548, # 2020 - 2021 18.073
      gas_unit_rate         = 2.607   # 2020 - 2021 10.62
    ) %>%
    mutate(
      gas_cost = gas * gas_unit_rate,
      electricity_cost = electricity * electricity_unit_rate
    ) %>%
    mutate(
      total_gas = cumsum(gas),
      total_electricity = cumsum(electricity),
      total_gas_cost = cumsum(gas_cost),
      total_electricity_cost = cumsum(electricity_cost),
    )

tidy_energy <- energy %>%
    gather("fuel", "cost", c("gas_cost", "electricity_cost"))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("My Energy Use"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlotly({
        ggplot(tidy_energy, aes(x = date, y = cost, colour = fuel)) +
            geom_point() +
            geom_smooth()
    })
}

# Run the application
shinyApp(ui = ui, server = server)
