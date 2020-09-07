library(googlesheets4)
library(shiny)
library(simputation)
library(tidyverse)

gs4_deauth()

energy_2019 <- "15nKk44UVxxex7OrhdV3bZRTD0NsniclZmfwtqL0ls18" %>%
    range_read("2019")

energy_2020 <- "15nKk44UVxxex7OrhdV3bZRTD0NsniclZmfwtqL0ls18" %>%
    range_read("2020")

energy <- bind_rows(energy_2019, energy_2020) %>%
    filter(!is.na(electricity)) %>%
    filter(!is.na(gas))

energy <- energy %>%
    impute_lm(gas ~ date) %>%
    impute_lm(electricity ~ date)

energy <- energy %>%
    arrange(date) #%>%
    #mutate(
    #  electricity_unit_rate = 15.886, # 2018 - 2019
    #  gas_unit_rate         = 3.869,  # 2018 - 2019
    #  electricity_unit_rate = 15.61,  # 2019 - 2020 19.33 standing charge
    #  gas_unit_rate         = 3.33,   # 2019 - 2020 19.23 standing charge
    #  electricity_unit_rate = 13.548, # 2020 - 2021 18.073
    #  gas_unit_rate         = 2.607   # 2020 - 2021 10.62
    #) %>%
    #mutate(
    #  gas         = gas * gas_unit_rate,
    #  electricity = electricity * electricity_unit_rate
    #)

tidy_energy <- energy %>%
    gather("fuel", "kwH", c("gas", "electricity"))

ui <- fluidPage(

    titlePanel("My Energy Use"),

    plotOutput("distPlot")
)

server <- function(input, output) {

    output$distPlot <- renderPlot({
        ggplot(tidy_energy, aes(x = date, y = kwH)) +
            geom_point() +
            geom_smooth() +
            theme_minimal() +
            theme(legend.position = "none") +
            facet_wrap(~ fuel, ncol = 1, scales = "free") +
            xlab("")
    })
}

shinyApp(ui = ui, server = server)
