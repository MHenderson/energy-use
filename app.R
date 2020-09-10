#library(googlesheets4)
library(shiny)
library(shinyMobile)
library(simputation)
library(tidyverse)

library(readxl)

gas_rate <- .02607
gas_standing <- .1062
electricity_rate <- .13548
electricity_standing <- .18073

#gs4_deauth()

#energy_2019 <- "15nKk44UVxxex7OrhdV3bZRTD0NsniclZmfwtqL0ls18" %>%
#  range_read("2019")

#energy_2020 <- "15nKk44UVxxex7OrhdV3bZRTD0NsniclZmfwtqL0ls18" %>%
#  range_read("2020")

energy_2019 <- read_xlsx("energy.xlsx", sheet = "2019")
energy_2020 <- read_xlsx("energy.xlsx", sheet = "2020")

energy <- bind_rows(energy_2019, energy_2020) %>%
  filter(!is.na(electricity)) %>%
  filter(!is.na(gas))

energy <- energy %>%
  impute_lm(gas ~ date) %>%
  impute_lm(electricity ~ date)

energy <- energy %>%
  arrange(date) %>%
  select(date, gas, electricity)

tidy_energy <- energy %>%
  gather("fuel", "kwH", c("gas", "electricity"))

tidy_energy <- tidy_energy %>%
  mutate(
    cost = case_when(
      fuel == "gas" ~ round(kwH * gas_rate + gas_standing, 2),
      fuel == "electricity" ~ round(kwH * electricity_rate + electricity_standing, 2)
    )
  ) %>%
  group_by(fuel) %>%
  mutate(
    GBP = cumsum(cost)
  )

ui = f7Page(
  title = "My Energy Use",
  dark_mode = FALSE,
  init = f7Init(skin = "ios", theme = "light"),
  f7SingleLayout(
    navbar = f7Navbar(
      title = "My Energy Use",
      hairline = TRUE,
      shadow = TRUE
    ),
    toolbar = f7Toolbar(
      position = "bottom",
      f7Link(label = "Link 1", src = "https://www.google.com"),
      f7Link(label = "Link 2", src = "https://www.google.com", external = TRUE)
    ),
    f7Shadow(
      intensity = 16,
      hover = TRUE,
      f7Card(
        title = "Electricity usage yesterday",
        textOutput("electricity_yesterday")
      )
    ),
    f7Shadow(
      intensity = 16,
      hover = TRUE,
      f7Card(
        title = "Electricity cost yesterday",
        textOutput("electricity_cost_yesterday")
      )
    ),
    f7Shadow(
      intensity = 16,
      hover = TRUE,
      f7Card(
        title = "Gas usage yesterday",
        textOutput("gas_yesterday")
      )
    ),
    f7Shadow(
      intensity = 16,
      hover = TRUE,
      f7Card(
        title = "Gas cost yesterday",
        textOutput("gas_cost_yesterday")
      )
    ),
    f7Shadow(
      intensity = 16,
      hover = TRUE,
      f7Card(
        title = "Daily use",
        plotOutput("kwHPlot"),
      )
    ),
    f7Shadow(
      intensity = 16,
      hover = TRUE,
      f7Card(
        title = "Total cost",
        plotOutput("costPlot"),
      )
    )
  )
)

server <- function(input, output) {

  output$electricity_yesterday <- renderText({
    tidy_energy %>%
      filter(fuel == "electricity") %>%
      tail(1) %>%
      pull(kwH) %>%
      paste("kwH")
  })

  output$electricity_cost_yesterday <- renderText({
    tidy_energy %>%
      filter(fuel == "electricity") %>%
      tail(1) %>%
      pull(cost) %>%
      paste("GBP")
  })

  output$gas_yesterday <- renderText({
    tidy_energy %>%
      filter(fuel == "gas") %>%
      tail(1) %>%
      pull(kwH) %>%
      paste("kwH")
  })

  output$gas_cost_yesterday <- renderText({
    tidy_energy %>%
      filter(fuel == "gas") %>%
      tail(1) %>%
      pull(cost) %>%
      paste("GBP")
  })

  output$kwHPlot <- renderPlot({
    ggplot(tidy_energy, aes(x = date, y = kwH)) +
      geom_point(alpha = .5) +
      geom_smooth() +
      theme_minimal() +
      theme(legend.position = "none") +
      facet_wrap(~ fuel, ncol = 1, scales = "free") +
      xlab("")
  })

  output$costPlot <- renderPlot({
    ggplot(tidy_energy, aes(x = date, y = GBP)) +
      geom_line(alpha = .5) +
      theme_minimal() +
      theme(legend.position = "none") +
      facet_wrap(~ fuel, ncol = 1, scales = "free") +
      xlab("")
    })

}

shinyApp(ui = ui, server = server)
