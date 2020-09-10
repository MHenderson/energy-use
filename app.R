library(googlesheets4)
library(mobileCharts)
library(shiny)
library(shinyMobile)
library(simputation)
library(tidyverse)

#library(readxl)

gas_rate <- .02607
gas_standing <- .1062
electricity_rate <- .13548
electricity_standing <- .18073

gs4_deauth()

energy_2019 <- "15nKk44UVxxex7OrhdV3bZRTD0NsniclZmfwtqL0ls18" %>%
  range_read("2019")

energy_2020 <- "15nKk44UVxxex7OrhdV3bZRTD0NsniclZmfwtqL0ls18" %>%
  range_read("2020")

#energy_2019 <- read_xlsx("energy.xlsx", sheet = "2019")
#energy_2020 <- read_xlsx("energy.xlsx", sheet = "2020")

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
  f7TabLayout(
    navbar = f7Navbar(
      title = "My Energy Use",
      hairline = TRUE,
      shadow = TRUE
    ),
    f7Tabs(
      animated = TRUE,
      #swipeable = TRUE,
      f7Tab(
        tabName = "Gas",
        icon = f7Icon("flame"),
        active = TRUE,
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
           title = "Gas usage yesterday",
           textOutput("gas_yesterday")
          )
        ),
        f7Shadow(
          intensity = 16,
          hover = TRUE,
          f7Card(
            title = "Daily use",
            mobileOutput("gas_kwHPlot"),
          )
        ),
        f7Shadow(
          intensity = 16,
          hover = TRUE,
          f7Card(
            title = "Total cost",
            mobileOutput("gas_costPlot"),
          )
        )
      ),
      f7Tab(
        tabName = "Electricity",
        icon = f7Icon("lightbulb"),
        active = FALSE,
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
            title = "Electricity usage yesterday",
            textOutput("electricity_yesterday")
          )
        ),
        f7Shadow(
          intensity = 16,
          hover = TRUE,
          f7Card(
            title = "Daily use",
            mobileOutput("electricity_kwHPlot"),
          )
        ),
        f7Shadow(
          intensity = 16,
          hover = TRUE,
          f7Card(
            title = "Total cost",
            mobileOutput("electricity_costPlot"),
          )
        )
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

  output$gas_kwHPlot <- render_mobile({
    tidy_energy %>%
      filter(fuel == "gas") %>%
      mobile(aes(x = date, y = kwH)) %>%
        mobile_point() %>%
        mobile_scale_x(type = "timeCat", tickCount = 5) %>%
        mobile_scale_y(type = "linear")
  })

  output$gas_costPlot <- render_mobile({
    tidy_energy %>%
      filter(fuel == "gas") %>%
        mobile(aes(x = date, y = GBP)) %>%
          mobile_line(alpha = .5) %>%
          mobile_scale_x(type = "timeCat", tickCount = 5) %>%
          mobile_scale_y(type = "linear")
    })

  output$electricity_kwHPlot <- render_mobile({
    tidy_energy %>%
      filter(fuel == "electricity") %>%
      mobile(aes(x = date, y = kwH)) %>%
        mobile_point() %>%
        mobile_scale_x(type = "timeCat", tickCount = 5) %>%
        mobile_scale_y(type = "linear")
  })

  output$electricity_costPlot <- render_mobile({
    tidy_energy %>%
      filter(fuel == "electricity") %>%
      mobile(aes(x = date, y = GBP)) %>%
        mobile_line(alpha = .5) %>%
        mobile_scale_x(type = "timeCat", tickCount = 5) %>%
        mobile_scale_y(type = "linear")
  })

}

shinyApp(ui = ui, server = server)
