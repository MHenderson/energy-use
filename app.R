library(mobileCharts)
library(shiny)
library(shinyMobile)
library(simputation)
library(tidyverse)

gas_rate <- .02607
gas_standing <- .1062
electricity_rate <- .13548
electricity_standing <- .18073

dev_mode <- FALSE

if(dev_mode) {

  energy_2019 <- readxl::read_xlsx("energy.xlsx", sheet = "2019")
  energy_2020 <- readxl::read_xlsx("energy.xlsx", sheet = "2020")
  energy_2021 <- readxl::read_xlsx("energy.xlsx", sheet = "2021")

} else {

  googlesheets4::gs4_deauth()

  # should be a secret?
  sheet_id <- "15nKk44UVxxex7OrhdV3bZRTD0NsniclZmfwtqL0ls18"

  energy_2019 <- sheet_id %>%
    googlesheets4::range_read("2019")

  energy_2020 <- sheet_id %>%
    googlesheets4::range_read("2020")

  energy_2021 <- sheet_id %>%
    googlesheets4::range_read("2021")

}

prep_tidy_energy <- function(energy) {
  energy %>%
    filter(!is.na(electricity)) %>%
    filter(!is.na(gas)) %>%
    impute_lm(gas ~ date) %>%
    impute_lm(electricity ~ date) %>%
    arrange(date) %>%
    select(date, gas, electricity) %>%
    gather("fuel", "kwH", c("gas", "electricity")) %>%
    mutate(
      cost = case_when(
        fuel == "gas" ~ round(kwH * gas_rate + gas_standing, 2),
        fuel == "electricity" ~ round(kwH * electricity_rate + electricity_standing, 2)
      )
    ) %>%
    group_by(fuel) %>%
    mutate(
      GBP = cumsum(cost)
    ) %>%
    mutate(
      year = lubridate::year(date),
      month = lubridate::month(date)
    )
}

tidy_energy <- bind_rows(energy_2019, energy_2020, energy_2021) %>%
  prep_tidy_energy()

monthly_bills <- tidy_energy %>%
  group_by(fuel, year, month) %>%
  summarise(
    cost = sum(cost)
  ) %>%
  ungroup() %>%
  mutate(ymd = lubridate::ymd(paste(year, month, 1, sep = "-")))

annual_bill <- tidy_energy %>%
  group_by(fuel, year) %>%
  summarise(
    cost = sum(cost)
  ) %>%
  ungroup()

ui = f7Page(
  title = "My Energy Use",
  dark_mode = FALSE,
  init = f7Init(skin = "ios", theme = "light"),
  allowPWA = TRUE,
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
            title = "Monthly bill",
            mobileOutput("gas_monthlyPlot"),
          )
        ),
        f7Shadow(
          intensity = 16,
          hover = TRUE,
          f7Card(
            title = "Annual bill",
            mobileOutput("gas_annualPlot"),
          )
        ),
        f7Shadow(
          intensity = 16,
          hover = TRUE,
          f7Card(
            title = "Total cost",
            mobileOutput("gas_totalPlot"),
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
            title = "Monthly bill",
            mobileOutput("electricity_monthlyPlot"),
          )
        ),
        f7Shadow(
          intensity = 16,
          hover = TRUE,
          f7Card(
            title = "Annual bill",
            mobileOutput("electricity_annualPlot"),
          )
        ),
        f7Shadow(
          intensity = 16,
          hover = TRUE,
          f7Card(
            title = "Total cost",
            mobileOutput("electricity_totalPlot"),
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

  output$gas_totalPlot <- render_mobile({
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

  output$electricity_totalPlot <- render_mobile({
    tidy_energy %>%
      filter(fuel == "electricity") %>%
      mobile(aes(x = date, y = GBP)) %>%
        mobile_line(alpha = .5) %>%
        mobile_scale_x(type = "timeCat", tickCount = 5) %>%
        mobile_scale_y(type = "linear")
  })

  output$electricity_monthlyPlot <- render_mobile({
    monthly_bills %>%
      filter(fuel == "electricity") %>%
      mobile(aes(x = ymd, y = cost)) %>%
        mobile_line(alpha = .5) %>%
        mobile_scale_x(type = "timeCat", tickCount = 5) %>%
        mobile_scale_y(type = "linear")
  })

  output$gas_monthlyPlot <- render_mobile({
    monthly_bills %>%
      filter(fuel == "gas") %>%
      mobile(aes(x = ymd, y = cost)) %>%
      mobile_line(alpha = .5) %>%
      mobile_scale_x(type = "timeCat", tickCount = 5) %>%
      mobile_scale_y(type = "linear")
  })

  output$electricity_annualPlot <- render_mobile({
    annual_bill %>%
      filter(fuel == "electricity") %>%
      mobile(aes(x = year, y = cost)) %>%
        mobile_line(alpha = .5) %>%
        mobile_scale_y(type = "linear")
  })

  output$gas_annualPlot <- render_mobile({
    annual_bill %>%
      filter(fuel == "gas") %>%
      mobile(aes(x = year, y = cost)) %>%
        mobile_line(alpha = .5) %>%
        mobile_scale_y(type = "linear")
  })

}

shinyApp(ui = ui, server = server)
