## code to prepare `energy` dataset goes here

googlesheets4::gs4_deauth()

# should be a secret?
sheet_id <- "15nKk44UVxxex7OrhdV3bZRTD0NsniclZmfwtqL0ls18"

energy_2019 <- sheet_id %>%
  googlesheets4::range_read("2019")

energy_2020 <- sheet_id %>%
  googlesheets4::range_read("2020")

energy_2021 <- sheet_id %>%
  googlesheets4::range_read("2021")

energy <- dplyr::bind_rows(energy_2019, energy_2020, energy_2021)

tidy_energy <- prep_tidy_energy(energy)

bills <- sheet_id %>%
  googlesheets4::range_read("bills")

readings <- sheet_id %>%
  googlesheets4::range_read("readings")

usethis::use_data(tidy_energy, bills, readings, overwrite = TRUE)

