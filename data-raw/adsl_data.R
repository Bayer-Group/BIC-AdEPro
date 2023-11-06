## code to prepare `adae` dataset goes here
adsl_data <- haven::read_sas((here::here("data-raw", "adsl.sas7bdat")))
usethis::use_data(adsl_data, overwrite = TRUE)
