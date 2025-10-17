## code to prepare `adae` and `adsl` datasets goes here
adae_data <- haven::read_sas((here::here("data-raw", "adae.sas7bdat")))
adsl_data <- haven::read_sas((here::here("data-raw", "adsl.sas7bdat")))
usethis::use_data(adae_data, adsl_data, overwrite = TRUE, internal = TRUE)
