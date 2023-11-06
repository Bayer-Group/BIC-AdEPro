## code to prepare `adae` dataset goes here
adae_data <- haven::read_sas((here::here("data-raw", "adae.sas7bdat")))
usethis::use_data(adae_data, overwrite = TRUE)
