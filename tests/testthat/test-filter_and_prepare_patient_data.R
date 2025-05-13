test_that("multiplication works", {

    adae_data <- tibble(
    SUBJIDN = c(123450, 123450,123451),
    AEDECOD = "Pain",
    AESTDY = c(1,10,5),
    AEENDY = c(5,15,10),
    AESEVN = 1,
    AETRTEMN = 1,
    AESERN = 0,
    AERELN = 1,
    TRT01A = "Treatment",
    TRTSDT = "2010-01-01",
    LVDT =  c("2010-12-31","2010-12-31","2010-06-30"),
    DTHDT = NA,
    SAFFN = c(0,0,1)
  )

  results <- filter_and_prepare_patient_data(
    data = adae_data,
    SAFFN = "SAFFN",
    LVDT = "LVDT",
    TRTSDT = "TRTSDT",
    TRT01A = "TRT01A",
    SUBJIDN = "SUBJIDN",
    DTHDT = "DTHDT"
  )

  testthat::expect_equal(nrow(results),1)

})
