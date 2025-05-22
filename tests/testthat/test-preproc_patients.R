test_that("multiplication works", {
  adae_and_adsl <- join_adae_and_adsl(dat_adae = adae_data, dat_adsl = adsl_data, SUBJIDN = "SUBJIDN")

  filtered <- filter_for_safety_flag(dat = adae_and_adsl, SAFFN = "SAFFN")

  imputed <- calculate_and_impute_required_variables_missing_values(data = filtered, severity_grading_flag = "Severity")

  prepared <- prepare_data_for_adepro(dat = imputed$data)

  Q <- initQ(prepared$ae_data)

  ae_data <- preproc_ae(prepared$ae_data,grading=FALSE)

  ae_data <- ae_data[which(Q[, 1]), ]

  patients <- preproc_patients(prepared$pat_data, 18)

  testthat::expect_equal(is.data.frame(patients),TRUE)
  testthat::expect_equal(colnames(patients), c("ps", "treat", "end", "death","AGE","SEX","REGION","X","Y"))

})
