test_that("preproc_ae works", {
  adae_and_adsl <- join_adae_and_adsl(dat_adae = adae_data, dat_adsl = adsl_data, SUBJIDN = "SUBJIDN")

  filtered <- filter_for_safety_flag(dat = adae_and_adsl, SAFFN = "SAFFN")

  imputed <- calculate_and_impute_required_variables_missing_values(data = filtered, severity_grading_flag = "Severity")

  prepared <- prepare_data_for_adepro(dat = imputed$data)

  Q <- initQ(prepared$ae_data)

  ae_data <- preproc_ae(prepared$ae_data,grading=FALSE)


  testthat::expect_equal(is.data.frame(ae_data),TRUE)
  testthat::expect_equal(colnames(ae_data), c("day_start", "day_end", "patient", "ae","sev","r","d"))

})
