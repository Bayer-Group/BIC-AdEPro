test_that("init_q works", {
  adae_and_adsl <- join_adae_and_adsl(dat_adae = adae_data, dat_adsl = adsl_data, SUBJIDN = "SUBJIDN")

  filtered <- filter_for_safety_flag(dat = adae_and_adsl, SAFFN = "SAFFN")

  imputed <- calculate_and_impute_required_variables_missing_values(data = filtered, severity_grading_flag = "Severity")

  prepared <- prepare_data_for_adepro(dat = imputed$data)

  q <- init_q(prepared$ae_data)

  testthat::expect_equal(colnames(q), c("trtem", "ser", "nonser", "studrel","studrelser","replace_ae_start","replace_ae_end"))
  testthat::expect_equal(all(is.logical(q)),TRUE)
  testthat::expect_equal(nrow(prepared$ae_data),nrow(q))
})
