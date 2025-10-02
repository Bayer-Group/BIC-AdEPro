test_that("multiplication works", {

  #demo data adae only
  joint_data <- adae_data

  joint_filtered_data <- filter_for_safety_flag(joint_data, "SAFFN")

  imputed <- calculate_and_impute_required_variables_missing_values(
    data = joint_filtered_data,
    LVDT = "LVDT",
    DTHDT = "DTHDT" ,
    TRTSDT = "TRTSDT",
    AEDECOD = "AEDECOD",
    AESTDY = "AESTDY",
    AETRTEMN = "AETRTEMN",
    AEENDY = "AEENDY",
    AESEVN = "AESEVN",
    severity_grading_flag = "Severity"
  )

  testthat::expect_equal(is.list(imputed),TRUE)
  testthat::expect_equal(is.data.frame(imputed$data),TRUE)

  testthat::expect_equal("AEDECOD_imputed_flag" %in% colnames(imputed$data), FALSE)
  testthat::expect_equal(imputed$number_unknown_aes, 0)

  testthat::expect_equal("AEEENDY_imputed_flag" %in% colnames(imputed$data), FALSE)
  testthat::expect_equal(imputed$number_ae_end_missing,0)

  testthat::expect_equal("LVDT_imputed_flag" %in% colnames(imputed$data), FALSE)
  testthat::expect_equal(imputed$number_missing_lvdt,0)

  testthat::expect_equal("AESTDY_imputed_flag" %in% colnames(imputed$data), FALSE)
  testthat::expect_equal(imputed$number_ae_start_missing,0)

  testthat::expect_equal("AESEVN_imputed_flag" %in% colnames(imputed$data), FALSE)
  testthat::expect_equal(imputed$number_severe_missing,0)

  testthat::expect_equal(imputed$number_days_removed,0)


  ###demo data adae and adsl
  joint_data <- join_adae_and_adsl (dat_adae = adae_data, dat_adsl = adsl_data, "SUBJIDN")
  joint_filtered_data <- filter_for_safety_flag(joint_data, "SAFFN")

  imputed <- calculate_and_impute_required_variables_missing_values(
    data = joint_filtered_data,
    LVDT = "LVDT",
    DTHDT = "DTHDT" ,
    TRTSDT = "TRTSDT",
    AEDECOD = "AEDECOD",
    AESTDY = "AESTDY",
    AETRTEMN = "AETRTEMN",
    AEENDY = "AEENDY",
    AESEVN = "AESEVN",
    severity_grading_flag = "Severity"
  )

  testthat::expect_equal(is.list(imputed),TRUE)
  testthat::expect_equal(is.data.frame(imputed$data),TRUE)

  testthat::expect_equal("AEDECOD_imputed_flag" %in% colnames(imputed$data), FALSE)
  testthat::expect_equal(imputed$number_unknown_aes, 0)

  testthat::expect_equal("AEEENDY_imputed_flag" %in% colnames(imputed$data), FALSE)
  testthat::expect_equal(imputed$number_ae_end_missing,0)

  testthat::expect_equal("LVDT_imputed_flag" %in% colnames(imputed$data), FALSE)
  testthat::expect_equal(imputed$number_missing_lvdt,0)

  testthat::expect_equal("AESTDY_imputed_flag" %in% colnames(imputed$data), FALSE)
  testthat::expect_equal(imputed$number_ae_start_missing,0)

  testthat::expect_equal("AESEVN_imputed_flag" %in% colnames(imputed$data), FALSE)
  testthat::expect_equal(imputed$number_severe_missing,0)

  testthat::expect_equal(imputed$number_days_removed,0)

})



test_that("check function prepare_data_for_adepro:", {

 #create adae test data for test_that package
 adae_data <- data.frame(
    rbind(
       #subject 1234500 with no expected error/warning and 3 entries
       # "SUBJIDN", "AEDECOD", "AESTDY", "AEENDY", "AESEVN","AETRTEMN", "AESERN", "AERELN",  "TRT01A",        "TRTSDT",     "LVDT",  "DTHDT","SAFFN","AERELPRN","AEACNN"
       t(c(1234500,   "Pain",     2,        4,       1,       1,           0,       1,        "Trtmt",    "2010-01-01", "2010-12-31", NA,     1,      1,      1)),
       t(c(1234500,   "Pain",     6,        7,       1,       1,           0,       1,        "Trtmt",    "2010-01-01", "2010-12-31", NA,     1,      1,      1)),
       t(c(1234500,   "Headache", 10,       130,     1,       1,           0,       1,        "Trtmt",    "2010-01-01", "2010-12-31", NA,     1,      1,      1)),

       #subject 1234501 with no expected error/warning and 2 entries
       t(c(1234501,   "Pain",     2,        4,       1,       1,           0,       1,        "Placebo",  "2010-01-01", "2010-12-31", NA,     1,      0,      0)),
       t(c(1234501,   "Pain",     6,        7,       3,       1,           0,       1,        "Placebo",  "2010-01-01", "2010-12-31", NA,     1,      0,      0)),

       #subject 1234502 with no expected error/warning and 1 entries
       t(c(1234502,   "Headache", 1,        2,       2,       1,           0,       1,        "Placebo",  "2010-01-01", "2010-12-31", NA,     1,      NA,      NA)),

       #subject 1234503 with no expected error/warning and 1 entries
       t(c(1234503,   "Headache", 1,        2,       2,       1,           0,       1,        "Trtmt",    "2010-01-01", "2010-12-31", NA,     1,      NA,      NA)),

       #subject 1234504 with safety flag 0
       t(c(1234504,   "Headache", 1,        2,       2,       1,           0,       1,        "Trtmt",    "2010-01-01", "2010-12-31", NA,     0,      NA,      NA)),

       #subject 1234505 with safety adverse event ended before day 1
       t(c(1234505,   "Headache", -7,        -2,       2,       1,           0,       1,        "Placebo",  "2010-01-01", "2010-12-31", NA,     1,      NA,      NA)),
       t(c(1234505,   "Pain",     1,        2,       2,       1,           0,       1,        "Placebo",  "2010-01-01", "2010-12-31", NA,     1,      NA,      NA)),

       ## warnings/error

       #subject 1234506 with missing start date
       t(c(1234506,   "Headache", 1,        2,       2,       1,           0,       1,        "Placebo",  NA,           "2010-12-31", NA,     1,      NA,      NA)),
       t(c(1234506,   "Pain",     1,        2,       2,       1,           0,       1,        "Placebo",  NA,           "2010-12-31", NA,     1,      NA,      NA)),

       #subject 1234507 with missing start and last visit date
       t(c(1234507,   "Headache", 1,        2,       2,       1,           0,       1,        "Placebo",  NA,           NA,           NA,     1,      NA,      NA)),
       t(c(1234507,   "Pain",     1,        2,       2,       1,           0,       1,        "Placebo",  NA,           NA,           NA,     1,      NA,      NA)),

       #subject 1234508 with missing start date
       t(c(1234508,   "Headache", 1,        2,       2,       1,           0,       1,        "Placebo",  NA,           "2010-12-31", NA,     1,      NA,      NA)),
       t(c(1234508,   "Pain",     1,        2,       2,       1,           0,       1,        "Placebo",  NA,           "2010-12-31", NA,     1,      NA,      NA)),

       #subject 1234509 with missing ae start time
       t(c(1234509,   "Headache",NA,        2,       2,       1,           0,       1,        "Placebo",  "2010-01-01", "2010-12-31", NA,     1,      NA,      NA)),
       t(c(1234509,   "Pain",    NA,        2,       2,       1,           0,       1,        "Placebo",  "2010-01-01", "2010-12-31", NA,     1,      NA,      NA)),

       #subject 1234510 with non-unique row
       t(c(1234510,   "Headache",1,        2,       2,       1,           0,       1,        "Placebo",  "2010-01-01", "2010-12-31", NA,     1,      NA,      NA)),
       t(c(1234510,   "Headache",1,        2,       2,       1,           0,       1,        "Placebo",  "2010-01-01", "2010-12-31", NA,     1,      NA,      NA)),

       #subject 1234511 ae start day after ae end day
       t(c(1234511,   "Headache",3,        2,       2,       1,           0,       1,        "Placebo",  "2010-01-01", "2010-12-31", NA,     1,      NA,      NA)),

       #subject 1234512 with missing ae end day
       t(c(1234512,   "Headache",1,        NA,       2,       1,           0,       1,        "Placebo",  "2010-01-01", "2010-12-31", NA,     1,      NA,      NA)),

       #subject 1234513 with missing subject id
       t(c(NA     ,   "Headache",1,        2,       2,       1,           0,       1,        "Placebo",  "2010-01-01", "2010-12-31", NA,     1,      NA,      NA)),

       #subject 1234514 with wrong ae severity grade
       t(c(1234514,   "Headache",1,        3,       10,       1,           0,       1,        "Placebo",  "2010-01-01", "2010-12-31", NA,     1,      NA,      NA)),

       #subject 1234515 with death date before trt start date
       t(c(1234515,   "Headache",1,        3,       2,       1,           0,       1,        "Placebo",  "2010-01-01", "2010-12-31", "2009-01-01",     1,      NA,      NA)),

       #subject 1234516 with wrong coding for ae related flag
       t(c(1234516,   "Headache",1,        3,       2,       1,           0,       1,        "Placebo",  "2010-01-01", "2010-12-31", "2009-01-01",     1,     3,      NA)),

       #subject 1234517 with ae start day and end day missing
       t(c(1234517,   "Headache",NA,        NA,       2,       1,           0,       1,        "Placebo",  "2010-01-01", "2010-12-31", "2009-01-01",     1,     3,      NA)),

       #subject 1234518 with ae start day and end day missing and ae code missing
       t(c(1234518,   NA ,NA,        NA,       2,       1,           0,       1,        "Trtmt",  "2010-01-01", "2010-12-31", "2009-01-01",     1,     3,      NA)),

       #subject 1234519 with no expected error/warning and 1 entries
       t(c(1234519,   NA, 1,        2,       2,       1,           0,       1,        "Placebo",  "2010-01-01", "2010-12-31", NA,     1,      NA,      NA)),

       #subject 1234502 with no expected error/warning and 1 entries
       t(c(1234520,   "", 1,        2,       2,       1,           0,       1,        "Placebo",  "2010-01-01", "2010-12-31", NA,     1,      NA,      NA)),

       #subject 1234521 with no expected error/warning and 1 entries
       t(c(1234521,   "Headache", 10,        2,       2,       1,           0,       1,        "Placebo",  "2010-01-01", "2010-12-31", NA,     1,      NA,      NA)),

       t(c(1234522,   "Pain",     2,        4,       NA,       1,           0,       1,        "Trtmt",    "2010-01-01", "2010-12-31", NA,     1,      1,      1)),
       t(c(1234522,    NA,     6,        7,       NA,       1,           0,       1,        "Trtmt",    "2010-01-01", "2010-12-31", NA,     1,      1,      1)),
       t(c(1234522,   "Headache", 10,       130,     1,       1,           0,       1,        "Trtmt",    "2010-01-01", "2010-12-31", NA,     1,      1,      1)),

       #subject 1234501 with no expected error/warning and 2 entries
       t(c(1234523,   "",     2,        4,       NA,       1,           0,       1,        "Placebo",  "2010-01-01", "2010-12-31", NA,     1,      0,      0)),
       t(c(1234523,   "Pain",     6,        7,       5,       1,           0,       1,        "Placebo",  "2010-01-01", "2010-12-31", NA,     1,      0,      0))
     )
  )

  colnames(adae_data) <- c("SUBJIDN", "AEDECOD", "AESTDY", "AEENDY", "AESEVN","AETRTEMN", "AESERN", "AERELN", "TRT01A","TRTSDT",     "LVDT",       "DTHDT","SAFFN","AERELPRN","AEACNN")
  adae_data$SUBJIDN <- as.numeric(adae_data$SUBJIDN)
  adae_data$AESTDY <- as.numeric(adae_data$AESTDY)
  adae_data$AEENDY <- as.numeric(adae_data$AEENDY)
  adae_data$AESEVN <- as.numeric(adae_data$AESEVN)
  adae_data$AETRTEMN <- as.numeric(adae_data$AETRTEMN)
  adae_data$AERELN <- as.numeric(adae_data$AERELN)
  adae_data$SAFFN <- as.numeric(adae_data$SAFFN)
  adae_data$AESTDY <- as.numeric(adae_data$AESTDY)
  adae_data$AESERN <- as.numeric(adae_data$AESERN)

  adae_data$TRTSDT <- as.Date(adae_data$TRTSDT)
  adae_data$LVDT <- as.Date(adae_data$LVDT)


  filtered <- filter_for_safety_flag(dat = adae_data, SAFFN = "SAFFN")
  #subject 1234505 should be removed since saffn = 0

  imputed <- calculate_and_impute_required_variables_missing_values(data = filtered, severity_grading_flag = "Severity")

  testthat::expect_equal(is.list(imputed),TRUE)
  testthat::expect_equal(is.data.frame(imputed$data),TRUE)

  #aes for subject 19 and 20
  testthat::expect_equal(imputed$number_unknown_aes, 4)
  testthat::expect_equal(sum(imputed$data$AEDECOD_imputed_flag == 1),4)

  #aes for subject 12 and 17
  testthat::expect_equal(imputed$number_ae_end_missing,2)
  testthat::expect_equal(sum(imputed$data$AEENDY_imputed_flag == 1),2)

  # expect subject 07
  testthat::expect_equal(imputed$number_missing_lvdt,1)
  testthat::expect_equal(sum(imputed$data %>% dplyr::select(SUBJIDN,LVDT_imputed_flag) %>% distinct() %>% dplyr::pull(LVDT_imputed_flag) == 1),1)

  # two for subject 09 and one fo 17
  testthat::expect_equal(imputed$number_ae_start_missing,3)
  testthat::expect_equal(sum(imputed$data$AESTDY_imputed_flag == 1),3)

  # subject 14
  testthat::expect_equal(imputed$number_severe_missing, 5)
  testthat::expect_equal(sum(imputed$data$AESEVN_imputed_flag == 1),5)

  # subject 11 and subject 21
  testthat::expect_equal(imputed$number_days_removed,2)
  testthat::expect_equal(nrow(imputed$data[imputed$data$SUBJIDN == 1234511,]),0)
  testthat::expect_equal(nrow(imputed$data[imputed$data$SUBJIDN == 1234521,]),0)


  #create adae test data for test_that package
 adsl_data <- data.frame(
    rbind(
      #subject 1234500
       # "SUBJIDN",  "TRT01A",        "TRTSDT",     "LVDT",  "DTHDT","SAFFN", "REGION", "SEX", "AGE")
       t(c(1234500,  "Trtmt",    "2010-01-01", "2010-12-31", NA,     1,      "Europe",   "M",   20)),

      #subject 1234501
       t(c(1234501, "Placebo",  "2010-01-01", "2010-12-31", NA,     1,      "Europe",   "M",   20)),

       #subject 1234502 with different TRT01A than in adae
       t(c(1234502,  "Trtmt",  "2010-01-01",  "2010-12-31", NA,     1,      "Europe",   "M",   20)),

       #subject 1234503 different/missing TRTSDT than in adae
       t(c(1234503,  "Trtmt",    NA,          "2010-12-31", NA,     1,      "Europe",   "M",   60)),

       #subject 1234504
       t(c(1234504,  "Trtmt",   "2010-01-01", "2010-12-31", NA,     0,      "Europe",   "M",   20)),

       #subject 1234505
       t(c(1234505, "Placebo",  "2010-01-01", "2010-12-31", NA,     1,       "Europe",   "M",   50)),

       #subject 1234506 set subjid to missing
       t(c(NA,  "Placebo",      NA,           "2010-12-31", NA,     1,       "Europe",   "M",   20)),

       #subject 1234507
       t(c(1234507, "Placebo",  NA,           NA,           NA,     1,     "Europe",   "F",   20)),

       #subject 1234508
       t(c(1234508, "Placebo",  NA,           "2010-12-31", NA,     1,     "Europe",   "M",   40)),

       #subject 1234509
       t(c(1234509, "Placebo",  "2010-01-01", "2010-12-31", NA,     1,      "Europe",   "M",   20)),

       #subject 1234510
       t(c(1234510, "Placebo",  "2010-01-01", "2010-12-31", NA,     1,      "Europe",   "F",   20)),

       #subject 1234511
       t(c(1234511, "Placebo",  "2010-01-01", "2010-12-31", NA,     1,       "Europe",   "M",   20)),

       #subject 1234512
       t(c(1234512, "Placebo",  "2010-01-01", "2010-12-31", NA,     1,     "Europe",   "M",   20)),

       #subject 1234513 add subject id
       t(c(1234513, "Placebo",  "2010-01-01", "2010-12-31", NA,     1,       "Europe",   "M",   30)),

       #subject 1234514
       t(c(1234514, "Placebo",  "2010-01-01", "2010-12-31", NA,     1,      "Europe",   "F",   20)),

       #subject 1234515
       t(c(1234515, "Placebo",  "2010-01-01", "2010-12-31", "2009-01-01", 1,     "Europe",   "M",   20)),

       #subject 1234516
       t(c(1234516,  "Placebo",  "2010-01-01", "2010-12-31", "2009-01-01", 1,    "Europe",   "M",   20)),

       #subject 1234522 with missing severity grades
       t(c(1234522,   "Trtmt",    "2010-01-01", "2010-12-31",    NA,   1,      "Asia",   "F",   30)),

       #subject 1234523 with missing severity grades
       t(c(1234523,    "Placebo",     "2010-01-01", "2010-12-31",   NA,   1,   "Europe",   "F",   40))


     )
  )
  colnames(adsl_data) <- c("SUBJIDN",  "TRT01A",        "TRTSDT",     "LVDT",  "DTHDT","SAFFN", "REGION", "SEX", "AGE")
  adsl_data$SUBJIDN <- as.numeric(adsl_data$SUBJIDN)
  adsl_data$TRTSDT <- as.Date(adsl_data$TRTSDT)
  #check also different formats like character and Date
  adsl_data$LVDT <- as.Date(adsl_data$LVDT)
  adsl_data$DTHDT <- as.character(adsl_data$DTHDT)
  adsl_data$SAFFN <- as.numeric(adsl_data$SAFFN)

  adae_and_adsl <- join_adae_and_adsl(dat_adae = adae_data, dat_adsl = adsl_data %>% dplyr::filter(SUBJIDN != 1234502 & SUBJIDN != 1234503 & SUBJIDN != 1234505), SUBJIDN = "SUBJIDN")

  filtered <- filter_for_safety_flag(dat = adae_and_adsl, SAFFN = "SAFFN")
  #subject 1234505 should be removed since saffn = 0

  imputed <- calculate_and_impute_required_variables_missing_values(data = filtered, severity_grading_flag = "Severity")

  testthat::expect_equal(is.list(imputed),TRUE)
  testthat::expect_equal(is.data.frame(imputed$data),TRUE)


  #aes for subject 19 and 20
  testthat::expect_equal(imputed$number_unknown_aes, 4)
  testthat::expect_equal(sum(imputed$data$AEDECOD_imputed_flag == 1),4)

  #aes for subject 12 and 17
  testthat::expect_equal(imputed$number_ae_end_missing,2)
  testthat::expect_equal(sum(imputed$data$AEENDY_imputed_flag == 1),2)

  # expect subject 07
  testthat::expect_equal(imputed$number_missing_lvdt,1)
  testthat::expect_equal(sum(imputed$data %>% dplyr::select(SUBJIDN,LVDT_imputed_flag) %>% distinct() %>% dplyr::pull(LVDT_imputed_flag) == 1),1)

  # two for subject 09 and one fo 17
  testthat::expect_equal(imputed$number_ae_start_missing,3)
  testthat::expect_equal(sum(imputed$data$AESTDY_imputed_flag == 1),3)

  # subject 14
  testthat::expect_equal(imputed$number_severe_missing, 5)
  testthat::expect_equal(sum(imputed$data$AESEVN_imputed_flag == 1),5)

  # subject 11 and subject 21
  testthat::expect_equal(imputed$number_days_removed,2)
  testthat::expect_equal(nrow(imputed$data[imputed$data$SUBJIDN == 1234511,]),0)
  testthat::expect_equal(nrow(imputed$data[imputed$data$SUBJIDN == 1234521,]),0)
})
