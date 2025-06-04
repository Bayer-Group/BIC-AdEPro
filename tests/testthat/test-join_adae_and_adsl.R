test_that("test joining for demo data", {

  joint_data <- join_adae_and_adsl(dat_adae = adae_data, dat_adsl = adsl_data, "SUBJIDN")

  testthat::expect_equal(joint_data %>% nrow > 0, TRUE)
  testthat::expect_equal(joint_data %>% nrow, 728)

  length_subject_ae <- adae_data$SUBJIDN %>% unique() %>% length()
  length_subject_sl <- adsl_data$SUBJIDN %>% unique() %>% length()
  length_subject_tot <- joint_data$SUBJIDN %>% unique() %>% length()

  testthat::expect_equal(length_subject_sl, length_subject_tot)
  testthat::expect_equal(length_subject_ae, joint_data %>% dplyr::filter(!is.na(AEDECOD)) %>% dplyr::pull(SUBJIDN) %>% unique() %>% length())
})


test_that("test joining created data", {

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
       t(c(1234521,   "Headache", 10,        12,       2,       1,           0,       1,        "Placebo",  "2010-01-01", "2010-12-31", NA,     4,      NA,      NA)),

       #subject 1234522 with missing severity grades
       t(c(1234522,   "Pain",     2,        4,       NA,       1,           0,       1,        "Trtmt",    "2010-01-01", "2010-12-31", NA,     1,      1,      1)),
       t(c(1234522,    NA,     6,        7,       NA,       1,           0,       1,        "Trtmt",    "2010-01-01", "2010-12-31", NA,     1,      1,      1)),
       t(c(1234522,   "Headache", 10,       130,     1,       1,           0,       1,        "Trtmt",    "2010-01-01", "2010-12-31", NA,     1,      1,      1)),

       #subject 1234523 with missing severity grades
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

  #create adae test data for test_that package
  adsl_data <- data.frame(
    rbind(
      #subject 1234500
       # "SUBJIDN",  "TRT01A",        "TRTSDT",     "LVDT",  "DTHDT",       "SAFFN", "REGION", "SEX", "AGE")
       t(c(1234500,  "Trtmt",    "2010-01-01", "2010-12-31", NA,            1,      "Europe",   "M",   20)),

      #subject 1234501
       t(c(1234501, "Placebo",  "2010-01-01", "2010-12-31", NA,            1,      "Europe",   "M",   20)),

       #subject 1234502 with different TRT01A than in adae
       t(c(1234502,  "Trtmt",  "2010-01-01",  "2010-12-31", NA,            1,      "Europe",   "M",   20)),

       #subject 1234503 different/missing TRTSDT than in adae
       t(c(1234503,  "Trtmt",    NA,          "2010-12-31", NA,            1,      "Europe",   "M",   60)),

       #subject 1234504
       t(c(1234504,  "Trtmt",   "2010-01-01", "2010-12-31", NA,            0,      "Europe",   "M",   20)),

       #subject 1234505
       t(c(1234505, "Placebo",  "2010-01-01", "2010-12-31", NA,            1,       "Europe",   "M",   50)),

       #subject 1234506 set subjid to missing
       t(c(NA,  "Placebo",      NA,           "2010-12-31", NA,            1,       "Europe",   "M",   20)),

       #subject 1234507
       t(c(1234507, "Placebo",  NA,           NA,           NA,            1,     "Europe",   "F",   20)),

       #subject 1234508
       t(c(1234508, "Placebo",  NA,           "2010-12-31", NA,            1,     "Europe",   "M",   40)),

       #subject 1234509
       t(c(1234509, "Placebo",  "2010-01-01", "2010-12-31", NA,            1,      "Europe",   "M",   20)),

       #subject 1234510
       t(c(1234510, "Placebo",  "2010-01-01", "2010-12-31", NA,            1,      "Europe",   "F",   20)),

       #subject 1234511
       t(c(1234511, "Placebo",  "2010-01-01", "2010-12-31", NA,            1,       "Europe",   "M",   20)),

       #subject 1234512
       t(c(1234512, "Placebo",  "2010-01-01", "2010-12-31", NA,            1,     "Europe",   "M",   20)),

       #subject 1234513 add subject id
       t(c(1234513, "Placebo",  "2010-01-01", "2010-12-31", NA,            1,     "Europe",   "M",   30)),

       #subject 1234514
       t(c(1234514, "Placebo",  "2010-01-01", "2010-12-31", NA,            1,      "Europe",   "F",   20)),

       #subject 1234515
       t(c(1234515, "Placebo",  "2010-01-01", "2010-12-31", "2009-01-01",  1,     "Europe",   "M",   20)),

       #subject 1234516
       t(c(1234516,  "Placebo",  "2010-01-01", "2010-12-31", "2009-01-01", 1,    "Europe",   "M",   20)),

      #subject 1234522 with missing severity grades
       t(c(1234522,   "Trtmt",    "2010-01-01", "2010-12-31", NA,           1,    "Asia",   "F",   30)),

       #subject 1234523 with missing severity grades
       t(c(1234523,    "Placebo", "2010-01-01", "2010-12-31", NA,           1,     "Europe",   "F",   40))

     )
  )

  colnames(adsl_data) <- c("SUBJIDN",  "TRT01A",        "TRTSDT",     "LVDT",  "DTHDT","SAFFN", "REGION", "SEX", "AGE")
  adsl_data$SUBJIDN <- as.numeric(adsl_data$SUBJIDN)
  adsl_data$TRTSDT <- as.Date(adsl_data$TRTSDT)
  #check also different formats like character and Date
  adsl_data$LVDT <- as.Date(adsl_data$LVDT)
  adsl_data$DTHDT <- as.character(adsl_data$DTHDT)
  adsl_data$SAFFN <- as.numeric(adsl_data$SAFFN)



  ####
  #tests
  # should return error since "SUBJID" is not in the data
  testthat::expect_error(
    join_adae_and_adsl(
    dat_adae = adae_data,
    dat_adsl = adsl_data,
    SUBJIDN = "SUBJID"
    )
  )

  #Columns TRT01A are different in adae and adsl for subject 1234502
  adae_and_adsl <- join_adae_and_adsl(
    dat_adae = adae_data,
    dat_adsl = adsl_data,
    SUBJIDN = "SUBJIDN"
  )

  testthat::expect_equal(
    colnames(adae_and_adsl),
    c("SUBJIDN", "TRT01A.x", "TRTSDT.x", "LVDT",
      "DTHDT", "SAFFN", "REGION", "SEX", "AGE",
      "AEDECOD", "AESTDY", "AEENDY", "AESEVN",
      "AETRTEMN", "AESERN", "AERELN", "TRT01A.y",
      "TRTSDT.y", "AERELPRN", "AEACNN"
    )
  )

  #remove subject 1234502 to avoid treatment conflicts
  adae_and_adsl2 <- join_adae_and_adsl(
    dat_adae = adae_data,
    dat_adsl = adsl_data %>%
      dplyr::filter(SUBJIDN != 1234502),
    SUBJIDN = "SUBJIDN"
  )

  testthat::expect_equal(
    colnames(adae_and_adsl2),
    c("SUBJIDN", "TRT01A", "TRTSDT.x", "LVDT",
      "DTHDT", "SAFFN", "REGION", "SEX", "AGE",
      "AEDECOD", "AESTDY", "AEENDY", "AESEVN",
      "AETRTEMN", "AESERN", "AERELN",
      "TRTSDT.y", "AERELPRN", "AEACNN"
    )
  )

  adae_and_adsl3 <- join_adae_and_adsl(
    dat_adae = adae_data,
    dat_adsl = adsl_data %>%
      dplyr::filter(SUBJIDN != 1234502 & SUBJIDN != 1234503),
    SUBJIDN = "SUBJIDN"
  )

  testthat::expect_equal(
    colnames(adae_and_adsl3),
    c("SUBJIDN", "TRT01A", "TRTSDT", "LVDT",
      "DTHDT", "SAFFN", "REGION", "SEX", "AGE",
      "AEDECOD", "AESTDY", "AEENDY", "AESEVN",
      "AETRTEMN", "AESERN", "AERELN", "AERELPRN",
      "AEACNN"
    )
  )
  testthat::expect_equal(adae_and_adsl3 %>% nrow, 37)

})
