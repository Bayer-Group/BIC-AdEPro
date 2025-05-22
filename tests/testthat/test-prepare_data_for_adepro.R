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


  filtered <- filter_for_safety_flag(dat = adae_data, SAFFN = "SAFFN")
  #subject 1234505 should be removed since saffn = 0

  imputed <- calculate_and_impute_required_variables_missing_values(data = filtered, severity_grading_flag = "Severity")

  res <- prepare_data_for_adepro(dat = imputed$data)
  #check function without adsl data
  #general
  testthat::expect_equal(is.list(res), TRUE)
  testthat::expect_equal(length(res), 2)
  testthat::expect_equal(names(res), c("ae_data","pat_data"))

  #check ae_data:
  testthat::expect_equal(all(c("patient","day_start","day_end","ae","sev","trtem","ser","nonser") %in% colnames(res$ae_data)), TRUE)

  #check if 3 adverse events are available for subject 1234500
  testthat::expect_equal(dim(res$ae_data[res$ae_data$patient == 1234500 &!is.na(res$ae_data$patient),]), c(3,15))
  #check if 2 adverse events are available for subject 1234501
  testthat::expect_equal(dim(res$ae_data[res$ae_data$patient == 1234501 &!is.na(res$ae_data$patient),]), c(2,15))
  #check if 1 adverse events are available for subject 1234505 (one has safetyflag 0)
  testthat::expect_equal(dim(res$ae_data[res$ae_data$patient == 1234505 &!is.na(res$ae_data$patient),]), c(1,15))
  #check if 1 adverse event is available for subject 1234510 (duplicated rows)
  testthat::expect_equal(dim(res$ae_data[res$ae_data$patient == 1234510 &!is.na(res$ae_data$patient),]), c(1,15))
  #check if no adverse events are available for subject 1234513 (subjectid is missing)
  testthat::expect_equal(dim(res$ae_data[res$ae_data$patient == 1234513 &!is.na(res$ae_data$patient),]), c(0,15))

  #check if missing ae is not imputed since ae start day and end day are also missing
  expect_equal(nrow(res$ae_data[res$ae_data$patient == 1234518 &!is.na(res$ae_data$patient),]),0)
  #check if missing ae is imputed
  expect_equal(as.character(res$ae_data[res$ae_data$patient == 1234519 &!is.na(res$ae_data$patient),]$ae),"Unknown type of AE")
  #check if missing ae is imputed
  expect_equal(as.character(res$ae_data[res$ae_data$patient == 1234520 &!is.na(res$ae_data$patient),]$ae),"Unknown type of AE")
  #check if missing ae is not imputed since ae start day and end day are also missing
  expect_equal(nrow(res$ae_data[res$ae_data$patient == 1234521 &!is.na(res$ae_data$patient),]),0)

  testthat::expect_equal(res$ae_data[res$ae_data$patient == 1234503 &!is.na(res$ae_data$patient),"day_start"], 1)
  testthat::expect_equal(res$ae_data[res$ae_data$patient == 1234503 &!is.na(res$ae_data$patient),"day_end"], 2)


  #check pat_data:
  testthat::expect_equal(all(c("ps","treat","end","death")%in%colnames(res$pat_data)), TRUE)
  testthat::expect_equal(res$pat_data[res$pat_data$ps == 1234503 &!is.na(res$pat_data$ps),"end"],365)
  testthat::expect_equal(is.na(res$pat_data[res$pat_data$ps == 1234507 &!is.na(res$pat_data$ps),"end"]),TRUE)
  testthat::expect_equal(res$pat_data[res$pat_data$ps == 1234500 &!is.na(res$pat_data$ps),"death"],99999)
  testthat::expect_equal(res$pat_data[res$pat_data$ps == 1234516 &!is.na(res$pat_data$ps),"death"],-364)
  testthat::expect_equal(res$pat_data[res$pat_data$ps == 1234513 &!is.na(res$pat_data$ps),"death"], numeric(0))

  #Check Errors & Warnings
  #test for data.frame
  testthat::expect_error(prepare_data_for_adepro(dat = NULL))
  testthat::expect_error(prepare_data_for_adepro(dat = 1))

  # without required variables in data set dat:
  testthat::expect_error(prepare_data_for_adepro(dat = adae_data[,-grep("SUBJIDN", colnames(adae_data))]))
  testthat::expect_error(prepare_data_for_adepro(dat = adae_data[,-grep("AEDECOD", colnames(adae_data))]))
  testthat::expect_error(prepare_data_for_adepro(dat = adae_data[,-grep("AESTDY", colnames(adae_data))]))
  testthat::expect_error(prepare_data_for_adepro(dat = adae_data[,-grep("AESEVN", colnames(adae_data))]))
  testthat::expect_error(prepare_data_for_adepro(dat = adae_data[,-grep("AETRTEMN", colnames(adae_data))]))
  testthat::expect_error(prepare_data_for_adepro(dat = adae_data[,-grep("AEENDY", colnames(adae_data))]))
  testthat::expect_error(prepare_data_for_adepro(dat = adae_data[,-grep("TRT01A", colnames(adae_data))]))
  testthat::expect_error(prepare_data_for_adepro(dat = adae_data[,-grep("TRTSDT", colnames(adae_data))]))
  testthat::expect_error(prepare_data_for_adepro(dat = adae_data[,-grep("LVDT", colnames(adae_data))]))
  testthat::expect_error(prepare_data_for_adepro(dat = adae_data[,-grep("DTHDT", colnames(adae_data))]))
  testthat::expect_error(prepare_data_for_adepro(dat = adae_data[,-grep("SAFFN", colnames(adae_data))]))

  #expect no error for optional vars
  testthat::expect_no_error(prepare_data_for_adepro(dat = adae_data[,-grep("AERELN", colnames(adae_data))]))
  testthat::expect_no_error(prepare_data_for_adepro(dat = adae_data[,-grep("AESERN", colnames(adae_data))]))
  testthat::expect_no_error(prepare_data_for_adepro(dat = adae_data[,-grep("AERELPRN", colnames(adae_data))]))
  testthat::expect_no_error(prepare_data_for_adepro(dat = adae_data[,-grep("AEACNN", colnames(adae_data))]))


  #
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
       t(c(1234522,   "Trtmt",    "2010-01-01", "2010-12-31",       NA,  1,     "Asia",   "F",   30)),

       #subject 1234523 with missing severity grades
       t(c(1234523,    "Placebo",     "2010-01-01", "2010-12-31", NA, 1,    "Europe",   "F",   40))

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
  #remove subjects 1234502 and 1234503 with expected error from adsl
  res2 <- prepare_data_for_adepro(dat = imputed$data)
      #res2 <- prepare_data(dat = adae_data, adsl_data = adsl_data %>% dplyr::filter(SUBJIDN != 1234502 & SUBJIDN != 1234503 & SUBJIDN != 1234505))

  #check function without adsl data
  #general
  testthat::expect_equal(is.list(res2), TRUE)
  testthat::expect_equal(length(res2), 2)
  testthat::expect_equal(names(res2), c("ae_data","pat_data"))

  #check ae_data:
  testthat::expect_equal(all(c("patient","day_start","day_end","ae","sev","trtem","ser","nonser") %in% colnames(res2$ae_data)), TRUE)
  #check if 3 adverse events are available for subject 1234500
  testthat::expect_equal(dim(res2$ae_data[res2$ae_data$patient == 1234500 &!is.na(res2$ae_data$patient),]), c(3,15))
  #check if 2 adverse events are available for subject 1234501
  testthat::expect_equal(dim(res2$ae_data[res2$ae_data$patient == 1234501 &!is.na(res2$ae_data$patient),]), c(2,15))
  #check if 1 adverse events are available for subject 1234505 (one has safetyflag 0)
  testthat::expect_equal(dim(res2$ae_data[res2$ae_data$patient == 1234505 &!is.na(res2$ae_data$patient),]), c(1,15))
  #check if 1 adverse event is available for subject 1234510 (duplicated rows)
  testthat::expect_equal(dim(res2$ae_data[res2$ae_data$patient == 1234510 &!is.na(res2$ae_data$patient),]), c(1,15))
  #check if no adverse events are available for subject 1234513 (subjectid is missing)
  testthat::expect_equal(dim(res2$ae_data[res2$ae_data$patient == 1234513 &!is.na(res2$ae_data$patient),]), c(0,15))

  testthat::expect_equal(res2$ae_data[res2$ae_data$patient == 1234503 &!is.na(res2$ae_data$patient),"day_start"], 1)
  testthat::expect_equal(res2$ae_data[res2$ae_data$patient == 1234503 &!is.na(res2$ae_data$patient),"day_end"], 2)


  #check pat_data:
  testthat::expect_equal(all(c("ps","treat","end","death") %in% colnames(res2$pat_data)), TRUE)
  testthat::expect_equal(res2$pat_data[res2$pat_data$ps == 1234503 &!is.na(res2$pat_data$ps),"end"],365)
  testthat::expect_equal(is.na(res2$pat_data[res2$pat_data$ps == 1234507 &!is.na(res2$pat_data$ps),"end"]),TRUE)
  testthat::expect_equal(res2$pat_data[res2$pat_data$ps == 1234500 &!is.na(res2$pat_data$ps),"death"],99999)
  testthat::expect_equal(res2$pat_data[res2$pat_data$ps == 1234516 &!is.na(res2$pat_data$ps),"death"],-364)
  #missing subject information in adae but available in adsl
  testthat::expect_equal(res2$pat_data[res2$pat_data$ps == 1234513 &!is.na(res2$pat_data$ps),"death"], 99999)

})



test_that("check function prepare_data_for_adepro with demo data:", {


  filtered <- filter_for_safety_flag(dat = adae_data, SAFFN = "SAFFN")
  #subject 1234505 should be removed since saffn = 0

  imputed <- calculate_and_impute_required_variables_missing_values(data = filtered, severity_grading_flag = "Severity")

  res <- prepare_data_for_adepro(dat = imputed$data)

  testthat::expect_equal(is.list(res), TRUE)
  testthat::expect_equal(length(res), 2)
  testthat::expect_equal(names(res), c("ae_data","pat_data"))

  #check ae_data:
  testthat::expect_equal(all(c("patient","day_start","day_end","ae","sev","trtem","ser","nonser") %in% colnames(res$ae_data)), TRUE)
  testthat::expect_equal(length(unique(res$ae_data$patient)),255)
  #check pat_data:
  testthat::expect_equal(all(c("ps","treat","end","death") %in% colnames(res$pat_data)), TRUE)
  testthat::expect_equal(length(unique(res$pat_data$ps)), length(res$pat_data$ps))
  testthat::expect_equal(length(unique(res$pat_data$ps)),255)
  testthat::expect_equal(length(unique(res$pat_data$patient)),0)

  #with adsl_data
  adae_and_adsl <- join_adae_and_adsl(dat_adae = adae_data, dat_adsl = adsl_data, "SUBJIDN")
  res2 <- prepare_data_for_adepro(dat = adae_and_adsl)

  #check function without adsl data
  #general
  testthat::expect_equal(is.list(res2), TRUE)
  testthat::expect_equal(length(res2), 2)
  testthat::expect_equal(names(res2), c("ae_data","pat_data"))

  #check ae_data:
  testthat::expect_equal(all(c("patient","day_start","day_end","ae","sev","trtem","ser","nonser") %in% colnames(res2$ae_data)), TRUE)
  testthat::expect_equal(length(unique(res2$ae_data$patient)),255)
  #check pat_data:
  testthat::expect_equal(all(c("ps","treat","end","death") %in% colnames(res2$pat_data)), TRUE)
  testthat::expect_equal(length(unique(res2$pat_data$ps)), length(res2$pat_data$ps))
  testthat::expect_equal(length(unique(res2$pat_data$ps)), 300)
})

