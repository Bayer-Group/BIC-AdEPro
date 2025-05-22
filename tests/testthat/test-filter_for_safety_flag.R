test_that("Check filter for safety flag is working (Part1)", {

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


  ffsf <- filter_for_safety_flag(dat = adae_data, SAFFN = "SAFFN")
  #subject 1234505 should be removed since safety flag is 0 and subject 1234521 since invalid safety flag (4)

  testthat::expect_equal(is.data.frame(ffsf), TRUE)
  testthat::expect_equal(nrow(adae_data %>% dplyr::filter(SAFFN == 1)), nrow(ffsf))
  testthat::expect_equal(nrow(adae_data) - 2, nrow(ffsf))

  testthat::expect_equal(nrow(adae_data %>% dplyr::filter(SUBJIDN == 1234504)),1)
  testthat::expect_equal(nrow(adae_data %>% dplyr::filter(SUBJIDN == 1234521)),1)
  testthat::expect_equal(nrow(ffsf %>% dplyr::filter(SUBJIDN == 1234504)),0)
  testthat::expect_equal(nrow(ffsf %>% dplyr::filter(SUBJIDN == 1234521)),0)
})

test_that("Check filter for safety flag is working (Part2)", {

 ffsf <- filter_for_safety_flag(dat = adae_data, SAFFN = "SAFFN")
  #subject 1234505 should be removed since safety flag is 0 and subject 1234521 since invalid safety flag (4)

  testthat::expect_equal(is.data.frame(ffsf), TRUE)
  testthat::expect_equal(nrow(adae_data %>% dplyr::filter(SAFFN == 1)), nrow(ffsf))
  testthat::expect_equal(nrow(adae_data), nrow(ffsf))

})

test_that("Check filter for safety flag is working with character safety flag (Part3)", {

 #create adae test data for test_that package
 adae_data <- data.frame(
    rbind(
       #subject 1234500 with no expected error/warning and 3 entries
       # "SUBJIDN", "AEDECOD", "AESTDY", "AEENDY", "AESEVN","AETRTEMN", "AESERN", "AERELN",  "TRT01A",        "TRTSDT",     "LVDT",  "DTHDT","SAFFN","AERELPRN","AEACNN"
       t(c(1234500,   "Pain",     2,        4,       1,       1,           0,       1,        "Trtmt",    "2010-01-01", "2010-12-31", NA,     "YES",      1,      1)),
       t(c(1234500,   "Pain",     6,        7,       1,       1,           0,       1,        "Trtmt",    "2010-01-01", "2010-12-31", NA,     "YES",      1,      1)),
       t(c(1234500,   "Headache", 10,       130,     1,       1,           0,       1,        "Trtmt",    "2010-01-01", "2010-12-31", NA,     "YES",      1,      1)),

       #subject 1234501 with no expected error/warning and 2 entries
       t(c(1234501,   "Pain",     2,        4,       1,       1,           0,       1,        "Placebo",  "2010-01-01", "2010-12-31", NA,     "YES",      0,      0)),
       t(c(1234501,   "Pain",     6,        7,       3,       1,           0,       1,        "Placebo",  "2010-01-01", "2010-12-31", NA,     "YES",     0,      0)),

       #subject 1234502 with no expected error/warning and 1 entries
       t(c(1234502,   "Headache", 1,        2,       2,       1,           0,       1,        "Placebo",  "2010-01-01", "2010-12-31", NA,     "YES",     NA,      NA)),

       #subject 1234503 with no expected error/warning and 1 entries
       t(c(1234503,   "Headache", 1,        2,       2,       1,           0,       1,        "Trtmt",    "2010-01-01", "2010-12-31", NA,     "YES",      NA,      NA)),

       #subject 1234504 with safety flag 0
       t(c(1234504,   "Headache", 1,        2,       2,       1,           0,       1,        "Trtmt",    "2010-01-01", "2010-12-31", NA,     "NO",     NA,      NA)),

       #subject 1234505 with safety adverse event ended before day 1
       t(c(1234505,   "Headache", -7,        -2,       2,       1,           0,       1,        "Placebo",  "2010-01-01", "2010-12-31", NA,     "YES",      NA,      NA)),
       t(c(1234505,   "Pain",     1,        2,       2,       1,           0,       1,        "Placebo",  "2010-01-01", "2010-12-31", NA,     "YES",     NA,      NA)),

       ## warnings/error

       #subject 1234506 with missing start date
       t(c(1234506,   "Headache", 1,        2,       2,       1,           0,       1,        "Placebo",  NA,           "2010-12-31", NA,     "YES",     NA,      NA)),
       t(c(1234506,   "Pain",     1,        2,       2,       1,           0,       1,        "Placebo",  NA,           "2010-12-31", NA,     "YES",     NA,      NA)),

       #subject 1234507 with missing start and last visit date
       t(c(1234507,   "Headache", 1,        2,       2,       1,           0,       1,        "Placebo",  NA,           NA,           NA,     "yes",      NA,      NA)),
       t(c(1234507,   "Pain",     1,        2,       2,       1,           0,       1,        "Placebo",  NA,           NA,           NA,     "yes",      NA,      NA)),

       #subject 1234508 with missing start date
       t(c(1234508,   "Headache", 1,        2,       2,       1,           0,       1,        "Placebo",  NA,           "2010-12-31", NA,     "YES",      NA,      NA)),
       t(c(1234508,   "Pain",     1,        2,       2,       1,           0,       1,        "Placebo",  NA,           "2010-12-31", NA,     "YES",      NA,      NA)),

       #subject 1234509 with missing ae start time
       t(c(1234509,   "Headache",NA,        2,       2,       1,           0,       1,        "Placebo",  "2010-01-01", "2010-12-31", NA,     "YES",      NA,      NA)),
       t(c(1234509,   "Pain",    NA,        2,       2,       1,           0,       1,        "Placebo",  "2010-01-01", "2010-12-31", NA,     "YES",      NA,      NA)),

       #subject 1234510 with non-unique row
       t(c(1234510,   "Headache",1,        2,       2,       1,           0,       1,        "Placebo",  "2010-01-01", "2010-12-31", NA,     "YES",      NA,      NA)),
       t(c(1234510,   "Headache",1,        2,       2,       1,           0,       1,        "Placebo",  "2010-01-01", "2010-12-31", NA,     "YES",      NA,      NA)),

       #subject 1234511 ae start day after ae end day
       t(c(1234511,   "Headache",3,        2,       2,       1,           0,       1,        "Placebo",  "2010-01-01", "2010-12-31", NA,     "YES",      NA,      NA)),

       #subject 1234512 with missing ae end day
       t(c(1234512,   "Headache",1,        NA,       2,       1,           0,       1,        "Placebo",  "2010-01-01", "2010-12-31", NA,     "YES",      NA,      NA)),

       #subject 1234513 with missing subject id
       t(c(NA     ,   "Headache",1,        2,       2,       1,           0,       1,        "Placebo",  "2010-01-01", "2010-12-31", NA,     "YES",      NA,      NA)),

       #subject 1234514 with wrong ae severity grade
       t(c(1234514,   "Headache",1,        3,       10,       1,           0,       1,        "Placebo",  "2010-01-01", "2010-12-31", NA,     "YES",     NA,      NA)),

       #subject 1234515 with death date before trt start date
       t(c(1234515,   "Headache",1,        3,       2,       1,           0,       1,        "Placebo",  "2010-01-01", "2010-12-31", "2009-01-01",    "YES",      NA,      NA)),

       #subject 1234516 with wrong coding for ae related flag
       t(c(1234516,   "Headache",1,        3,       2,       1,           0,       1,        "Placebo",  "2010-01-01", "2010-12-31", "2009-01-01",     "YES",     3,      NA)),

       #subject 1234517 with ae start day and end day missing
       t(c(1234517,   "Headache",NA,        NA,       2,       1,           0,       1,        "Placebo",  "2010-01-01", "2010-12-31", "2009-01-01",     "YES",    3,      NA)),

       #subject 1234518 with ae start day and end day missing and ae code missing
       t(c(1234518,   NA ,NA,        NA,       2,       1,           0,       1,        "Trtmt",  "2010-01-01", "2010-12-31", "2009-01-01",     "YES",     3,      NA)),

       #subject 1234519 with no expected error/warning and 1 entries
       t(c(1234519,   NA, 1,        2,       2,       1,           0,       1,        "Placebo",  "2010-01-01", "2010-12-31", NA,     "YES",      NA,      NA)),

       #subject 1234502 with no expected error/warning and 1 entries
       t(c(1234520,   "", 1,        2,       2,       1,           0,       1,        "Placebo",  "2010-01-01", "2010-12-31", NA,     "YES",      NA,      NA)),

       #subject 1234521 with no expected error/warning and 1 entries
       t(c(1234521,   "Headache", 10,        12,       2,       1,           0,       1,        "Placebo",  "2010-01-01", "2010-12-31", NA,     "YESSS",      NA,      NA)),

       #subject 1234522 with missing severity grades
       t(c(1234522,   "Pain",     2,        4,       NA,       1,           0,       1,        "Trtmt",    "2010-01-01", "2010-12-31", NA,     "YES",      1,      1)),
       t(c(1234522,    NA,     6,        7,       NA,       1,           0,       1,        "Trtmt",    "2010-01-01", "2010-12-31", NA,     "YES",      1,      1)),
       t(c(1234522,   "Headache", 10,       130,     1,       1,           0,       1,        "Trtmt",    "2010-01-01", "2010-12-31", NA,     "YES",      1,      1)),

       #subject 1234523 with missing severity grades
       t(c(1234523,   "",     2,        4,       NA,       1,           0,       1,        "Placebo",  "2010-01-01", "2010-12-31", NA,     "YES",      0,      0)),
       t(c(1234523,   "Pain",     6,        7,       5,       1,           0,       1,        "Placebo",  "2010-01-01", "2010-12-31", NA,     "YES",      0,      0))


     )
  )

  colnames(adae_data) <- c("SUBJIDN", "AEDECOD", "AESTDY", "AEENDY", "AESEVN","AETRTEMN", "AESERN", "AERELN", "TRT01A","TRTSDT",     "LVDT",       "DTHDT","SAFFN","AERELPRN","AEACNN")
  adae_data$SUBJIDN <- as.numeric(adae_data$SUBJIDN)
  adae_data$AESTDY <- as.numeric(adae_data$AESTDY)
  adae_data$AEENDY <- as.numeric(adae_data$AEENDY)
  adae_data$AESEVN <- as.numeric(adae_data$AESEVN)
  adae_data$AETRTEMN <- as.numeric(adae_data$AETRTEMN)
  adae_data$AERELN <- as.numeric(adae_data$AERELN)
  adae_data$SAFFN <- as.character(adae_data$SAFFN)
  adae_data$AESTDY <- as.numeric(adae_data$AESTDY)
  adae_data$AESERN <- as.numeric(adae_data$AESERN)

  adae_data$TRTSDT <- as.Date(adae_data$TRTSDT)
  adae_data$LVDT <- as.Date(adae_data$LVDT)


  ffsf <- filter_for_safety_flag(dat = adae_data, SAFFN = "SAFFN")
  #subject 1234505 should be removed since safety flag is 0 and subject 1234521 since invalid safety flag (4)

  testthat::expect_equal(is.data.frame(ffsf), TRUE)

  testthat::expect_equal(nrow(adae_data %>% dplyr::filter(SAFFN %in% c("YES","yes"))), nrow(ffsf))

  testthat::expect_equal(nrow(adae_data) - 2, nrow(ffsf))

  testthat::expect_equal(nrow(adae_data %>% dplyr::filter(SUBJIDN == 1234504)),1)
  testthat::expect_equal(nrow(adae_data %>% dplyr::filter(SUBJIDN == 1234521)),1)
  testthat::expect_equal(nrow(ffsf %>% dplyr::filter(SUBJIDN == 1234504)),0)
  testthat::expect_equal(nrow(ffsf %>% dplyr::filter(SUBJIDN == 1234521)),0)
})
