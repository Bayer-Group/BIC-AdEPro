 utils::globalVariables(c("death", "cols", "treat", "ps", "new_AESEVN", "patient", "day_start", "day_end", "output"))

#' prepare_data - read SAS or CSV raw data and prepare the data sets
#' @description
#' Creates a list with two data sets 'pat_data' and 'ae_data' in format which is used in the AdEPro Application
#'
#' @param dat adae data set.
#' @param SUBJIDN Subject Id as numeric (required)
#' @param TRT01A Treatment Group as character (required)
#' @param SAFFN Safety Flag as numeric (required)
#' @param LVDT  Last Visit Date as numeric (required)
#' @param DTHDT Death Date as numeric (required)
#' @param TRTSDT Treatment Start Date as numeric (required)
#' @param AEDECOD Adverse Event Code as character (required)
#' @param AESTDY Adverse Event Start Date as numeric (required)
#' @param AETRTEMN Adverse Event Treatment Emergency Flag as numeric (required)
#' @param AEENDY Adverse Event End date as numeric (required)
#' @param AESEVN Adverse Event Severity Grade Flag as numeric (required)
#' @param AESERN Adverse Event Serious Flag as numeric (optional)
#' @param AERELN Adverse Event Related Flag as numeric (optional)
#' @param AERELPRN Adverse Event      Flag as numeric (optional)
#' @param AEACNN Adverse Event        Flag as numeric (optional)
#' @param adsl_data Subject Level data set (optional)
#'
#' @keywords internal
#'

prepare_data2 <- function(
  dat,
  SUBJIDN = "SUBJIDN",
  TRT01A = "TRT01A",
  SAFFN = "SAFFN",
  LVDT = "LVDT",
  DTHDT = "DTHDT",
  TRTSDT = "TRTSDT",
  AEDECOD = "AEDECOD",
  AESTDY = "AESTDY",
  AETRTEMN = "AETRTEMN",
  AEENDY = "AEENDY",
  AESEVN = "AESEVN",
  AESERN = "AESERN",
  AERELN = "AERELN",
  AERELPRN = "AERELPRN",
  AEACNN = "AEACNN",
  adsl_data = NULL
) {

  dat_ <<- dat
  adsl_data_ <<- adsl_data
  SUBJIDN_ <<- SUBJIDN
  TRT01A_ <<- TRT01A
  SAFFN_ <<- SAFFN
  LVDT_ <<- LVDT
  DTHDT_ <<- DTHDT
  TRTSDT_ <<- TRTSDT
  AEDECOD_ <<- AEDECOD
  AESTDY_ <<- AESTDY
  AETRTEMN_ <<- AETRTEMN
  AEENDY_ <<- AEENDY
  AESEVN_ <<- AESEVN
  AESERN_ <<- AESERN
  AERELN_ <<- AERELN
  AERELPRN_ <<- AERELPRN
  AEACNN_ <<- AEACNN

  # dat <- dat_
  # adsl_data <- adsl_data_
  # SUBJIDN <- SUBJIDN_
  # TRT01A <- TRT01A_
  # SAFFN <- SAFFN_
  # LVDT <- LVDT_
  # DTHDT <- DTHDT_
  # TRTSDT <- TRTSDT_
  # AEDECOD <- AEDECOD_
  # AESTDY <- AESTDY_
  # AETRTEMN <- AETRTEMN_
  # AEENDY <- AEENDY_
  # AESEVN <- AESEVN_
  # AESERN <- AESERN_
  # AERELN <- AERELN_
  # AERELPRN <- AERELPRN_
  # AEACNN <- AEACNN_


  if (!is.data.frame(dat)) {
    stop("Parameter 'dat' must be a data frame!")
  }

  required_vars <- c(SUBJIDN, TRT01A, SAFFN ,LVDT, DTHDT, TRTSDT, AEDECOD, AESTDY, AETRTEMN, AEENDY, AESEVN)

  #merge adsl_data when available
  if (!is.null(adsl_data)) {
    #get joint variables in adae and adsl

    joint_vars <- dplyr::intersect(colnames(dat), colnames(adsl_data))

    subject_id_index <- dplyr::intersect(dat[,SUBJIDN],adsl_data[,SUBJIDN]) %>% na.omit()
    #compare if the variable for merging have the same values e.g. a subject has the same treatment in adae and adsl
    comp1 <- dat %>%
      dplyr::filter(SUBJIDN %in% dplyr::pull(subject_id_index)) %>%
      dplyr::select(!!!rlang::syms(joint_vars)) %>%
      dplyr::distinct() %>%
      dplyr::arrange(!!rlang::sym(SUBJIDN))
    comp2 <- adsl_data %>%
      dplyr::filter(SUBJIDN %in% dplyr::pull(subject_id_index)) %>%
      dplyr::select(!!!rlang::syms(joint_vars)) %>%
      dplyr::distinct() %>%
      dplyr::arrange(!!rlang::sym(SUBJIDN))

    attr(comp1, "ATT") <- NULL
    attr(comp2, "ATT") <- NULL

    index_match <- sapply(joint_vars, function(x){identical(comp1 %>% dplyr::pull(x),comp2 %>% dplyr::pull(x))})

    joint_vars_match <- joint_vars[which(index_match)]
    joint_vars_no_match <- joint_vars[which(!index_match)]


    if (any(joint_vars_no_match %in% required_vars)) {
      stop(paste0("Entries for adae and adsl mismatch in required variables used for merging: ",paste(joint_vars_no_match[(joint_vars_no_match %in% required_vars)], collapse=" "), " !"))
    }
    ####

    pat_dat <- dplyr::full_join(adsl_data,dat, by = join_by(!!!rlang::syms(joint_vars_match)))

  } else {
    pat_dat <- dat
  }

  #check for required variables in adae data
  if (!all(required_vars %in% colnames(pat_dat))) {
    stop("Required variables are missing in data set 'dat'!")
  }
  #create variables ps, treat, end, death and filter for safety flags
  prepared_data <- filter_and_prepare_patient_data(data = pat_dat, SAFFN = SAFFN, LVDT = LVDT, TRTSDT = TRTSDT, TRT01A = TRT01A, SUBJIDN = SUBJIDN, DTHDT = DTHDT)

  #filter for adverse event data set
  ae_data <- prepared_data %>%
    dplyr::filter(
      (!!rlang::sym(SAFFN) == 1 | !!rlang::sym(SAFFN) == "Y" | !!rlang::sym(SAFFN) == "Yes" | !!rlang::sym(SAFFN) == "YES" | !!rlang::sym(SAFFN) == "yes" | !!rlang::sym(SAFFN) == "y") &
      (!!rlang::sym(AETRTEMN) == 1 | !!rlang::sym(AETRTEMN) == "Y" | !!rlang::sym(AETRTEMN) == "Yes" | !!rlang::sym(AETRTEMN) == "YES" | !!rlang::sym(AETRTEMN) == "yes" | !!rlang::sym(AETRTEMN) == "y") &
      !is.na(!!rlang::sym(AEDECOD)) & !!rlang::sym(AEDECOD) != ""
    )

  #create severity flag
  ae_data <- ae_data %>%
    dplyr::mutate(new_AESEVN = dplyr::case_when(
      !!rlang::sym(AESEVN) == "MILD" ~ "1",
      !!rlang::sym(AESEVN) == "MODERATE" ~ "2",
      !!rlang::sym(AESEVN)== "SEVERE" ~"3",
      TRUE ~ as.character(!!rlang::sym(AESEVN))
    )
  )

  #create required variables for data set ae_data: day_start, day_end, patient, ae, sev, trtem
  ae_data <- ae_data %>%
    dplyr::mutate(
      day_start = as.numeric(!!rlang::sym(AESTDY)),
      day_end = as.numeric(!!rlang::sym(AEENDY)),
      patient = as.numeric(!!rlang::sym(SUBJIDN)),
      ae = as.factor(!!rlang::sym(AEDECOD)),
      sev = as.numeric(new_AESEVN),
      trtem = ifelse(
        (!!rlang::sym(AETRTEMN) == 1 | !!rlang::sym(AETRTEMN) == "Y" | !!rlang::sym(AETRTEMN) == "Yes" | !!rlang::sym(AETRTEMN) == "YES" | !!rlang::sym(AETRTEMN) == "yes" | !!rlang::sym(AETRTEMN) == "y"),
        1,
        0
      )
    )

  #required variables for adverse event data
  var_list <- c("day_start", "day_end", "patient", "ae", "sev","trtem")

    #create optional variables for adverse event data
    #create ser and nonser from serious flag
    if (!is.null(AESERN)) {
      if (AESERN %in% colnames(dat)) {
        ae_data <- ae_data %>%
          dplyr::mutate(
            ser = ifelse(
              !!rlang::sym(AESERN) == 1 |
              !!rlang::sym(AESERN) == "Y" |
              !!rlang::sym(AESERN) == "Yes" |
              !!rlang::sym(AESERN) == "YES" |
              !!rlang::sym(AESERN) == "yes" |
              !!rlang::sym(AESERN) == "y",
              1,
              0
            ),
            nonser = ifelse(
              !!rlang::sym(AESERN) == 0 |
              !!rlang::sym(AESERN) == "N" |
              !!rlang::sym(AESERN) == "No" |
              !!rlang::sym(AESERN) == "NO" |
              !!rlang::sym(AESERN) == "no" |
              !!rlang::sym(AESERN) == "n" |
              !!rlang::sym(AESERN) == "",
              1,
              0
            )
          )
        var_list <- c(var_list, c("ser", "nonser"))
      }
    }
    # create studlrel for drug related flag
    if (!is.null(AERELN)) {
      if (AERELN %in% colnames(dat)) {
        ae_data <- ae_data %>%
          dplyr::mutate(studrel = ifelse(!!rlang::sym(AERELN) == 1 | !!rlang::sym(AERELN) == "Y" | !!rlang::sym(AERELN) == "Yes" | !!rlang::sym(AERELN) == "YES" | !!rlang::sym(AERELN) == "yes" | !!rlang::sym(AERELN) == "y", 1, 0))
        var_list <- c(var_list, c("studrel"))
      }
    }

    if (!is.null(AERELN) & !is.null(AESERN)) {
      if (AERELN %in% colnames(dat) & AESERN %in% colnames(dat)) {
        ae_data <- ae_data %>%
          dplyr::mutate(studrelser = ifelse(!!rlang::sym(AERELN) == 1 | !!rlang::sym(AERELN) == "Y" | !!rlang::sym(AERELN) == "Yes" | !!rlang::sym(AERELN) == "YES" | !!rlang::sym(AERELN) == "yes" | !!rlang::sym(AERELN) == "y", 1, 0) * ifelse(!!rlang::sym(AESERN) == 1 | !!rlang::sym(AESERN) == "Y" | !!rlang::sym(AESERN) == "Yes" | !!rlang::sym(AESERN) == "YES" | !!rlang::sym(AESERN) == "yes" | !!rlang::sym(AESERN) == "y", 1, 0))
        var_list <- c(var_list, c("studrelser"))
      }
    }

    if (!is.null(AERELPRN)) {
      if (AERELPRN %in% colnames(dat)) {
        ae_data <- ae_data %>%
          dplyr::mutate(relprot = ifelse(!!rlang::sym(AERELPRN) == 1 | !!rlang::sym(AERELPRN) == "Y" | !!rlang::sym(AERELPRN) == "Yes" | !!rlang::sym(AERELPRN) == "YES" | !!rlang::sym(AERELPRN) == "yes" | !!rlang::sym(AERELPRN) == "y", 1, 0))
        var_list <- c(var_list, c("relprot"))
      }
    }

    if (!is.null(AEACNN)) {
      if (AEACNN %in% colnames(dat)) {
        ae_data <- ae_data %>%
          dplyr::mutate(resdisc = ifelse(!!rlang::sym(AEACNN) == 1 | !!rlang::sym(AEACNN) == "DRUG WITHDRAWN", 1, 0))
        var_list <- c(var_list, c("resdisc"))
      }
    }

    if (!is.null(AERELN) & !is.null(AEACNN) & !is.null(AESERN)) {
      if (AERELN %in% colnames(dat) & AEACNN %in% colnames(dat)) {
        ae_data <- ae_data %>%
          dplyr::mutate(studrelresdisc = ifelse(!!rlang::sym(AERELN) == 1 | !!rlang::sym(AERELN) == "Y" | !!rlang::sym(AERELN) == "Yes" | !!rlang::sym(AERELN) == "YES" | !!rlang::sym(AERELN) == "yes" | !!rlang::sym(AERELN) == "y", 1, 0) * ifelse(!!rlang::sym(AESERN) == 1 | !!rlang::sym(AESERN) == "Y" | !!rlang::sym(AESERN) == "Yes" | !!rlang::sym(AESERN) == "YES" | !!rlang::sym(AESERN) == "yes" | !!rlang::sym(AESERN) == "y", 1 ,0) * ifelse(!!rlang::sym(AEACNN) == 1 | !!rlang::sym(AEACNN) == "DRUG WITHDRAWN", 1, 0))
        var_list <- c(var_list, c("studrelresdisc"))
      }
    }

    #select only the required variables for AdEPro
    ae_data <- ae_data %>%
      dplyr::select(all_of(var_list)) %>%
      dplyr::arrange(patient, day_start, day_end) %>%
      dplyr::distinct()

    available_cols <- c(SUBJIDN,AEDECOD,AESTDY,AEENDY,AESEVN,AESERN,AERELN,TRTSDT,TRT01A,TRTSDT,DTHDT,SAFFN,AETRTEMN,AERELPRN,AEACNN,LVDT)[c(SUBJIDN,AEDECOD,AESTDY,AEENDY,AESEVN,AESERN,AERELN,TRTSDT,TRT01A,TRTSDT,DTHDT,SAFFN,AETRTEMN,AERELPRN,AEACNN,LVDT) %in% colnames(prepared_data)]

    pat_data <- prepared_data %>%
      dplyr::relocate(c(ps,treat,end,death)) %>%
      dplyr::select(-one_of(
          available_cols
        ) )%>%
      distinct_at(c("ps","treat","end","death"), .keep_all = TRUE) %>%
      dplyr::arrange(ps)

  return(
    list(
      "ae_data" = as.data.frame(ae_data),
      "pat_data" = as.data.frame(pat_data)
    )
  )
}
