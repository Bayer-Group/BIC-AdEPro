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
#' @param AERELPRN Adverse Event Flag as numeric (optional)
#' @param AEACNN Adverse Event Flag as numeric (optional)
#'
#' @keywords internal
#'

prepare_data_for_adepro <- function(
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
  AEACNN = "AEACNN"
) {

  #check if parameter dat is data.frame
  if (!is.data.frame(dat)) {
    stop("Parameter 'dat' must be a data frame!")
  }

  #create vector with required variable names
  required_vars <- c(SUBJIDN, TRT01A, SAFFN ,LVDT, TRTSDT, AEDECOD, AESTDY, AETRTEMN, AEENDY, AESEVN)

  #check for required variables in adae data
  if (!all(required_vars %in% colnames(dat))) {
    stop("Required variables are missing in data set 'dat'!")
  }

  #create variables ps, treat, end, death and filter for safety flag
  prepared_data <- filter_and_prepare_patient_data(data = dat, SAFFN = SAFFN, LVDT = LVDT, TRTSDT = TRTSDT, TRT01A = TRT01A, SUBJIDN = SUBJIDN, DTHDT = DTHDT)

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
      !!rlang::sym(AESEVN)== "LIFE-THREATENING" ~"4",
      !!rlang::sym(AESEVN)== "DEATH" ~"5",
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

  # create variable studrel as drug related flag
  if (!is.null(AERELN)) {
    if (AERELN %in% colnames(dat)) {
      ae_data <- ae_data %>%
        dplyr::mutate(studrel = ifelse(!!rlang::sym(AERELN) == 1 | !!rlang::sym(AERELN) == "Y" | !!rlang::sym(AERELN) == "Yes" | !!rlang::sym(AERELN) == "YES" | !!rlang::sym(AERELN) == "yes" | !!rlang::sym(AERELN) == "y", 1, 0))
      var_list <- c(var_list, c("studrel"))
    }
  }

    # create variable studrelser as serious drug related flag
  if (!is.null(AERELN) & !is.null(AESERN)) {
    if (AERELN %in% colnames(dat) & AESERN %in% colnames(dat)) {
      ae_data <- ae_data %>%
        dplyr::mutate(studrelser = ifelse(!!rlang::sym(AERELN) == 1 | !!rlang::sym(AERELN) == "Y" | !!rlang::sym(AERELN) == "Yes" | !!rlang::sym(AERELN) == "YES" | !!rlang::sym(AERELN) == "yes" | !!rlang::sym(AERELN) == "y", 1, 0) * ifelse(!!rlang::sym(AESERN) == 1 | !!rlang::sym(AESERN) == "Y" | !!rlang::sym(AESERN) == "Yes" | !!rlang::sym(AESERN) == "YES" | !!rlang::sym(AESERN) == "yes" | !!rlang::sym(AESERN) == "y", 1, 0))
      var_list <- c(var_list, c("studrelser"))
    }
  }


  # create variable relprot as drug related as protocol flag
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
  if (paste0(AESTDY,"_imputed_flag") %in% colnames(ae_data)) {
    ae_data <- ae_data %>%
      dplyr::mutate(replace_ae_start = !!rlang::sym(paste0(AESTDY,"_imputed_flag")))
  } else {
    ae_data <- ae_data %>%
      dplyr::mutate(replace_ae_start = 0)
  }

  if (paste0(AEENDY,"_imputed_flag") %in% colnames(ae_data)) {
    ae_data <- ae_data %>%
      dplyr::mutate(replace_ae_end = !!rlang::sym(paste0(AEENDY,"_imputed_flag")))
  } else {
    ae_data <- ae_data %>%
      dplyr::mutate(replace_ae_end = 0)
  }

  ae_data <- ae_data  %>%
    dplyr::select(all_of(c(var_list,"replace_ae_start","replace_ae_end"))) %>%
    dplyr::arrange(patient, day_start, day_end) %>%
    dplyr::distinct()

  #return  variable names from function parameter list which are available in prepared data (to deselect)
  available_cols <- c(SUBJIDN,AEDECOD,AESTDY,AEENDY,AESEVN,AESERN,AERELN,TRTSDT,TRT01A,TRTSDT,DTHDT,SAFFN,AETRTEMN,AERELPRN,AEACNN,LVDT)[c(SUBJIDN,AEDECOD,AESTDY,AEENDY,AESEVN,AESERN,AERELN,TRTSDT,TRT01A,TRTSDT,DTHDT,SAFFN,AETRTEMN,AERELPRN,AEACNN,LVDT) %in% colnames(prepared_data)]

  # order columns and rows, deselect not needed columns and remove duplicated rows
  pat_data <- prepared_data %>%
    dplyr::relocate(c(ps,treat,end,death)) %>%
    dplyr::select(-one_of(available_cols)) %>%
    distinct_at(c("ps", "treat", "end", "death"), .keep_all = TRUE) %>%
    dplyr::arrange(ps)

  #return data as list
  return(
    list(
      "ae_data" = as.data.frame(ae_data),
      "pat_data" = as.data.frame(pat_data)
    )
  )
}
