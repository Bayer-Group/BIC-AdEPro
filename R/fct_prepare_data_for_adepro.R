#' prepare_data - read SAS or CSV raw data and prepare the data sets
#' @description
#' Creates a list with two data sets 'pat_data' and 'ae_data' in format which is
#' used in the AdEPro Application
#'
#' @param dat adae data set.
#' @param subjidn Subject Id as numeric (required)
#' @param trt01a Treatment Group as character (required)
#' @param saffn Safety Flag as numeric (required)
#' @param lvdt  Last Visit Date as numeric (required)
#' @param dthdt Death Date as numeric (required)
#' @param trtsdt Treatment Start Date as numeric (required)
#' @param aedecod Adverse Event Code as character (required)
#' @param aestdy Adverse Event Start Date as numeric (required)
#' @param aetrtemn Adverse Event Treatment Emergency Flag as numeric (required)
#' @param aeendy Adverse Event End date as numeric (required)
#' @param aesevn Adverse Event Severity Grade Flag as numeric (required)
#' @param aesern Adverse Event Serious Flag as numeric (optional)
#' @param aereln Adverse Event Related Flag as numeric (optional)
#' @param aerelprn Adverse Event Flag as numeric (optional)
#' @param aeacnn Adverse Event Flag as numeric (optional)
#'
#' @keywords internal
#'

prepare_data_for_adepro <- function(
  dat,
  subjidn = "SUBJIDN",
  trt01a = "TRT01A",
  saffn = "SAFFN",
  lvdt = "LVDT",
  dthdt = "DTHDT",
  trtsdt = "TRTSDT",
  aedecod = "AEDECOD",
  aestdy = "AESTDY",
  aetrtemn = "AETRTEMN",
  aeendy = "AEENDY",
  aesevn = "AESEVN",
  aesern = "AESERN",
  aereln = "AERELN",
  aerelprn = "AERELPRN",
  aeacnn = "AEACNN"
) {

  #check if parameter dat is data.frame
  if (!is.data.frame(dat)) {
    stop("Parameter 'dat' must be a data frame!")
  }

  #create vector with required variable names
  required_vars <- c(subjidn, trt01a, saffn, lvdt, trtsdt, aedecod, aestdy,
                     aetrtemn, aeendy, aesevn)

  #check for required variables in adae data
  if (!all(required_vars %in% colnames(dat))) {
    stop("Required variables are missing in data set 'dat'!")
  }

  #create variables ps, treat, end, death and filter for safety flag
  prepared_data <- filter_prepare_patient_data(
    data = dat, saffn = saffn, lvdt = lvdt, trtsdt = trtsdt, trt01a = trt01a,
    subjidn = subjidn, dthdt = dthdt
  )

  #filter for adverse event data set
  yes_synonyms <- c(1, "Y", "YES", "yes", "Yes", "y")
  no_synonyms <- c(0, "N", "No", "NO", "no", "n", "")

  ae_data <- prepared_data %>%
    dplyr::filter(
      .data[[saffn]] %in% yes_synonyms,
      .data[[aetrtemn]] %in% yes_synonyms,
      !is.na(.data[[aedecod]]) & .data[[aedecod]] != ""
    )

  #create severity flag
  ae_data <- ae_data %>%
    dplyr::mutate(
      new_aesevn = dplyr::case_when(
        .data[[aesevn]] == "MILD" ~ "1",
        .data[[aesevn]] == "MODERATE" ~ "2",
        .data[[aesevn]] == "SEVERE" ~ "3",
        .data[[aesevn]] == "LIFE-THREATENING" ~ "4",
        .data[[aesevn]] == "DEATH" ~ "5",
        TRUE ~ as.character(.data[[aesevn]])
      )
    )

  #create required variables for data set ae_data: day_start, day_end, patient,
  # ae, sev, trtem
  ae_data <- ae_data %>%
    dplyr::mutate(
      day_start = as.numeric(.data[[aestdy]]),
      day_end = as.numeric(.data[[aeendy]]),
      patient = as.numeric(.data[[subjidn]]),
      ae = as.factor(.data[[aedecod]]),
      sev = as.numeric(.data$new_aesevn),
      trtem = ifelse(.data[[aetrtemn]] %in% yes_synonyms, 1, 0)
    )

  #required variables for adverse event data
  var_list <- c("day_start", "day_end", "patient", "ae", "sev", "trtem")

  #create optional variables for adverse event data
  #create ser and nonser from serious flag
  if (!is.null(aesern)) {
    if (aesern %in% colnames(dat)) {
      ae_data <- ae_data %>%
        dplyr::mutate(
          ser = ifelse(.data[[aesern]] %in% yes_synonyms, 1, 0),
          nonser = ifelse(.data[[aesern]] %in% no_synonyms, 1, 0)
        )
      var_list <- c(var_list, c("ser", "nonser"))
    }
  }

  # create variable studrel as drug related flag
  if (!is.null(aereln)) {
    if (aereln %in% colnames(dat)) {
      ae_data <- ae_data %>%
        dplyr::mutate(studrel = ifelse(.data[[aereln]] %in% yes_synonyms, 1, 0))
      var_list <- c(var_list, c("studrel"))
    }
  }

  # create variable studrelser as serious drug related flag
  if (!is.null(aereln) && !is.null(aesern)) {
    if (aereln %in% colnames(dat) && aesern %in% colnames(dat)) {
      ae_data <- ae_data %>%
        dplyr::mutate(
          studrelser = ifelse(.data[[aereln]] %in% yes_synonyms, 1, 0) *
            ifelse(.data[[aesern]] %in% yes_synonyms, 1, 0)
        )
      var_list <- c(var_list, c("studrelser"))
    }
  }


  # create variable relprot as drug related as protocol flag
  if (!is.null(aerelprn)) {
    if (aerelprn %in% colnames(dat)) {
      ae_data <- ae_data %>%
        dplyr::mutate(
          relprot = ifelse(.data[[aerelprn]] %in% yes_synonyms, 1, 0)
        )
      var_list <- c(var_list, c("relprot"))
    }
  }

  if (!is.null(aeacnn)) {
    if (aeacnn %in% colnames(dat)) {
      ae_data <- ae_data %>%
        dplyr::mutate(
          resdisc = ifelse(.data[[aeacnn]] %in% c(1, "DRUG WITHDRAWN"), 1, 0)
        )
      var_list <- c(var_list, c("resdisc"))
    }
  }

  if (!is.null(aereln) && !is.null(aeacnn) && !is.null(aesern)) {
    if (all(c(aereln, aesern, aeacnn) %in% colnames(dat))) {
      ae_data <- ae_data %>%
        dplyr::mutate(
          studrelresdisc = ifelse(.data[[aereln]] %in% yes_synonyms, 1, 0) *
            ifelse(.data[[aesern]] %in% yes_synonyms, 1, 0) *
            ifelse(.data[[aeacnn]] %in% c(1, "DRUG WITHDRAWN"), 1, 0)
        )
      var_list <- c(var_list, c("studrelresdisc"))
    }
  }

  #select only the required variables for AdEPro
  if (paste0(aestdy, "_imputed_flag") %in% colnames(ae_data)) {
    ae_data <- ae_data %>%
      dplyr::mutate(replace_ae_start = .data[[paste0(aestdy, "_imputed_flag")]])
  } else {
    ae_data <- ae_data %>%
      dplyr::mutate(replace_ae_start = 0)
  }

  if (paste0(aeendy, "_imputed_flag") %in% colnames(ae_data)) {
    ae_data <- ae_data %>%
      dplyr::mutate(replace_ae_end = .data[[paste0(aeendy, "_imputed_flag")]])
  } else {
    ae_data <- ae_data %>%
      dplyr::mutate(replace_ae_end = 0)
  }

  ae_data <- ae_data  %>%
    dplyr::select(
      tidyselect::all_of(c(var_list, "replace_ae_start", "replace_ae_end"))
    ) %>%
    dplyr::arrange(.data$patient, .data$day_start, .data$day_end) %>%
    dplyr::distinct()

  # return  variable names from function parameter list which are available in
  # prepared data (to deselect)
  cols_vector <- c(subjidn, aedecod, aestdy, aeendy, aesevn, aesern, aereln,
                   trtsdt, trt01a, dthdt, saffn, aetrtemn, aerelprn,
                   aeacnn, lvdt)

  available_cols <- cols_vector[cols_vector %in% colnames(prepared_data)]

  # order columns and rows, deselect not needed columns and remove duplicated
  # rows
  pat_data <- prepared_data %>%
    dplyr::relocate(c("ps", "treat", "end", "death")) %>%
    dplyr::select(-tidyselect::any_of(available_cols)) %>%
    dplyr::distinct_at(c("ps", "treat", "end", "death"), .keep_all = TRUE) %>%
    dplyr::arrange(.data$ps)

  #return data as list
  return(
    list(
      "ae_data" = as.data.frame(ae_data),
      "pat_data" = as.data.frame(pat_data)
    )
  )
}
