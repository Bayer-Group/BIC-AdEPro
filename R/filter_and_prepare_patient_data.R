
#' filter_and_prepare_patient_data - filter for safety flag and create required variables
#' @description
#' Creates a list with two data sets 'pat_data' and 'ae_data' in format which is used in the AdEPro Application
#'
#' @param data adae (optional merged adsl) data set
#' @param SUBJIDN Subject Id as numeric (required)
#' @param TRT01A Treatment Group as character (required)
#' @param SAFFN Safety Flag as numeric (required)
#' @param LVDT  Last Visit Date as numeric (required)
#' @param DTHDT Death Date as numeric (required)
#' @param TRTSDT Treatment Start Date as numeric (required)
#'
#' @keywords internal
#'

filter_and_prepare_patient_data <- function(data,SAFFN = SAFFN, LVDT = LVDT, TRTSDT = TRTSDT, TRT01A = TRT01A, SUBJIDN = SUBJIDN, DTHDT = DTHDT) {

    #helper function
    is.convertible.to.date <- function(x) !is.na(as.Date(as.character(x), tz = 'UTC', format = '%Y-%m-%d'))

    #filter for safety flag and create ps and treat variable for subject and treatment
    pat_data <- data  %>%
      dplyr::filter(
        !!rlang::sym(SAFFN) == 1 |
        !!rlang::sym(SAFFN) == "Y" |
        !!rlang::sym(SAFFN) == "Yes" |
        !!rlang::sym(SAFFN) == "YES" |
        !!rlang::sym(SAFFN) == "yes" |
        !!rlang::sym(SAFFN) == "y"
      ) %>%
      dplyr::mutate(
        ps = as.numeric(!!rlang::sym(SUBJIDN)),
        treat = as.factor(!!rlang::sym(TRT01A))
      )

    # if death date and treatment start date is a character variable and is convertible to date use as.Date()
    # else use as.numeric()
    #
    if (any(is.convertible.to.date(data[[LVDT]])) & any(is.convertible.to.date(data[[TRTSDT]]))) {
      pat_data <- pat_data  %>%
        dplyr::mutate(
          death = case_when(
            is.na(as.numeric(as.Date(!!rlang::sym(DTHDT)))) ~ 99999,
            !is.na(as.numeric(as.Date(!!rlang::sym(DTHDT)))) ~ as.numeric(as.Date(!!rlang::sym(DTHDT))) - as.numeric(as.Date(!!rlang::sym(TRTSDT))) + 1,
          ),
          end = (as.numeric(as.Date(!!rlang::sym(LVDT))) - as.numeric(as.Date(!!rlang::sym(TRTSDT))) + 1)
        )
    } else {
      pat_data <- pat_data  %>%
        dplyr::mutate(
          death = case_when(
            is.na(as.numeric((!!rlang::sym(DTHDT)))) ~ 99999,
            !is.na(as.numeric((!!rlang::sym(DTHDT)))) ~ as.numeric(!!rlang::sym(DTHDT)) - as.numeric(!!rlang::sym(TRTSDT)) + 1,
          ),
          end = as.numeric(!!rlang::sym(LVDT)) - as.numeric(!!rlang::sym(TRTSDT)) + 1
        )

    }
    return(pat_data)
  }
