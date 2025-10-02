#' Filter for Safety Flag and Create Required Variables
#'
#' @param data frame with adverse event data set
#' @param SUBJIDN Subject Id as numeric (required)
#' @param TRT01A Treatment Group as character (required)
#' @param SAFFN Safety Flag as numeric (required)
#' @param LVDT  Last Visit Date as numeric (required)
#' @param DTHDT Death Date as numeric (required)
#' @param TRTSDT Treatment Start Date as numeric (required)
#'
#' @return data frame with filtered adverse event data
#'

filter_and_prepare_patient_data <- function(data, SAFFN = SAFFN, LVDT = LVDT, TRTSDT = TRTSDT, TRT01A = TRT01A, SUBJIDN = SUBJIDN, DTHDT = DTHDT) {

    #check if parameter dat is data.frame
    if (!is.data.frame(data)) {
      stop("Parameter 'data' must be a data frame!")
    }

    #check if required parameter are in data
    if (!SAFFN %in% colnames(data)) {stop("Parameter 'SAFFN' must be in data frame!")}
    if (!LVDT %in% colnames(data)) {stop("Parameter 'LVDT' must be in data frame!")}
    if (!TRTSDT %in% colnames(data)) {stop("Parameter 'TRTSDT' must be in data frame!")}
    if (!TRT01A %in% colnames(data)) {stop("Parameter 'TRT01A' must be in data frame!")}
    if (!SUBJIDN %in% colnames(data)) {stop("Parameter 'SUBJIDN' must be in data frame!")}
    # if (!DTHDT %in% colnames(data)) {stop("Parameter 'DTHDT' must be in data frame!")}

    #helper function
    is.convertible.to.date <- function(x) !is.na(as.Date(anytime::anydate(as.character(x))))

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
    if (!any(is.numeric(data[[LVDT]])) & any(is.convertible.to.date(data[[LVDT]])) & any(is.convertible.to.date(data[[TRTSDT]]))) {
      pat_data <- pat_data  %>%
        dplyr::mutate(
          death = case_when(
            is.na(as.numeric(as.Date(anytime::anydate(!!rlang::sym(DTHDT))))) ~ 99999,
            !is.na(as.numeric(as.Date(anytime::anydate(!!rlang::sym(DTHDT))))) ~ as.numeric(as.Date(anytime::anydate(!!rlang::sym(DTHDT)))) - as.numeric(as.Date(anytime::anydate(!!rlang::sym(TRTSDT)))) + 1,
          ),
          end = (as.numeric(as.Date(anytime::anydate(!!rlang::sym(LVDT)))) - as.numeric(as.Date(anytime::anydate(!!rlang::sym(TRTSDT)))) + 1)
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
