#' Checks if column name exists, throw an error otherwise
#' @param colname A character, the column name to check
#' @param data A data object

check_colname_present <- function(colname, data) {
  if (!colname %in% colnames(data)) {
    stop(
      paste0("Parameter '", toupper(colname), "' must be in data frame!")
    )
  }
}

#' Checks if variable can be converted to date
#'
#' Returns TRUE if at least a value can be parsed to date with anytime::anydate
#'
#' @param variable A vector, the variable
#'
is_convertible_to_date <- function(variable) {
  # anytime::anydate throws a warning each time it results in a NA value
  suppressWarnings(
    any(!is.na(as.Date(anytime::anydate(as.character(variable)))))
  )
}

#' Obtains date as number
#' @param variable A vector, the variable
#'
as_numeric_date <- function(variable) {
  # anytime::anydate throws a warning each time it results in a NA value
  suppressWarnings(
    as.numeric(as.Date(anytime::anydate(variable)))
  )
}

#' Filter for Safety Flag and Create Required Variables
#'
#' @param data frame with adverse event data set
#' @param subjidn Subject Id as numeric (required)
#' @param trt01a Treatment Group as character (required)
#' @param saffn Safety Flag as numeric (required)
#' @param lvdt  Last Visit Date as numeric (required)
#' @param dthdt Death Date as numeric (required)
#' @param trtsdt Treatment Start Date as numeric (required)
#'
#' @return data frame with filtered adverse event data

filter_prepare_patient_data <- function(
  data,
  saffn,
  lvdt,
  trtsdt,
  trt01a,
  subjidn,
  dthdt
) {
  #check if parameter dat is data.frame
  if (!is.data.frame(data)) {
    stop("Parameter 'data' must be a data frame!")
  }

  #check if required parameter are in data
  check_colname_present(saffn, data)
  check_colname_present(lvdt, data)
  check_colname_present(trtsdt, data)
  check_colname_present(trt01a, data)
  check_colname_present(subjidn, data)

  # filter for safety flag and create ps and treat variable for subject and
  # treatment
  yes_synonyms <- c(1, "Y", "Yes", "YES", "yes", "y")
  pat_data <- data  %>%
    dplyr::filter(.data[[saffn]] %in% yes_synonyms) %>%
    dplyr::mutate(
      ps = as.numeric(.data[[subjidn]]),
      treat = as.factor(.data[[trt01a]])
    )

  # if there is no date death present in the data, add a missing column
  if (!dthdt %in% colnames(data)) {
    pat_data <- pat_data %>%
      dplyr::mutate("{dthdt}" := NA_character_)
  }

  # if death date and treatment start date is a character variable and is
  # convertible to date use as.Date()
  # else use as.numeric()
  if (!any(is.numeric(data[[lvdt]])) && is_convertible_to_date(data[[lvdt]])
      && is_convertible_to_date(data[[trtsdt]])) {
    pat_data <- pat_data  %>%
      dplyr::mutate(
        death = dplyr::case_when(
          is.na(as_numeric_date(.data[[dthdt]])) ~ 99999,
          !is.na(as_numeric_date(.data[[dthdt]])) ~
            as_numeric_date(.data[[dthdt]]) -
              as_numeric_date(.data[[trtsdt]]) + 1,
        ),
        end = as_numeric_date(.data[[lvdt]]) -
          as_numeric_date(.data[[trtsdt]]) + 1
      )
  } else {
    pat_data <- pat_data  %>%
      dplyr::mutate(
        death = dplyr::case_when(
          is.na(as.numeric((.data[[dthdt]]))) ~ 99999,
          !is.na(as.numeric((.data[[dthdt]]))) ~
            as.numeric(.data[[dthdt]]) - as.numeric(.data[[trtsdt]]) + 1,
        ),
        end = as.numeric(.data[[lvdt]]) - as.numeric(.data[[trtsdt]]) + 1
      )
  }
  pat_data
}
