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
#' @param optional_vars List optional Variables for the patient data set.
#' @param adsl_data Subject Level data set (optional)
#'
#' @keywords internal
#'

prepare_data <- function(
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

  # corresponds to the original required data structure
  var_list <- c("ps", "treat", "end", "death")

  #filter for safety flag and create required variables: ps, start, end, death
  pat_data <- filter_and_prepare_patient_data(data = dat, SAFFN = SAFFN, LVDT = LVDT, TRTSDT = TRTSDT, TRT01A = TRT01A, SUBJIDN = SUBJIDN, DTHDT = DTHDT)

  #exclude columns with only NA's
  pat_data <- pat_data[colSums(!is.na(pat_data)) > 0]

  # use other columns
  optional_vars <- colnames(pat_data)

  #if adsl data are uploaded
  if (!is.null(adsl_data)) {
    if (SUBJIDN %in% names(adsl_data)) {
      pat_data2 <- filter_and_prepare_patient_data(adsl_data, SAFFN = SAFFN, LVDT = LVDT, TRTSDT = TRTSDT, TRT01A = TRT01A, SUBJIDN = SUBJIDN, DTHDT = DTHDT)

      #deselect variables which are only included in one of the data sets
      pat_data <- pat_data %>%
        dplyr::select(all_of(intersect(colnames(pat_data), colnames(pat_data2))))
      pat_data2 <- pat_data2 %>%
        dplyr::select(all_of(intersect(colnames(pat_data), colnames(pat_data2))))

      diff_subjid <- setdiff(
        pat_data2 %>%
          select(all_of(!!rlang::sym(SUBJIDN))) %>%
          dplyr::pull() %>% as.vector(),
        pat_data %>%
          dplyr::select(all_of(!!rlang::sym(SUBJIDN))) %>%
          dplyr::pull() %>% as.vector()
      )

      jointly_vars <- names(which(unlist(lapply(lapply(pat_data2,class), function(l){l[[1]]})) == unlist(lapply(lapply(pat_data, class),function(l){l[[1]]}))))

      #merge adae and adsl data and inlude subjects from adsl which have no adverse events
      pat_data <- rbind(
        pat_data %>%
              dplyr::select(all_of(jointly_vars)),
        pat_data2 %>%
          dplyr::select(all_of(jointly_vars)) %>%
          dplyr::filter(SUBJIDN %in% diff_subjid)
      )
    } else {
      #if variable name for subject identifier is not included in adsl return NULL
      return(
        list(
          "ae_data" = as.data.frame(NULL, stringsAsFactors = FALSE),
          "pat_data" = as.data.frame(NULL, stringsAsFactors = FALSE)
        )
      )
    }
  }

  if (!is.null(adsl_data)) {
    optional_vars <- colnames(pat_data)
    #optional_vars <- colnames(adsl_data)
  }

  for (i in unique(pat_data$ps)) {
    index <- apply(pat_data[pat_data$ps == i, optional_vars], 2, function(x){length(unique(x))})
    optional_vars <- intersect(optional_vars, names(index[index == 1]))
  }

  grouped_pat_dat <- pat_data %>%
    dplyr::group_by(treat) %>%
    tidyr::nest()

  tmp <- NULL

  for (i in 1:length(grouped_pat_dat$data)){
    tmp <- rbind(tmp, apply(grouped_pat_dat$data[[i]][, optional_vars[-which(optional_vars == "treat")]], 2, function(x){length(unique(x))}))
  }

  tmp2 <- apply(tmp,2,function(x){!all(x == 1)})
  optional_vars <- names(tmp2[tmp2 == TRUE])

  for (i in optional_vars) {
    if (i %in% colnames(dat)) {
      var_list <- c(var_list, i)
    }
  }

  if (SUBJIDN %in% names(adsl_data) | is.null(adsl_data)) {
    pat_data <- pat_data %>%
      dplyr::select(all_of(var_list)) %>%
      dplyr::arrange(treat, ps) %>%
      unique()
  }

  ae_data <- dat %>%
    dplyr::filter(
      (!!rlang::sym(SAFFN) == 1 | !!rlang::sym(SAFFN) == "Y" | !!rlang::sym(SAFFN) == "Yes" | !!rlang::sym(SAFFN) == "YES" | !!rlang::sym(SAFFN) == "yes" | !!rlang::sym(SAFFN) == "y") &
      (!!rlang::sym(AETRTEMN) == 1 | !!rlang::sym(AETRTEMN) == "Y" | !!rlang::sym(AETRTEMN) == "Yes" | !!rlang::sym(AETRTEMN) == "YES" | !!rlang::sym(AETRTEMN) == "yes" | !!rlang::sym(AETRTEMN) == "y") &
      !is.na(!!rlang::sym(AEDECOD)) & !!rlang::sym(AEDECOD) != ""
    )

  ae_data <- ae_data %>%
    dplyr::mutate(new_AESEVN = dplyr::case_when(
      !!rlang::sym(AESEVN) == "MILD" ~ "1",
      !!rlang::sym(AESEVN) == "MODERATE" ~ "2",
      !!rlang::sym(AESEVN)== "SEVERE" ~"3",
      TRUE ~ as.character(!!rlang::sym(AESEVN))
    )
  )

  ae_data <- ae_data %>%
    dplyr::mutate(
      day_start = as.numeric(!!rlang::sym(AESTDY)),
      day_end = as.numeric(!!rlang::sym(AEENDY)),
      patient = as.numeric(!!rlang::sym(SUBJIDN)),
      ae = as.factor(!!rlang::sym(AEDECOD)),
      sev = as.numeric(new_AESEVN),
      trtem = ifelse((!!rlang::sym(AETRTEMN) == 1 | !!rlang::sym(AETRTEMN) == "Y" | !!rlang::sym(AETRTEMN) == "Yes" | !!rlang::sym(AETRTEMN) == "YES" | !!rlang::sym(AETRTEMN) == "yes" | !!rlang::sym(AETRTEMN) == "y"), 1, 0))
  #required variables for adverse event data
  var_list <- c("day_start", "day_end", "patient", "ae", "sev","trtem")

    #optional variables for adverse event data
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

    if(!is.null(AERELN)) {
      if (AERELN %in% colnames(dat)) {
        ae_data <- ae_data %>%
          dplyr::mutate(studrel = ifelse(!!rlang::sym(AERELN) == 1 | !!rlang::sym(AERELN) == "Y" | !!rlang::sym(AERELN) == "Yes" | !!rlang::sym(AERELN) == "YES" | !!rlang::sym(AERELN) == "yes" | !!rlang::sym(AERELN) == "y", 1, 0))
        var_list <- c(var_list, c("studrel"))
      }
    }

    if(!is.null(AERELN) & !is.null(AESERN)) {
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
      dplyr::arrange(patient, day_start, day_end)

    pat_data <- pat_data %>%
      dplyr::relocate(c(ps,treat,end,death))

  return(
    list(
      "ae_data" = as.data.frame(ae_data),
      "pat_data" = as.data.frame(pat_data)
    )
  )
}

