#' Impute missing values in adverse event data and return number of imputed data
#'
#' @param data frame with adverse event data set
#' @param LVDT  Last Visit Date as numeric (required)
#' @param DTHDT Death Date as numeric (required)
#' @param TRTSDT Treatment Start Date as numeric (required)
#' @param AEDECOD Adverse Event Code as character (required)
#' @param AESTDY Adverse Event Start Date as numeric (required)
#' @param AETRTEMN Adverse Event Treatment Emergency Flag as numeric (required)
#' @param AEENDY Adverse Event End date as numeric (required)
#' @param AESEVN Adverse Event Severity Grade Flag as numeric (required)
#' @param severity_grading_flag character value if 3 or 5 step grading is used
#'
#' @return data frame with imputed adverse event data
#'

calculate_and_impute_required_variables_missing_values <- function(
  data,
  SUBJIDN = "SUBJIDN",
  LVDT = "LVDT",
  DTHDT = "DTHDT",
  TRTSDT = "TRTSDT",
  AEDECOD = "AEDECOD",
  AESTDY = "AESTDY",
  AETRTEMN = "AETRTEMN",
  AEENDY = "AEENDY",
  AESEVN = "AESEVN",
  severity_grading_flag
) {


  #check if parameter dat is data.frame
  if (!is.data.frame(data)) {
    stop("Parameter 'data' must be a data frame!")
  }

  #check if required parameter are in data
  if (!LVDT %in% colnames(data)) {stop("Parameter 'LVDT' must be in data frame!")}
  if (!TRTSDT %in% colnames(data)) {stop("Parameter 'TRTSDT' must be in data frame!")}
  if (!AEDECOD %in% colnames(data)) {stop("Parameter 'AEDECOD' must be in data frame!")}
  if (!AESTDY %in% colnames(data)) {stop("Parameter 'AESTDY' must be in data frame!")}
  if (!AEENDY %in% colnames(data)) {stop("Parameter 'AEENDY' must be in data frame!")}
  if (!AETRTEMN %in% colnames(data)) {stop("Parameter 'AETRTEMN' must be in data frame!")}
  if (!AESEVN %in% colnames(data)) {stop("Parameter 'AESEVN' must be in data frame!")}
  if (!severity_grading_flag %in% c("Severity","Grading")) {stop("Parameter 'severity_grading_flag' must be either character 'Severity' or 'Grading'!")}

  #filter for aes with adverse event end day after equal day 1
  if (!is.null(AEENDY)) {
    data <- data %>%
      dplyr::filter(is.na(AEENDY) | AEENDY >= 1)
  }

  #remove missing subject ids
  if (!is.null(SUBJIDN)) {
    data <- data %>%
      dplyr::filter(!is.na(SUBJIDN))
  }

  ## check for missing values in required variables:
  #1. Get the number of unknown Adverse Events:
  if (!is.null(AEDECOD) & !is.null(AESTDY)  & !is.null(AEENDY)) {
      number_unknown_aes <- calculate_number_unknown_aes(dat = data, sel_aedecod = AEDECOD, sel_aestdy = AESTDY, sel_aeendy = AEENDY)
  } else {
    number_unknown_aes <- 0
  }
  #1b.impute unknown Adverse Events with character "Unknown type of AE" if start and end day are not missing
  #replace_number_unknown_aes()
  if (number_unknown_aes > 0) {
    if (AEDECOD %in% colnames(data)) {
      data <- data %>%
        dplyr::mutate(!!rlang::sym(paste0(AEDECOD,"_raw")):= !!rlang::sym(AEDECOD)) %>%
        dplyr::mutate(!!rlang::sym(AEDECOD):= ifelse((is.na(!!rlang::sym(AEDECOD)) | !!rlang::sym(AEDECOD) == "" ) & (!is.na(!!rlang::sym(AESTDY)) | !is.na(!!rlang::sym(AEENDY)) ),"Unknown type of AE",!!rlang::sym(AEDECOD))) %>%
        dplyr::mutate(!!rlang::sym(paste0(AEDECOD,"_imputed_flag")):= dplyr::case_when(
          is.na(!!rlang::sym(paste0(AEDECOD,"_raw"))) & !is.na(!!rlang::sym(AEDECOD)) ~ 1,
          !is.na(!!rlang::sym(paste0(AEDECOD,"_raw"))) & is.na(!!rlang::sym(AEDECOD)) ~ 1,
          is.na(!!rlang::sym(paste0(AEDECOD,"_raw"))) & is.na(!!rlang::sym(AEDECOD)) ~ 0,
          !is.na(!!rlang::sym(paste0(AEDECOD,"_raw"))) & !is.na(!!rlang::sym(AEDECOD)) & !!rlang::sym(paste0(AEDECOD,"_raw")) != !!rlang::sym(AEDECOD) ~ 1,
          !is.na(!!rlang::sym(paste0(AEDECOD,"_raw"))) & !is.na(!!rlang::sym(AEDECOD)) & !!rlang::sym(paste0(AEDECOD,"_raw")) == !!rlang::sym(AEDECOD) ~ 0,
          )
        )
       #check if calculated number is the same as mutated number
        if(number_unknown_aes != sum(data[[AEDECOD]]=="Unknown type of AE",na.rm=TRUE)) stop()
    }
  }

  #2. Get the number of missing Severity Grades:
  if (!is.null(AESEVN)) {
    if (!is.null(data)) {
      if (!is.null(severity_grading_flag)) {
        number_severe_missing <- calculate_number_missing_severity_flag(dat = data, sel_aesevn = AESEVN, sel_aedecod = AEDECOD, severity_grading_flag = severity_grading_flag)
      }
    }
  } else {
    number_severe_missing <- 0
  }

  if (number_severe_missing > 0) {
    #2b.Replace missing severity grades with character SEVERE
    if (!is.null(AESEVN)) {
      if (is.numeric(data[[AESEVN]])) {
        if (severity_grading_flag == "Severity") {
          data <- data %>%
            dplyr::mutate(!!rlang::sym(paste0(AESEVN,"_raw")):= !!rlang::sym(AESEVN)) %>%
            # dplyr::mutate(!!rlang::sym(AESEVN):= ifelse((!is.na(!!rlang::sym(AESEVN)) & !!rlang::sym(AESEVN)%in%c(1,2,3)),!!rlang::sym(AESEVN),3)) %>%
            dplyr::mutate(!!rlang::sym(AESEVN):= dplyr::case_when(
              is.na(!!rlang::sym(AESEVN)) & !is.na(!!rlang::sym(AEDECOD)) ~ 3,
              is.na(!!rlang::sym(AESEVN)) & is.na(!!rlang::sym(AEDECOD)) ~ !!rlang::sym(AESEVN),
              !!rlang::sym(AESEVN) %in% c(1,2,3) & !is.na(!!rlang::sym(AESEVN)) ~ !!rlang::sym(AESEVN),
              !(!!rlang::sym(AESEVN) %in% c(1,2,3)) & !is.na(!!rlang::sym(AESEVN)) ~ 3
              )
            ) %>%
            dplyr::mutate(!!rlang::sym(paste0(AESEVN,"_imputed_flag")):= dplyr::case_when(
              is.na(!!rlang::sym(paste0(AESEVN,"_raw"))) & !is.na(!!rlang::sym(AESEVN))  ~ 1,
              !is.na(!!rlang::sym(paste0(AESEVN,"_raw"))) & is.na(!!rlang::sym(AESEVN)) ~ 1,
              is.na(!!rlang::sym(paste0(AESEVN,"_raw"))) & is.na(!!rlang::sym(AESEVN)) ~ 0,
              !is.na(!!rlang::sym(paste0(AESEVN,"_raw"))) & !is.na(!!rlang::sym(AESEVN)) & !!rlang::sym(paste0(AESEVN,"_raw")) != !!rlang::sym(AESEVN) ~ 1,
              !is.na(!!rlang::sym(paste0(AESEVN,"_raw"))) & !is.na(!!rlang::sym(AESEVN)) & !!rlang::sym(paste0(AESEVN,"_raw")) == !!rlang::sym(AESEVN) ~ 0,
              )
            )
        } else {
          data <- data %>%
            dplyr::mutate(!!rlang::sym(paste0(AESEVN,"_raw")):= !!rlang::sym(AESEVN)) %>%
            #dplyr::mutate(!!rlang::sym(AESEVN):= ifelse((!is.na(!!rlang::sym(AESEVN)) & !!rlang::sym(AESEVN)%in%c(1,2,3,4,5)) & !is.na(AEDECOD),!!rlang::sym(AESEVN),3)) %>%
            dplyr::mutate(!!rlang::sym(AESEVN):= dplyr::case_when(
              is.na(!!rlang::sym(AESEVN)) & !is.na(!!rlang::sym(AEDECOD)) ~ 3,
              is.na(!!rlang::sym(AESEVN)) & is.na(!!rlang::sym(AEDECOD)) ~ !!rlang::sym(AESEVN),
              !!rlang::sym(AESEVN) %in% c(1,2,3,4,5) & !is.na(!!rlang::sym(AESEVN)) ~ !!rlang::sym(AESEVN),
              !(!!rlang::sym(AESEVN) %in% c(1,2,3,4,5)) & !is.na(!!rlang::sym(AESEVN)) ~ 3
              )
            ) %>%
            dplyr::mutate(!!rlang::sym(paste0(AESEVN,"_imputed_flag")):= dplyr::case_when(
              is.na(!!rlang::sym(paste0(AESEVN,"_raw"))) & !is.na(!!rlang::sym(AESEVN)) ~ 1,
              !is.na(!!rlang::sym(paste0(AESEVN,"_raw"))) & is.na(!!rlang::sym(AESEVN)) ~ 1,
              is.na(!!rlang::sym(paste0(AESEVN,"_raw"))) & is.na(!!rlang::sym(AESEVN)) ~ 0,
              !is.na(!!rlang::sym(paste0(AESEVN,"_raw"))) & !is.na(!!rlang::sym(AESEVN)) & !!rlang::sym(paste0(AESEVN,"_raw")) != !!rlang::sym(AESEVN) ~ 1,
              !is.na(!!rlang::sym(paste0(AESEVN,"_raw"))) & !is.na(!!rlang::sym(AESEVN)) & !!rlang::sym(paste0(AESEVN,"_raw")) == !!rlang::sym(AESEVN) ~ 0,
              )
            )
        }
      } else if (is.character(data[[AESEVN]])) {
         if (severity_grading_flag == "Severity") {
          data <- data %>%
            dplyr::mutate(!!rlang::sym(paste0(AESEVN,"_raw")):= !!rlang::sym(AESEVN)) %>%
            # dplyr::mutate(!!rlang::sym(AESEVN):= ifelse((!is.na(!!rlang::sym(AESEVN)) & !!rlang::sym(AESEVN)%in%c("MILD","MODERATE","SEVERE")),!!rlang::sym(AESEVN),"SEVERE")) %>%
            dplyr::mutate(!!rlang::sym(AESEVN):= dplyr::case_when(
              is.na(!!rlang::sym(AESEVN)) & !is.na(!!rlang::sym(AEDECOD)) ~ "SEVERE",
              is.na(!!rlang::sym(AESEVN)) & is.na(!!rlang::sym(AEDECOD)) ~ !!rlang::sym(AESEVN),
              !!rlang::sym(AESEVN) %in% c("MILD","MODERATE","SEVERE") & !is.na(!!rlang::sym(AESEVN)) ~ !!rlang::sym(AESEVN),
              !(!!rlang::sym(AESEVN) %in% c("MILD","MODERATE","SEVERE")) & !is.na(!!rlang::sym(AESEVN)) ~ "SEVERE"
              )
            ) %>%
             dplyr::mutate(!!rlang::sym(paste0(AESEVN,"_imputed_flag")):= dplyr::case_when(
              is.na(!!rlang::sym(paste0(AESEVN,"_raw"))) & !is.na(!!rlang::sym(AESEVN)) ~ 1,
              !is.na(!!rlang::sym(paste0(AESEVN,"_raw"))) & is.na(!!rlang::sym(AESEVN)) ~ 1,
              is.na(!!rlang::sym(paste0(AESEVN,"_raw"))) & is.na(!!rlang::sym(AESEVN)) ~ 0,
              !is.na(!!rlang::sym(paste0(AESEVN,"_raw"))) & !is.na(!!rlang::sym(AESEVN)) & !!rlang::sym(paste0(AESEVN,"_raw")) != !!rlang::sym(AESEVN) ~ 1,
              !is.na(!!rlang::sym(paste0(AESEVN,"_raw"))) & !is.na(!!rlang::sym(AESEVN)) & !!rlang::sym(paste0(AESEVN,"_raw")) == !!rlang::sym(AESEVN) ~ 0,
              )
            )
        } else {
          data <- data %>%
            dplyr::mutate(!!rlang::sym(paste0(AESEVN,"_raw")):= !!rlang::sym(AESEVN)) %>%
            #dplyr::mutate(!!rlang::sym(AESEVN):= ifelse((!is.na(!!rlang::sym(AESEVN)) & !!rlang::sym(AESEVN)%in%c("MILD","MODERATE","SEVERE","LIFE-THREATENING","DEATH")),!!rlang::sym(AESEVN),"SEVERE")) %>%
               dplyr::mutate(!!rlang::sym(AESEVN):= dplyr::case_when(
              is.na(!!rlang::sym(AESEVN)) & !is.na(!!rlang::sym(AEDECOD)) ~ "SEVERE",
              is.na(!!rlang::sym(AESEVN)) & is.na(!!rlang::sym(AEDECOD)) ~ !!rlang::sym(AESEVN),
              !!rlang::sym(AESEVN) %in% c("MILD","MODERATE","SEVERE","LIFE-THREATENING","DEATH") & !is.na(!!rlang::sym(AESEVN)) ~ !!rlang::sym(AESEVN),
              !(!!rlang::sym(AESEVN) %in% c("MILD","MODERATE","SEVERE","LIFE-THREATENING","DEATH")) & !is.na(!!rlang::sym(AESEVN)) ~ "SEVERE"
              )
            ) %>%
            dplyr::mutate(!!rlang::sym(paste0(AESEVN,"_imputed_flag")):= dplyr::case_when(
              is.na(!!rlang::sym(paste0(AESEVN,"_raw"))) & !is.na(!!rlang::sym(AESEVN)) ~ 1,
              !is.na(!!rlang::sym(paste0(AESEVN,"_raw"))) & is.na(!!rlang::sym(AESEVN)) ~ 1,
              is.na(!!rlang::sym(paste0(AESEVN,"_raw"))) & is.na(!!rlang::sym(AESEVN)) ~ 0,
              !is.na(!!rlang::sym(paste0(AESEVN,"_raw"))) & !is.na(!!rlang::sym(AESEVN)) & !!rlang::sym(paste0(AESEVN,"_raw")) != !!rlang::sym(AESEVN) ~ 1,
              !is.na(!!rlang::sym(paste0(AESEVN,"_raw"))) & !is.na(!!rlang::sym(AESEVN)) & !!rlang::sym(paste0(AESEVN,"_raw")) == !!rlang::sym(AESEVN) ~ 0,
              )
            )
        }
      }
    }
  }
  # #3.Get the number of adverse events removed due to missing treatment emergency flag
  # if (!is.null(input$sel_aedecod) & !is.null(input$sel_aetrtemn)) {
  #
  #   aes_removed_since_treatment_emergent <- calculate_number_aes_not_treatment_emergent(dat = data, sel_aedecod = input$sel_aedecod, sel_aetrtemn = input$sel_aetrtemn)
  #
  #
  #4.Get the number of adverse events start day missing
  if (!is.null(AESTDY)) {
    number_ae_start_missing <- calculate_number_ae_start_missing(dat = data, sel_aestdy = AESTDY, sel_aedecod = AEDECOD, sel_aeendy = AEENDY)
  } else {
    number_ae_start_missing <- 0
  }

  if (number_ae_start_missing > 0) {
    #4b.Replace
    if (AESTDY %in% colnames(data)) {
      # data <- data %>%
      #   dplyr::mutate(!!rlang::sym(AESTDY):= ifelse((is.na(!!rlang::sym(AESTDY))) & !(is.na(!!rlang::sym(AEDECOD)) & is.na(!!rlang::sym(AEENDY))), 1,!!rlang::sym(AESTDY)))

      data <- data %>%
        dplyr::mutate(!!rlang::sym(paste0(AESTDY,"_raw")):= !!rlang::sym(AESTDY)) %>%
        dplyr::mutate(!!rlang::sym(AESTDY):= ifelse((is.na(!!rlang::sym(AESTDY))) & !(is.na(!!rlang::sym(AEDECOD)) & is.na(!!rlang::sym(AEENDY))), 1,!!rlang::sym(AESTDY))) %>%
        dplyr::mutate(!!rlang::sym(paste0(AESTDY,"_imputed_flag")):= dplyr::case_when(
          is.na(!!rlang::sym(paste0(AESTDY,"_raw"))) & !is.na(!!rlang::sym(AESTDY)) ~ 1,
          !is.na(!!rlang::sym(paste0(AESTDY,"_raw"))) & is.na(!!rlang::sym(AESTDY)) ~ 1,
          is.na(!!rlang::sym(paste0(AESTDY,"_raw"))) & is.na(!!rlang::sym(AESTDY)) ~ 0,
          !is.na(!!rlang::sym(paste0(AESTDY,"_raw"))) & !is.na(!!rlang::sym(AESTDY)) & !!rlang::sym(paste0(AESTDY,"_raw")) != !!rlang::sym(AESTDY) ~ 1,
          !is.na(!!rlang::sym(paste0(AESTDY,"_raw"))) & !is.na(!!rlang::sym(AESTDY)) & !!rlang::sym(paste0(AESTDY,"_raw")) == !!rlang::sym(AESTDY) ~ 0,
          )
        )
    }
  }

  #5.Get the number of adverse events end day missing
    if (!is.null(AEENDY) & !is.null(AESTDY) & !is.null(AEDECOD) ) {
       #number_ae_end_missing <- calculate_number_ae_end_missing(dat = adae_data, sel_aeendy = AEENDY)
       number_ae_end_missing <- calculate_number_ae_end_missing(dat = data, sel_aeendy = AEENDY, sel_aedecod = AEDECOD, sel_aestdy = AESTDY)
    } else {
      number_ae_end_missing <- 0
    }
  if (number_ae_end_missing > 0) {
    if(!is.null(AESTDY) & !is.null(LVDT) & !is.null(AEENDY) & !is.null(TRTSDT) & !is.null(DTHDT)){
     if (AESTDY %in% colnames(data) & LVDT %in% colnames(data) & AEENDY %in% colnames(data) & TRTSDT %in% colnames(data)) {

      #calculate last study day
       ## as.Date
      last_study_day <- data %>%
        dplyr::mutate(
          end1 = as.numeric(as.numeric(as.Date(anytime::anydate(!!rlang::sym(LVDT))) - as.Date(anytime::anydate(!!rlang::sym(TRTSDT))))+ 1),
          end2 = as.numeric(as.Date(anytime::anydate(!!rlang::sym(DTHDT))) - as.Date(anytime::anydate(!!rlang::sym(TRTSDT))) + 1),
          end3 = !!rlang::sym(AEENDY)) %>%
        dplyr::mutate(end = max(end1,end2,end3,na.rm=TRUE)) %>%
        dplyr::pull(end) %>%
        unique()

      # data <- data %>%
      #   dplyr::mutate(!!rlang::sym(AEENDY):= ifelse((is.na(!!rlang::sym(AEENDY))) & !(is.na(!!rlang::sym(AEDECOD)) & is.na(!!rlang::sym(AESTDY))), last_study_day ,!!rlang::sym(AEENDY)))
      #
      data <- data %>%
        dplyr::mutate(!!rlang::sym(paste0(AEENDY,"_raw")):= !!rlang::sym(AEENDY)) %>%
        dplyr::mutate(!!rlang::sym(AEENDY):= ifelse((is.na(!!rlang::sym(AEENDY))) & !(is.na(!!rlang::sym(AEDECOD)) & is.na(!!rlang::sym(AESTDY))), last_study_day ,!!rlang::sym(AEENDY)))%>%
        dplyr::mutate(!!rlang::sym(paste0(AEENDY,"_imputed_flag")):= dplyr::case_when(
          is.na(!!rlang::sym(paste0(AEENDY,"_raw"))) & !is.na(!!rlang::sym(AEENDY)) ~ 1,
          !is.na(!!rlang::sym(paste0(AEENDY,"_raw"))) & is.na(!!rlang::sym(AEENDY)) ~ 1,
          is.na(!!rlang::sym(paste0(AEENDY,"_raw"))) & is.na(!!rlang::sym(AEENDY)) ~ 0,
          !is.na(!!rlang::sym(paste0(AEENDY,"_raw"))) & !is.na(!!rlang::sym(AEENDY)) & !!rlang::sym(paste0(AEENDY,"_raw")) != !!rlang::sym(AEENDY) ~ 1,
          !is.na(!!rlang::sym(paste0(AEENDY,"_raw"))) & !is.na(!!rlang::sym(AEENDY)) & !!rlang::sym(paste0(AEENDY,"_raw")) == !!rlang::sym(AEENDY) ~ 0,
          )
        )
      }
    }
  }

  #6.Get the number of missing last visit dates
  if (!is.null(LVDT)) {
    number_missing_lvdt <- calculate_number_missing_last_visit_dates(dat = data, sel_lvdt = LVDT, sel_subjidn = SUBJIDN)
  } else{
    number_missing_lvdt <- 0
  }

  if (number_missing_lvdt  > 0) {


    last_date <- data %>% dplyr::mutate(LVDT_ = case_when(is.na(!!rlang::sym(LVDT)) ~ NA, !is.na(!!rlang::sym(LVDT)) ~ anytime::anydate(!!rlang::sym(LVDT)))) %>% dplyr::pull(LVDT_) %>% sort() %>% tail() %>% unique()
    #last_date <- tail(sort(anytime::anydate(data[LVDT]),na.rm = TRUE), n = 1)

      data <- data %>%
        dplyr::mutate(!!rlang::sym(paste0(LVDT,"_raw")):= case_when(is.na(!!rlang::sym(LVDT)) ~ NA, !is.na(LVDT) ~ anytime::anydate(!!rlang::sym(LVDT)))) %>%
        dplyr::mutate(!!rlang::sym(LVDT):=
                        case_when(is.na(!!rlang::sym(LVDT)) ~ last_date, !is.na(!!rlang::sym(LVDT)) ~ anytime::anydate(!!rlang::sym(LVDT)))) %>%
        dplyr::mutate(!!rlang::sym(paste0(LVDT,"_imputed_flag")):= dplyr::case_when(
          is.na(!!rlang::sym(paste0(LVDT,"_raw"))) & !is.na(!!rlang::sym(LVDT)) ~ 1,
          !is.na(!!rlang::sym(paste0(LVDT,"_raw"))) & is.na(!!rlang::sym(LVDT)) ~ 1,
          is.na(!!rlang::sym(paste0(LVDT,"_raw"))) & is.na(!!rlang::sym(LVDT)) ~ 0,
          !is.na(!!rlang::sym(paste0(LVDT,"_raw"))) & !is.na(!!rlang::sym(LVDT)) & !!rlang::sym(paste0(LVDT,"_raw")) != !!rlang::sym(LVDT) ~ 1,
          !is.na(!!rlang::sym(paste0(LVDT,"_raw"))) & !is.na(!!rlang::sym(LVDT)) & !!rlang::sym(paste0(LVDT,"_raw")) == !!rlang::sym(LVDT) ~ 0,
          )
        )
  }

  #7.Get the number of cases when adverse event start day is after the adverse event end day
  if (!is.null(AESTDY) & !is.null(AEENDY)) {
    if (AESTDY %in% colnames(data) & AEENDY %in% colnames(data)) {
      number_days_removed <- calculate_number_ae_end_is_earlier_than_start(dat = data, sel_aestdy = AESTDY, sel_aeendy = AEENDY)
    } else {
      number_days_removed <- 0
    }
  } else  {
    number_days_removed <- 0
  }

  if (number_days_removed > 0) {
     data %>%
        dplyr::mutate(!!rlang::sym(paste0(LVDT,"_raw")):= !!rlang::sym(LVDT)) %>%
        dplyr::mutate(!!rlang::sym(LVDT):= dplyr::if_else(is.na(!!rlang::sym(LVDT)),as.Date(anytime::anydate(last_date)),as.Date(anytime::anydate(!!rlang::sym(LVDT))))) %>%
        dplyr::mutate(!!rlang::sym(paste0(LVDT,"_imputed_flag")):= dplyr::case_when(
          is.na(!!rlang::sym(paste0(LVDT,"_raw"))) & !is.na(!!rlang::sym(LVDT)) ~ 1,
          !is.na(!!rlang::sym(paste0(LVDT,"_raw"))) & is.na(!!rlang::sym(LVDT)) ~ 1,
          is.na(!!rlang::sym(paste0(LVDT,"_raw"))) & is.na(!!rlang::sym(LVDT)) ~ 0,
          !is.na(!!rlang::sym(paste0(LVDT,"_raw"))) & !is.na(!!rlang::sym(LVDT)) & !!rlang::sym(paste0(LVDT,"_raw")) != !!rlang::sym(LVDT) ~ 1,
          !is.na(!!rlang::sym(paste0(LVDT,"_raw"))) & !is.na(!!rlang::sym(LVDT)) & !!rlang::sym(paste0(LVDT,"_raw")) == !!rlang::sym(LVDT) ~ 0,
          )
        )
     data <- data %>%
      dplyr::filter((!!rlang::sym(AESTDY) <= !!rlang::sym(AEENDY)) | is.na(AESTDY) & is.na(AEENDY))
  }

  #return imputed data and number of imputed values as list
  return(
    list(
      "data" = data,
      "number_unknown_aes" = number_unknown_aes,
      "number_severe_missing" = number_severe_missing,
      "number_missing_lvdt" = number_missing_lvdt,
      "number_ae_start_missing" = number_ae_start_missing,
      "number_ae_end_missing" = number_ae_end_missing,
      "number_days_removed" = number_days_removed
    )
  )
}
