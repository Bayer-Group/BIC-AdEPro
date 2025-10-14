#' Join Adverse Event Data and Subject Level Data Sets
#'
#' @param dat_adae data frame with adverse event data set
#' @param dat_adsl data frame with subject level data set
#' @param SUBJIDN character with subject identifier variable (required column in both data sets)
#'
#' @return merged data set with adae and adsl information
#'

join_adae_and_adsl <- function(dat_adae, dat_adsl, SUBJIDN) {

  #check if parameter dat_adae is data.frame
  if (!is.data.frame(dat_adae)) {
    stop("Parameter 'dat_adae' must be a data frame!")
  }
  #check if parameter dat_adsl is data.frame
  if (!is.data.frame(dat_adsl)) {
    stop("Parameter 'dat_adsl' must be a data frame!")
  }
  #check if parameter SUBJIDN is a valid column in both data frames
  if (!(SUBJIDN %in% colnames(dat_adae)) & !(SUBJIDN %in% colnames(dat_adsl))) {
    stop("Parameter SUBJIDN must be in one of the data frames!")
  }

  #transfer to data frame object
  dat_adae <- as.data.frame(dat_adae)
  dat_adsl <- as.data.frame(dat_adsl)

  #get joint variables in adae and adsl
  joint_vars <- dplyr::intersect(colnames(dat_adae), colnames(dat_adsl))

  # check if subjects, which are available in adae and adsl have the same
  # entries for subject identifier variable

  # get vector with subject identifier available in adae and adsl
  subject_id_index <- dplyr::intersect(dat_adae[,SUBJIDN],dat_adsl[,SUBJIDN]) %>%
    stats::na.omit()

  #compare if the variable for merging have the same values e.g. a subject has the same treatment in adae and adsl
  #compare object 1
  comp1 <- dat_adae[dat_adae[,SUBJIDN] %in% subject_id_index,] %>%
    dplyr::select(!!!rlang::syms(joint_vars)) %>%
    dplyr::distinct() %>%
    dplyr::arrange(!!rlang::sym(SUBJIDN))

  #compare object 2
  comp2 <- dat_adsl[dat_adsl[,SUBJIDN] %in% subject_id_index,] %>%
    dplyr::select(!!!rlang::syms(joint_vars)) %>%
    dplyr::distinct() %>%
    dplyr::arrange(!!rlang::sym(SUBJIDN))

  #remove attributes from compare objects
  attr(comp1, "ATT") <- NULL
  attr(comp2, "ATT") <- NULL

  attributes(comp1)$label <- NULL
  attributes(comp2)$label <- NULL

  #return index of columns which are identical for adae and adsl in common subjects
  index_match <- sapply(joint_vars, function(x){identical(comp1 %>% dplyr::select(!!rlang::sym(x), !!rlang::sym(SUBJIDN)) %>% dplyr::distinct(),comp2 %>% dplyr::select(!!rlang::sym(x), !!rlang::sym(SUBJIDN)) %>% dplyr::distinct())})

  #get joint variables which match
  joint_vars_match <- joint_vars[which(index_match)]
  joint_vars_no_match <- joint_vars[which(!index_match)]

  # if a non required variables differs between adae and adsl, it is duplicated in the join and given the extention .x, .y
  # (e.g. ADSNAME with entries "ADSL" and "ADAE")
  if(length(joint_vars_match) > 0) {
    pat_dat <- dplyr::full_join(dat_adsl,dat_adae, by = dplyr::join_by(!!!rlang::syms(joint_vars_match)))
  } else {
    stop("Error: no variables with matching entries")
  }

  #return merged data
  return(pat_dat)
}
