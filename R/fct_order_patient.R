#' order_patient - Arranges patients by sequencing technique
#' @description
#' Arranges patients by sequencing technique (R Package required: 'dplyr', 'reshape2, 'seriation')
#'
#' @param ae_data Adverse event dataset
#' @param patients Patient dataset
#' @param variables Vector with Variable Names used to seriate
#' @param method_dist Character string with the name of the method to calculate the distance matrix
#' @param method_seriate Character string with the name of the method to seriate
#'
#' @keywords internal

order_patient <- function(
    ae_data = ae_data,
    patients = patient_d,
    variables = input$varSeq,
    method_dist = 'euclidean',
    method_seriate = input$methSeq
){

  # add column ae_count which includes the number of days between day_start and day_end
  tmp <- ae_data %>%
    dplyr::mutate(ae_count = day_end - day_start + 1)

  # transform variable ae to factor
  tmp$ae <- factor(tmp$ae)

  # create count variable for severe adverse events (Grad 1-3)
  tmp <- tmp %>%
    dplyr::mutate(
      ae_count_sev1 = ifelse(sev == 1, ae_count, 0),
      ae_count_sev2 = ifelse(sev == 2, ae_count, 0),
      ae_count_sev3 = ifelse(sev == 3, ae_count, 0)
    )

  tmp2 <- stats::aggregate(
    cbind(
      ae_count,
      ae_count_sev1,
      ae_count_sev2,
      ae_count_sev3
    ) ~ patient + ae,
    data = tmp,
    FUN = sum
  )

  #transform in wide format
  tmp3 <- tmp2 %>%
    tidyr::gather(-c(patient,ae), key = "tmp", value = "val") %>%
    dplyr::mutate(ae = paste(ae, tmp, sep = "_")) %>%
    dplyr::select(-tmp) %>%
    tidyr::spread(ae, val, fill = 0, sep = "_")

  #rename column names
  for (i in 1:length(colnames(tmp3)[-1])) {
    if (length(strsplit(colnames(tmp3)[i + 1], '_')[[1]]) == 5) {
        colnames(tmp3)[i + 1] <- paste(strsplit(colnames(tmp3)[i + 1], '_')[[1]][c(2,5)], collapse = "_")
    } else {
      colnames(tmp3)[i + 1] <- paste(strsplit(colnames(tmp3)[i + 1], '_')[[1]][c(2)], collapse = "_")
    }
  }

  # merge death flag to the data set
  tmp4 <- tmp3 %>%
    dplyr::right_join(patients %>%
                        dplyr::mutate(death = ifelse(death == 99999, 0, 1)) %>%
                        dplyr::select(ps, end, death, ae_frequency), by = c("patient" = "ps"))

  tmp4[is.na(tmp4)] <- 0
  # reduce data set to the selected variables

  tmp4 <- tmp4 %>%
    dplyr::select(tidyselect::all_of(c("patient", variables)))

  # calculate distance matrix
  dd <- stats::dist(tmp4[, -1], method = method_dist)

  # seriate distance matrix
  sq <- seriation::seriate(dd, method = method_seriate)

  # permute the data set in the calculated order
  tmp5 <- seriation::permute(tmp4, order = sq, margin = 1)

  if (dim(tmp5)[2]>2) {
    tmp5 <- rbind(
      tmp5[as.vector(apply(tmp5[,-1],1,function(x){sum(x) == 0})),],
      tmp5[as.vector(apply(tmp5[,-1],1,function(x){sum(x) != 0})),]
    )
  } else if (dim(tmp5)[1] == 2) {
    tmp5 <- rbind(
      tmp5[tmp5[,2]!=0,],
      tmp5[tmp5[,2]==0,]
    )
  }

  # create index variable
  tmp5$'SEQUENCING' <- 1:nrow(tmp5)
  tmp5 <- tmp5[c('patient','SEQUENCING')]

  return(tmp5)
}
