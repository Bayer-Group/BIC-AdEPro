test_that("slice plot works", {

  adae_and_adsl <- join_adae_and_adsl(dat_adae = adae_data, dat_adsl = adsl_data, SUBJIDN = "SUBJIDN")

  filtered <- filter_for_safety_flag(dat = adae_and_adsl, SAFFN = "SAFFN")

  imputed <- calculate_and_impute_required_variables_missing_values(data = filtered, severity_grading_flag = "Severity")

  prepared <- prepare_data_for_adepro(dat = imputed$data)

  Q <- initQ(prepared$ae_data)

  ae_data <- preproc_ae(prepared$ae_data,grading=FALSE)

  ae_data <- ae_data[which(Q[, 1]), ]

  patients <- preproc_patients(prepared$pat_data, 18)

  globals <- set_global_params(
    prepared$ae_data,
    prepared$pat_data,
    title = "",
    height = 18,
    treatment = c("Control","Experimental")
  )

  p <- function() adepro_slice_plot(
    data = ae_data,
    patients = patients,
    ae_list = c("Dizziness","Nausea","Pain","Headache","Cough","Diarrhea","Fatigue","Edema limbs","Anorexia","Alopecia"),#v
    global_params = globals,
    height = globals$height,
    width  = globals$width,
    xlines = globals$xlines,
    ylines = globals$ylines,
    xval = c(0, cumsum(globals$plines[[1]])[-length(globals$plines[[1]])]) + globals$plines[[1]]/2,
    title = as.character(unique(patients$treat)),
    subgroup = NULL,
    subjidn = "SUBJIDN",
    slider = 100,
    info = NULL,
    legend_ae = NULL,
    arrow_data = prepared$ae_data,
    show_arrows = FALSE
  )

  vdiffr::expect_doppelganger("Slice plot", p)

})
