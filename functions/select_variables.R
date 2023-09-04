#' Select Variables
#'
#' Select only the variables I need for my analysis 
#' @param dataset The compled dataset.
#' @export 
prepare_data <- function(dataset) {
    variables.to.include <- c("pt_Gender", "pt_age_yrs", "ed_sbp_value", "ed_rr_value", "ed_gcs_sum", "NISS")
    prepared.dataset <- dataset[, variables.to.include]
    return (prepared.dataset)
}
