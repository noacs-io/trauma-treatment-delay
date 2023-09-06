library(rofi)


#' Select Variables
#'
#' Select only the variables I need for my analysis 
#' @param dataset The complete dataset.
#' @export 
prepare_data <- function(dataset) {
  ## Import data
  data <- rofi::import_data(test = TRUE)
  
  combined.datasets <- rofi::merge_data(data, test = TRUE)
  combined.datasets$ofi <- rofi::create_ofi(combined.datasets)
  
  return (combined.datasets)
}
