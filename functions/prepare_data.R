library(rofi)

prepare_data <- function(dataset){
  
  ## Import data
  data <- rofi::import_data(test = TRUE)
    
  ## Create variable combined datasets by merging
  combined.datasets <- rofi::merge_data(data, test = TRUE)

    ## Adding ofi as variable
  combined.datasets$ofi <- rofi::create_ofi(combined.datasets)
  
  ## output
  return (combined.datasets)
}

