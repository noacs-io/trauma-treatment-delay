library(rofi)
## Creating function
Create_subset <- function(dataset){
 
   ## creating subset
  subset.datas <- prepared.data[, c("Gender", "DOB", "Deceased", "ed_emerg_proc", "pt_age_yrs", "ofi", "Problemomrade_.FMP")]
  
  ## Exclude patients with missing variables
  newsubset.data <- na.omit(subset.datas)
  
  return(newsubset.data)
}

