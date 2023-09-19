library(rofi)
## Creating function
Create_subset <- function(dataset)
  ## creating subset
  subset.data <- prepared.data[, c("Gender", "DOB", "Deceased", "ed_emerg_proc", "pt_age_yrs", "ofi")]
 ## Exclude patients with missing variables
  newsubset.data <- na.omit(subset.data[, c("Gender", "DOB", "Deceased", "ed_emerg_proc", "pt_age_yrs", "ofi")])

