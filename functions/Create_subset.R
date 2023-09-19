library(rofi)
## Creating function
Create_subset <- function(dataset)
  ## creating subset
  subset_data <- prepared.data[, c("Gender", "DOB", "Deceased", "ed_emerg_proc", "pt_age_yrs", "ofi")]
 ## Exclude patients with missing variables
subset.data <- na.omit(subset_data[, "Gender", "DOB", "Deceased", "ed_emerg_proc", "pt_age_yrs", "ofi"])

