library(rofi)

prepared.data <- prepare_data(data)

summary(prepared.data)

subset.datas <- prepared.data[, c("ofi", "Problemomrade_.FMP")]

table(prepared.data$Problemomrade_.FMP)

delayed_subset <- subset(prepared.data, Problemomrade_.FMP == "Handläggning")
rest_subset <- subset(prepared.data, Problemomrade_.FMP != "Handläggning")

prepared.data$Problemomrade_.FMP <- ifelse(prepared.data$Problemomrade_.FMP == "Handläggning", 1, 0)

cleaned_data <- subset(prepared.data, !is.na(Problemomrade_.FMP))

prepared.data$ofi <- ifelse(prepared.data$ofi == "success", 1, 0)
cleaned_data$Gender <- ifelse(cleaned_data$Gender == "M", 1, 0)

