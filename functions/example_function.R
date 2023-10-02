library(rofi)

prepared.data <- prepare_data(data)

summary(prepared.data)

subset.datas <- prepared.data[, c("ofi", "Problemomrade_.FMP")]

View(subset.datas)

na.omit(subset.datas)
table(subset.datas$Problemomrade_.FMP)
