library(HydroMe)

data("infilt")
infil <- infilt[319:751,]
infil2 <- data.frame(Time = infil$Time, Rate = infil$Rate)
infil2 <- infil2[1:49, ]

openxlsx::write.xlsx(infil2, file = "infilt_field_data.xlsx")
