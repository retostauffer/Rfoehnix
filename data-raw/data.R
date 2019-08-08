
sattelberg <- utils::read.table("sattelberg.csv", sep = ";", header = TRUE)
ellboegen  <- utils::read.table("ellboegen.csv",  sep = ";", header = TRUE)

devtools::use_data(sattelberg)
devtools::use_data(ellboegen)

