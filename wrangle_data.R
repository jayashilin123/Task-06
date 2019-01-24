# Load the data

carprice <- read.csv("data/CarPrice_LR.csv",stringsAsFactors = F)
save(carprice, file = "rda/carprice.rda")

