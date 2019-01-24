# Load the data

carprice <- read.csv("data/CarPrice_LR.csv",stringsAsFactors = F)
save(carprice, file = "rda/carprice.rda")

carPrice <- read.csv("data/CarPrice.csv",stringsAsFactors = F)
save(carPrice, file = "rda/carPrice.rda")
