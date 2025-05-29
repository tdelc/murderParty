## code to prepare `init_habemus` dataset goes here
init_habemus <- read_csv("data-raw/habemus/init_habemus.csv")
usethis::use_data(init_habemus, overwrite = TRUE)
