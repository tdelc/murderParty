## code to prepare `init_habemus_indices` dataset goes here
init_habemus_indices <- read_csv("data-raw/habemus/init_habemus_indices.csv")
usethis::use_data(init_habemus_indices, overwrite = TRUE)
