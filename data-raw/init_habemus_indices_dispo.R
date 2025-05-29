## code to prepare `init_habemus_indices_dispo` dataset goes here
init_habemus_indices_dispo <- read_csv("data-raw/habemus/init_habemus_indices_dispo.csv")
usethis::use_data(init_habemus_indices_dispo, overwrite = TRUE)
