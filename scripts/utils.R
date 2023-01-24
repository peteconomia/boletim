# Converte uma data para o formato bimestre, ex: "2020-02-01" -> "1B\n2020"
date_label_bimester <- function(date, label = "B", sep = "\n") {
  
  if (!lubridate::is.Date(date)) {
    rlang::abort("`date` should be format date")
  }
  
  bimesters <- list(
    "1" = 1:2,
    "2" = 3:4,
    "3" = 5:6,
    "4" = 7:8,
    "5" = 9:10,
    "6" = 11:12
  )

  purrr::keep(bimesters, ~lubridate::month(date) %in% .x) %>% 
    names() %>% 
    paste0(label, sep, lubridate::year(date))
}