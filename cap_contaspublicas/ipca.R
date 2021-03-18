#library(ipeadatar)
#library(dplyr)

# Retorna a inflação acumulada ou a inflação mensal (número-índice)
# Exemplos:
# ipca("2019-01", "2019-12") retorna a inflação acumulada de 2019
# ipca("2020-06") retorna a inflação de junho de 2020
get_ipca <- function(from, to = NULL, RDSfile = NULL) {
  response <- c()
  if (is.null(RDSfile)) {
    response <- ipeadatar::ipeadata("PRECOS12_IPCA12", language = "br")
  } else {
    response <- RDSfile
  }
  day_month <- ifelse(
    nrow(
      dplyr::filter(response, date == as.Date(paste0(from, "-01")) - 31) != 0
    ), 31, 30
  )
  ipca_index <- dplyr::filter(
    response,
    date == as.Date(paste0(from, "-01")) - day_month | 
    date == as.Date(paste0(ifelse(is.null(to), from, to), "-01"))
  )
  calc_ipca_acc <- ipca_index$value[2] / ipca_index$value[1]
  return(calc_ipca_acc)
}
