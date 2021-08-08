#library(ipeadatar)
#library(dplyr)
#library(lubridate)

# Retorna a inflação acumulada ou a inflação mensal (número-índice)
# Exemplos:
# ipca("2019-01-01", "2019-12-01") retorna a inflação acumulada de 2019
# ipca("2020-06-01") retorna a inflação de junho de 2020

ipca <- function(from, to = NULL) {
  
  from <- lubridate::as_date(from)
  to <- if (rlang::is_null(to)) {
    from
  } else {
    lubridate::as_date(to)
  }

  if (rlang::is_na(from)) {
    rlang::abort("`from` date format is invalid")
  }
  if (rlang::is_na(to)) {
    rlang::abort("`to` date format is invalid")
  }
  
  file <- paste0(tempdir(), "/", "ipeadata_PRECOS12_IPCA12.rds")
  
  if (!file.exists(file)) {
    ipeadatar::ipeadata("PRECOS12_IPCA12", language = "br") %>%
      readr::write_rds(file = file)
  }
  
  readr::read_rds(file) %>% 
    dplyr::filter(date == from %m-% base::months(1) | date == to) -> result
  
  result$value[2] / result$value[1]
}
