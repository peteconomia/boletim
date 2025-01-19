# Script para obter dados do novo CAGED

# 1) install.packages(c("RCurl", "readr", "timeDate"))
# 2) install archive via devtools
# 2.1) install.packages("devtools")
# 2.2) For windows, install Rtools
# 2.3) devtools::install_github("jimhester/archive")

# Argumentos
## type é um inteiro, 1 ou 2
## month é um inteiro entre 1 e 12
## year é um inteiro, ano

# Exemplo
## get_caged(type = 1, month = 2, year = 2020)


library(RCurl)
library(readr)
library(timeDate)
library(archive)

get_caged <- function(type = NULL, month = NULL, year = NULL, dir = tempdir()) {
  if (is.null(type)) {
    stop("Select a type, 1 or 2")
  }
  if (is.null(month)) {
    stop("Select a month, a value between 1 and 12")
  }
  if (type == 2) {
    if (is.null(year)) {
      stop("Select a year")
    }
    if (year < 2020) {
      stop("year arg >= 2020")
    }
    current_year <- timeDate::getRmetricsOptions("currentYear")
    if (year > current_year) {
      stop(paste0("year arg <=", current_year))
    }
  }
  types <- c("Estabelecimentos", "Movimenta%E7%F5es") # 1 or 2
  months <- c(
    "Janeiro",
    "Fevereiro",
    "Março",
    "Abril",
    "Maio",
    "Junho",
    "Julho",
    "Agosto",
    "Setembro",
    "Outubro",
    "Novembro",
    "Dezembro"
  )
  ftp_url <- "ftp://ftp.mtps.gov.br/pdet/microdados/NOVO CAGED/"
  response <- function(url) {
    req <- RCurl::getURL(
      utils::URLencode(paste0(ftp_url, url, "/")),
      .encoding = "ISO-8859-1",
      dirlistonly = TRUE
    )
    resul <- unlist(strsplit(gsub("\r\n", "\n", req), "\n"))
    return(resul)
  }
  current_month <- function(src) {
    months_index <- c()
    for (i in src) {
      months_index <- c(months_index, which(i == months))
    }
    current <- max(months_index)
    return(current)
  }
  # Estabelecimentos
  if (type == 1) {
    dir_months <- response(types[type])
    this_month_index <- current_month(dir_months)
    sub_current_dir <- response(
      paste0(types[type], "/", months[this_month_index])
    )
    file <- sub_current_dir[
      substr(sub_current_dir, 1, 16) == paste0("CAGEDESTAB", "2020", ifelse(month < 10, paste0("0", month), month))
    ]
    if (length(file) == 0) {
      stop("File not found!!!")
    }
    utils::download.file(
      url = utils::URLencode(
        paste0(ftp_url, types[type], "/", months[this_month_index], "/", file)
      ),
      destfile = paste0(dir, "/", file),
      mode = "wb"
    )
    result <- readr::read_csv2(
      archive::archive_read(paste0(dir, "/", file)),
      na = c("NA"),
      progress = show_progress()
    )
    return(result)
  }
  # Movimentações
  if (type == 2) {
    dir_year <- response(paste0(types[type]))
    check_year <- dir_year[substr(dir_year, 1, 4) == paste0(year)]
    if (length(check_year) == 0) {
      stop("File not found!!!")
    }
    dir_months <- response(paste0(types[type], "/", year))
    this_month_index <- current_month(dir_months)
    sub_current_dir <- response(
      paste0(types[type], "/", year, "/", months[this_month_index])
    )
    file <- sub_current_dir[
      substr(sub_current_dir, 1, 14) == paste0("CAGEDMOV", year, ifelse(month < 10, paste0("0", month), month))
    ]
    if (length(file) == 0) {
      stop("File not found!!!")
    }
    utils::download.file(
      url = utils::URLencode(
        paste0(
          ftp_url,
          types[type],
          "/",
          year,
          "/",
          months[this_month_index],
          "/",
          file
        )
      ),
      destfile = paste0(dir, "/", file),
      mode = "wb"
    )
    result <- readr::read_csv2(
      archive::archive_read(paste0(dir, "/", file)),
      na = c("NA"),
      progress = show_progress()
    )
    return(result)
  }
}
