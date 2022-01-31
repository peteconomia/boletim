library(RCurl)
library(tidyverse)
library(survey)
library(convey)
library(PNADcIBGE)
library(ecoseries)
library(ipeadatar)
library(devtools)
library(RCurl)
library(readr)
library(timeDate)
library(safejoin)
library(petgg)
library(extrafont)




# Rodar o c?digo da fun??o at? o ?ltimo colchete.
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
    resul <- unlist(strsplit(req, "\n")) # "\r\n" for windows
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


#Agora rodar o primeiro script, ir? fazer o download dos dados.
caged_Janeiro <- get_caged(type = 2, month = 1, year = 2020)
caged_Fevereiro <- get_caged(type = 2, month = 2, year = 2020)
caged_Março <- get_caged(type = 2, month = 3, year = 2020)
caged_Abril <- get_caged(type = 2, month = 4, year = 2020)
caged_Maio <- get_caged(type = 2, month = 5, year = 2020)
caged_Junho <- get_caged(type = 2, month = 6, year = 2020)
caged_Julho <- get_caged(type = 2, month = 7, year = 2020)
caged_Agosto <- get_caged(type = 2, month = 8, year =2020)
caged_Setembro <- get_caged(type = 2, month = 9, year =2020)
caged_Outubro <- get_caged(type = 2, month = 10, year =2020)
caged_Novembro <- get_caged(type = 2, month = 11, year = 2020)





# RAÇA/COR DOS ADMITIDOS
caged_TO_RA1 <- caged_Janeiro %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(raçacor) 

caged_TO_RA2 <- caged_Fevereiro %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(raçacor) 

caged_TO_RA3 <- caged_Março %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(raçacor)

caged_TO_RA4 <- caged_Abril %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(raçacor)

caged_TO_RA5 <- caged_Maio %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(raçacor)

caged_TO_RA6 <- caged_Junho %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(raçacor)

caged_TO_RA7 <- caged_Julho %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(raçacor)

caged_TO_RA8 <- caged_Agosto %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(raçacor)

caged_TO_RA9 <- caged_Setembro %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(raçacor)

#Resultado Parciais

Resultado <- safe_left_join(caged_TO_RA1, caged_TO_RA2, by = c("raçacor"), 
                            conflict = ~ .x+ ifelse(is.na(.y),0,.y))

Resultado_2 <- safe_left_join(caged_TO_RA3, caged_TO_RA4, by = c("raçacor"), 
                              conflict = ~ .x+ ifelse(is.na(.y),0,.y))

Resultado_3 <- safe_left_join(caged_TO_RA5, caged_TO_RA6, by = c("raçacor"), 
                              conflict = ~ .x+ ifelse(is.na(.y),0,.y))


#FInal

Resultado_Final <- safe_left_join(Resultado, Resultado_2, by = c("raçacor"), 
                                  conflict = ~ .x+ ifelse(is.na(.y),0,.y))

Resultado_Final2 <- safe_left_join(Resultado_Final, Resultado_3, by = c("raçacor"), 
                                   conflict = ~ .x+ ifelse(is.na(.y),0,.y))



# RA?A/COR DOS DEMITIDOS
caged_TO_RD1 <- caged_Janeiro %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == -1) %>%
  count(raçacor) 

caged_TO_RD2 <- caged_Fevereiro %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação== -1) %>%
  count(raçacor) 

caged_TO_RD3 <- caged_Março %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == -1) %>%
  count(raçacor)

caged_TO_RD4 <- caged_Abril %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == -1) %>%
  count(raçacor)

caged_TO_RD5 <- caged_Maio %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == -1) %>%
  count(raçacor)

caged_TO_RD6 <- caged_Junho %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == -1) %>%
  count(raçacor)

caged_TO_RD7 <- caged_Julho %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == -1) %>%
  count(raçacor)

caged_TO_RD8 <- caged_Agosto %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == -1) %>%
  count(raçacor)

caged_TO_RD9 <- caged_Setembro%>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == -1) %>%
  count(raçacor)


#Resultado Parciais

Resultado <- safe_left_join(caged_TO_RD1, caged_TO_RD2, by = c("raçacor"), 
                            conflict = ~ .x+ ifelse(is.na(.y),0,.y))

Resultado_2 <- safe_left_join(caged_TO_RD3, caged_TO_RD4, by = c("raçacor"), 
                              conflict = ~ .x+ ifelse(is.na(.y),0,.y))

Resultado_3 <- safe_left_join(caged_TO_RD5, caged_TO_RD6, by = c("raçacor"), 
                              conflict = ~ .x+ ifelse(is.na(.y),0,.y))


#FInal

Resultado_Final <- safe_left_join(Resultado, Resultado_2, by = c("raçacor"), 
                                  conflict = ~ .x+ ifelse(is.na(.y),0,.y))

Resultado_Final2 <- safe_left_join(Resultado_Final, Resultado_3, by = c("raçacor"), 
                                   conflict = ~ .x+ ifelse(is.na(.y),0,.y))
