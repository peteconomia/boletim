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

#Diretório
setwd("~/Área de Trabalho/UFT/Boletim - PET/Dados Emprego")



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


#Rodar esse script para a leitura dos dados.
caged_data_janeiro <- read_caged(type = 2, dire = caged_janeiro$dire)
caged_data_fevereiro <- read_caged(type = 2, dire = caged_fevereiro$dire)
caged_data_março <- read_caged(type = 2, dire = caged_março$dire)
caged_data_Abril <- read_caged(type = 2, dire = caged_Abril$dire)
caged_data_Maio <- read_caged(type = 2, dire = caged_Maio$dire)
caged_data_Junho <- read_caged(type = 2, dire = caged_Junho$dire)
caged_data_julho <- read_caged(type =2, dire = caged_Julho$dire)


# Dados TOcantins - Janeiro
caged_Tocantins <- caged_Janeiro %>%
  filter(uf==17) %>% 
  group_by(saldomovimentação) %>% 
  summarise(saldomovimentação = sum(saldomovimentação), .groups = "drop")

 
#Dados Tocantins - Fevereiro

caged_Tocantins2 <- caged_Fevereiro %>% 
  filter(uf==17) %>% 
  group_by(saldomovimentação) %>% 
  summarise(saldomovimentação = sum(saldomovimentação), .groups = "drop")

#Dados Tocantins - Mar?o

Caged_Tocantins3 <- caged_Março %>% 
  filter(uf==17) %>% 
  group_by(saldomovimentação) %>% 
  summarise(saldomovimentação = sum(saldomovimentação), .groups = "drop")

#Dados Tocantins - Abril

Caged_Tocantins4 <- caged_Abril %>% 
  filter(uf==17) %>% 
  group_by(saldomovimentação) %>% 
  summarise(saldomovimentação = sum(saldomovimentação), .groups = "drop")

#Dados Tocantins - Maio

Caged_Tocantins5 <- caged_Maio %>% 
  filter(uf==17) %>% 
  group_by(saldomovimentação) %>% 
  summarise(saldomovimentação = sum(saldomovimentação), .groups = "drop")

#Dados Tocantins - Junho

Caged_Tocantins6 <- caged_Junho %>% 
  filter(uf==17) %>% 
  group_by(saldomovimentação) %>% 
  summarise(saldomovimentação = sum(saldomovimentação), .groups = "drop")

#Dados Tocantins - Julho

Caged_Tocantins7 <- caged_Julho %>% 
  filter(uf==17) %>% 
  group_by(saldomovimentação) %>% 
  summarise(saldomovimentação = sum(saldomovimentação), .groups = "drop")

#Dados Tocantins - Agosto

Caged_Tocantins8 <- caged_Agosto %>% 
  filter(uf==17) %>% 
  group_by(saldomovimentação) %>% 
  summarise(saldomovimentação= sum(saldomovimentação), .groups = "drop")


#Dados Tocantins - Setembro

Caged_Tocantins9 <- caged_Setembro %>% 
  filter(uf==17) %>% 
  group_by(saldomovimentação) %>% 
  summarise(saldomovimentação= sum(saldomovimentação), .groups = "drop")


#Dados Norte - Janeiro

Caged_Norte1 <- caged_Janeiro %>% 
  filter(região==1) %>% 
  group_by(saldomovimentação) %>% 
  summarise(saldomovimentação = sum(saldomovimentação), .groups = "drop")

#Dados Norte - Fevereiro 

Caged_Norte2 <- caged_Fevereiro %>% 
  filter(região==1) %>% 
  group_by(saldomovimentação) %>% 
  summarise(saldomovimentação = sum(saldomovimentação), .groups = "drop")

#Dados Norte - Mar?o

Caged_Norte3 <- caged_Março %>% 
  filter(região==1) %>% 
  group_by(saldomovimentação) %>% 
  summarise(saldomovimentação = sum(saldomovimentação), .groups ="drop")

#Dados Norte - Abril

Caged_Norte4 <- caged_Abril %>% 
  filter(região==1) %>% 
  group_by(saldomovimentação) %>% 
  summarise(saldomovimentação = sum(saldomovimentação), .groups ="drop")

#Dados Norte - Maio

Caged_Norte5 <- caged_Maio %>% 
  filter(região==1) %>% 
  group_by(saldomovimentação) %>% 
  summarise(saldomovimentação = sum(saldomovimentação), .groups ="drop")

#Dados Norte - Junho

Caged_Norte6 <- caged_Junho %>% 
  filter(região==1) %>% 
  group_by(saldomovimentação) %>% 
  summarise(saldomovimentação = sum(saldomovimentação), .groups ="drop")

#Dados Norte - Julho

Caged_Norte7 <- caged_Julho %>% 
  filter(região==1) %>% 
  group_by(saldomovimentação) %>% 
  summarise(saldomovimentação = sum(saldomovimentação), .groups ="drop")


# Dados Norte - Agosto

Caged_Norte8 <- caged_Agosto %>% 
  filter(região == 1) %>% 
  group_by(saldomovimentação) %>% 
  summarise(saldomovimentação = sum(saldomovimentação), .groups = "drop")

#Dados Norte - Setembro 

Caged_Norte9 <- caged_Setembro %>% 
  filter(região == 1) %>% 
  group_by(saldomovimentação) %>% 
  summarise(saldomovimentação= sum(saldomovimentação), .groups = "drop")


# GRAU DE INSTRU??O DOS ADMITIDOS
caged_TO_GIA1 <- caged_Janeiro %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(graudeinstrução) 

caged_TO_GIA2 <- caged_Fevereiro %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(graudeinstrução)

caged_TO_GIA3 <- caged_Março %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(graudeinstrução)

caged_TO_GIA4 <- caged_Abril %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(graudeinstrução)

caged_TO_GIA5 <- caged_Maio %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(graudeinstrução)

caged_TO_GIA6 <- caged_Junho %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(graudeinstrução)

caged_TO_GIA7 <- caged_Julho%>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(graudeinstrução)


caged_TO_GIA8 <- caged_Agosto %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(graudeinstrução)

caged_TO_GIA9 <- caged_Setembro %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(graudeinstrução)

#Resultado Parciais

Resultado <- safe_left_join(caged_TO_GIA1, caged_TO_GIA2, by = c("graudeinstrução"), 
                            conflict = ~ .x+ ifelse(is.na(.y),0,.y))

Resultado_2 <- safe_left_join(caged_TO_GIA3, caged_TO_GIA4, by = c("graudeinstrução"), 
                                  conflict = ~ .x+ ifelse(is.na(.y),0,.y))

Resultado_3 <- safe_left_join(caged_TO_GIA5, caged_TO_GIA6, by = c("graudeinstrução"), 
                              conflict = ~ .x+ ifelse(is.na(.y),0,.y))

#FInal

Resultado_Final <- safe_left_join(Resultado, Resultado_2, by = c("graudeinstrução"), 
                            conflict = ~ .x+ ifelse(is.na(.y),0,.y))

Resultado_Final2 <- safe_left_join(Resultado_Final, Resultado_3, by = c("graudeinstrução"), 
                                  conflict = ~ .x+ ifelse(is.na(.y),0,.y))


# GRAU DE INSTRU??O DOS DEMITIDOS
caged_TO_GID1 <- caged_Janeiro%>%
  filter(uf == 17) %>% 
  filter(saldomovimentação== -1) %>%
  count(graudeinstrução) 

caged_TO_GID2 <- caged_Fevereiro %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == -1) %>%
  count(graudeinstrução)

caged_TO_GID3 <- caged_Março %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == -1) %>%
  count(graudeinstrução)

caged_TO_GID4 <- caged_Abril%>%
  filter(uf == 17) %>% 
  filter(saldomovimentação== -1) %>%
  count(graudeinstrução) 

caged_TO_GID5 <- caged_Maio%>%
  filter(uf == 17) %>% 
  filter(saldomovimentação== -1) %>%
  count(graudeinstrução) 

caged_TO_GID6 <- caged_Junho%>%
  filter(uf == 17) %>% 
  filter(saldomovimentação== -1) %>%
  count(graudeinstrução) 

caged_TO_GID7 <- caged_Julho%>%
  filter(uf == 17) %>% 
  filter(saldomovimentação== -1) %>%
  count(graudeinstrução) 

caged_TO_GID8 <- caged_Agosto%>%
  filter(uf == 17) %>% 
  filter(saldomovimentação== -1) %>%
  count(graudeinstrução) 

caged_TO_GID9 <- caged_Setembro%>%
  filter(uf == 17) %>% 
  filter(saldomovimentação== -1) %>%
  count(graudeinstrução) 


#Resultado Parciais

Resultado <- safe_left_join(caged_TO_GID1, caged_TO_GID2, by = c("graudeinstrução"), 
                            conflict = ~ .x+ ifelse(is.na(.y),0,.y))

Resultado_2 <- safe_left_join(caged_TO_GID3, caged_TO_GID4, by = c("graudeinstrução"), 
                              conflict = ~ .x+ ifelse(is.na(.y),0,.y))

Resultado_3 <- safe_left_join(caged_TO_GID5, caged_TO_GID6, by = c("graudeinstrução"), 
                              conflict = ~ .x+ ifelse(is.na(.y),0,.y))


#FInal

Resultado_Final <- safe_left_join(Resultado, Resultado_2, by = c("graudeinstrução"), 
                                  conflict = ~ .x+ ifelse(is.na(.y),0,.y))

Resultado_Final2 <- safe_left_join(Resultado_Final, Resultado_3, by = c("graudeinstrução"), 
                                   conflict = ~ .x+ ifelse(is.na(.y),0,.y))





# SETOR DE TRABALHO DOS ADMITIDOS
caged_TO_SA1 <- caged_Janeiro %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(seção) 

caged_TO_SA2 <- caged_Fevereiro %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(seção) 

caged_TO_SA3 <- caged_Março %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(seção)

caged_TO_SA4 <- caged_Abril %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(seção) 

caged_TO_SA5 <- caged_Maio %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(seção) 

caged_TO_SA6 <- caged_Junho %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(seção) 

caged_TO_SA7 <- caged_Julho %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(seção) 

caged_TO_SA8 <- caged_Agosto %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(seção) 

caged_TO_SA9 <- caged_Setembro %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(seção) 

#Resultado Parciais

Resultado <- safe_left_join(caged_TO_SA1, caged_TO_SA2, by = c("seção"), 
                            conflict = ~ .x+ ifelse(is.na(.y),0,.y))

Resultado_2 <- safe_left_join(caged_TO_SA3, caged_TO_SA4, by = c("seção"), 
                              conflict = ~ .x+ ifelse(is.na(.y),0,.y))

Resultado_3 <- safe_left_join(caged_TO_SA5, caged_TO_SA6, by = c("seção"), 
                              conflict = ~ .x+ ifelse(is.na(.y),0,.y))


#FInal

Resultado_Final <- safe_left_join(Resultado, Resultado_2, by = c("seção"), 
                                  conflict = ~ .x+ ifelse(is.na(.y),0,.y))

Resultado_Final2 <- safe_left_join(Resultado_Final, Resultado_3, by = c("seção"), 
                                   conflict = ~ .x+ ifelse(is.na(.y),0,.y))



# SETOR DE TRABALHO DOS DEMITIDOS
caged_TO_SD1 <- caged_Janeiro %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == -1) %>%
  count(seção) 

caged_TO_SD2 <- caged_Fevereiro %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == -1) %>%
  count(seção) 

caged_TO_SD3 <- caged_Março%>%
  filter(uf == 17) %>% 
  filter(saldomovimentação== -1) %>%
  count(seção) 

caged_TO_SD4 <- caged_Abril%>%
  filter(uf == 17) %>% 
  filter(saldomovimentação== -1) %>%
  count(seção) 

caged_TO_SD5 <- caged_Maio%>%
  filter(uf == 17) %>% 
  filter(saldomovimentação== -1) %>%
  count(seção) 

caged_TO_SD6 <- caged_Junho%>%
  filter(uf == 17) %>% 
  filter(saldomovimentação== -1) %>%
  count(seção) 

caged_TO_SD7 <- caged_Julho%>%
  filter(uf == 17) %>% 
  filter(saldomovimentação== -1) %>%
  count(seção) 

caged_TO_SD8 <- caged_Agosto%>%
  filter(uf == 17) %>% 
  filter(saldomovimentação== -1) %>%
  count(seção) 

caged_TO_SD9 <- caged_Setembro%>%
  filter(uf == 17) %>% 
  filter(saldomovimentação== -1) %>%
  count(seção) 

#Resultado Parciais

Resultado <- safe_left_join(caged_TO_SD1, caged_TO_SD2, by = c("seção"), 
                            conflict = ~ .x+ ifelse(is.na(.y),0,.y))

Resultado_2 <- safe_left_join(caged_TO_SD3, caged_TO_SD4, by = c("seção"), 
                              conflict = ~ .x+ ifelse(is.na(.y),0,.y))

Resultado_3 <- safe_left_join(caged_TO_SD5, caged_TO_SD6, by = c("seção"), 
                              conflict = ~ .x+ ifelse(is.na(.y),0,.y))


#FInal

Resultado_Final <- safe_left_join(Resultado, Resultado_2, by = c("seção"), 
                                  conflict = ~ .x+ ifelse(is.na(.y),0,.y))

Resultado_Final2 <- safe_left_join(Resultado_Final, Resultado_3, by = c("seção"), 
                                   conflict = ~ .x+ ifelse(is.na(.y),0,.y))

#Gráfico de Setores - 1T

Setores <- data.frame("Empregos" = 1:5, "Saldo" = c(277,109,961,-1950,-660), "Setores" = c("Agricultura","Indústrias", "Construção", "Comércio", "Serviços"))
arrange(Lala)


Setores %>%
  mutate(Setores = factor(Setores, levels=c("Agricultura","Indústrias", "Construção", "Comércio", "Serviços"))) %>%
  ggplot(aes(x=Setores, y = Saldo)) + 
  geom_bar(stat='identity', colour = ifelse(Setores$Saldo > 0, "#104E8B", "#8b1010"), fill = ifelse(Setores$Saldo > 0, "#104E8B", "#8B1010")) + theme_classic() +
  geom_hline(yintercept=0, colour="black", linetype="dashed") + 
  scale_y_continuous(limits = c(-2000, 2500),breaks = seq(from = -2000, to = 2500, by = 1000)) + 
  labs(x=" ") +
  theme(plot.title = element_text(hjust = 0.5, size = 13)) +  
  geom_text(aes(label = Saldo, y = Saldo + 0.08),  position = position_dodge(0.9), vjust = 0.1)



# IDADE DOS ADMITIDOS
caged_TO_IA1 <- caged_Janeiro %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(idade) 

caged_TO_IA2 <- caged_Fevereiro %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(idade) 

caged_TO_IA3 <- caged_Março %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(idade)

caged_TO_IA4 <- caged_Abril %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(idade)

caged_TO_IA5 <- caged_Maio %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(idade)

caged_TO_IA6 <- caged_Junho %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(idade)

caged_TO_IA7 <- caged_Julho %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(idade)

caged_TO_IA8 <- caged_Agosto %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(idade)

caged_TO_IA9 <- caged_Setembro %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(idade)


#Resultado Parciais

Resultado <- safe_left_join(caged_TO_IA1, caged_TO_IA2, by = c("idade"), 
                            conflict = ~ .x+ ifelse(is.na(.y),0,.y))

Resultado_2 <- safe_left_join(caged_TO_IA3, caged_TO_IA4, by = c("idade"), 
                              conflict = ~ .x+ ifelse(is.na(.y),0,.y))

Resultado_3 <- safe_left_join(caged_TO_IA5, caged_TO_IA6, by = c("idade"), 
                              conflict = ~ .x+ ifelse(is.na(.y),0,.y))


#FInal

Resultado_Final <- safe_left_join(Resultado, Resultado_2, by = c("idade"), 
                                  conflict = ~ .x+ ifelse(is.na(.y),0,.y))

Resultado_Final2 <- safe_left_join(Resultado_Final, Resultado_3, by = c("idade"), 
                                   conflict = ~ .x+ ifelse(is.na(.y),0,.y))




# IDADE DOS DEMITIDOS
caged_TO_ID1 <- caged_Janeiro %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == -1) %>%
  count(idade)

caged_TO_ID2 <- caged_Fevereiro %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == -1) %>%
  count(idade) 

caged_TO_ID3 <- caged_Março %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == -1) %>%
  count(idade)

caged_TO_ID4 <- caged_Abril %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == -1) %>%
  count(idade)

caged_TO_ID5 <- caged_Maio %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == -1) %>%
  count(idade)


caged_TO_ID6 <- caged_Junho %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == -1) %>%
  count(idade)

caged_TO_ID7 <- caged_Julho %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == -1) %>%
  count(idade)

caged_TO_ID8 <- caged_Agosto %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == -1) %>%
  count(idade)

caged_TO_ID9 <- caged_Setembro %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == -1) %>%
  count(idade)

#Resultado Parciais

Resultado <- safe_left_join(caged_TO_ID1, caged_TO_ID2, by = c("idade"), 
                            conflict = ~ .x+ ifelse(is.na(.y),0,.y))

Resultado_2 <- safe_left_join(caged_TO_ID3, caged_TO_ID4, by = c("idade"), 
                              conflict = ~ .x+ ifelse(is.na(.y),0,.y))

Resultado_3 <- safe_left_join(caged_TO_ID5, caged_TO_ID6, by = c("idade"), 
                              conflict = ~ .x+ ifelse(is.na(.y),0,.y))


#FInal

Resultado_Final <- safe_left_join(Resultado, Resultado_2, by = c("idade"), 
                                  conflict = ~ .x+ ifelse(is.na(.y),0,.y))

Resultado_Final2 <- safe_left_join(Resultado_Final, Resultado_3, by = c("idade"), 
                                   conflict = ~ .x+ ifelse(is.na(.y),0,.y))


# SEXO DOS ADMITIDOS
caged_TO_SA1 <- caged_Janeiro %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação== 1) %>%
  count(sexo) 

caged_TO_SA2 <- caged_Fevereiro %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação== 1) %>%
  count(sexo) 

caged_TO_SA3 <- caged_Março %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(sexo)

caged_TO_SA4 <- caged_Abril %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(sexo)

caged_TO_SA5 <- caged_Maio %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(sexo)

caged_TO_SA6 <- caged_Junho %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(sexo)

caged_TO_SA7 <- caged_Julho %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(sexo)

caged_TO_SA8 <- caged_Agosto %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(sexo)

caged_TO_SA9 <- caged_Setembro %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(sexo)

#Resultado Parciais

Resultado <- safe_left_join(caged_TO_SA1, caged_TO_SA2, by = c("sexo"), 
                            conflict = ~ .x+ ifelse(is.na(.y),0,.y))

Resultado_2 <- safe_left_join(caged_TO_SA3, caged_TO_SA4, by = c("sexo"), 
                              conflict = ~ .x+ ifelse(is.na(.y),0,.y))

Resultado_3 <- safe_left_join(caged_TO_SA5, caged_TO_SA6, by = c("sexo"), 
                              conflict = ~ .x+ ifelse(is.na(.y),0,.y))


#FInal

Resultado_Final <- safe_left_join(Resultado, Resultado_2, by = c("sexo"), 
                                  conflict = ~ .x+ ifelse(is.na(.y),0,.y))

Resultado_Final2 <- safe_left_join(Resultado_Final, Resultado_3, by = c("sexo"), 
                                   conflict = ~ .x+ ifelse(is.na(.y),0,.y))



# SEXO DOS DEMITIDOS
caged_TO_SD1 <- caged_Janeiro %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == -1) %>%
  count(sexo) 

caged_TO_SD2 <- caged_Fevereiro %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == -1) %>%
  count(sexo) 

caged_TO_SD3 <- caged_Março %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == -1) %>%
  count(sexo)

caged_TO_SD4 <- caged_Abril %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == -1) %>%
  count(sexo)

caged_TO_SD5 <- caged_Maio %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == -1) %>%
  count(sexo)

caged_TO_SD6 <- caged_Junho %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == -1) %>%
  count(sexo)

caged_TO_SD7 <- caged_Julho %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == -1) %>%
  count(sexo)

caged_TO_SD8 <- caged_Agosto %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == -1) %>%
  count(sexo)

caged_TO_SD9 <- caged_Setembro %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == -1) %>%
  count(sexo)


#Resultado Parciais

Resultado <- safe_left_join(caged_TO_SD1, caged_TO_SD2, by = c("sexo"), 
                            conflict = ~ .x+ ifelse(is.na(.y),0,.y))

Resultado_2 <- safe_left_join(caged_TO_SD3, caged_TO_SD4, by = c("sexo"), 
                              conflict = ~ .x+ ifelse(is.na(.y),0,.y))

Resultado_3 <- safe_left_join(caged_TO_SD5, caged_TO_SD6, by = c("sexo"), 
                              conflict = ~ .x+ ifelse(is.na(.y),0,.y))


#FInal

Resultado_Final <- safe_left_join(Resultado, Resultado_2, by = c("sexo"), 
                                  conflict = ~ .x+ ifelse(is.na(.y),0,.y))

Resultado_Final2 <- safe_left_join(Resultado_Final, Resultado_3, by = c("sexo"), 
                                   conflict = ~ .x+ ifelse(is.na(.y),0,.y))


#gráficos

Sexo <- data.frame("Genero" = 1:2, "Saldo" = c(182,-1448), "Sexo" = c("Homem", "Mulher"))

Sexo %>%
  mutate(Sexo = factor(Sexo, levels=c("Homem", "Mulher"))) %>%
  ggplot(aes(x=Sexo, y = Saldo)) + 
  geom_bar(stat='identity', colour = ifelse(Sexo$Saldo > 0, "#104E8B", "#8b1010"), fill = ifelse(Sexo$Saldo > 0, "#104E8B", "#8B1010"), width = 0.3) + theme_classic() +
  geom_hline(yintercept=0, colour="black", linetype="dashed") + 
  scale_y_continuous(limits = c(-2000, 1000),breaks = seq(from = -2000, to = 2500, by = 1000)) + 
  theme(plot.title = element_text(hjust = 0.5, size = 13)) +  
  labs(x = " ") + 
  geom_text(aes(label = Saldo, y = Saldo + 0.08),  position = position_dodge(0.9), vjust = 0.1)


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



#gráficos

Etnia <- data.frame("Raça" = 1:7, "Saldo" = c(-443, -75, -777,-72, -2,630, -527), "Etnia" = c("Branca", "Preta", "Parda", "Amarela", "Indígena", "N/Informada", "N/indentficada"))

Etnia %>%
  mutate(Etnia = factor(Etnia, levels=c("Branca", "Preta", "Parda", "Amarela", "Indígena", "N/Informada", "N/indentficada"))) %>%
  ggplot(aes(x=Etnia, y = Saldo)) + 
  geom_bar(stat='identity', colour = ifelse(Etnia$Saldo > 0, "#104E8B", "#8b1010"), fill = ifelse(Etnia$Saldo > 0, "#104E8B", "#8B1010"), width = 0.3) + theme_classic() +
  geom_hline(yintercept=0, colour="black", linetype="dashed") + 
  scale_y_continuous(limits = c(-800, 1500),breaks = seq(from = -800, to = 1500, by = 500)) + 
  theme(plot.title = element_text(hjust = 0.5, size = 13)) +  
  labs(x= " ") + 
  geom_text(aes(label = Saldo, y = Saldo + 0.09),  position = position_dodge(0.9), vjust = 0.1)



# DEFICIENCIA FISICA DOS ADMITIDOS
caged_TO_DFA1 <- caged_janeiro %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(tipodedeficiência) 

caged_TO_DFA2 <- caged_fevereiro %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(tipodedeficiência) 

caged_TO_DFA3 <- caged_março %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(tipodedeficiência) 

caged_TO_DFA4 <- caged_Abril %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(tipodedeficiência) 

caged_TO_DFA5 <- caged_Maio %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(tipodedeficiência) 

caged_TO_DFA6 <- caged_Junho %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(tipodedeficiência) 

caged_TO_DFA7 <- caged_Julho %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(tipodedeficiência) 

caged_TO_DFA8 <- caged_Agosto %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(tipodedeficiência) 

caged_TO_DFA9 <- caged_Setembro %>%
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>%
  count(tipodedeficiência) 


# DEFICIENCIA FISICA DOS DEMITIDOS
caged_TO_DFD1 <- caged_data_janeiro %>%
  filter(uf == 17) %>% 
  filter(saldomovimenta??o == -1) %>%
  count(tipodedefici?ncia) 

caged_TO_DFD2 <- caged_data_fevereiro %>%
  filter(uf == 17) %>% 
  filter(saldomovimenta??o == -1) %>%
  count(tipodedefici?ncia) 

caged_TO_DFD3 <- caged_data_mar?o %>%
  filter(uf == 17) %>% 
  filter(saldomovimenta??o == -1) %>%
  count(tipodedefici?ncia)

caged_TO_DFD3 <- caged_data_mar?o %>%
  filter(uf == 17) %>% 
  filter(saldomovimenta??o == -1) %>%
  count(tipodedefici?ncia)


#Cargos 

caged_cargosA <- caged_janeiro %>% 
  filter(uf == 17) %>% 
  filter(saldomovimentação == 1) %>% 
  select %in% c(cbo2002ocupação, salário) %>% 
  count(salário)


#gráfico - Tocantins


Lala<- data.frame("Empregos" = 1:9, "Saldo" = c(199,1482,-86,-2850,-1251,1240,1810,2096,1790), "Meses" = c("Janeiro","Fevereiro", "Março", "Abril", "Maio", "Junho", "Julho", "Agosto", "Setembro"))
arrange(Lala)



Lala %>%
  mutate(Meses = factor(Meses, levels=c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho", "Julho", "Agosto", "Setembro"))) %>%
  ggplot(aes(x=Meses, y = Saldo)) + 
  geom_bar(stat='identity', colour = ifelse(Lala$Saldo > 0, "#104E8B", "#8b1010"), fill = ifelse(Lala$Saldo > 0, "#104E8B", "#8B1010")) + theme_classic() +
  geom_hline(yintercept=0, colour="black", linetype="dashed") + 
  scale_y_continuous(limits = c(-4500, 4500), breaks = seq(from = -4000, to = 4000, by = 1000)) + 
  theme(plot.title = element_text(hjust = 0.5, size = 13)) +
  labs(x= " ") +
  geom_text(aes(label = Saldo, y = Saldo + 0.05),  position = position_dodge(0.9), vjust = 0.1)

#Gráfico - Norte.

Norte <- data.frame("Empregos" = 1:9, "Saldo" = c(2764,10419,-6370,-30747,-11146,6093,16015,22483,20640), "Meses" = c("Janeiro","Fevereiro", "Março", "Abril", "Maio", "Junho", "Julho", "Agosto", "Setembro"))

#Gráfico

Norte %>%
  mutate(Meses = factor(Meses, levels=c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho", "Julho", "Agosto", "Setembro"))) %>%
  ggplot(aes(x=Meses, y = Saldo)) + 
  geom_bar(stat='identity', colour = ifelse(Norte$Saldo > 0, "#104E8B", "#8b1010"), fill = ifelse(Norte$Saldo > 0, "#104E8B", "#8B1010")) + 
  theme_classic()  + 
  geom_hline(yintercept=0, colour="black", linetype="dashed") + 
  geom_text(aes(label = Saldo, y = Saldo + 0.05),  position = position_dodge(0.9), vjust = 0.1) +
  scale_y_continuous(limits = c(-35000, 25000), breaks = seq(from = -35000, to = 25000, by = 10000)) + 
  theme(plot.title = element_text(hjust = 0.5, size = 13)) +  labs(x= " ") 



#Download dos Dados Caged - 2019.


repository_update_CAGED <- function() {
  url_path <- "ftp://ftp.mtps.gov.br/pdet/microdados/CAGED/"
  RCurl::getURL(url_path) %>% 
    strsplit(., "\n") %>% 
    {
      .[[1]] %>% 
        strsplit(x = ., split = "<DIR>") %>% 
        lapply(X = ., 
               FUN = function(x) {dplyr::tibble("name_path" = suppressWarnings(as.numeric(x[2])), 
                                                "date_update" = as.Date(x[1], tryFormats = c("%m-%d-%y")))}) %>% 
        dplyr::bind_rows() %>% 
        dplyr::filter(!is.na(`name_path`)) %>% 
        dplyr::mutate("url_path" = paste0(`url_path`, `name_path`),
                      "date_check" = Sys.Date()) %>% 
        dplyr::select(`url_path`, `name_path`, `date_update`, `date_check`)
    }
}


available_CAGED <- function(m = "01", y = "2019") {
  url_path <- "ftp://ftp.mtps.gov.br/pdet/microdados/CAGED"
  url_file <- file.path(url_path, y, paste0("CAGEDEST_", m, y, ".7z"))
  check_file <- RCurl::url.exists(url_file)
  if(check_file) {
    cat("\n Dados e endere?o dispon?vel! Ref.:", paste(m, y, sep = "/"), "\n")
  } else {
    cat("\n Dados OU endere?o INDISPON?VEL! M?s/Ano:", paste(m, y, sep = "/"), "\n")
  }
  invisible(check_file)
}


download_CAGED <- function(m = "12", y = "2019", dir.output = ".") {
  m <- if (is.null(m)) { format(Sys.Date(), "%m") } else { m }
  y <- if (is.null(y)) { format(Sys.Date(), "%Y") } else { y }
  
  url_path <- "ftp://ftp.mtps.gov.br/pdet/microdados/CAGED"
  url_file <- file.path(url_path, y, paste0("CAGEDEST_", m, y, ".7z"))
  dir_file <- file.path(dir.output, paste("CAGEDEST_", m, y, ".7z", sep = ""))
  
  check_file <- available_CAGED(m, y)
  if (check_file) {
    utils::download.file(url = url_file, destfile = dir_file, mode = "wb")
  } else {
    stop("\n Erro ao realizar download do arquivo! \n")
  }
  
  invisible(list(check_file = check_file,
                 dir_file   = dir_file))
}


download_CAGED(m="12", y="2019")




for (i in 1:5) {
  
  txt_file <- str_c('data/raw/CAGEDMOV20200', i, '.txt')
  
  Dados_Caged <-
    read_delim(file = "CAGEDEST_112019.txt",
               delim = ";",
               trim_ws = TRUE,
               escape_double = FALSE,
               locale = locale(
                 decimal_mark = ",",
                 grouping_mark = ".",
                 encoding = "ISO-8859-1"
               )
    )
}



Dados_tocantins <- Dados_Caged %>% 
  count(`Admitidos/Desligados`)

#Leitura de Dados Tocantins - Janeiro

Dados_Tocantins_Janeiro <- Dados_Caged %>%
  filter(UF==17) %>% 
  count(`Admitidos/Desligados`)

#Leitura de Dados Norte - Janeiro

Dados_Norte_Janeiro <- Dados_Caged %>% 
  filter(UF %in% c(17,11,12,13,14,15,16)) %>% 
  count(`Admitidos/Desligados`)


#Gr?ficos 

Saldo_Total_TO <-  data.frame("Empregos" = 1:6, "Saldo" = c(324,141,1226,-1336,1595,-2861), "Trimestres" = c("1T-2019","2T-2019", "3T-2019", "4T-2019", "1T-2020","2T-2020"))
Saldo_TotalN <-  data.frame("Empregos" = 1:6, "Saldo" = c(-8175,11204,27053,-5456,6813,-35800), "Trimestres" = c("1T-2019","2T-2019", "3T-2019", "4T-2019", "1T-2020", "2T-2020"))



Saldo_Total_TO %>%
  mutate(Trimestres = factor(Trimestres, levels=c("1T-2019", "2T-2019", "3T-2019", "4T-2019", "1T-2020", "2T-2020"))) %>%
  ggplot(aes(x=Trimestres, y = Saldo)) + 
  geom_bar(stat='identity', colour = ifelse(Saldo_Total_TO$Saldo > 0, "#104E8B", "#8b1010"), fill = ifelse(Saldo_Total_TO$Saldo > 0, "#104E8B", "#8b1010")) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),) + 
  geom_hline(yintercept=0, colour="black", linetype="dashed")+ 
  theme_classic() + 
  scale_y_continuous(limits = c(-3000, 2500)) + 
  labs(x = " ") +
  theme(plot.title = element_text(hjust = 0.5, size = 13)) + geom_text(aes(label = Saldo, y = Saldo + 0.05),  position = position_dodge(0.9), vjust = 0.1)


Saldo_TotalN %>%
  mutate(Trimestres = factor(Trimestres, levels=c("1T-2019", "2T-2019", "3T-2019", "4T-2019", "1T-2020", "2T-2020"))) %>%
  ggplot(aes(x=Trimestres, y = Saldo)) + 
  geom_bar(stat='identity', colour = ifelse(Saldo_TotalN$Saldo > 0, "#104E8B", "#8b1010"), fill = ifelse(Saldo_TotalN$Saldo > 0, "#104e8b", "#8B1010")) + 
  theme_classic() +
  geom_hline(yintercept=0, colour="black", linetype="dashed") + 
  scale_y_continuous(limits = c(-50000, 50000)) + 
  labs(x = " ") + 
  theme(plot.title = element_text(hjust = 0.5, size = 13)) + geom_text(aes(label = Saldo, y = Saldo + 0.05),  position = position_dodge(0.9), vjust = 0.1)


#Desemprego

dadosPNADc_brutos <- get_pnadc(year = 2019, quarter = 4, vars = c("UF","VD4001", "VD4002"), design = TRUE)


taxa_Desemprego <-svyby(~VD4002 == "Pessoas desocupadas", ~interaction(UF=="Tocantins"), dadosPNADc_brutos, svyratio, denominator = ~VD4001 == "Pessoas na for?a de trabalho", na.rm = T, vartype = "cv")


cv(taxa_Desemprego)
confint(taxa_Desemprego)

# TAXA DE DESEMPREGO 01/18

dadosPNADc_brutos <- get_pnadc(year = 2018, quarter = 1, vars = c("UF","VD4001", "VD4002"), design = TRUE)

txDesemprego1_18 <-svyby(~VD4002 == "Pessoas desocupadas", ~interaction(UF == "Tocantins"), dadosPNADc_brutos, svyratio, denominator = ~VD4001 == "Pessoas na for?a de trabalho", na.rm = T, vartype = "cv")

cv(txDesemprego1_18)
confint(txDesemprego1_18)



# TAXA DE DESEMPREGO 02/18

dadosPNADc_brutos <- get_pnadc(year = 2018, quarter = 2, vars = c("UF","VD4001", "VD4002"), design = TRUE)

txDesemprego2_18 <-svyby(~VD4002 == "Pessoas desocupadas", ~interaction(UF == "Tocantins"), dadosPNADc_brutos, svyratio, denominator = ~VD4001 == "Pessoas na for?a de trabalho", na.rm = T, vartype = "cv")

cv(txDesemprego2_18)
confint(txDesemprego2_18)


# TAXA DE DESEMPREGO 03/18

dadosPNADc_brutos <- get_pnadc(year = 2018, quarter = 3, vars = c("UF","VD4001", "VD4002"), design = TRUE)

txDesemprego3_18 <-svyby(~VD4002 == "Pessoas desocupadas", ~interaction(UF == "Tocantins"), dadosPNADc_brutos, svyratio, denominator = ~VD4001 == "Pessoas na for?a de trabalho", na.rm = T, vartype = "cv")

cv(txDesemprego3_18)
confint(txDesemprego3_18)



# TAXA DE DESEMPREGO 04/18

dadosPNADc_brutos <- get_pnadc(year = 2018, quarter = 4, vars = c("UF","VD4001", "VD4002"), design = TRUE)

txDesemprego4_18 <-svyby(~VD4002 == "Pessoas desocupadas", ~interaction(UF == "Tocantins"), dadosPNADc_brutos, svyratio, denominator = ~VD4001 == "Pessoas na for?a de trabalho", na.rm = T, vartype = "cv")

cv(txDesemprego4_18)
confint(txDesemprego4_18)



# TAXA DE DESEMPREGO 01/19

dadosPNADc_brutos <- get_pnadc(year = 2019, quarter = 1, vars = c("UF","VD4001", "VD4002"), design = TRUE)

txDesemprego1_19 <-svyby(~VD4002 == "Pessoas desocupadas", ~interaction(UF == "Tocantins"), dadosPNADc_brutos, svyratio, denominator = ~VD4001 == "Pessoas na for?a de trabalho", na.rm = T, vartype = "cv")

cv(txDesemprego1_19)
confint(txDesemprego1_19)



# TAXA DE DESEMPREGO 02/19

dadosPNADc_brutos <- get_pnadc(year = 2019, quarter = 2, vars = c("UF","VD4001", "VD4002"), design = TRUE)

txDesemprego2_19 <-svyby(~VD4002 == "Pessoas desocupadas", ~interaction(UF == "Tocantins"), dadosPNADc_brutos, svyratio, denominator = ~VD4001 == "Pessoas na for?a de trabalho", na.rm = T, vartype = "cv")

cv(txDesemprego2_19)
confint(txDesemprego2_19)



# TAXA DE DESEMPREGO 03/19

dadosPNADc_brutos <- get_pnadc(year = 2019, quarter = 3, vars = c("UF","VD4001", "VD4002"), design = TRUE)

txDesemprego3_19 <-svyby(~VD4002 == "Pessoas desocupadas", ~interaction(UF == "Tocantins"), dadosPNADc_brutos, svyratio, denominator = ~VD4001 == "Pessoas na for?a de trabalho", na.rm = T, vartype = "cv")

cv(txDesemprego3_19)
confint(txDesemprego3_19)



# TAXA DE DESEMPREGO 04/19

dadosPNADc_brutos <- get_pnadc(year = 2019, quarter = 4, vars = c("UF","VD4001", "VD4002"), design = TRUE)

txDesemprego4_19 <-svyby(~VD4002 == "Pessoas desocupadas", ~interaction(UF == "Tocantins"), dadosPNADc_brutos, svyratio, denominator = ~VD4001 == "Pessoas na for?a de trabalho", na.rm = T, vartype = "cv")

cv(txDesemprego4_19)
confint(txDesemprego4_19)



# TAXA DE DESEMPREGO 01/20

dadosPNADc_brutos <- get_pnadc(year = 2020, quarter = 1, vars = c("UF","VD4001", "VD4002"), design = TRUE)

txDesemprego1_20 <-svyby(~VD4002 == "Pessoas desocupadas", ~interaction(UF == "Tocantins"), dadosPNADc_brutos, svyratio, denominator = ~VD4001 == "Pessoas na for?a de trabalho", na.rm = T, vartype = "cv")

cv(txDesemprego1_20)
confint(txDesemprego1_20)



# TAXA DE DESEMPREGO 02/20

dadosPNADc_brutos <- get_pnadc(year = 2020, quarter = 2, vars = c("UF","VD4001", "VD4002"), design = TRUE)

txDesemprego2_20 <-svyby(~VD4002 == "Pessoas desocupadas", ~interaction(UF == "Tocantins"), dadosPNADc_brutos, svyratio, denominator = ~VD4001 == "Pessoas na força de trabalho", na.rm = T, vartype = "cv")

cv(txDesemprego2_20)
confint(txDesemprego2_20)

# TAXA DE DESEMPREGO 03/20

dadosPNADc_brutos <- get_pnadc(year = 2020, quarter = 3, vars = c("UF","VD4001", "VD4002"), design = TRUE)

txDesemprego2_20 <-svyby(~VD4002 == "Pessoas desocupadas", ~interaction(UF == "Tocantins"), dadosPNADc_brutos, svyratio, denominator = ~VD4001 == "Pessoas na força de trabalho", na.rm = T, vartype = "cv")

cv(txDesemprego2_20)
confint(txDesemprego2_20)

#Usando o IpeadataR

taxa_desemprego <- series_ipeadata(1847514623, periodicity = c("Q"),save = "")

data<-ipeadata(c("PNADC_TXDES_UF"))

data_populaçãoOCUP <- ipeadata(c("PNADC_POPOCUP_UF"))

dados_TO1 <- data_populaçãoOCUP %>%  
  filter(tcode == 17)

dados_BR <- data %>% 
  filter(tcode == 0)

dados_to  <- data %>% 
  filter(tcode == 17)

write.csv2(dados_to, file = "Taxa de desemprego - TO.csv")

dados_to %>% 
  ggplot(aes(x = date, y = value, group = 1))  +
  geom_line(size = 0.9, colour = "#4682B4")  +  
  theme_classic() + 
  scale_y_continuous(limits = c(0, 15), breaks = seq(from = -0, to = 15, by = 3)) + 
  labs( x= "Período" , y= "Taxa") + 
  theme(plot.title = element_text(hjust = 0.5, size = 13)) + 
  geom_rect(  xmin = as.Date("2019-01-01"),
              xmax = as.Date("2019-04-01"),
              ymin = -Inf,
              ymax = Inf,
              # deixar o retangulo mais transparente
              alpha = 0.01) +
  geom_rect(xmin = as.Date("2019-10-01"),
            xmax = as.Date("2020-01-01"),
            ymin = -Inf,
            ymax = Inf,
            # deixar o retangulo mais transparente
            alpha = 0.01) + 
  geom_text(aes(label = value))

dados_BR %>% 
  ggplot(aes(x = date, y = value, group = 1))  +
  geom_line(size = 0.9, colour = "#4682B4")  +  
  theme_classic() + 
  scale_y_continuous(limits = c(0, 15), breaks = seq(from = -0, to = 15, by = 3)) + 
  labs(title="Taxa de Desemprego - TO", x= "Período" , y= "Taxa",  caption = "Fonte: IBGE \n Elaborado por: PET Economia") + 
  theme(plot.title = element_text(hjust = 0.5, size = 13)) + 
  geom_rect(  xmin = as.Date("2019-01-01"),
              xmax = as.Date("2019-04-01"),
              ymin = -Inf,
              ymax = Inf,
              # deixar o retangulo mais transparente
              alpha = 0.01) +
  geom_rect(xmin = as.Date("2019-10-01"),
            xmax = as.Date("2020-01-01"),
            ymin = -Inf,
            ymax = Inf,
            # deixar o retangulo mais transparente
            alpha = 0.01) 
#Trabalho Parcial ou Integral - Janeiro. Admitidos/Desligados - Tocantins


Trab_ParcialouIntegral <- caged_janeiro %>% 
  filter(uf==17) %>% 
  filter(saldomovimentação == 1) %>% 
  count(indtrabparcial)

Trab_ParcialouIntegralNorte <- caged_data_janeiro %>% 
  filter(regi?o == 1) %>% 
  filter(saldomovimenta??o == 1) %>% 
  count(indtrabparcial)



Trab_ParcialouIntegral <- caged_data_janeiro %>% 
  filter(uf==17) %>% 
  filter(saldomovimenta??o == -1) %>% 
  count(indtrabparcial)

Trab_ParcialouIntegralNorte <- caged_data_janeiro %>% 
  filter(regi?o == 1) %>% 
  filter(saldomovimenta??o == 1) %>% 
  count(indtrabparcial)


#Trabalho Parcial ou Integral - Fevereiro - Tocantins e Regi?o Norte


Trab_ParcialouIntegral2 <- caged_data_fevereiro %>% 
  filter(uf==17) %>% 
  filter(saldomovimenta??o == 1) %>% 
  count(indtrabparcial)

Trab_ParcialouIntegralNorte2 <- caged_data_fevereiro %>% 
  filter(regi?o == 1) %>% 
  filter(saldomovimenta??o == 1) %>% 
  count(indtrabparcial)

Trab_ParcialouIntegral2 <- caged_data_fevereiro %>% 
  filter(uf==17) %>% 
  filter(saldomovimenta??o == -1) %>% 
  count(indtrabparcial)

Trab_ParcialouIntegralNorte2 <- caged_data_fevereiro %>% 
  filter(regi?o==1) %>% 
  filter(saldomovimenta??o == 1) %>% 
  count(indtrabparcial)


#Trabalho Parcial ou Integral - Mar?o - Tocantins

Trab_ParcialouIntegral3 <- caged_data_mar?o %>% 
  filter(uf==17) %>% 
  filter(saldomovimenta??o == 1) %>% 
  count(indtrabparcial)

Trab_ParcialouIntegralNorte3 <- caged_data_fevereiro %>% 
  filter(regi?o==1) %>% 
  filter(saldomovimenta??o == 1) %>% 
  count(indtrabparcial)

Trab_ParcialouIntegral3 <- caged_data_mar?o%>% 
  filter(uf==17) %>% 
  filter(saldomovimenta??o == -1) %>% 
  count(indtrabparcial)

Trab_ParcialouIntegralNorte3 <- caged_data_fevereiro %>% 
  filter(regi?o==1) %>% 
  filter(saldomovimenta??o == 1) %>% 
  count(indtrabparcial)

#Categoria do Trabalhador - Tocantins ou Regi?o Norte - Janeiro

categoriaTrabalhadores <- caged_Janeiro %>% 
  filter(uf==17) %>% 
  count(cbo2002ocupação) %>% 
  filter(n >= 300)

#Categoria do Trabalhador - Tocantins - Fevereiro
categoriaTrabalhadores2 <- caged_Fevereiro %>% 
  filter(uf==17) %>% 
  filter(saldomovimentação == 1) %>% 
  count(cbo2002ocupação) %>% 
  filter(n >= 350)


#Categoria do Trabalhador - Tocantins - Mar?o

CategoriasTrabalhadores3 <- caged_Março %>% 
  filter(uf==17) %>% 
  filter(saldomovimentação == 1) %>% 
  count(cbo2002ocupação) %>% 
  filter(n >= 350)

CategoriasTrabalhadores3 <- caged_Abril %>% 
  filter(uf==17) %>% 
  filter(saldomovimentação == 1) %>% 
  count(cbo2002ocupação) %>% 
  filter(n >= 350)


CategoriasTrabalhadores3 <- caged_Maio %>% 
  filter(uf==17) %>% 
  filter(saldomovimentação == 1) %>% 
  count(cbo2002ocupação) %>% 
  filter(n >= 350)

CategoriasTrabalhadores3 <- caged_Junho %>% 
  filter(uf==17) %>% 
  filter(saldomovimentação == 1) %>% 
  count(cbo2002ocupação) %>% 
  filter(n >= 350)

CategoriasTrabalhadores3 <- caged_Julho %>% 
  filter(uf==17) %>% 
  filter(saldomovimentação == 1) %>% 
  count(cbo2002ocupação) %>% 
  filter(n >= 350)

CategoriasTrabalhadores3 <- caged_Agosto %>% 
  filter(uf==17) %>% 
  filter(saldomovimentação == 1) %>% 
  count(cbo2002ocupação) %>% 
  filter(n >= 350)

CategoriasTrabalhadores3 <- caged_Setembro %>% 
  filter(uf==17) %>% 
  filter(saldomovimentação == 1) %>% 
  count(cbo2002ocupação) %>% 
  filter(n >= 350)


#Join dos dados

#Pedidos de Seguro Desemprego 

y <- c(9833, 9833, 8832, 8784, 9642, 9324, 9155, 8182, 8785, 10642)
x <- c("1T-2018","2T-2018", "3T-2018", "4T-2018", "1T-2019","2T-2019", "3T-2019", "4T-2019","1T-2020","2T-2020")

dados <- data.frame(x,y)


dados %>% 
  mutate(x = factor(x, levels=c("1T-2018","2T-2018", "3T-2018", "4T-2018", "1T-2019","2T-2019", "3T-2019", "4T-2019","1T-2020","2T-2020"))) %>%
  ggplot(aes(x=x, y = y, group = 1)) + geom_line(size = 0.9, colour = "#4682B4")+
  theme_classic()   + 
  scale_y_continuous(limits = c(3000, 15000), breaks = seq(from = 0, to = 15000, by = 5000)) + 
  labs(title="Pedidos de Seguro desemprego - TO", x= "Per?odo", y= "Seguro Desemprego",  caption = "Fonte: Minist?rio do Trabalho \n Elaborado por: PET Economia") + theme(plot.title = element_text(hjust = 0.5, size = 13))


dados %>% 
  mutate(x = factor(x, levels=c("1T-2018","2T-2018", "3T-2018", "4T-2018", "1T-2019","2T-2019", "3T-2019", "4T-2019","1T-2020","2T-2020"))) %>%
  ggplot(aes(x=x, y = y, group = 1)) +
  geom_bar(stat = "identity", width = 0.5, fill = "#4682B4")+
  theme_classic()   + 
  scale_y_continuous(limits = c(0, 15000), breaks = seq(from = 0, to = 13000, by = 2500)) + 
  labs( x= " ", y= "Seguro Desemprego") + 
  theme(plot.title = element_text(hjust = 0.5, size = 13)) +
  geom_text(aes(label = y, y = y + 1),  position = position_dodge(0.9), vjust = 0.1)



#População ocupada


data_populaçãoOCUP <- ipeadata(c("PNADC_TXPARTFT_UF"))

Dados_TOCANTINS <- data_populaçãoOCUP %>% 
  filter(tcode == 17)

Dados_TOCANTINS %>% 
  ggplot(aes(x = date, y = value, group = 1))  +
  geom_line(size = 0.9, colour = "#4682B4")  +  
  theme_classic() + 
  scale_y_continuous(limits = c(0, 70), breaks = seq(from = -0, to = 70, by = 20)) + 
  labs(x= " " , y= "Taxa") + 
  theme(plot.title = element_text(hjust = 0.5, size = 13)) + 
  geom_rect(  xmin = as.Date("2019-01-01"),
              xmax = as.Date("2019-04-01"),
              ymin = -Inf,
              ymax = Inf,
              # deixar o retangulo mais transparente
              alpha = 0.01) +
  geom_rect(xmin = as.Date("2019-10-01"),
            xmax = as.Date("2020-01-01"),
            ymin = -Inf,
            ymax = Inf,
            # deixar o retangulo mais transparente
            alpha = 0.01) 

#Rendimento médio





rendimento_TO <- ipeadata(c("PNADC_RENDREALPRINCHAB_UF"))

rendimento_tocantins <- rendimento_TO %>% 
  filter(tcode == 17)

rendimento_br <- rendimento_TO %>% 
  filter(tcode == 0)

rendimento_norte <- rendimento_TO %>% 
  filter(tcode == 1)


rendimento_tocantins %>% 
  ggplot(aes(x = date, y = value, group = 1))  +
  geom_line(size = 0.9, colour = "#4682B4")  +  
  theme_classic() + 
  scale_y_continuous(limits = c(0, 3500), breaks = seq(from = -0, to = 3500, by = 500)) + 
  labs(x= " " , y= "Rendimento") + 
  theme(plot.title = element_text(hjust = 0.5, size = 13)) + 
  geom_rect(  xmin = as.Date("2019-01-01"),
              xmax = as.Date("2019-04-01"),
              ymin = -Inf,
              ymax = Inf,
              # deixar o retangulo mais transparente
              alpha = 0.01) +
  geom_rect(xmin = as.Date("2019-10-01"),
            xmax = as.Date("2020-01-01"),
            ymin = -Inf,
            ymax = Inf,
            # deixar o retangulo mais transparente
            alpha = 0.01) 


rendimento_br %>% 
  ggplot(aes(x = date, y = value, group = 1))  +
  geom_line(size = 0.9, colour = "#4682B4")  +  
  theme_classic() + 
  scale_y_continuous(limits = c(0, 3500), breaks = seq(from = -0, to = 3500, by = 500)) + 
  labs(x= " " , y= "Rendimento") + 
  theme(plot.title = element_text(hjust = 0.5, size = 13)) + 
  geom_rect(  xmin = as.Date("2019-01-01"),
              xmax = as.Date("2019-04-01"),
              ymin = -Inf,
              ymax = Inf,
              # deixar o retangulo mais transparente
              alpha = 0.01) +
  geom_rect(xmin = as.Date("2019-10-01"),
            xmax = as.Date("2020-01-01"),
            ymin = -Inf,
            ymax = Inf,
            # deixar o retangulo mais transparente
            alpha = 0.01) 


rendimento_norte %>% 
  ggplot(aes(x = date, y = value, group = 1))  +
  geom_line(size = 0.9, colour = "#4682B4")  +  
  theme_classic() + 
  scale_y_continuous(limits = c(0, 3500), breaks = seq(from = -0, to = 3500, by = 500)) + 
  labs(x= " " , y= "Rendimento") + 
  theme(plot.title = element_text(hjust = 0.5, size = 13)) + 
  geom_rect(  xmin = as.Date("2019-01-01"),
              xmax = as.Date("2019-04-01"),
              ymin = -Inf,
              ymax = Inf,
              # deixar o retangulo mais transparente
              alpha = 0.01) +
  geom_rect(xmin = as.Date("2019-10-01"),
            xmax = as.Date("2020-01-01"),
            ymin = -Inf,
            ymax = Inf,
            # deixar o retangulo mais transparente
            alpha = 0.01) 





Setores <- data.frame("Empregos" = 1:3, "Saldo" = c(-254, -1810, -63), "Setores" = c("14-34", "35-64", "65+"))



Setores %>%
  mutate(Setores = factor(Setores, levels=c("14-34", "35-64", "65+"))) %>%
  ggplot(aes(x=Setores, y = Saldo)) + 
  geom_bar(stat='identity', width = 0.5, colour = ifelse(Setores$Saldo > 0, "#104E8B", "#8b1010"), fill = ifelse(Setores$Saldo > 0, "#104E8B", "#8B1010")) + theme_classic() +
  geom_hline(yintercept=0, colour="black", linetype="dashed") + 
  scale_y_continuous(limits = c(-2000, 2500),breaks = seq(from = -2000, to = 2500, by = 1000)) + 
  theme(plot.title = element_text(hjust = 0.5, size = 13)) + 
  labs(x=" ") +
  geom_text(aes(label = Saldo, y = Saldo + 2),  position = position_dodge(0.9), vjust = 0.1)




#PNAD COVID


PNADCovid <- read_xlsx("PNADCOVID.xlsx")


