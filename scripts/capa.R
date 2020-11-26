# Script para gerar figura de fundo da capa
# Instalação do geobr <https://github.com/ipeaGIT/geobr>
library(geobr)
library(ggplot2)

# remove os eixos, grade e deixa o fundo transparent
config <- theme(
  panel.background = element_rect(fill = "transparent", color = NA),
  plot.background = element_rect(fill = "transparent", color = NA),
  axis.title=element_blank(),
  axis.text=element_blank(),
  axis.ticks=element_blank(),
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank()
)

# read_municipality é uma função do pacote geobr
all_muni_to <- read_municipality(code_muni = "TO", year= 2010)

ggplot() +
  geom_sf(data=all_muni_to, fill="#0969a5", color="#00609D", size=.15, show.legend = FALSE) +
  theme_minimal() +
  config +
  ggsave('../tex/logos/bg.pdf', height = 20, units = "cm") # salva como pdf
