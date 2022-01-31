library(devtools)
library(tidyverse)
library(sidrar)
library(RColorBrewer)
library(petgg)
library(scales)

petgg::set_theme(base_family = "EB Garamond")
my_colors <- RColorBrewer::brewer.pal(8, "Blues")

#Importando dados de pib (taxa acumulada de quatro trimestres 
#e em rela??o ao trimestre anterior)

pib <- get_sidra(5932, 
                 variable = 6562, 
                 period = c("201902-202003"), 
                 geo = "Brazil",
                 classific = 'c11255', 
                 header = TRUE, 
                 format = 1, 
                 digits = "default")

pib_an <- get_sidra(5932, variable = 6564, 
                      period = c("202001-202003"), 
                      geo = "Brazil",
                      classific = 'c11255', 
                      header = TRUE,
                      format = 1, 
                      digits = "default")

pib <- rename(pib,
              Trimestre = `Trimestre (Código)`,
              Setores_e_subsetores = `Setores e subsetores (Código)`,
              Variável = `Variável (Código)`)

pib_an <- rename(pib_an,
              Trimestre = `Trimestre (Código)`,
              Setores_e_subsetores = `Setores e subsetores (Código)`,
              Variável = `Variável (Código)`)

#Separando dados PIB
var_pib <- pib %>% 
  filter(Setores_e_subsetores == '90707')
         
var_pib_an <- pib_an %>% 
  filter(Setores_e_subsetores == '90707')

#Gr?fico PIB
graf_pib_ac <- ggplot(var_pib, aes(x=Trimestre, y = Valor))+
  geom_bar(stat="identity", position = "dodge")+
  #theme(axis.text.y=element_blank())+ylab("")+
  scale_x_discrete(labels=c("201902"="2T/2019","201903"="3T/2019",
                            "201904"="4T/2019","202001"="1T/2020", 
                            "202002"="2T/2020","202003"="3T/2020"))+
  geom_text(aes(label = paste(Valor),vjust = ifelse(Valor >= 0, -0.3, 1)),
            position = position_dodge(width = 0.7),size=4, show.legend = FALSE)+
  labs(caption = "Fonte: IBGE")
graf_pib_ac

#Varia??o do total do PIB em rela??o ao trimestre anterior(dessazonalizado)
graf_pib_an <- ggplot(var_pib_an, aes(x=Trimestre, y = Valor))+
  geom_bar(stat="identity")+
  scale_y_continuous(labels = function(n) paste0(n, "%"))+
  scale_x_discrete(labels=c("202001"="1T","202002"="2T",
                            "202003"="3T"))+
  geom_text(aes(label = paste(Valor),vjust = ifelse(Valor >= 0, -0.3, 1)),
            position = position_dodge(width = 0.7),size=4, show.legend = FALSE)
graf_pib_an

#Separando dados da DA
da <- pib %>% 
  filter(Trimestre == '202001'
         |Trimestre == '202002'
         |Trimestre == '202003')
da <- da %>%
  filter(Setores_e_subsetores == '93404' 
         |Setores_e_subsetores == '93405'
         | Setores_e_subsetores == '93406' 
         | Setores_e_subsetores == '93407' 
         | Setores_e_subsetores == '93408')

new_da <- da %>% 
  group_by(Setores_e_subsetores)

da_an <- pib_an %>% 
  filter(Trimestre == '202001'
         |Trimestre == '202002'
         |Trimestre == '202003')
da_an <- da_an %>%
  filter(Setores_e_subsetores == '93404' 
         |Setores_e_subsetores == '93405'
         | Setores_e_subsetores == '93406' 
         | Setores_e_subsetores == '93407' 
         | Setores_e_subsetores == '93408')

new_da_an <- da_an %>% 
  group_by(Setores_e_subsetores)

#Gr?fico DA
graf_da <- ggplot(new_da, aes(x=Setores_e_subsetores, y = Valor, fill = Trimestre))+
  geom_bar(stat="identity", position = "dodge")+
  scale_x_discrete(labels=c("93404" = "Consumo\ndas famílias","93405" = "Consumo\ndo governo", 
                            "93406" = "Investimentos", "93407" = "Exportações", "93408" = "Importações"))+
  #theme(axis.text.y=element_blank())+ylab("")+ 
  scale_fill_manual(labels = c("1T", "2T", "3T"), 
                    values = c("#9ECAE1","#4292C6","#084594"))+
  geom_text(aes(label = paste(Valor),vjust = ifelse(Valor >= 0, -0.2, 1)), show.legend = F)
graf_da

graf_da_an <- ggplot(new_da_an, aes(x=Setores_e_subsetores, y = Valor, fill = Trimestre))+
  geom_bar(stat="identity", position = "dodge", gap="m")+
  scale_y_continuous(labels = function(x) paste0(x,"%")) +
  scale_x_discrete(labels=c("93404" = "Consumo\ndas famílias","93405" = "Consumo\ndo governo", 
                            "93406" = "Investimentos", "93407" = "Exportações", "93408" = "Importações"))+
  scale_fill_manual(labels = c("1T", "2T", "3T"), 
                    values = petgg::colors[1:3])+
  geom_text(aes(label = paste(Valor),vjust = ifelse(Valor >= 0, -0.2, 1)),
            position = position_dodge(width = 0.7), show.legend = FALSE, size = 3.6)
graf_da_an

#Separando dados da Oferta
oferta <- pib %>% 
  filter(Trimestre == '202001'
         |Trimestre == '202002'
         |Trimestre == '202003')

oferta <- oferta %>% 
  filter(Setores_e_subsetores == '90687' 
         |Setores_e_subsetores == '90691'
         | Setores_e_subsetores == '90696')

new_oferta <- oferta %>% 
  group_by(Setores_e_subsetores)

oferta_an <- pib_an %>% 
  filter(Trimestre == '202001'
         |Trimestre == '202002'
         |Trimestre == '202003')

oferta_an <- oferta_an %>% 
  filter(Setores_e_subsetores == '90687' 
         |Setores_e_subsetores == '90691'
         | Setores_e_subsetores == '90696')

new_oferta_an <- oferta_an %>% 
  group_by(Setores_e_subsetores)

#Gr?ficos oferta
graf_oferta <- ggplot(new_oferta, aes(x=Setores_e_subsetores, y = Valor, fill = Trimestre))+
  geom_bar(stat="identity", position = "dodge")+
  scale_y_continuous(labels = function(x) paste0(x,"%"))+
  scale_x_discrete(labels=c("90687" = "Agropecuária","90691" = "Indústria", 
                            "90696" = "Serviços"))+
  scale_fill_manual(labels = c("1T/2020", "2T/2020", "3T/2020"), 
                    values = petgg::colors[1:3])+
  geom_text(aes(label = paste(Valor),vjust = ifelse(Valor >= 0, -0.3, 1)),
            position = position_dodge(width = 0.7),size=3.6, show.legend = FALSE)
graf_oferta

graf_oferta_an <- ggplot(new_oferta_an, aes(x=Setores_e_subsetores, y = Valor, fill = Trimestre))+
  geom_bar(stat="identity", position = "dodge", gap ="m")+
  scale_y_continuous(labels = function(x) paste0(x,"%"))+
  scale_x_discrete(labels=c("90687" = "Agropecuária","90691" = "Indústria", 
                            "90696" = "Serviços"))+
  scale_fill_manual(labels = c("1T", "2T", "3T"), 
                    values = petgg::colors[1:3])+
  geom_text(aes(label = paste(Valor),vjust = ifelse(Valor >= 0, -0.3, 1)),
            position = position_dodge(width = 0.7),size=3.6, show.legend = FALSE)
graf_oferta_an

#Salvando os gr?ficos
#ggsave(filename = "C:/Users/lucas/OneDrive/?rea de Trabalho/Boletim/pib_ac.jpg", 
#      plot = graf_pib_ac)
ggsave(filename = "C:/Users/lucas/OneDrive/?rea de Trabalho/Instagram PET/pib_an.pdf", 
      plot = graf_pib_an)
#ggsave(filename = "C:/Users/lucas/OneDrive/?rea de Trabalho/Boletim/demanda.jpg", 
#       plot = graf_da)
#ggsave(filename = "C:/Users/lucas/OneDrive/?rea de Trabalho/Boletim/oferta.jpg", 
#       plot = graf_oferta)
ggsave(filename = "C:/Users/lucas/OneDrive/?rea de Trabalho/Instagram PET/demanda.jpg", 
       plot = graf_da_an)
ggsave(filename = "C:/Users/lucas/OneDrive/?rea de Trabalho/Instagram PET/oferta.jpg", 
       plot = graf_oferta_an)
