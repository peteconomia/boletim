library(sidrar)
library(tidyverse)
library(petgg)

petgg::set_theme(base_family = "EB Garamond")

pmc <- get_sidra(api = "/t/3416/n1/all/n3/17/v/564/p/last%2011/c11046/33534/d/v564%201") %>% 
  mutate(date = parse_date(`Mês (Código)`, format = '%Y%m')) %>% 
  select(`Brasil e Unidade da Federação`, Valor, date)

pmc_plot <- ggplot(pmc, aes(x=date, y = Valor, fill = `Brasil e Unidade da Federa??o`))+
  geom_bar(stat="identity", position = position_dodge(width = 25), width = 25)+
  scale_x_date(date_labels="%b",date_breaks  ="1 month")+
  geom_text(aes(label = paste(Valor),vjust = ifelse(Valor >= 0, -0.3, 1)),
            position = position_dodge(width = 25),size=3, show.legend = FALSE)+
  scale_y_continuous(labels = function(n) paste0(n, "%"))
pmc_plot
