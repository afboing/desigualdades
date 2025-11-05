#Lendo pacotes necessários
library(survey)
library(ggplot2)
library(dplyr)
library(tictoc)
library(foreign)
library(forcats)
library(tidyverse)

load("dados/pns2019.Rdata")

# Filtrando apenas os moradores selecionados para responder o questionário individual
# V0025A == 1: Morador de 15 anos ou mais selecionado para o questionário
pns2019.1 <- pns2019 %>% filter(V0025A == 1) 

moradores_selecionados <- pns2019 |> count(V0025A) |> filter(V0025A == 1) |> select(n)
soma_pesos <- sum(pns2019.1$V00291, na.rm = T)

# Criando o peso calibrado para os moradores selecionados
# V00291: Peso do morador selecionado (já calibrado pela pesquisa)
# 60202: Número total de moradores selecionados na amostra (soma de V0025A == 1)
# 145572211: Soma de V00291 para todos os selecionados, representando a população total estimada
pns2019.1<-pns2019.1 %>% mutate(peso_morador_selec=((V00291*(moradores_selecionados$n/soma_pesos))))

# Removendo valores NA na variável de peso ajustado
pns2019.1 <- pns2019.1 %>% filter(!is.na(peso_morador_selec))

pns2019.2 <- pns2019.1 |>
  select(C006, R015, R014, R01701, V00291, V0024, C008, VDF003, C009, UPA_PNS, peso_morador_selec) |>
  mutate(
    mamografia = case_when(
      R015 == 1 ~ 0,  # Se R015 for 1, atribui 0 ("Sim")
      R015 == 2 ~ 1,  # Se R015 for 2, atribui 1 ("Não")
      TRUE ~ NA_real_  # Mantém NAs
    ),
    mamografia = ifelse(R014 == 2, 1, mamografia),  # Se R014 == 2, define como 1
    mamografia2anos = mamografia,  # Criando a variável mamografia2anos a partir de mamografia
    
    # Se mamografia2anos for 0 e R01701 for 3, 4 ou 9, altera para 1
    mamografia2anos = ifelse(mamografia2anos == 0 & R01701 %in% c(3, 4, 9), 1, mamografia2anos),
    
    mamografia = factor(mamografia, levels = c(0, 1), labels = c("Sim", "Não")), # Define labels
    
    mamografia2anos = factor(mamografia2anos, levels = c(0, 1), labels = c("Sim", "Não")),
    
    cor_raca = case_when(
      C009 == 3 ~ NA_real_,  # Equivalente a .a (missing)
      C009 == 5 ~ NA_real_,  # Equivalente a .b (missing)
      C009 == 9 ~ NA_real_,  # Equivalente a .c (missing)
      C009 == 4 ~ 2,         # 4 → 2
      C009 == 2 ~ 3,         # 2 → 3
      TRUE ~ C009            # Mantém os demais valores inalterados
    ),
    
    cor_raca = factor(cor_raca, levels = c(1, 2, 3), labels = c("Branca", "Parda", "Preta")),
  )

#

# Resultados para mamografia

desPNSR2019=svydesign(id=~UPA_PNS, strat=~V0024, weight=~peso_morador_selec, nest=TRUE, data=pns2019.2)

# Criando um subconjunto do survey design com mulheres de 50 a 69 anos
desPNSR_5069_2019 <- subset(desPNSR2019, C006 == 2 & C008 >= 50 & C008 <= 69)

# Médias de mamografia ao menos uma vez na vida
resultado_mamografia_2019 <- svymean(~mamografia, design = desPNSR_5069_2019, na.rm = TRUE)

# Médias de mamografia nos ultimos 2 anos
resultado_mamografia_2019_2anos <- svymean(~mamografia2anos, design = desPNSR_5069_2019, na.rm = TRUE)

# Calculando os quintis de renda ponderados
quintis_renda_2019 <- svyquantile(~VDF003, design = desPNSR_5069_2019, quantiles = seq(0, 1, 0.2), na.rm = TRUE)

# Obtendo os pontos de corte dos quintis
quintil_cortes_2019 <- quintis_renda_2019$VDF003[,"quantile"]

# Criando a variável de quintil de renda dentro do survey design
desPNSR_5069_2019 <- update(
  desPNSR_5069_2019,
  quintil_renda = cut(VDF003, 
                      breaks = c(quintil_cortes_2019), 
                      labels = c("1º Quintil", "2º Quintil", "3º Quintil", "4º Quintil", "5º Quintil"), 
                      include.lowest = TRUE)
)


#  Calculando a proporção de mulheres que fizeram mamografia por quintil de renda
resultado_renda_2019 <- svyby(~mamografia, ~quintil_renda, design = desPNSR_5069_2019, svymean, na.rm = TRUE)

resultado_renda_2019_2anos <- svyby(~mamografia2anos, ~quintil_renda, design = desPNSR_5069_2019, svymean, na.rm = TRUE)

## Raça/cor

resultado_raca_2019 <- svyby(~mamografia, ~cor_raca, design = desPNSR_5069_2019, svymean, na.rm = TRUE)

resultado_raca_2019_2anos <- svyby(~mamografia2anos, ~cor_raca, design = desPNSR_5069_2019, svymean, na.rm = TRUE)

###

tabela_resultados_2019 <- data.frame(variavel = c("Populacao",
                                                  rep("Renda",5),
                                                  rep("Raca_cor",3)),
                                     categoria = c("total_populacao",
                                                   rownames(resultado_renda_2019),
                                                   rownames(resultado_raca_2019)
                                     )
) |>
  mutate(valor = c(resultado_mamografia_2019[1],
                   unlist(resultado_renda_2019[2]),
                   unlist(resultado_raca_2019[2])),
         Ano = 2019
  )

tabela_resultados_2019_2anos <- data.frame(variavel = c("Populacao",
                                                        rep("Renda",5),
                                                        rep("Raca_cor",3)),
                                           categoria = c("total_populacao",
                                                         rownames(resultado_renda_2019_2anos),
                                                         rownames(resultado_raca_2019_2anos)
                                           )
) |>
  mutate(valor = c(resultado_mamografia_2019_2anos[1],
                   unlist(resultado_renda_2019_2anos[2]),
                   unlist(resultado_raca_2019_2anos[2])),
         Ano = 2019
  )


tabela_resultados <- rbind(tabela_resultados_2013, tabela_resultados_2019)

tabela_resultados_2anos <- rbind(tabela_resultados_2013_2anos, tabela_resultados_2019_2anos)



tabela_resultados |>
  filter(categoria == "total_populacao") |>
  ggplot(aes(x = valor*100, y = categoria, color = factor(Ano))) +
  geom_point() +
  scale_x_continuous(limits = c(0, 100)) +
  theme_minimal()


tabela_resultados |>
  filter(variavel == "Renda") |>
  ggplot(aes(x = valor*100, y = categoria, color = factor(Ano))) +
  geom_point() +
  scale_x_continuous(limits = c(0, 100)) +
  xlab("Porcentagem") +
  ylab("Quintil de Renda Familiar per Capita") +
  theme_minimal()


tabela_resultados |>
  filter(variavel == "Raca_cor") |>
  ggplot(aes(x = valor*100, y = fct_rev(categoria), color = factor(Ano))) +
  geom_point() +
  scale_x_continuous(limits = c(0, 100)) +
  xlab("Porcentagem") +
  ylab("Raça/cor") +
  theme_minimal()

##

tabela_resultados_2anos |>
  filter(categoria == "total_populacao") |>
  ggplot(aes(x = valor*100, y = categoria, color = factor(Ano))) +
  geom_point() +
  scale_x_continuous(limits = c(0, 100)) +
  theme_minimal()


tabela_resultados_2anos |>
  filter(variavel == "Renda") |>
  ggplot(aes(x = valor*100, y = categoria, color = factor(Ano))) +
  geom_point() +
  scale_x_continuous(limits = c(0, 100)) +
  xlab("Porcentagem") +
  ylab("Quintil de Renda Familiar per Capita") +
  theme_minimal()


tabela_resultados_2anos |>
  filter(variavel == "Raca_cor") |>
  ggplot(aes(x = valor*100, y = fct_rev(categoria), color = factor(Ano))) +
  geom_point() +
  scale_x_continuous(limits = c(0, 100)) +
  xlab("Porcentagem") +
  ylab("Raça/cor") +
  theme_minimal()

fwrite(tabela_resultados_2anos, file = "dados/tabela_resultados_2anos.csv")
fwrite(tabela_resultados, file = "dados/tabela_resultados.csv")
