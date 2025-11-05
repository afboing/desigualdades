############################################
#### PREPARAR AMBIENTE PARA ANÁLISE
############################################
#setwd("C:/Users/morde/Documents/GitHub/curso_desigualdade_saude/dados/sinasc")
# Carregar pacotes
require(tidyverse)
require(geobr)
require(ggnetwork)
require(microdatasus)
require(data.table)
require(plotly)

ini = Sys.time()

for(i in 2000:2023) {

print(i)  

SINASC_YEAR = fetch_datasus(year_start =  i, year_end = i, information_system = "SINASC")

SINASC_YEAR <- SINASC_YEAR %>% select(ESCMAE, CONSULTAS, CODMUNNASC, PESO, DTNASC, RACACOR) %>%
  mutate(PESO = as.numeric(PESO),
         PESO = case_when(is.na(PESO) ~ NA,
                          PESO < 500 ~ 0,
                          PESO >= 500 & PESO < 1500 ~ 1,
                          PESO >= 1500 & PESO < 2500  ~ 2,
                          TRUE ~ 3),
         DTNASC = as.numeric(substr(DTNASC, 5,8)))

SINASC_YEAR = SINASC_YEAR %>% count(ESCMAE, CONSULTAS, CODMUNNASC, PESO, DTNASC, RACACOR)

filename = paste0("dados/sinasc/sinasc_",i,".csv")

print(paste0("Writing: ", filename))
fwrite(SINASC_YEAR, file = filename)

rm(SINASC_YEAR);gc()
}

fin = Sys.time()
fin - ini

### Read files
paste0("dados/sinasc/sinasc_",i,".csv")
filelist <- list.files(path = "./dados/sinasc/")

SINASC_YEAR = data.frame()

for(i in 1:length(filelist)) {
  print(i)
sinasc_temp = fread(paste0("dados/sinasc/",filelist[i])) |> data.frame()
SINASC_YEAR = rbind(SINASC_YEAR, sinasc_temp)
}

###
# Selecionando apenas colunas de interesse e removendo valores ausentes ou "ignorados"
DN <- SINASC_YEAR %>%
  
  filter(DTNASC == 2000) |>
  
  select(ESCMAE, CONSULTAS, CODMUNNASC, PESO, DTNASC) %>%
  
  drop_na(ESCMAE, CONSULTAS, CODMUNNASC, PESO, DTNASC) %>%
  
  #filter(ESCMAE != 9) %>%
  
  filter(CONSULTAS != 9) %>%
  
  mutate(
    # escolaridade = factor(ESCMAE, 
    #                            levels = c(1,2,3,4,5),
    #                            labels =  c("Nenhuma",
    #                                        "1 a 3 anos",
    #                                        "4 a 7 anos",
    #                                        "8 a 11 anos",
    #                                        "12 e mais")),
         
         ideal_consultas = ifelse(CONSULTAS == 4, 1, 0)
  )

#rm(SINASC);gc()
fwrite(SINASC_YEAR, file = "dados/sinasc_data.csv")

###
DN <- SINASC_YEAR %>%
  
  filter(DTNASC == 2000) |>
  
  filter(CONSULTAS != 9) %>%
  
  drop_na(CONSULTAS, CODMUNNASC) %>%
  
  mutate(ideal_consultas = ifelse(CONSULTAS == 4, n, 0)) %>%
  
  select(CODMUNNASC, ideal_consultas, n)
  


ver = SINASC_YEAR |> filter(CODMUNNASC == 1709807)

#rm(SINASC_YEAR);gc()

map_ideal <- DN %>% 
  group_by(CODMUNNASC) %>%
  reframe(n_ideal = sum(ideal_consultas, na.rm = T),
          total = sum(n, na.rm = T),
          p_ideal = sum(ideal_consultas, na.rm = T)/sum(n, na.rm = T)*100) %>%
  ungroup() %>%
  mutate(CODMUNNASC = as.numeric(CODMUNNASC))

mapa_muni = read_municipality(code_muni = 'all', year = 2022) 
mapa_uf = read_state(code_state = 'all', year = 2020) 

save(mapa_muni, file = "dados/mapa_muni.Rdata")
save(mapa_uf, file = "dados/mapa_uf.Rdata")

#save(mapa_muni2, map_ideal, file = "mapa_muni2.Rdata")

mapa_muni2 <- mapa_muni %>%
  left_join(map_ideal, by = c("code_muni" = "CODMUNNASC")) %>%
  mutate(p_ideal = ifelse(is.na(p_ideal), 0, round(p_ideal,1)))

pm = ggplot() +
  geom_sf(data = mapa_muni2, aes(fill = p_ideal,
                                 text = paste0(name_muni, ": ", p_ideal,"%")), color = NA) + # Define o preenchimento pela variável `p_valor` e remove as bordas
  scale_fill_distiller(palette = "Reds", direction = 1) + # Aplica tons de vermelho de acordo com `p_valor`
  theme_minimal() + # Substitui o tema para algo mais limpo
  theme(
    legend.position = "right", 
    plot.title = element_text(hjust = 0.5),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) +
  ggtitle("Proporção de nascimentos com mais de 7 consultas pré-natal: 2020")

ggplotly(pm, tooltip = "text")


