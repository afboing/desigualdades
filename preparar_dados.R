# Carregar pacotes
require(tidyverse)
require(geobr)
require(ggnetwork)
require(microdatasus)
require(data.table)
require(plotly)
require(data.table)
require(sf)
# 
# ## Load files
# paste0("dados/sinasc/sinasc_",i,".csv")
# filelist <- list.files(path = "./dados/sinasc/")
# 
# SINASC_YEAR = data.frame()
# 
# for(i in 1:length(filelist)) {
#   print(i)
#   sinasc_temp = fread(paste0("dados/sinasc/",filelist[i])) |> data.frame()
#   SINASC_YEAR = rbind(SINASC_YEAR, sinasc_temp)
# }
# 
# fwrite(SINASC_YEAR, file = "dados/sinasc_data.csv")
# 
mapa_muni = read_municipality(code_muni = 'all', year = 2022)
mapa_uf = read_state(code_state = 'all', year = 2020)

# Transformar para o CRS EPSG:4326 
mapa_muni <- st_transform(mapa_muni, crs = 4326)
mapa_uf <- st_transform(mapa_uf, crs = 4326)

mapa_uf <- mapa_uf |> mutate(name_state = gsub(" Do ", " do ", name_state),
                             name_state = gsub(" De ", " de ", name_state),
                             name_state = gsub("zôn", "zon", name_state))

lista_codigos_uf <- mapa_uf |> data.frame() |> select(name_state, code_state) |> distinct()

#fwrite(lista_codigos_uf, file = "dados/lista_codigos_uf.csv")

# Extrair centroides
mapa_uf_latlon = mapa_uf |> st_centroid() |> 
  mutate(lon = st_coordinates(geom)[, 1],
         lat = st_coordinates(geom)[, 2]) |>
  data.frame() |>
  select(-geom, -code_region, -name_region)

mapa_uf <- mapa_uf |>
  select(-code_region, -name_region) |>
 left_join(mapa_uf_latlon, by = c("code_state","name_state","abbrev_state"))

mapa_muni <- mapa_muni |> mutate(code_muni_curto = as.numeric(substr(code_muni,1,6))) |>
  select(-abbrev_state, -name_state, -code_region, -name_region)

save(mapa_muni, file = "dados/mapa_muni.Rdata")
save(mapa_uf, file = "dados/mapa_uf.Rdata")

### Read files
SINASC <- fread(file = "dados/sinasc_data.csv")
lista_municipio_uf <- fread(file = "dados/lista_municipio_uf.csv")
lista_codigos_uf <- fread(file = "dados/lista_codigos_uf.csv")

load(file = "dados/mapa_muni.Rdata")
load(file = "dados/mapa_uf.Rdata")

## Preparar tabelas

lista_municipio_uf2 = lista_municipio_uf |>
  
  pivot_longer(names_to = "codigo", 
               values_to = "code_muni", 
               cols = c(code_muni, code_muni_curto)) |>
  
  select(-codigo)

tabela_p_ideal_uf <- SINASC %>%
  
  filter(CONSULTAS != 9) %>%
  
  drop_na(CONSULTAS, CODMUNNASC) %>%
  
  mutate(ideal_consultas = ifelse(CONSULTAS == 4, n, 0)) %>%
  
  left_join(lista_municipio_uf2, by = c("CODMUNNASC" = "code_muni")) |>
  
  group_by(name_state, DTNASC) %>%
  
  reframe(total = sum(n, na.rm = T),
          p_ideal = round(sum(ideal_consultas, na.rm = T)/total*100,1)) %>%
  
  ungroup() |>
  
  arrange(DTNASC, name_state) |>
  
  select(-total) |>
  
  pivot_wider(names_from = DTNASC, values_from = p_ideal, values_fill = 0) |>
  
  drop_na(name_state)

tabela_p_ideal_muni <- SINASC %>%
  
  filter(CONSULTAS != 9) %>%
  
  drop_na(CONSULTAS, CODMUNNASC) %>%
  
  mutate(ideal_consultas = ifelse(CONSULTAS == 4, n, 0)) %>%
  
  mutate(code_muni_curto = as.numeric(substr(CODMUNNASC,1,6))) |>
  
  group_by(code_muni_curto, DTNASC) %>%
  
  reframe(total = sum(n, na.rm = T),
          p_ideal = round(sum(ideal_consultas, na.rm = T)/total*100,1)) %>%
  
  ungroup() |>
  
  arrange(DTNASC, code_muni_curto) |>
  
  select(-total) |> 
  
  pivot_wider(names_from = DTNASC, values_from = p_ideal, values_fill = 0)

tabela_n_muni <- SINASC %>%
  
  filter(CONSULTAS != 9) %>%
  
  drop_na(CONSULTAS, CODMUNNASC) %>%
  
  mutate(code_muni_curto = as.numeric(substr(CODMUNNASC,1,6))) |>
  
  group_by(code_muni_curto, DTNASC) %>%
  
  reframe(total = sum(n, na.rm = T)) %>%
  
  ungroup() |>
  
  arrange(DTNASC, code_muni_curto, total) 

fwrite(tabela_p_ideal_muni, file = "dados/tabela_p_ideal_muni.csv")
fwrite(tabela_p_ideal_uf, file = "dados/tabela_p_ideal_uf.csv")

##
# Calcular os centroides
mapa_uf_ordenadas <- mapa_uf %>%
  mutate(
    centroide = st_centroid(geom),           # Calcula os centroides
    latitude = st_coordinates(centroide)[,2] # Extrai a latitude
  ) |>
  # Criar coluna com as regiões (associando cada UF à sua região)
  mutate(
    regiao = case_when(
      abbrev_state %in% c("AC", "AP", "AM", "PA", "RO", "RR", "TO") ~ "Norte",
      abbrev_state %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
      abbrev_state %in% c("DF", "GO", "MT", "MS") ~ "Centro-Oeste",
      abbrev_state %in% c("ES", "MG", "RJ", "SP") ~ "Sudeste",
      abbrev_state %in% c("PR", "RS", "SC") ~ "Sul",
      TRUE ~ "Desconhecida"
    ),
    regiao = factor(regiao, levels = c("Norte","Nordeste","Centro-Oeste","Sudeste","Sul"), ordered = TRUE)
  ) |>
  # Ordenar mapa_uf pela região e pela latitude (norte para sul)
  arrange(regiao, desc(latitude)) %>%
  data.frame() |>
  select(name_state, regiao, latitude)

# Criar vetor com as mapa_uf ordenadas
mapa_uf_vetor <- mapa_uf_ordenadas$name_state

# Grafico de percentis para ideal (numero de consultas prenatal por UF)

tabela_percentil_ideal <- tabela_p_ideal_muni |>
  pivot_longer(names_to = "DTNASC", values_to = "p", cols = starts_with('2')) |>
  mutate(DTNASC = as.numeric(DTNASC))|>
  inner_join(tabela_n_muni |> filter(total > 100), by = c("code_muni_curto", "DTNASC")) |>
  left_join(lista_municipio_uf |> select(name_state, code_muni_curto),
            by = c("code_muni_curto")) |>
  group_by(name_state, DTNASC) |>
  reframe(pmin = min(p, na.rm = T),
          p25 = quantile(p, probs = 0.25),
          p50 = quantile(p, probs = 0.5),
          p75 = quantile(p, probs = 0.75),
          pmax = max(p, na.rm = T)) |>
  arrange(DTNASC, name_state) |>
  pivot_longer(names_to = "metrica", values_to = "valor", cols = starts_with('p')) |>
  mutate(metrica = factor(metrica, 
                          levels = c("pmin","p25","p50","p75","pmax"),
                          labels = c("Mínimo","25 percentil","Mediana","75 percentil","Máximo"),
                          ordered = TRUE))

fwrite(tabela_percentil_ideal, file = "dados/tabela_percentil_ideal.csv")

tabela_summarybox_consulta <- tabela_p_ideal_muni |>
  pivot_longer(names_to = "DTNASC", values_to = "p", cols = starts_with('2')) |>
  mutate(DTNASC = as.numeric(DTNASC))|>
  inner_join(tabela_n_muni |> filter(total > 100), by = c("code_muni_curto", "DTNASC")) |>
  group_by(DTNASC) |>
  reframe(p90 = quantile(p, probs = 0.9),
          p10 = quantile(p, probs = 0.1),
          razao = format(round(p90/p10,2),decimal.mark = ","),
          coef_var = format(round(sd(p, na.rm = TRUE)/mean(p, na.rm = TRUE)*100,1),decimal.mark = ","),
          ) |>
  arrange(DTNASC) |>
  mutate(
    ratio = p90/p10,
    coef_var_num = as.numeric(gsub(",",".",coef_var)),
    p90_num = p90,
    p10_num = p10,
    p90 = trimws(paste0(format(p90,decimal.mark = ",", digits = 1), "%")),
    p10 = trimws(paste0(format(p10,decimal.mark = ",", digits = 1), "%"))
  )


fwrite(tabela_summarybox_consulta, file = "dados/tabela_summarybox_consulta.csv")

###### PESO

tabela_p_peso_uf <- SINASC %>%
  
  drop_na(PESO, CODMUNNASC) %>%
  
  mutate(baixo_peso = ifelse(PESO < 3, n, 0)) %>%
  
  left_join(lista_municipio_uf2, by = c("CODMUNNASC" = "code_muni")) |>
  
  group_by(name_state, DTNASC) %>%
  
  reframe(total = sum(n, na.rm = T),
          p_peso = round(sum(baixo_peso, na.rm = T)/total*100,1)
          ) %>%
  
  ungroup() %>%
  
  arrange(DTNASC, name_state) |>
  
  select(-total) |>
  
  pivot_wider(names_from = DTNASC, values_from = p_peso, values_fill = 0) |>
  
  drop_na(name_state)

###
tabela_p_peso_muni <- SINASC %>%
  
  drop_na(PESO, CODMUNNASC) %>%
  
  mutate(code_muni_curto = as.numeric(substr(CODMUNNASC,1,6)),
         baixo_peso = ifelse(PESO < 3, n, 0)) |>
  
  group_by(code_muni_curto, DTNASC) %>%
  
  reframe(total = sum(n, na.rm = T),
          p_peso = round(sum(baixo_peso, na.rm = T)/total*100,1)
  ) %>%
  
  ungroup() %>%
  
  arrange(DTNASC, code_muni_curto) |>
  
  select(-total) |> 
  
  pivot_wider(names_from = DTNASC, values_from = p_peso, values_fill = 0)

fwrite(tabela_p_peso_muni, file = "dados/tabela_p_peso_muni.csv")
fwrite(tabela_p_peso_uf, file = "dados/tabela_p_peso_uf.csv")

# Tabela com total de registros validos (excluindo NA) para peso
tabela_n_muni_peso <- SINASC %>%
  
  drop_na(PESO, CODMUNNASC) %>%
  
  mutate(code_muni_curto = as.numeric(substr(CODMUNNASC,1,6))) |>
  
  group_by(code_muni_curto, DTNASC) %>%
  
  reframe(total = sum(n, na.rm = T)) %>%
  
  ungroup() |>
  
  arrange(DTNASC, code_muni_curto, total) 

# Grafico de percentis para baixo peso por UF

tabela_percentil_peso <- tabela_p_peso_muni |>
  pivot_longer(names_to = "DTNASC", values_to = "p", cols = starts_with('2')) |>
  mutate(DTNASC = as.numeric(DTNASC))|>
  inner_join(tabela_n_muni_peso |> filter(total > 100), by = c("code_muni_curto", "DTNASC")) |>
  left_join(lista_municipio_uf |> select(name_state, code_muni_curto),
            by = c("code_muni_curto")) |>
  group_by(name_state, DTNASC) |>
  reframe(pmin = min(p, na.rm = T),
          p25 = quantile(p, probs = 0.25),
          p50 = quantile(p, probs = 0.5),
          p75 = quantile(p, probs = 0.75),
          pmax = max(p, na.rm = T)) |>
  arrange(DTNASC, name_state) |>
  pivot_longer(names_to = "metrica", values_to = "valor", cols = starts_with('p')) |>
  mutate(metrica = factor(metrica, 
                          levels = c("pmin","p25","p50","p75","pmax"),
                          labels = c("Mínimo","25 percentil","Mediana","75 percentil","Máximo"),
                          ordered = TRUE)) |>
  drop_na(name_state)

fwrite(tabela_percentil_peso, file = "dados/tabela_percentil_peso.csv")
tabela_summarybox_peso <- tabela_p_peso_muni |>
  pivot_longer(names_to = "DTNASC", values_to = "p", cols = starts_with('2')) |>
  mutate(DTNASC = as.numeric(DTNASC))|>
  inner_join(tabela_n_muni |> filter(total > 100), by = c("code_muni_curto", "DTNASC")) |>
  group_by(DTNASC) |>
  reframe(p90 = quantile(p, probs = 0.9),
          p10 = quantile(p, probs = 0.1),
          razao = format(round(p90/p10,2),decimal.mark = ",", nsmall = 2),
          coef_var = format(round(sd(p, na.rm = TRUE)/mean(p, na.rm = TRUE)*100,1),decimal.mark = ","),
  ) |>
  arrange(DTNASC) |>
  mutate(
    ratio = p90/p10,
    coef_var_num = as.numeric(gsub(",",".",coef_var)),
    p90_num = p90,
    p10_num = p10,
    p90 = trimws(paste0(format(p90,decimal.mark = ",", digits = 1), "%")),
    p10 = trimws(paste0(format(p10,decimal.mark = ",", digits = 1), "%"))
  )

fwrite(tabela_summarybox_peso, file = "dados/tabela_summarybox_peso.csv")

### Adequacao de pré-natal

sinasc_adequados_total <- fread("dados/sinasc_adequados_total.csv")
sinasc_adequados_na <- fread("dados/sinasc_adequados_na.csv")
sinasc_adequados <- fread("dados/sinasc_adequados.csv")

sinasc_adequados_na_longer <- sinasc_adequados_na |>
  select(-Total) |>
  pivot_longer(names_to = "DTNASC", values_to = "n_ausentes", cols = starts_with("2"))

sinasc_adequados_total_longer <- sinasc_adequados_total |>
  select(-Total) |>
  pivot_longer(names_to = "DTNASC", values_to = "total", cols = starts_with("2")) |>
  left_join(sinasc_adequados_na_longer, by = c("Municipio", "DTNASC")) |>
  replace_na(list(n_ausentes = 0)) |>
  mutate(total = total - n_ausentes) |>
  select(-n_ausentes)

tabela_p_adeq_uf <- sinasc_adequados |>
  
  select(-Total) |>
  
  pivot_longer(names_to = "DTNASC", values_to = "n", cols = starts_with("2")) |>
  
  left_join(sinasc_adequados_total_longer, by = c("Municipio","DTNASC")) |>
  
  mutate(code_muni = as.numeric(substr(Municipio, 1, 6))) |>
  
  drop_na(code_muni) |>
  
  left_join(lista_municipio_uf2, by = c("code_muni")) |>
  
  group_by(name_state, DTNASC) |>
  
  reframe(n = sum(n, na.rm = T),
          total = sum(total, na.rm = T)) |>
  
  ungroup() |>
  
  mutate(p = round(n/total*100,1)) |>
  
  select(-n, -total) |>
  
  pivot_wider(names_from = DTNASC, values_from = p, values_fill = 0)

tabela_p_adeq_muni <- sinasc_adequados |>
  
  select(-Total) |>
  
  pivot_longer(names_to = "DTNASC", values_to = "n", cols = starts_with("2")) |>
  
  left_join(sinasc_adequados_total_longer, by = c("Municipio","DTNASC")) |>
  
  mutate(p = round(n/total*100,1)) |> 
  
  select(-n, -total) |>
  
  pivot_wider(names_from = DTNASC, values_from = p, values_fill = 0) |>
  
  mutate(code_muni_curto = as.numeric(substr(Municipio, 1, 6))) |>
  
  drop_na(code_muni_curto)

tabela_n_adeq_muni <- sinasc_adequados_total_longer %>%
  
  mutate(code_muni_curto = as.numeric(substr(Municipio, 1, 6)),
         DTNASC = as.numeric(DTNASC)) |>
  
  drop_na(code_muni_curto) |>
  
  select(-Municipio)

fwrite(tabela_p_adeq_muni, file = "dados/tabela_p_adeq_muni.csv")
fwrite(tabela_p_adeq_uf, file = "dados/tabela_p_adeq_uf.csv")

# Grafico de percentis para adequacao

tabela_percentil_adeq <- tabela_p_adeq_muni |>
  pivot_longer(names_to = "DTNASC", values_to = "p", cols = starts_with('2')) |>
  mutate(DTNASC = as.numeric(DTNASC))|>
  inner_join(tabela_n_adeq_muni |> filter(total > 100), by = c("code_muni_curto", "DTNASC")) |>
  left_join(lista_municipio_uf |> select(name_state, code_muni_curto),
            by = c("code_muni_curto")) |>
  group_by(name_state, DTNASC) |>
  reframe(pmin = min(p, na.rm = T),
          p25 = quantile(p, probs = 0.25),
          p50 = quantile(p, probs = 0.5),
          p75 = quantile(p, probs = 0.75),
          pmax = max(p, na.rm = T)) |>
  arrange(DTNASC, name_state) |>
  pivot_longer(names_to = "metrica", values_to = "valor", cols = starts_with('p')) |>
  mutate(metrica = factor(metrica, 
                          levels = c("pmin","p25","p50","p75","pmax"),
                          labels = c("Mínimo","25 percentil","Mediana","75 percentil","Máximo"),
                          ordered = TRUE))

fwrite(tabela_percentil_adeq, file = "dados/tabela_percentil_adeq.csv")

tabela_summarybox_adeq <- tabela_p_adeq_muni |>
  pivot_longer(names_to = "DTNASC", values_to = "p", cols = starts_with('2')) |>
  mutate(DTNASC = as.numeric(DTNASC))|>
  inner_join(tabela_n_adeq_muni |> filter(total > 100), by = c("code_muni_curto", "DTNASC")) |>
  group_by(DTNASC) |>
  reframe(p90 = quantile(p, probs = 0.9),
          p10 = quantile(p, probs = 0.1),
          razao = format(round(p90/p10,2),decimal.mark = ","),
          coef_var = format(round(sd(p, na.rm = TRUE)/mean(p, na.rm = TRUE)*100,1),decimal.mark = ","),
  ) |>
  arrange(DTNASC) |>
  mutate(
    ratio = p90/p10,
    coef_var_num = as.numeric(gsub(",",".",coef_var)),
    p90_num = p90,
    p10_num = p10,
    p90 = trimws(paste0(format(p90,decimal.mark = ",", digits = 1), "%")),
    p10 = trimws(paste0(format(p10,decimal.mark = ",", digits = 1), "%"))
  )
tabela_summarybox_adeq

fwrite(tabela_summarybox_adeq, file = "dados/tabela_summarybox_adeq.csv")

### Dados individuais

# Escolaridade da Mãe

p_escmae_consultas_ano = SINASC |> 
  mutate(escolaridade_mae_novo = factor(ESCMAE,
                                        levels = c(1, 2, 3, 4, 5),
                                        labels = c(1, 1, 2, 3, 4)),
         ideal_consultas = ifelse(CONSULTAS == 4, n, 0)) |>
  drop_na(escolaridade_mae_novo, ideal_consultas) |> group_by(DTNASC, escolaridade_mae_novo) |>
  reframe(
    n_ideal = sum(ideal_consultas, na.rm = TRUE),
    total = sum(n, na.rm = TRUE)) |>
  mutate(p_ideal = n_ideal/total*100)

# 1 – Nenhuma; 2 – 1 a 3 anos; 3 – 4 a 7 anos; 4 – 8 a 11 anos; 5 – 12 e mais; 9 – Ignorado

tabela_segmento_escmae = p_escmae_consultas_ano |>
  filter(escolaridade_mae_novo  %in% c("1","4")) |>
  mutate(escolaridade_mae_novo = factor(escolaridade_mae_novo,
                                        levels = c("1","4"),
                                        labels = c("menor_escolaridade","maior_escolaridade"))) |>
  select(DTNASC, escolaridade_mae_novo, p_ideal) |>
  pivot_wider(names_from = escolaridade_mae_novo, values_from = p_ideal)


p_razao_escmae_consultas_ano <- p_escmae_consultas_ano |>
  filter(escolaridade_mae_novo %in% c(1,4)) |>
  select(DTNASC, escolaridade_mae_novo, p_ideal) |>
  pivot_wider(names_from = escolaridade_mae_novo, values_from = p_ideal) |>
  mutate(razao = `4`/`1`) 

fwrite(p_escmae_consultas_ano, file = "dados/p_escmae_consultas_ano.csv")
fwrite(tabela_segmento_escmae, file = "dados/tabela_segmento_escmae.csv")
fwrite(p_razao_escmae_consultas_ano, file = "dados/p_razao_escmae_consultas_ano.csv")

# Raça/Cor

p_racacor_consultas_ano = SINASC |> 
  mutate(ideal_consultas = ifelse(CONSULTAS == 4, n, 0)) |>
  drop_na(RACACOR, ideal_consultas) |> 
  filter(RACACOR != 9) |>
  group_by(DTNASC, RACACOR) |>
  reframe(
    n_ideal = sum(ideal_consultas, na.rm = TRUE),
    total = sum(n, na.rm = TRUE)) |>
  mutate(p_ideal = n_ideal/total*100)

tabela_segmento_racacor = p_racacor_consultas_ano |>
  arrange(DTNASC, p_ideal) |>
  group_by(DTNASC) |>
  mutate(order = 1:n()) |>
  filter(order  %in% c(1, 5)) |>
  mutate(order = factor(order,
                        levels = c(1,5),
                        labels = c("menor_valor","maior_valor"))) |>
  select(DTNASC, order, p_ideal) |>
  pivot_wider(names_from = order, values_from = p_ideal)

p_razao_racacor_consultas_ano <- p_racacor_consultas_ano |>
  arrange(DTNASC, p_ideal) |>
  group_by(DTNASC) |>
  mutate(order = 1:n()) |>
  ungroup() |>
  filter(order %in% c(1,5)) |>
  select(DTNASC, order, p_ideal) |>
  pivot_wider(names_from = order, values_from = p_ideal) |>
  mutate(razao = `5`/`1`)

p_razao_racacor_consultas_ano <-   p_racacor_consultas_ano |>
  select(DTNASC, RACACOR, p_ideal) |>
  pivot_wider(names_from = RACACOR, values_from = p_ideal) |>
  mutate(razao_2 = `1`/`2`,
         razao_3 = `1`/`3`,
         razao_4 = `1`/`4`,
         razao_5 = `1`/`5`) |>
    select(DTNASC, starts_with("razao")) |>
    pivot_longer(names_to = "razao", values_to = "value", cols = starts_with("razao"))

fwrite(p_racacor_consultas_ano, file = "dados/p_racacor_consultas_ano.csv")
fwrite(tabela_segmento_racacor, file = "dados/tabela_segmento_racacor.csv")
fwrite(p_razao_racacor_consultas_ano, file = "dados/p_razao_racacor_consultas_ano.csv")

