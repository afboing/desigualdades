SINASC <- fread(file = "dados/sinasc_data.csv")
lista_municipio_uf <- fread(file = "dados/lista_municipio_uf.csv", encoding = "UTF-8")

load(file = "dados/mapa_muni.Rdata")
load(file = "dados/mapa_uf.Rdata")

tabela_p_ideal_muni <- fread(file = "dados/tabela_p_ideal_muni.csv", header = TRUE)
tabela_p_ideal_uf <- fread(file = "dados/tabela_p_ideal_uf.csv", header = TRUE)
estados_br <- sort(unique(mapa_uf$name_state))

lista_codigos_uf <- fread(file = "dados/lista_codigos_uf.csv")
vetor_codigos_uf = lista_codigos_uf$code_state
names(vetor_codigos_uf) <- lista_codigos_uf$name_state
vetor_codigos_uf <- vetor_codigos_uf[order(lista_codigos_uf$name_state)]
vetor_codigos_uf <- c("Todos" = 0, vetor_codigos_uf)

tabela_uf_zoom <- fread(file = "dados/uf_zoom.csv", header = TRUE) |> data.frame()
tabela_uf_zoom <- tabela_uf_zoom |> left_join(lista_codigos_uf, by = c("UF" = "name_state"))

mapa_uf_vetor = c("Roraima", "Amapá", "Pará", "Amazonas", "Acre", "Tocantins", "Rondônia", "Maranhão", "Ceará", "Rio Grande do Norte", "Paraíba", "Piauí", "Pernambuco", "Alagoas", "Sergipe", "Bahia", "Mato Grosso", "Distrito Federal", "Goiás", "Mato Grosso do Sul", "Minas Gerais", "Espírito Santo", "Rio de Janeiro", "São Paulo", "Paraná", "Santa Catarina", "Rio Grande do Sul")

tabela_percentil_ideal <- fread(file = "dados/tabela_percentil_ideal.csv")

linecolor = plasma(n = 5, begin = 0.1, end = 0.9, direction = -1)[4]

tabela_summarybox_consulta <- fread(file = "dados/tabela_summarybox_consulta.csv")

# Peso
tabela_p_peso_uf <- fread(file = "dados/tabela_p_peso_uf.csv", header = TRUE)
tabela_p_peso_muni <- fread(file = "dados/tabela_p_peso_muni.csv", header = TRUE)
tabela_percentil_peso <- fread(file = "dados/tabela_percentil_peso.csv")
tabela_summarybox_peso <- fread(file = "dados/tabela_summarybox_peso.csv")

# Adequação pré-natal

tabela_p_adeq_uf <- fread("dados/tabela_p_adeq_uf.csv", header = TRUE)
tabela_p_adeq_muni <- fread("dados/tabela_p_adeq_muni.csv", header = TRUE)
tabela_percentil_adeq <- fread("dados/tabela_percentil_adeq.csv")
tabela_summarybox_adeq <- fread("dados/tabela_summarybox_adeq.csv")

# Dados de proporção de 7 ou mais consultas de acordo com a escolaridade da mãe

p_escmae_consultas_ano <- fread(file = "dados/p_escmae_consultas_ano.csv")
tabela_segmento_escmae <- fread(file = "dados/tabela_segmento_escmae.csv")
p_razao_escmae_consultas_ano <- fread(file = "dados/p_razao_escmae_consultas_ano.csv")

# Dados de proporção de 7 ou mais consultas de acordo com a raça/cor do nascido

p_racacor_consultas_ano <- fread(file = "dados/p_racacor_consultas_ano.csv")
tabela_segmento_racacor <- fread(file = "dados/tabela_segmento_racacor.csv")
p_razao_racacor_consultas_ano <- fread(file = "dados/p_razao_racacor_consultas_ano.csv")
