# Verificar e instalar pacotes caso não estejam instalados
packages <- c("shiny", "bslib", "tidyverse", "geobr", "ggnetwork", "microdatasus", 
              "data.table", "plotly", "leaflet", "sf", "viridis", "summaryBox")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    require(pkg, character.only = TRUE)
  }
}

# Carregar dados para o dashboard
source("carregar_dados_shiny.R")

# Define UI for application
ui <- fluidPage(

  page_navbar(
    title = "Painel de Dados: SINASC",
    bg = "#eff7ff", # bg do navbar
    underline = TRUE,
    fluid = TRUE,

    nav_panel(
      title = "Consultas Pré-natal",
      
      navset_card_tab(
        height = "750px",
        nav_panel("Mapa",
                  
                  card(
                    full_screen = TRUE,
                    layout_sidebar(sidebar = 
                                   sidebar(
                                     open = "always",
                                     selectInput(inputId = "ano_mapa_consulta",
                                                 label="Selecione o ano:",
                                                 choices = 2000:2023,
                                                 selected = 2023,
                                                 multiple=FALSE
                                     ),
                                     
                                     selectInput(inputId = "uf_mapa_consulta",
                                                 label="Selecione o estado para visualização:",
                                                 choices = vetor_codigos_uf,
                                                 selected = "Todos",
                                                 multiple=FALSE
                                     )

                                   ), 
                                   
                                   leafletOutput("mapa_consultas")
                                   
                                   ), # layout_sidebar

                  ) # card

                  ), # nav_panel Mapa
        
        nav_panel("Gráfico de Desigualdade",
                  card(
                    full_screen = TRUE,
                    height = 700,
                    card_header("Distribuição da Proporção de Gestantes que Realizaram 7 ou Mais Consultas Pré-natal por Município"),
                    
                    layout_sidebar(sidebar = 
                                     sidebar(
                                       open = "always",
                                       selectInput(inputId = "ano_grafico_des_consulta",
                                                   label="Selecione o ano:",
                                                   choices = 2000:2023,
                                                   selected = 2023,
                                                   multiple=FALSE
                                       )
                                     ),
                                   fluidRow(
                                     #column(width = 3,
                                            summaryBox('Percentil 90', 
                                                       textOutput("p90_consultas"), 
                                                       width = 3, 
                                                       icon = 'fas  fa-circle-chevron-up'),
                                            
                                            summaryBox('Percentil 10', 
                                                       textOutput("p10_consultas"), 
                                                       width = 3, 
                                                       icon = 'fas  fa-circle-chevron-down'),
                                            
                                            summaryBox('Razão P90/P10', 
                                                       textOutput("razao_consultas"), 
                                                       width = 3,
                                                       style = "success",
                                                       icon = 'fas fa-divide'),
                                            
                                            summaryBox('Coef. de Variação', 
                                                       textOutput("cv_consultas"), 
                                                       width = 3, 
                                                       style = "primary",
                                                       icon = 'fas fa-align-center')
                                   ),
                                   
                                   plotlyOutput("grafico_des_consulta")
                                   #output
                                   
                    ) # layout_sidebar
                  ) # card
                  ), # nav_panel Gráfico de Desigualdade
    
        nav_panel("Evolução da Desigualdade",
                  card(
                    full_screen = TRUE,
                    card_header("Evolução da Desigualdade da Proporção de Gestantes que Realizaram 7 ou Mais Consultas Pré-natal"),
                    
                    layout_sidebar(sidebar = 
                                     sidebar(
                                       open = "always",
                                       selectInput(inputId = "evolucao_des_consulta",
                                                   label="Selecione o indicador:",
                                                   choices = c("Razão P90/P10" = "razao",
                                                               "Percentil 90 e 10" = "perc",
                                                               "Coeficiente de variação" = "coef_var"
                                                               ),
                                                   selected = "razao",
                                                   multiple=FALSE
                                       )
                                     ),
                    plotlyOutput("evolucao_razao_consultas")
                    ) # layout_sidebar
                  ) # card
        ) # nav_panel Evolução da Desigualdade
      ) # navset_card_tab
    ), # nav_panel "Consultas Pré-natal"
    
    nav_panel(
      title = "Adequação de pré-natal",
      
      navset_card_tab(
        height = "750px",
        nav_panel("Mapa",
                  
                  card(
                    full_screen = TRUE,
                    layout_sidebar(sidebar = 
                                     sidebar(
                                       open = "always",
                                       selectInput(inputId = "ano_mapa_adeq",
                                                   label="Selecione o ano:",
                                                   choices = 2014:2023,
                                                   selected = 2023,
                                                   multiple=FALSE
                                       ),
                                       
                                       selectInput(inputId = "uf_mapa_adeq",
                                                   label="Selecione o estado para visualização:",
                                                   choices = vetor_codigos_uf,
                                                   selected = "Todos",
                                                   multiple=FALSE
                                       )
                                       
                                     ), 
                                   
                                   leafletOutput("mapa_adeq")
                                   
                    ) # layout_sidebar
                  ) # card
                  
        ), # nav_panel Mapa
        
        nav_panel("Gráfico de Desigualdade",
                  card(
                    card_header("Distribuição da Proporção de Nascimentos Classificados como Adequado ou Mais que Adequado por Município"),
                    height = 700,
                    full_screen = TRUE,
                    layout_sidebar(sidebar = 
                                     sidebar(
                                       open = "always",
                                       selectInput(inputId = "ano_grafico_des_adeq",
                                                   label="Selecione o ano:",
                                                   choices = 2014:2023,
                                                   selected = 2023,
                                                   multiple=FALSE
                                       )
                                     ),
                                   
                                   fluidRow(
                                     #column(width = 3,
                                     summaryBox('Percentil 90', 
                                                textOutput("p90_adeq"), 
                                                width = 3, 
                                                icon = 'fas  fa-circle-chevron-up'),
                                     
                                     summaryBox('Percentil 10', 
                                                textOutput("p10_adeq"), 
                                                width = 3, 
                                                icon = 'fas  fa-circle-chevron-down'),
                                     
                                     summaryBox('Razão P90/P10', 
                                                textOutput("razao_adeq"), 
                                                width = 3,
                                                style = "success",
                                                icon = 'fas fa-divide'),
                                     
                                     summaryBox('Coef. de Variação', 
                                                textOutput("cv_adeq"), 
                                                width = 3, 
                                                style = "primary",
                                                icon = 'fas fa-align-center')
                                   ),
                                   
                                   plotlyOutput("grafico_des_adeq")
                                   
                    ) # layout_sidebar
                  ) # card
        ), # nav_panel Gráfico de Desigualdade
        
        nav_panel("Evolução da Desigualdade",
                  card(
                    full_screen = TRUE,
                    card_header("Evolução da Desigualdade da Proporção de Nascimentos Classificados como Adequado ou Mais que Adequado"),
                    
                    layout_sidebar(sidebar = 
                                     sidebar(
                                       open = "always",
                                       selectInput(inputId = "evolucao_des_adeq",
                                                   label="Selecione o indicador:",
                                                   choices = c("Razão P90/P10" = "razao",
                                                               "Percentil 90 e 10" = "perc",
                                                               "Coeficiente de variação" = "coef_var"
                                                   ),
                                                   selected = "razao",
                                                   multiple=FALSE
                                       )
                                     ),
                                   plotlyOutput("evolucao_razao_adeq")
                    ) # layout_sidebar
                  ) # card
        ) # nav_panel Evolução da Desigualdade
        
      ) # navset_card_tab
      
    ),
    
    nav_panel(
      title = "Peso do Recém-nascido",
      
      navset_card_tab(
        height = "750px",
        nav_panel("Mapa",
                  
                  card(
                    full_screen = TRUE,
                    layout_sidebar(sidebar = 
                                     sidebar(
                                       open = "always",
                                       selectInput(inputId = "ano_mapa_peso",
                                                   label="Selecione o ano:",
                                                   choices = 2000:2023,
                                                   selected = 2023,
                                                   multiple=FALSE
                                       ),
                                       
                                       selectInput(inputId = "uf_mapa_peso",
                                                   label="Selecione o estado para visualização:",
                                                   choices = vetor_codigos_uf,
                                                   selected = "Todos",
                                                   multiple=FALSE
                                       )
                                     ), 

                                   leafletOutput("mapa_peso")
                                   
                    ) # layout_sidebar
                  ) # card
                  
        ), # nav_panel Mapa
        
        nav_panel("Gráfico de Desigualdade",
                  card(
                    full_screen = TRUE,
                    height = 700,
                    card_header("Distribuição da Proporção de Nascimentos com Recém-nascido com Baixo Peso por Município"),
                    
                    layout_sidebar(sidebar = 
                                     sidebar(
                                       open = "always",
                                       selectInput(inputId = "ano_grafico_des_peso",
                                                   label="Selecione o ano:",
                                                   choices = 2000:2023,
                                                   selected = 2023,
                                                   multiple=FALSE
                                       )
                                     ),
                                   
                                   fluidRow(
                                     #column(width = 3,
                                     summaryBox('Percentil 90', 
                                                textOutput("p90_pesos"), 
                                                width = 3, 
                                                icon = 'fas  fa-circle-chevron-up'),
                                     
                                     summaryBox('Percentil 10', 
                                                textOutput("p10_pesos"), 
                                                width = 3, 
                                                icon = 'fas  fa-circle-chevron-down'),
                                     
                                     summaryBox('Razão P90/P10', 
                                                textOutput("razao_pesos"), 
                                                width = 3,
                                                style = "success",
                                                icon = 'fas fa-divide'),
                                     
                                     summaryBox('Coef. de Variação', 
                                                textOutput("cv_pesos"), 
                                                width = 3, 
                                                style = "primary",
                                                icon = 'fas fa-align-center')
                                   ),
                                  
                                 plotlyOutput("grafico_des_peso")
                                   
                    ) # layout_sidebar
                  ) # card
        ), # nav_panel Gráfico de Desigualdade
        
        nav_panel("Evolução da Desigualdade",
                  card(
                    full_screen = TRUE,
                    card_header("Evolução da Desigualdade da Proporção de Gestantes que Realizaram 7 ou Mais pesos Pré-natal"),
                    
                    layout_sidebar(sidebar = 
                                     sidebar(
                                       open = "always",
                                       selectInput(inputId = "evolucao_des_peso",
                                                   label="Selecione o indicador:",
                                                   choices = c("Razão P90/P10" = "razao",
                                                               "Percentil 90 e 10" = "perc",
                                                               "Coeficiente de variação" = "coef_var"
                                                   ),
                                                   selected = "razao",
                                                   multiple=FALSE
                                       )
                                     ),
                                   plotlyOutput("evolucao_razao_pesos")
                    ) # layout_sidebar
                  ) # card
        ) # nav_panel Evolução da Desigualdade
      ) # navset_card_tab
    ),
    
    nav_panel(
      title = "Escolaridade da Mãe",
      
      card(
        full_screen = FALSE,
        card_header("Proporção de Nascimentos com 7 ou Mais Consultas de Pré-natal por Tempo de Escolaridade da Mãe em 2023"),
        
        fluidRow(
          
          summaryBox('0 a 3 anos', 
                     value = textOutput("card_escolaridade_1"),
                     width = 2, 
                     icon = 'fas  fa-user-graduate',
                     style = "info",
                     border = 'left'),

          summaryBox('4 a 7 anos', 
                     value = textOutput("card_escolaridade_2"),
                     width = 2, 
                     icon = 'fas  fa-user-graduate',
                     style = "info",
                     border = 'left'),
          
          summaryBox('8 a 11 anos', 
                     value = textOutput("card_escolaridade_3"),
                     width = 2, 
                     icon = 'fas  fa-user-graduate',
                     style = "info",
                     border = 'left'),
          
          summaryBox('12 ou mais', 
                     value = textOutput("card_escolaridade_4"),
                     width = 2, 
                     icon = 'fas  fa-user-graduate',
                     style = "info",
                     border = 'left'),
          
          summaryBox('Razão (maior/menor escolaridade)', 
                     value = textOutput("card_razao_escolaridade"),
                     width = 4, 
                     icon = 'fas  fa-up-right-and-down-left-from-center',
                     style = "success",
                     border = 'left')
        )
      ),
      
      card(
        full_screen = TRUE,
        card_header("Evolução dos Indicadores por Ano de Nascimento"),
      
        fluidRow(
          
          column(7,
                 plotlyOutput("grafico_escolaridade_mae",
                            height = "500px")),
          
          column(5,
                 plotlyOutput("grafico_razao_escolaridade",
                            height = "500px"))
        ),
        
        column(1)
        
      ) # card
    ), # nav_panel "Escolaridade da Mãe"
    #
    nav_panel(
      title = "Raça/Cor do Nascido",
                  card(
                    full_screen = FALSE,
                    card_header("Proporção de Nascimentos com 7 ou Mais Consultas de Pré-natal por Raça/Cor em 2023"),
                    
                    fluidRow(
                      
                    summaryBox('Branco', 
                                 value = textOutput("card_raca_cor_1"),
                                 width = 2, 
                                 icon = 'fas  fa-child',
                                 style = "info",
                                 border = 'left'),
                    
                    summaryBox('Preto', 
                               value = textOutput("card_raca_cor_2"),
                               width = 2, 
                               icon = 'fas  fa-child',
                               style = "info",
                               border = 'left'),
                    
                    summaryBox('Amarelo', 
                               value = textOutput("card_raca_cor_3"),
                               width = 2, 
                               icon = 'fas  fa-child',
                               style = "info",
                               border = 'left'),
                    
                    summaryBox('Pardo', 
                               value = textOutput("card_raca_cor_4"),
                               width = 2, 
                               icon = 'fas  fa-child',
                               style = "info",
                               border = 'left'),
                    
                    summaryBox('Indígena', 
                               value = textOutput("card_raca_cor_5"),
                               width = 2, 
                               icon = 'fas  fa-child',
                               style = "info",
                               border = 'left')
                    ),
                    
                    fluidRow(
                    
                    summaryBox('Razão (Branco/Preto)', 
                               value = textOutput("card_razao_raca_cor_2"),
                               width = 3, 
                               icon = 'fas  fa-up-right-and-down-left-from-center',
                               style = "success",
                               border = 'left'),
                    
                    summaryBox('Razão (Branco/Amarelo)', 
                               value = textOutput("card_razao_raca_cor_3"),
                               width = 3, 
                               icon = 'fas  fa-up-right-and-down-left-from-center',
                               style = "success",
                               border = 'left'),
                    
                    summaryBox('Razão (Branco/Pardo)', 
                               value = textOutput("card_razao_raca_cor_4"),
                               width = 3, 
                               icon = 'fas  fa-up-right-and-down-left-from-center',
                               style = "success",
                               border = 'left'),
                    
                    summaryBox('Razão (Branco/Indígena)', 
                               value = textOutput("card_razao_raca_cor_5"),
                               width = 3, 
                               icon = 'fas  fa-up-right-and-down-left-from-center',
                               style = "success",
                               border = 'left'),
                    )
                  ),
                    
                    card(
                      full_screen = TRUE,
                      card_header("Evolução dos Indicadores por Ano de Nascimento"),
                    
                    fluidRow(
                      
                    column(6,
                    plotlyOutput("grafico_raca_cor",
                               height = "500px")),
                    
                     column(6,
                     plotlyOutput("grafico_razao_raca_cor",
                                height = "500px"))
                     )
                  ) # card

    ), # nav_panel "Raça/Cor do Nascido"
    
    nav_panel(
      title = "Sobre",
      
      card(
      tags$div(
        tags$b("Fonte de Dados:"),
        " SINASC (Sistema de Informações sobre Nascidos Vivos)", tags$br(), tags$br(),
        
        "Os dados de número de consultas pré-natal, peso do recém-nascido ao nascer, escolaridade da mãe e raça/cor foram extraídos do FTP DATASUS utilizando o pacote ",
        tags$a(href = "https://github.com/rfsaldanha/microdatasus", "microdatasus", target = "_blank"), ".", tags$br(),
        
        "Os dados de adequação de quantidade de pré-natal foram extraídos pelo ",
        tags$a(href = "https://datasus.saude.gov.br/informacoes-de-saude-tabnet/", "TabNet", target = "_blank"), ".", tags$br(), tags$br(),
        
        tags$b("Observações:"), tags$br(),
        tags$ul(
          tags$li("Para o cálculo da proporção de nascimentos e/ou gestantes, registros sem classificação ou informação disponível foram excluídos do denominador."),
          tags$li("Para o cálculo do percentil de municípios de acordo com determinado desfecho, foram considerados somente municípios com 100 ou mais registros de nascimentos para determinado ano.")
        )
      )
    ) # card
    )
    
  )
)

# Define server logic
server <- function(input, output) {

  ## Gerar gráficos
  
  output$mapa_consultas <- renderLeaflet({

  if(input$uf_mapa_consulta == 0) {

  map_ideal = mapa_uf |>
    left_join(tabela_p_ideal_uf |>
                rename(ano_selecionado = input$ano_mapa_consulta) |>
                select(name_state, ano_selecionado),
              by = "name_state")

  pm_leaflet <- leaflet(map_ideal) %>%
    addTiles() %>% # Adiciona o fundo do mapa
    addProviderTiles("CartoDB.Positron") |>
    addPolygons(
      fillColor = ~colorNumeric(palette = "Blues", domain = map_ideal$ano_selecionado)(ano_selecionado),
      weight = 0, # Remove as bordas
      fillOpacity = 1, # Define a opacidade do preenchimento
      color = NA, # Remove a cor da borda
      highlightOptions = highlightOptions(
        weight = 2,
        color = "black",
        bringToFront = TRUE
      ),
      label = ~paste0(name_state, ": ", ano_selecionado, "%"), # Tooltip ao passar o mouse
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "12px",
        direction = "auto"
      )
    ) %>%
    addLegend(
      pal = colorNumeric(palette = "Blues", domain = map_ideal$ano_selecionado),
      values = ~ano_selecionado,
      title = "Porcentagem",
      position = "bottomright"
    ) %>%
    setView(
      lng = mean(st_coordinates(map_ideal)[, 1]),
      lat = mean(st_coordinates(map_ideal)[, 2])+2,
      zoom = 4 # Ajusta o nível de zoom
    ) |>
    addLabelOnlyMarkers(data = map_ideal,
                        lng = ~lon, lat = ~lat-2, label = ~abbrev_state,
                        labelOptions = labelOptions(noHide = TRUE,
                                                    direction = 'top',
                                                    textOnly = TRUE,
                                                    style = list("color" = "orange")))  |>
    addControl(
      html = paste0("<h3 style='margin:0; text-align:center;'> Proporção de gestantes que realizaram 7 ou mais consultas de pré-natal em ", input$ano_mapa_consulta, "</h3>"),
      position = "topright" # Pode ser 'topleft', 'topright', 'bottomleft', ou 'bottomright'
    )

  } else {

  map_ideal = mapa_muni |>
    filter(code_state == input$uf_mapa_consulta) |>
    left_join(tabela_p_ideal_muni |>
                #rename(ano_selecionado = '2020') |>
                rename(ano_selecionado = input$ano_mapa_consulta) |>
                select(code_muni_curto, ano_selecionado),
              by = "code_muni_curto")  |>
    mutate(ano_selecionado = ifelse(is.na(ano_selecionado), 0, ano_selecionado))

  #nome_estado <- lista_codigos_uf[lista_codigos_uf$code_state == input$uf_mapa_consulta,"name_state"]
  centroide_estado = tabela_uf_zoom[tabela_uf_zoom$code_state == input$uf_mapa_consulta, c("lat","long","zoom")]

  ####
  pm_leaflet <- leaflet(map_ideal) %>%
    addTiles() %>% # Adiciona o fundo do mapa
    addProviderTiles("CartoDB.Positron") |>
    addPolygons(
      fillColor = ~colorNumeric(palette = "Blues", domain = map_ideal$ano_selecionado)(ano_selecionado),
      weight = 1, # Remove as bordas
      fillOpacity = 1, # Define a opacidade do preenchimento
      color = "darkgrey", # Remove a cor da borda
      highlightOptions = highlightOptions(
        weight = 2,
        color = "black",
        bringToFront = TRUE
      ),
      label = ~paste0(name_muni, ": ", ano_selecionado, "%"), # Tooltip ao passar o mouse
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "12px",
        direction = "auto"
      )
    ) %>%
    addLegend(
      pal = colorNumeric(palette = "Blues", domain = map_ideal$ano_selecionado),
      values = ~ano_selecionado,
      title = "Porcentagem",
      position = "bottomright"
    ) %>%
    setView(
      lng = mean(st_coordinates(map_ideal)[, 1]) + centroide_estado$long,
      lat = mean(st_coordinates(map_ideal)[, 2]) + centroide_estado$lat,
      zoom = centroide_estado$zoom # Ajusta o nível de zoom
    )  |>
    addControl(
      html = paste0("<h3 style='margin:0; text-align:center;'> Proporção de gestantes que realizaram 7 ou mais consultas de pré-natal em ", input$ano_mapa_consulta, "</h3>"),
      position = "topright" # Pode ser 'topleft', 'topright', 'bottomleft', ou 'bottomright'
    )
  }

  # Visualizar o mapa
  pm_leaflet
  })
  
  # Cards Informativos - Consultas
  
  output$p90_consultas <- renderText({
    unlist(tabela_summarybox_consulta[DTNASC == input$ano_grafico_des_consulta,"p90"])
    })
  
  output$p10_consultas <- renderText({
    unlist(tabela_summarybox_consulta[DTNASC == input$ano_grafico_des_consulta,"p10"])
  })
  
  output$razao_consultas <- renderText({
    format(unlist(tabela_summarybox_consulta[DTNASC == input$ano_grafico_des_consulta,"razao"]), nsmall = 2)
  })
  
  output$cv_consultas <- renderText({
    unlist(tabela_summarybox_consulta[DTNASC == input$ano_grafico_des_consulta,"coef_var"])
  })
  
  # Gráfico de Desigualdade - Consultas
  output$grafico_des_consulta <- renderPlotly({
    
    # Definir as cores da métrica usando a paleta "plasma"
    metrica_colors <- setNames(plasma(5, begin = 0.1, end = 0.9, direction = 1), 
                               c("Máximo", "75 percentil", "Mediana", "25 percentil", "Mínimo"))
    
    tabela_percentil_consulta_filtrada = tabela_percentil_ideal |>
      filter(DTNASC == as.numeric(input$ano_grafico_des_consulta)) |>
      mutate(name_state = factor(name_state, levels = rev(mapa_uf_vetor), ordered = TRUE),
             metrica = factor(metrica, levels = c("Máximo", "75 percentil", "Mediana", "25 percentil", "Mínimo"),
                              ordered = TRUE))
    
    
    tabela_segmento_consulta = tabela_percentil_consulta_filtrada |>
      filter(metrica %in% c("Mínimo","Máximo")) |>
      pivot_wider(names_from = metrica, values_from = valor) |>
      mutate(name_state = factor(name_state, levels = rev(mapa_uf_vetor), ordered = TRUE))
    
    plot_ly(height = 650) %>%
      add_trace(
        data = tabela_percentil_consulta_filtrada,
        x = ~valor, 
        y = ~name_state, 
        color = ~metrica,
        colors = metrica_colors,
        type = "scatter", mode = "markers",
        marker = list(size = 10, opacity = 0.9),
        hovertemplate = "none"
      ) %>%
      # Adiciona segmentos para representar os intervalos (mínimo-máximo)
      add_segments(
        data = tabela_segmento_consulta,
        x = ~Mínimo, xend = ~Máximo,
        y = ~name_state, yend = ~name_state,
        line = list(color = linecolor, width = 2),
        showlegend = FALSE
      ) %>%
      # Adiciona pontos para cada métrica (percentis)
      add_trace(
        data = tabela_percentil_consulta_filtrada,
        x = ~valor,
        y = ~name_state,
        color = ~metrica,
        showlegend = FALSE,
        type = "scatter", mode = "markers",
        marker = list(size = 10, opacity = 0.9),
        hovertemplate = ~paste0(metrica, ": ", format(round(valor,1), decimal.mark = ","), "%<extra></extra>")
      ) %>%
      layout(
        xaxis = list(
          title = "", 
          range = c(-2, 101), 
          tickvals = seq(0, 100, by = 25)
        ),
        yaxis = list(
          title = "", 
          range = c(-1, 27),
          categoryorder = "array", 
          categoryarray = rev(mapa_uf_vetor),
          tickangle = 0,  # Mantém os labels retos
          tickfont = list(size = 14),
          tickmode = "array",
          standoff = 0  # Aumenta a distância entre os labels e o eixo
        ),
        legend = list(
          title = list(text = ""), 
          orientation = "v",
          x = 1,  
          y = 0.5,  
          xanchor = "left",  
          yanchor = "middle"
        ),
        shapes = list(
          list(
            type = "line",
            x0 = 0, x1 = 100,  # De 0 a 100 no eixo X
            y0 = -0.5, y1 = -0.5,  # Linha na base do gráfico
            line = list(color = "black", width = 0.5)
          )
        ),
        font = list(size = 14, color = "black"),
        margin = list(t = 10) 
      ) |>
      config(displayModeBar = FALSE)
    
    })
  
  #
  
  output$evolucao_razao_consultas <- renderPlotly({
    
    if(input$evolucao_des_consulta == "razao") {
    plot_ly(data = tabela_summarybox_consulta, 
            x = ~DTNASC, 
            y = ~ratio, 
            type = 'scatter',
            mode = 'lines+markers',
            marker = list(color = 'steelblue'),
            line = list(color = 'steelblue'),
            hovertemplate = ~paste0("Razão: ", razao,
                                    "<br>P90: ", p90,
                                    "<br>P10: ", p10,
                                    "<extra></extra>")
    ) %>%
      layout(title = "Razão entre Percentil 90 e Percentil 10",
             xaxis = list(title = "",
                          tickvals = 2000:2023,
                          tickangle = 45,
                          showline = TRUE,
                          linecolor = "black",
                          tickfont = list(color = "black", size = 14)),
             yaxis = list(title = "Razão",
                          range = c(0, 15),
                          tickvals = seq(0, 14, by = 2),
                          showline = TRUE,
                          linecolor = "black",
                          tickfont = list(color = "black", size = 14)),
             plot_bgcolor = "white",
             margin = list(t = 50, b = 50, l = 50, r = 10))
    } else if (input$evolucao_des_consulta == "perc") {

      plot_ly(data = tabela_summarybox_consulta) %>%
        add_trace(x = ~DTNASC,
                  y = ~p90_num,
                  type = 'scatter',
                  mode = 'lines+markers',
                  name = "Percentil 90",
                  marker = list(color = 'steelblue'),
                  line = list(color = 'steelblue'),
                  hovertemplate = ~paste0("<br>P90: ", p90,"<extra></extra>")
        ) %>%
        add_trace(x = ~DTNASC,
                  y = ~p10,
                  type = 'scatter',
                  mode = 'lines+markers',
                  name = "Percentil 10",
                  marker = list(color = 'red'),
                  line = list(color = 'red'),
                  hovertemplate = ~paste0("<br>P10: ", p10,"<extra></extra>")
        ) %>%
        layout(title = "Evolução dos Percentis por Ano de Nascimento",
               xaxis = list(title = "",
                            tickvals = 2000:2023,
                            tickangle = 45,
                            showline = TRUE,
                            linecolor = "black",
                            tickfont = list(color = "black", size = 14)),
               yaxis = list(title = "Percentil",
                            range = c(0, 100),
                            tickvals = seq(0, 100, by = 10),
                            showline = TRUE,
                            linecolor = "black",
                            tickfont = list(color = "black", size = 14)),
               plot_bgcolor = "white",
               margin = list(t = 50, b = 50, l = 50, r = 10))

    } else if(input$evolucao_des_consulta == "coef_var") {

      plot_ly(data = tabela_summarybox_consulta,
              x = ~DTNASC,
              y = ~coef_var_num,
              type = 'scatter',
              mode = 'lines+markers',
              marker = list(color = 'steelblue'),
              line = list(color = 'steelblue'),
              hovertemplate = ~paste0("Coeficiente de variação: ", coef_var,
                                      "<extra></extra>")
      ) %>%
        layout(title = "Evolução do Coeficiente de Variação por Ano de Nascimento",
               xaxis = list(title = "",
                            tickvals = 2000:2023,
                            tickangle = 45,
                            showline = TRUE,
                            linecolor = "black",
                            tickfont = list(color = "black", size = 14)),
               yaxis = list(title = "Coeficiente de Variação",
                            range = c(0, 75),
                            tickvals = seq(0, 70, by = 10),
                            showline = TRUE,
                            linecolor = "black",
                            tickfont = list(color = "black", size = 14)),
               plot_bgcolor = "white",
               margin = list(t = 50, b = 50, l = 50, r = 10))
    }
    
  })
  
  # Peso
  
  output$mapa_peso <- renderLeaflet({
   
    if(input$uf_mapa_peso == 0) {
      
      map_peso = mapa_uf |>
        left_join(tabela_p_peso_uf |>
                    rename(ano_selecionado = input$ano_mapa_peso) |>
                   # rename(ano_selecionado = "2020") |>
                    select(name_state, ano_selecionado),
                  by = "name_state")
      
      max_value_peso <- max(map_peso$ano_selecionado, na.rm = TRUE)
      
      map_peso_leaflet <- leaflet(map_peso) %>%
        addTiles() %>% # Adiciona o fundo do mapa
        addProviderTiles("CartoDB.Positron") |>
        addPolygons(
          fillColor = ~colorNumeric(palette = "Blues", domain = c(0, max_value_peso))(ano_selecionado), # Define domínio fixo
          weight = 0, # Remove as bordas
          fillOpacity = 1, # Define a opacidade do preenchimento
          color = NA, # Remove a cor da borda
          highlightOptions = highlightOptions(
            weight = 2,
            color = "black",
            bringToFront = TRUE
          ),
          label = ~paste0(name_state, ": ", ano_selecionado, "%"), # Tooltip ao passar o mouse
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "12px",
            direction = "auto"
          )
        ) %>%
        addLegend(
          pal = colorNumeric(palette = "Blues", domain = c(0, max_value_peso)), # Domínio fixo para a legenda
          values = c(0, max_value_peso), # Valores fixos para a escala
          title = "Porcentagem",
          position = "bottomright"
        ) %>%
        setView(
          lng = mean(st_coordinates(map_peso)[, 1]),
          lat = mean(st_coordinates(map_peso)[, 2]) + 2,
          zoom = 4 # Ajusta o nível de zoom
        ) |>
        addLabelOnlyMarkers(data = map_peso,
                            lng = ~lon, lat = ~lat - 2, label = ~abbrev_state,
                            labelOptions = labelOptions(noHide = TRUE,
                                                        direction = 'top',
                                                        textOnly = TRUE,
                                                        style = list("color" = "orange")))  |>
        addControl(
          html = paste0("<h3 style='margin:0; text-align:center;'> Proporção de Nascimentos com Recém-nascido com Baixo Peso em ", input$ano_mapa_peso, "</h3>"),
          position = "topright" # Pode ser 'topleft', 'topright', 'bottomleft', ou 'bottomright'
        )
      
    } else {
      
      map_peso = mapa_muni |>
        #filter(code_state == 11) |>
        filter(code_state == input$uf_mapa_peso) |>
        left_join(tabela_p_peso_muni |>
                    #rename(ano_selecionado = '2020') |>
                    rename(ano_selecionado = input$ano_mapa_peso) |>
                    select(code_muni_curto, ano_selecionado),
                  by = "code_muni_curto")  |>
        mutate(ano_selecionado = ifelse(is.na(ano_selecionado), 0, ano_selecionado))
      
      #nome_estado <- lista_codigos_uf[lista_codigos_uf$code_state == input$uf_mapa_peso,"name_state"]
      centroide_estado = tabela_uf_zoom[tabela_uf_zoom$code_state == input$uf_mapa_peso, c("lat","long","zoom")]
      #centroide_estado = tabela_uf_zoom[tabela_uf_zoom$code_state == 11, c("lat","long","zoom")]
      
      ####
      max_value_peso <- max(map_peso$ano_selecionado, na.rm = TRUE)
      
      map_peso_leaflet <- leaflet(map_peso) %>%
        addTiles() %>% # Adiciona o fundo do mapa
        addProviderTiles("CartoDB.Positron") |>
        addPolygons(
          fillColor = ~colorNumeric(palette = "Blues", domain = c(0,max_value_peso))(ano_selecionado),
          weight = 1, # Remove as bordas
          fillOpacity = 1, # Define a opacidade do preenchimento
          color = "darkgrey", # Remove a cor da borda
          highlightOptions = highlightOptions(
            weight = 2,
            color = "black",
            bringToFront = TRUE
          ),
          label = ~paste0(name_muni, ": ", ano_selecionado, "%"), # Tooltip ao passar o mouse
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "12px",
            direction = "auto"
          )
        ) %>%
        addLegend(
          pal = colorNumeric(palette = "Blues", domain = c(0,max_value_peso)),
          values = c(0, max_value_peso), # Define um intervalo fixo para a legenda
          title = "Porcentagem",
          position = "bottomright"
        ) %>%
        setView(
          lng = mean(st_coordinates(map_peso)[, 1]) + centroide_estado$long,
          lat = mean(st_coordinates(map_peso)[, 2]) + centroide_estado$lat,
          zoom = centroide_estado$zoom # Ajusta o nível de zoom
        )  |>
        addControl(
          html = paste0("<h3 style='margin:0; text-align:center;'> Proporção de Nascimentos com Recém-nascido com Baixo Peso em ", input$ano_mapa_peso, "</h3>"),
          position = "topright" # Pode ser 'topleft', 'topright', 'bottomleft', ou 'bottomright'
        )
    }
    map_peso_leaflet
  })
  
  # Cards Informativos - Peso
  
  output$p90_pesos <- renderText({
    unlist(tabela_summarybox_peso[DTNASC == input$ano_grafico_des_peso,"p90"])
  })
  
  output$p10_pesos <- renderText({
    unlist(tabela_summarybox_peso[DTNASC == input$ano_grafico_des_peso,"p10"])
  })
  
  output$razao_pesos <- renderText({
    format(unlist(tabela_summarybox_peso[DTNASC == input$ano_grafico_des_peso,"razao"]), nsmall = 2)
  })
  
  output$cv_pesos <- renderText({
    unlist(tabela_summarybox_peso[DTNASC == input$ano_grafico_des_peso,"coef_var"])
  })
  
output$grafico_des_peso <- renderPlotly({
  
  
  # Definir as cores da métrica usando a paleta "plasma"
  metrica_colors <- setNames(plasma(5, begin = 0.1, end = 0.9, direction = 1), 
                             c("Máximo", "75 percentil", "Mediana", "25 percentil", "Mínimo"))
  
  tabela_percentil_peso_filtrada = tabela_percentil_peso |>
    #filter(DTNASC == 2020)  %>%
    filter(DTNASC == as.numeric(input$ano_grafico_des_peso)) |>
    mutate(name_state = factor(name_state, levels = rev(mapa_uf_vetor), ordered = TRUE),
           metrica = factor(metrica, levels = c("Máximo", "75 percentil", "Mediana", "25 percentil", "Mínimo"),
                            ordered = TRUE))
  
  
  tabela_segmento_peso = tabela_percentil_peso_filtrada |>
    filter(metrica %in% c("Mínimo","Máximo")) |>
    pivot_wider(names_from = metrica, values_from = valor) |>
    mutate(name_state = factor(name_state, levels = rev(mapa_uf_vetor), ordered = TRUE))
  
  plot_ly(height = 650) %>%
    add_trace(
      data = tabela_percentil_peso_filtrada,
      x = ~valor, 
      y = ~name_state, 
      color = ~metrica,
      colors = metrica_colors,
      type = "scatter", mode = "markers",
      marker = list(size = 10, opacity = 0.9),
      hovertemplate = "none"
    ) %>%
    # Adiciona segmentos para representar os intervalos (mínimo-máximo)
    add_segments(
      data = tabela_segmento_peso,
      x = ~Mínimo, xend = ~Máximo,
      y = ~name_state, yend = ~name_state,
      line = list(color = linecolor, width = 2),
      showlegend = FALSE
    ) %>%
    # Adiciona pontos para cada métrica (percentis)
    add_trace(
      data = tabela_percentil_peso_filtrada,
      x = ~valor,
      y = ~name_state,
      color = ~metrica,
      showlegend = FALSE,
      type = "scatter", mode = "markers",
      marker = list(size = 10, opacity = 0.9),
      hovertemplate = ~paste0(metrica, ": ", format(round(valor,1), decimal.mark = ","), "%<extra></extra>")
    ) %>%
    layout(
      xaxis = list(
        title = "", 
        range = c(-2, 101), 
        tickvals = seq(0, 100, by = 25)
      ),
      yaxis = list(
        title = "", 
        range = c(-1, 27),
        categoryorder = "array", 
        categoryarray = rev(mapa_uf_vetor),
        tickangle = 0,  # Mantém os labels retos
        tickfont = list(size = 14),
        tickmode = "array",
        standoff = 0  # Aumenta a distância entre os labels e o eixo
      ),
      legend = list(
        title = list(text = ""), 
        orientation = "v",
        x = 1,  
        y = 0.5,  
        xanchor = "left",  
        yanchor = "middle"
      ),
      shapes = list(
        list(
          type = "line",
          x0 = 0, x1 = 100,  # De 0 a 100 no eixo X
          y0 = -0.5, y1 = -0.5,  # Linha na base do gráfico
          line = list(color = "black", width = 0.5)
        )
      ),
      font = list(size = 14, color = "black"),
      margin = list(t = 10) 
    ) |>
    config(displayModeBar = FALSE)

})


output$evolucao_razao_pesos <- renderPlotly({
  
  if(input$evolucao_des_peso == "razao") {
    plot_ly(data = tabela_summarybox_peso, 
            x = ~DTNASC, 
            y = ~ratio, 
            type = 'scatter',
            mode = 'lines+markers',
            marker = list(color = 'steelblue'),
            line = list(color = 'steelblue'),
            hovertemplate = ~paste0("Razão: ", razao,
                                    "<br>P90: ", p90,
                                    "<br>P10: ", p10,
                                    "<extra></extra>")
    ) %>%
      layout(title = "Razão entre Percentil 90 e Percentil 10",
             xaxis = list(title = "",
                          tickvals = 2000:2023,
                          tickangle = 45,
                          showline = TRUE,
                          linecolor = "black",
                          tickfont = list(color = "black", size = 14)),
             yaxis = list(title = "Razão",
                          range = c(0, 5),
                          tickvals = seq(0, 5, by = 1),
                          showline = TRUE,
                          linecolor = "black",
                          tickfont = list(color = "black", size = 14)),
             plot_bgcolor = "white",
             margin = list(t = 50, b = 50, l = 50, r = 10))
  } else if (input$evolucao_des_peso == "perc") {
    
    plot_ly(data = tabela_summarybox_peso) %>%
      add_trace(x = ~DTNASC,
                y = ~p90_num,
                type = 'scatter',
                mode = 'lines+markers',
                name = "Percentil 90",
                marker = list(color = 'steelblue'),
                line = list(color = 'steelblue'),
                hovertemplate = ~paste0("<br>P90: ", format(round(p90_num,1), decimal.mark = ",", nsmall = 1),"%<extra></extra>")
      ) %>%
      add_trace(x = ~DTNASC,
                y = ~p10_num,
                type = 'scatter',
                mode = 'lines+markers',
                name = "Percentil 10",
                marker = list(color = 'red'),
                line = list(color = 'red'),
                hovertemplate = ~paste0("<br>P10: ", format(round(p10_num,1), decimal.mark = ",", nsmall = 1),"%<extra></extra>")
      ) %>%
      layout(title = "Evolução dos Percentis por Ano de Nascimento",
             xaxis = list(title = "",
                          tickvals = 2000:2023,
                          tickangle = 45,
                          showline = TRUE,
                          linecolor = "black",
                          tickfont = list(color = "black", size = 14)),
             yaxis = list(title = "Percentil",
                          range = c(0, 12),
                          tickvals = seq(0, 12, by = 2),
                          showline = TRUE,
                          linecolor = "black",
                          tickfont = list(color = "black", size = 14)),
             plot_bgcolor = "white",
             margin = list(t = 50, b = 50, l = 50, r = 10))
    
  } else if(input$evolucao_des_peso == "coef_var") {
    
    plot_ly(data = tabela_summarybox_peso,
            x = ~DTNASC,
            y = ~coef_var_num,
            type = 'scatter',
            mode = 'lines+markers',
            marker = list(color = 'steelblue'),
            line = list(color = 'steelblue'),
            hovertemplate = ~paste0("Coeficiente de variação: ", coef_var,
                                    "<extra></extra>")
    ) %>%
      layout(title = "Evolução do Coeficiente de Variação por Ano de Nascimento",
             xaxis = list(title = "",
                          tickvals = 2000:2023,
                          tickangle = 45,
                          showline = TRUE,
                          linecolor = "black",
                          tickfont = list(color = "black", size = 14)),
             yaxis = list(title = "Coeficiente de Variação",
                          range = c(0, 75),
                          tickvals = seq(0, 70, by = 10),
                          showline = TRUE,
                          linecolor = "black",
                          tickfont = list(color = "black", size = 14)),
             plot_bgcolor = "white",
             margin = list(t = 50, b = 50, l = 50, r = 10))
  }
  
})

  # Adequação Pré-natal

output$mapa_adeq <- renderLeaflet({
  
  if(input$uf_mapa_adeq == 0) {
    
    map_adeq = mapa_uf |>
      left_join(tabela_p_adeq_uf |>
                  #rename(ano_selecionado = "2016") |>
                  rename(ano_selecionado = input$ano_mapa_adeq) |>
                  select(name_state, ano_selecionado),
                by = "name_state")
    
    leaflet(map_adeq) %>%
      addTiles() %>% # Adiciona o fundo do mapa
      addProviderTiles("CartoDB.Positron") |>
      addPolygons(
        fillColor = ~colorNumeric(palette = "Blues", domain = map_adeq$ano_selecionado)(ano_selecionado),
        weight = 0, # Remove as bordas
        fillOpacity = 1, # Define a opacidade do preenchimento
        color = NA, # Remove a cor da borda
        highlightOptions = highlightOptions(
          weight = 2,
          color = "black",
          bringToFront = TRUE
        ),
        label = ~paste0(name_state, ": ", ano_selecionado, "%"), # Tooltip ao passar o mouse
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "12px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = colorNumeric(palette = "Blues", domain = map_adeq$ano_selecionado),
        values = ~ano_selecionado,
        title = "Porcentagem",
        position = "bottomright"
      ) %>%
      setView(
        lng = mean(st_coordinates(map_adeq)[, 1]),
        lat = mean(st_coordinates(map_adeq)[, 2])+2,
        zoom = 4 # Ajusta o nível de zoom
      ) |>
      addLabelOnlyMarkers(data = map_adeq,
                          lng = ~lon, lat = ~lat-2, label = ~abbrev_state,
                          labelOptions = labelOptions(noHide = TRUE,
                                                      direction = 'top',
                                                      textOnly = TRUE,
                                                      style = list("color" = "orange")))  |>
      addControl(
        html = paste0("<h3 style='margin:0; text-align:center;'> Proporção de Nascimentos Classificados como Adequado ou Mais que Adequado</h3>"),
        position = "topright" # Pode ser 'topleft', 'topright', 'bottomleft', ou 'bottomright'
      )
    
  } else {
    
    map_adeq = mapa_muni |>
      filter(code_state == input$uf_mapa_adeq) |>
      #filter(code_state == 11) |>
      left_join(tabela_p_adeq_muni |>
                 # rename(ano_selecionado = '2020') |>
                  rename(ano_selecionado = input$ano_mapa_adeq) |>
                  select(code_muni_curto, ano_selecionado),
                by = "code_muni_curto")  |>
      mutate(ano_selecionado = ifelse(is.na(ano_selecionado), 0, ano_selecionado))
    
    centroide_estado = tabela_uf_zoom[tabela_uf_zoom$code_state == input$uf_mapa_adeq, c("lat","long","zoom")]

    ####
    leaflet(map_adeq) %>%
      addTiles() %>% # Adiciona o fundo do mapa
      addProviderTiles("CartoDB.Positron") |>
      addPolygons(
        fillColor = ~colorNumeric(palette = "Blues", domain = map_adeq$ano_selecionado)(ano_selecionado),
        weight = 1, # Remove as bordas
        fillOpacity = 1, # Define a opacidade do preenchimento
        color = "darkgrey", # Remove a cor da borda
        highlightOptions = highlightOptions(
          weight = 2,
          color = "black",
          bringToFront = TRUE
        ),
        label = ~paste0(name_muni, ": ", ano_selecionado, "%"), # Tooltip ao passar o mouse
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "12px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = colorNumeric(palette = "Blues", domain = map_adeq$ano_selecionado),
        values = ~ano_selecionado,
        title = "Porcentagem",
        position = "bottomright"
      ) %>%
      setView(
        lng = mean(st_coordinates(map_adeq)[, 1]) + centroide_estado$long,
        lat = mean(st_coordinates(map_adeq)[, 2]) + centroide_estado$lat,
        zoom = centroide_estado$zoom # Ajusta o nível de zoom
      )  |>
      addControl(
        html = paste0("<h3 style='margin:0; text-align:center;'> Proporção de Nascimentos Classificados como Adequado ou Mais que Adequado</h3>"),
        position = "topright" # Pode ser 'topleft', 'topright', 'bottomleft', ou 'bottomright'
      )
    
  }
})

# Cards Informativos - Peso

output$p90_adeq <- renderText({
  unlist(tabela_summarybox_adeq[DTNASC == input$ano_grafico_des_adeq,"p90"])
})

output$p10_adeq <- renderText({
  unlist(tabela_summarybox_adeq[DTNASC == input$ano_grafico_des_adeq,"p10"])
})

output$razao_adeq <- renderText({
  format(unlist(tabela_summarybox_adeq[DTNASC == input$ano_grafico_des_adeq,"razao"]), nsmall = 2)
})

output$cv_adeq <- renderText({
  unlist(tabela_summarybox_adeq[DTNASC == input$ano_grafico_des_adeq,"coef_var"])
})

# Gráfico de Desigualdade - Adequacao Pre-natal
output$grafico_des_adeq <- renderPlotly({
  
  # Definir as cores da métrica usando a paleta "plasma"
  metrica_colors <- setNames(plasma(5, begin = 0.1, end = 0.9, direction = 1), 
                             c("Máximo", "75 percentil", "Mediana", "25 percentil", "Mínimo"))
  
  tabela_percentil_adeq_filtrada = tabela_percentil_adeq |>
    filter(DTNASC == as.numeric(input$ano_grafico_des_adeq)) |>
    mutate(name_state = factor(name_state, levels = rev(mapa_uf_vetor), ordered = TRUE),
           metrica = factor(metrica, levels = c("Máximo", "75 percentil", "Mediana", "25 percentil", "Mínimo"),
                            ordered = TRUE))
  
  
  tabela_segmento_adeq = tabela_percentil_adeq_filtrada |>
    filter(metrica %in% c("Mínimo","Máximo")) |>
    pivot_wider(names_from = metrica, values_from = valor) |>
    mutate(name_state = factor(name_state, levels = rev(mapa_uf_vetor), ordered = TRUE))
  
  plot_ly(height = 650) %>%
    add_trace(
      data = tabela_percentil_adeq_filtrada,
      x = ~valor, 
      y = ~name_state, 
      color = ~metrica,
      colors = metrica_colors,
      type = "scatter", mode = "markers",
      marker = list(size = 10, opacity = 0.9),
      hovertemplate = 'none'
    ) %>%
    # Adiciona segmentos para representar os intervalos (mínimo-máximo)
    add_segments(
      data = tabela_segmento_adeq,
      x = ~Mínimo, xend = ~Máximo,
      y = ~name_state, yend = ~name_state,
      line = list(color = linecolor, width = 2),
      showlegend = FALSE
    ) %>%
    # Adiciona pontos para cada métrica (percentis)
    add_trace(
      data = tabela_percentil_adeq_filtrada,
      x = ~valor,
      y = ~name_state,
      color = ~metrica,
      showlegend = FALSE,
      type = "scatter", mode = "markers",
      marker = list(size = 10, opacity = 0.9),
      hovertemplate = ~paste0(metrica, ": ", format(round(valor,1), decimal.mark = ","), "%<extra></extra>")
    ) %>%
    layout(
      xaxis = list(
        title = "", 
        range = c(-2, 101), 
        tickvals = seq(0, 100, by = 25)
      ),
      yaxis = list(
        title = "", 
        range = c(-1, 27),
        categoryorder = "array", 
        categoryarray = rev(mapa_uf_vetor),
        tickangle = 0,  # Mantém os labels retos
        tickfont = list(size = 14),
        tickmode = "array",
        standoff = 0  # Aumenta a distância entre os labels e o eixo
      ),
      legend = list(
        title = list(text = ""), 
        orientation = "v",
        x = 1,  
        y = 0.5,  
        xanchor = "left",  
        yanchor = "middle"
      ),
      shapes = list(
        list(
          type = "line",
          x0 = 0, x1 = 100,  # De 0 a 100 no eixo X
          y0 = -0.5, y1 = -0.5,  # Linha na base do gráfico
          line = list(color = "black", width = 0.5)
        )
      ),
      font = list(size = 14, color = "black"),
      margin = list(t = 10) 
    ) |>
    config(displayModeBar = FALSE)
  
})



output$evolucao_razao_adeq <- renderPlotly({
  
  if(input$evolucao_des_adeq == "razao") {
    plot_ly(data = tabela_summarybox_adeq, 
            x = ~DTNASC, 
            y = ~ratio, 
            type = 'scatter',
            mode = 'lines+markers',
            marker = list(color = 'steelblue'),
            line = list(color = 'steelblue'),
            hovertemplate = ~paste0("Razão: ", razao,
                                    "<br>P90: ", p90,
                                    "<br>P10: ", p10,
                                    "<extra></extra>")
    ) %>%
      layout(title = "Razão entre Percentil 90 e Percentil 10",
             xaxis = list(title = "",
                          tickvals = 2000:2023,
                          tickangle = 45,
                          showline = TRUE,
                          linecolor = "black",
                          tickfont = list(color = "black", size = 14)),
             yaxis = list(title = "Razão",
                          range = c(0, 2.2),
                          tickvals = seq(0, 2, by = 0.4),
                          showline = TRUE,
                          linecolor = "black",
                          tickfont = list(color = "black", size = 14)),
             plot_bgcolor = "white",
             margin = list(t = 50, b = 50, l = 50, r = 10))
  } else if (input$evolucao_des_adeq == "perc") {
    
    plot_ly(data = tabela_summarybox_adeq) %>%
      add_trace(x = ~DTNASC,
                y = ~p90_num,
                type = 'scatter',
                mode = 'lines+markers',
                name = "Percentil 90",
                marker = list(color = 'steelblue'),
                line = list(color = 'steelblue'),
                hovertemplate = ~paste0("<br>P90: ", p90,"<extra></extra>")
      ) %>%
      add_trace(x = ~DTNASC,
                y = ~p10,
                type = 'scatter',
                mode = 'lines+markers',
                name = "Percentil 10",
                marker = list(color = 'red'),
                line = list(color = 'red'),
                hovertemplate = ~paste0("<br>P10: ", p10,"<extra></extra>")
      ) %>%
      layout(title = "Evolução dos Percentis por Ano de Nascimento",
             xaxis = list(title = "",
                          tickvals = 2000:2023,
                          tickangle = 45,
                          showline = TRUE,
                          linecolor = "black",
                          tickfont = list(color = "black", size = 14)),
             yaxis = list(title = "Percentil",
                          range = c(0, 100),
                          tickvals = seq(0, 100, by = 10),
                          showline = TRUE,
                          linecolor = "black",
                          tickfont = list(color = "black", size = 14)),
             plot_bgcolor = "white",
             margin = list(t = 50, b = 50, l = 50, r = 10))
    
  } else if(input$evolucao_des_adeq == "coef_var") {
    
    plot_ly(data = tabela_summarybox_adeq,
            x = ~DTNASC,
            y = ~coef_var_num,
            type = 'scatter',
            mode = 'lines+markers',
            marker = list(color = 'steelblue'),
            line = list(color = 'steelblue'),
            hovertemplate = ~paste0("Coeficiente de variação: ", coef_var,
                                    "<extra></extra>")
    ) %>%
      layout(title = "Evolução do Coeficiente de Variação por Ano de Nascimento",
             xaxis = list(title = "",
                          tickvals = 2000:2023,
                          tickangle = 45,
                          showline = TRUE,
                          linecolor = "black",
                          tickfont = list(color = "black", size = 14)),
             yaxis = list(title = "Coeficiente de Variação",
                          range = c(0, 25),
                          tickvals = seq(0, 24, by = 4),
                          showline = TRUE,
                          linecolor = "black",
                          tickfont = list(color = "black", size = 14)),
             plot_bgcolor = "white",
             margin = list(t = 50, b = 50, l = 50, r = 10))
  }
  
})

# Dados individuais
output$grafico_escolaridade_mae <- renderPlotly({
  p_escmae_consultas_ano <- p_escmae_consultas_ano |>
    mutate(escolaridade_mae_novo = factor(escolaridade_mae_novo,
                                          levels = 1:4,
                                          labels = c("0 a 3 anos",
                                                     "4 a 7 anos",
                                                     "8 a 11 anos",
                                                     "12 ou mais")))
  
  plot_ly() %>%
    # Add scatter plot points
    add_trace(
      data = p_escmae_consultas_ano,
      x = ~DTNASC, y = ~p_ideal, color = ~factor(escolaridade_mae_novo),
      type = "scatter", mode = "markers",
      colors = plasma(n = 4, begin = 0.1, end = 0.9, direction = -1),
      marker = list(size = 10, opacity = 0.9),
      hovertemplate = "none",
      showlegend = FALSE
    ) %>%
    # Add segments for the min-max range of escolaridade_mae_novo
    add_segments(
      data = tabela_segmento_escmae,
      x = ~DTNASC, xend = ~DTNASC,
      y = ~menor_escolaridade, yend = ~maior_escolaridade,
      line = list(color = plasma(n = 4, begin = 0.1, end = 0.9, direction = -1)[4], width = 2),
      showlegend = FALSE
    ) %>%
    # Add scatter plot points
    add_trace(
      data = p_escmae_consultas_ano,
      x = ~DTNASC, y = ~p_ideal, color = ~factor(escolaridade_mae_novo),
      type = "scatter", mode = "markers",
      colors = plasma(n = 4, begin = 0.1, end = 0.9, direction = -1),
      marker = list(size = 10, opacity = 0.9),
      hovertemplate = ~paste0("Escolaridade: ", escolaridade_mae_novo, 
                              "<br>Porcentagem: ", format(round(p_ideal,1), decimal.mark = ","), "%<extra></extra>")
    ) %>%
    layout(
      xaxis = list(
        title = "\nAno de Nascimento",
        tickvals = 2000:2023,  # Ensure specific year breaks
        tickangle = 45,
        tickfont = list(size = 14),
        showgrid = TRUE
      ),
      yaxis = list(
        title = "Porcentagem de nascimentos com 7\nou mais consultas pré-natal",
        range = c(0, 100),
        tickfont = list(size = 14)
      ),
      legend = list(
        title = list(text = "Tempo de\nEscolaridade\nda Mãe"),
        orientation = "v",
        x = 1, y = 1,
        xanchor = "left", yanchor = "top"
      ),
      font = list(size = 16, color = "black"),
      margin = list(t = 50),
      hovermode = "closest"
    ) |>
    config(displayModeBar = FALSE)
  
})

output$grafico_razao_escolaridade <- renderPlotly({
  
  plot_ly(
    data = p_razao_escmae_consultas_ano,
    x = ~DTNASC,
    y = ~razao,
    type = "scatter",
    mode = "markers+lines",
    marker = list(size = 10, color = "steelblue", opacity = 0.9),
    hovertemplate = ~paste0("Razão: ", format(round(razao,2), decimal.mark = ","), "<extra></extra>")
  ) %>%
    layout(
      xaxis = list(
        title = "Ano de Nascimento\n",
        tickvals = 2000:2023,  # Explicit year breaks
        tickangle = 45,
        tickfont = list(size = 14),
        showgrid = TRUE
      ),
      yaxis = list(
        title = "\nRazão da proporção\nmaior/menor categoria",
        tickfont = list(size = 14),
        range = c(0,3)
      ),
      font = list(size = 16, color = "black"),
      margin = list(t = 50),
      hovermode = "closest",
      showlegend = FALSE  # Remove legend
    ) |>
    config(displayModeBar = FALSE)
  
  
})

output$card_escolaridade_1 <- renderText({
  p_escmae_consultas_ano |>
    filter(DTNASC == 2023) |>
    filter(escolaridade_mae_novo == 1) |>
    select(p_ideal) |>
    mutate(p_ideal = format(round(p_ideal, 1), decimal.mark = ",")) |>
    unlist()
})

output$card_escolaridade_2 <- renderText({
  p_escmae_consultas_ano |>
    filter(DTNASC == 2023) |>
    filter(escolaridade_mae_novo == 2) |>
    select(p_ideal) |>
    mutate(p_ideal = format(round(p_ideal, 1), decimal.mark = ",")) |>
    unlist()
})

output$card_escolaridade_3 <- renderText({
  p_escmae_consultas_ano |>
    filter(DTNASC == 2023) |>
    filter(escolaridade_mae_novo == 3) |>
    select(p_ideal) |>
    mutate(p_ideal = format(round(p_ideal, 1), decimal.mark = ",")) |>
    unlist()
})

output$card_escolaridade_4 <- renderText({
  p_escmae_consultas_ano |>
    filter(DTNASC == 2023) |>
    filter(escolaridade_mae_novo == 4) |>
    select(p_ideal) |>
    mutate(p_ideal = format(round(p_ideal, 1), decimal.mark = ",")) |>
    unlist()
})

output$card_razao_escolaridade <- renderText({
  p_razao_escmae_consultas_ano |>
    filter(DTNASC == 2023) |>
    select(razao) |>
    mutate(razao = format(round(razao, 2), decimal.mark = ",", nsmall = 2)) |>
    unlist()
})

output$grafico_raca_cor <- renderPlotly({
  
  p_racacor_consultas_ano <-  p_racacor_consultas_ano |>
    mutate(RACACOR = factor(RACACOR,
                            levels = 1:5,
                            labels = c("Branca","Preta","Amarela","Parda","Indígena")
    ))
  
  tabela_segmento_racacor = data.frame(tabela_segmento_racacor)
  plot_ly() %>%
    # Add scatter plot points
    add_trace(
      data = p_racacor_consultas_ano,
      x = ~DTNASC, y = ~p_ideal, color = ~factor(RACACOR),
      type = "scatter", mode = "markers",
      colors = plasma(n = 5, begin = 0.1, end = 0.9, direction = -1),
      marker = list(size = 10, opacity = 0.9),
      hovertemplate = "none",
      showlegend = FALSE
    ) %>%
    # Add segments for the min-max range of RACACOR
    add_segments(
      data = tabela_segmento_racacor,
      x = ~DTNASC, xend = ~DTNASC,
      y = ~menor_valor, yend = ~maior_valor,
      line = list(color = plasma(n = 4, begin = 0.1, end = 0.9, direction = -1)[4], width = 1),
      showlegend = FALSE
    ) %>%
    # Add scatter plot points
    add_trace(
      data = p_racacor_consultas_ano,
      x = ~DTNASC, y = ~p_ideal, color = ~factor(RACACOR),
      type = "scatter", mode = "markers",
      colors = plasma(n = 4, begin = 0.1, end = 0.9, direction = -1),
      marker = list(size = 10, opacity = 0.9),
      hovertemplate = ~paste0("Raça/cor: ", RACACOR, 
                              "<br>Porcentagem: ", format(round(p_ideal,1), decimal.mark = ","), "%<extra></extra>")
    ) %>%
    layout(
      xaxis = list(
        title = "Ano de Nascimento",
        tickvals = 2000:2023,  # Ensure specific year breaks
        tickangle = 45,
        tickfont = list(size = 14),
        showgrid = TRUE
      ),
      yaxis = list(
        title = "Porcentagem de nascimentos com 7\nou mais consultas pré-natal",
        range = c(0, 100),
        tickfont = list(size = 14)
      ),
      legend = list(
        title = list(text = "Raça/cor"),
        orientation = "h",  # Make the legend horizontal
        x = 0.5, y = -0.2,  # Position below the plot
        xanchor = "center", yanchor = "top"
      ),
      font = list(size = 14, color = "black"),
      margin = list(t = 50, b = 100),  # Adjust bottom margin for space for the legend
      hovermode = "closest"
    ) |>
    config(displayModeBar = FALSE)
})

output$grafico_razao_raca_cor <- renderPlotly({
  
  p_razao_racacor_consultas_ano <- p_razao_racacor_consultas_ano |>
    mutate(razao = factor(razao, levels = c("razao_2",
                                            "razao_3",
                                            "razao_4",
                                            "razao_5"),
                          labels = c("Preto",
                                     "Amarelo",
                                     "Pardo",
                                     "Indígena")))
  
  plot_ly(data = p_razao_racacor_consultas_ano) %>%
    # Adiciona pontos com legenda
    add_trace(
      x = ~DTNASC,
      y = ~value,
      color = ~razao,
      colors = "Set1",  # Escolha uma paleta apropriada
      type = "scatter",
      mode = "markers",
      marker = list(size = 6),
      hovertemplate = ~paste0("Ano: ", DTNASC, "<br>Razão: ", format(round(value,2), decimal.mark = ","), "<extra></extra>")
    ) %>%
    # Adiciona linhas sem legenda
    add_trace(
      x = ~DTNASC,
      y = ~value,
      color = ~razao,
      colors = "Set1",
      type = "scatter",
      mode = "lines",
      showlegend = FALSE
    ) %>%
    layout(
      yaxis = list(title = "\nRazão da proporção entre categorias", range = c(0, 5)),
      xaxis = list(
        title = "Ano de Nascimento\n",
        tickvals = 2000:2023,
        tickangle = 45  # Rotaciona os rótulos do eixo X
      ),
      legend = list(
        title = list(text = "Razão entre branco e"),
        orientation = "h",  # Make the legend horizontal
        x = 0.5, y = -0.2,  # Position below the plot
        xanchor = "center", yanchor = "top"
      ),
      font = list(size = 14, color = "black"),
      margin = list(t = 50),
      showlegend = TRUE
    ) |> 
    config(displayModeBar = FALSE)
})

#
output$card_raca_cor_1 <- renderText({
  p_racacor_consultas_ano |>
    filter(DTNASC == 2023) |>
    filter(RACACOR == 1) |>
    select(p_ideal) |>
    mutate(p_ideal = format(round(p_ideal, 1), decimal.mark = ",")) |>
    unlist()
})

output$card_raca_cor_2 <- renderText({
  p_racacor_consultas_ano |>
    filter(DTNASC == 2023) |>
    filter(RACACOR == 2) |>
    select(p_ideal) |>
    mutate(p_ideal = format(round(p_ideal, 1), decimal.mark = ",")) |>
    unlist()
})

output$card_raca_cor_3 <- renderText({
  p_racacor_consultas_ano |>
    filter(DTNASC == 2023) |>
    filter(RACACOR == 3) |>
    select(p_ideal) |>
    mutate(p_ideal = format(round(p_ideal, 1), decimal.mark = ",")) |>
    unlist()
})

output$card_raca_cor_4 <- renderText({
  p_racacor_consultas_ano |>
    filter(DTNASC == 2023) |>
    filter(RACACOR == 4) |>
    select(p_ideal) |>
    mutate(p_ideal = format(round(p_ideal, 1), decimal.mark = ",")) |>
    unlist()
})

output$card_raca_cor_5 <- renderText({
  p_racacor_consultas_ano |>
    filter(DTNASC == 2023) |>
    filter(RACACOR == 5) |>
    select(p_ideal) |>
    mutate(p_ideal = format(round(p_ideal, 1), decimal.mark = ",")) |>
    unlist()
})


output$card_razao_raca_cor_2 <- renderText({
  p_razao_racacor_consultas_ano |>
    filter(DTNASC == 2023) |>
    filter(razao == "razao_2") |>
    select(value) |>
    mutate(value = format(round(value, 2), decimal.mark = ",", nsmall = 2)) |>
    unlist()
})

output$card_razao_raca_cor_3 <- renderText({
  p_razao_racacor_consultas_ano |>
    filter(DTNASC == 2023) |>
    filter(razao == "razao_3") |>
    select(value) |>
    mutate(value = format(round(value, 2), decimal.mark = ",", nsmall = 2)) |>
    unlist()
})

output$card_razao_raca_cor_4 <- renderText({
  p_razao_racacor_consultas_ano |>
    filter(DTNASC == 2023) |>
    filter(razao == "razao_4") |>
    select(value) |>
    mutate(value = format(round(value, 2), decimal.mark = ",", nsmall = 2)) |>
    unlist()
})

output$card_razao_raca_cor_5 <- renderText({
  p_razao_racacor_consultas_ano |>
    filter(DTNASC == 2023) |>
    filter(razao == "razao_5") |>
    select(value) |>
    mutate(value = format(round(value, 2), decimal.mark = ",", nsmall = 2)) |>
    unlist()
})


}

# Run the application 
shinyApp(ui = ui, server = server)


