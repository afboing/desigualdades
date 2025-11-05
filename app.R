# Verificar e instalar pacotes caso não estejam instalados
packages <- c("remotes", "shiny", "bslib", "tidyverse", "data.table", "plotly", "viridis", "summaryBox")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    
    if (pkg == "summaryBox") {
      remotes::install_github("deepanshu88/summaryBox")
    } else {
      install.packages(pkg)
    }
    
    # Try loading again after installation
    require(pkg, character.only = TRUE)
  }
}

# Carregar dados para o dashboard
#source("carregar_dados_shiny.R")

# Define UI for application
ui <- fluidPage(
  
  page_navbar(
    title = "Painel de Dados: Taxa de Realização de Mamografia",
    bg = "#eff7ff", # bg do navbar
    underline = TRUE,
    fluid = TRUE,
    
    nav_panel(
      title = "População geral",
      
      card(
        full_screen = TRUE,
        height = 800,
        card_header("Proporção de mulheres que realizaram mamografia de acordo com ano da PNS"),
        
        fluidRow(
          #column(width = 3,
          summaryBox('Razão do resultado do PNS 2019 e 2013 para exame realizado alguma vez na vida', 
                     2,#textOutput("razao_renda_2013"), 
                     width = 6, 
                     icon = 'fas  fa-divide',
                     style = "primary"),
          
          summaryBox('Razão do resultado do PNS 2019 e 2013 para exames realizados nos últimos 2 anos', 
                     1,#textOutput("razao_renda_2019"), 
                     width = 6, 
                     icon = 'fas  fa-divide',
                     style = "info")
        ),
        
        fluidRow(
          
        column(width = 2),  
        column(width = 8,
        plotlyOutput("plot_populacao")
          ),
        column(width = 2)
        )
      ) # card
    ),
    
    nav_panel(
      title = "Perfil por Renda Familiar",
      
      card(
        full_screen = TRUE,
        height = 800,
        card_header("Proporção de mulheres que realizaram mamografia de acordo com a renda familiar "),
        
        layout_sidebar(sidebar = 
                         sidebar(
                           open = "always",
                           selectInput(inputId = "renda_exame",
                                       label= "Recorte temporal do exame:",
                                       choices = c("Alguma vez na vida", "Nos últimos 2 anos"),
                                       selected = "Alguma vez na vida",
                                       multiple=FALSE
                           )
                         ),
                       fluidRow(
                         #column(width = 3,
                         summaryBox('Razão entre maior e menor renda em 2013', 
                                    textOutput("razao_renda_2013"), 
                                    width = 5, 
                                    icon = 'fas  fa-divide',
                                    style = "primary"),
                         
                         summaryBox('Razão entre maior e menor renda em 2019', 
                                    textOutput("razao_renda_2019"), 
                                    width = 5, 
                                    icon = 'fas  fa-divide',
                                    style = "info")
                       ),
                       
                       plotlyOutput("plot_exame_renda")
                       #output
                       
        ) # layout_sidebar
      ) # card
    ),
    
    nav_panel(
      title = "Perfil por Raça/Cor",
      
      
      card(
        full_screen = TRUE,
        height = 800,
        card_header("Proporção de mulheres que realizaram mamografia de acordo com a raça/cor "),
        
        layout_sidebar(sidebar = 
                         sidebar(
                           open = "always",
                           selectInput(inputId = "raca_cor_exame",
                                       label="Recorte temporal do exame:",
                                       choices = c("Alguma vez na vida", "Nos últimos 2 anos"),
                                       selected = "Alguma vez na vida",
                                       multiple=FALSE
                           )
                         ),
                       fluidRow(
                         #column(width = 3,
                         summaryBox('Razão entre brancos e pardos em 2013', 
                                    textOutput("razao_raca_cor_pardo_2013"), 
                                    width = 4, 
                                    icon = 'fas  fa-divide',
                                    style = "primary"),
                         
                         summaryBox('Razão entre brancos e pardos em 2019', 
                                    textOutput("razao_raca_cor_pardo_2019"), 
                                    width = 4, 
                                    icon = 'fas  fa-divide',
                                    style = "info")
                       ),
                       
                       plotlyOutput("plot_exame_raca_cor")
                       #output
                       
        ) # layout_sidebar
      ) # card
    ),
    
    nav_panel(title = "Sobre",
              card(
              tags$div(
                tags$p(
                  tags$strong("A Pesquisa Nacional de Saúde (PNS)"),
                  " é um inquérito de base domiciliar realizado pelo Instituto Brasileiro de Geografia e Estatística (IBGE), em parceria com o Ministério da Saúde, que tem como objetivo principal produzir informações sobre as condições de saúde, estilos de vida, acesso e uso de serviços de saúde da população brasileira. Trata-se de uma pesquisa amostral com representatividade nacional, regional e estadual, que adota um plano amostral complexo por conglomerados em múltiplos estágios. A PNS é uma das principais fontes de dados sobre a situação de saúde da população e subsidia o planejamento e a avaliação de políticas públicas na área da saúde."
                ),
                
                tags$p(
                  "O painel atual busca descrever o perfil da proporção de mulheres que realizaram exame de mamografia ao menos uma vez na vida ou nos últimos dois anos, tanto para a população geral quanto para recortes de renda e raça/cor."
                ),
                
                tags$h5("Resumo metodológico:"),
                
                tags$p(
                  "A análise utiliza dados da Pesquisa Nacional de Saúde (PNS) de 2013 e 2019 para estimar a cobertura de mamografia em mulheres brasileiras entre 50 e 69 anos. 
                  Considerou-se apenas os moradores selecionados para o questionário individual, e foi calculado um peso ajustado para esse grupo, garantindo a representatividade da amostra. 
                  Em seguida, foram criados dois indicadores de realização de mamografia: um para mulheres que fizeram o exame alguma vez na vida e outro restrito aos últimos dois anos. 
                  Utilizando o plano amostral da pesquisa, estimaram-se as proporções de realização do exame na população total e por subgrupos de renda 
                  (renda domiciliar per capita) e raça/cor. Os resultados foram organizados em tabelas com estimativas estratificadas para a população amostrada como um todo, por quintis de 
                  renda e categorias de cor/raça."
                ),
                
                tags$h5("Referências:"),
                
                tags$ul(
                  tags$li(
                    tags$a(href = "https://www.ibge.gov.br/estatisticas/sociais/saude/9160-pesquisa-nacional-de-saude.html",
                           target = "_blank",
                           "INSTITUTO BRASILEIRO DE GEOGRAFIA E ESTATÍSTICA – IBGE. Pesquisa Nacional de Saúde – PNS.")
                  ),
                  tags$li(
                    tags$a(href = "https://www.pns.icict.fiocruz.br/",
                           target = "_blank",
                           "FIOCRUZ. Base de Dados da PNS.")
                  )
                )
              )
              )
              )
)
)

# Define server logic
server <- function(input, output) {
  
  linecolor = plasma(n = 5, begin = 0.1, end = 0.9, direction = -1)[4]
  
  # Definir as cores da métrica usando a paleta "plasma"
  metrica_colors <- setNames(plasma(2, begin = 0.1, end = 0.9, direction = -1), 
                             c("2013", "2019"))
 
  tabela_resultados_2anos <- fread(file = "dados/tabela_resultados_2anos.csv")
  tabela_resultados <- fread(file = "dados/tabela_resultados.csv")
  
  output$plot_populacao <- renderPlotly({
    tabela_populacao = tabela_resultados |> filter(variavel == "Populacao") |> mutate(tempo = "Alguma vez na vida") |>
      rbind(tabela_resultados_2anos |> filter(variavel == "Populacao") |> mutate(tempo = "Últimos 2 anos"))|>
      mutate(valor = valor*100,
             Ano = factor(Ano))
    
    tabela_segmento_pop = tabela_populacao |>
      mutate(Ano = factor(Ano, levels = c(2013, 2019), labels = c("cmin", "cmax"))) |>
      pivot_wider(names_from = Ano, values_from = valor)
    
    plot_ly(height = 550) %>%
      add_trace(
        data = tabela_populacao,
        x = ~valor, 
        y = ~tempo, 
        color = ~Ano,
        colors = metrica_colors,
        type = "scatter", mode = "markers",
        marker = list(size = 10, opacity = 0.9),
        hovertemplate = "none"
      ) %>%
      # Adiciona segmentos para representar os intervalos (mínimo-máximo)
      add_segments(
        data = tabela_segmento_pop,
        x = ~cmin, xend = ~cmax,
        y = ~tempo, yend = ~tempo,
        line = list(color = linecolor, width = 2),
        showlegend = FALSE
      ) %>%
      # Adiciona pontos para cada métrica (percentis)
      add_trace(
        data = tabela_populacao,
        x = ~valor, 
        y = ~tempo, 
        color = ~Ano,
        showlegend = FALSE,
        type = "scatter", mode = "markers",
        marker = list(size = 10, opacity = 0.9),
        hovertemplate = ~paste0("Ano: ", Ano,
                                "<br>", tempo, ": ", format(round(valor,1), decimal.mark = ",", nsmall = 2), "%<extra></extra>")
      ) %>%
      layout(
        xaxis = list(
          title = "Proporção de mulheres que realizaram mamografia (%)", 
          range = c(23, 101), 
          tickvals = seq(25, 100, by = 25)
        ),
        yaxis = list(
          title = "Recorte temporal do exame", 
          #range = c(-1, 27),
          categoryorder = "array", 
          #  categoryarray = rev(mapa_uf_vetor),
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
            x0 = 25, x1 = 100,  # De 0 a 100 no eixo X
            y0 = -0.5, y1 = -0.5,  # Linha na base do gráfico
            line = list(color = "black", width = 0.5)
          )
        ),
        font = list(size = 14, color = "black"),
        margin = list(t = 10) 
      ) |>
      config(displayModeBar = FALSE)
    
    
  })
  
  tabela_exame <- reactive ({ 
    
  if(input$renda_exame == "Alguma vez na vida") {
    tabela_resultados
  } else {
    tabela_resultados_2anos
  }
  })
  
  tabela_raca_cor <- reactive ({
    
    if(input$raca_cor_exame == "Alguma vez na vida") {
      tabela_resultados
    } else {
      tabela_resultados_2anos
    }
  })
  
  output$razao_renda_2019 <- renderText({
    
    tabela_exame() |> 
      filter(variavel == "Renda" & Ano == 2019) |>
      mutate(ord = 1:n()) |>
      filter(ord %in% c(1,5)) |>
      summarise(val = last(valor)/first(valor),
                val = format(round(val,2), decimal.mark = ",", nsmall = 2)) |>
      unlist()
  })
  
  output$razao_renda_2013 <- renderText({
    
    tabela_exame() |> 
      filter(variavel == "Renda" & Ano == 2013) |>
      mutate(ord = 1:n()) |>
      filter(ord %in% c(1,5)) |>
      summarise(val = last(valor)/first(valor),
                val = format(round(val,2), decimal.mark = ",", nsmall = 2))|>
      unlist()
    
  })
  
  
  output$plot_exame_renda <- renderPlotly({

    tabela_percentil_exame_filtrada = tabela_exame() |>
      filter(variavel == "Renda") |>
      mutate(valor = valor*100,
             Ano = factor(Ano))
    
    tabela_segmento_exame = tabela_exame() |>
      filter(variavel == "Renda") |>
      mutate(valor = valor*100) |>
      mutate(Ano = factor(Ano, levels = c(2013, 2019), labels = c("cmin", "cmax"))) |>
      pivot_wider(names_from = Ano, values_from = valor)
    
    plot_ly(height = 550) %>%
      add_trace(
        data = tabela_percentil_exame_filtrada,
        x = ~valor, 
        y = ~categoria, 
        color = ~Ano,
        colors = metrica_colors,
        type = "scatter", mode = "markers",
        marker = list(size = 10, opacity = 0.9),
        hovertemplate = "none"
      ) %>%
      # Adiciona segmentos para representar os intervalos (mínimo-máximo)
      add_segments(
        data = tabela_segmento_exame,
        x = ~cmin, xend = ~cmax,
        y = ~categoria, yend = ~categoria,
        line = list(color = linecolor, width = 2),
        showlegend = FALSE
      ) %>%
      # Adiciona pontos para cada métrica (percentis)
      add_trace(
        data = tabela_percentil_exame_filtrada,
        x = ~valor, 
        y = ~categoria, 
        color = ~Ano,
        showlegend = FALSE,
        type = "scatter", mode = "markers",
        marker = list(size = 10, opacity = 0.9),
        hovertemplate = ~paste0("Ano: ", Ano,
          "<br>", categoria, ": ", format(round(valor,1), decimal.mark = ",", nsmall = 2), "%<extra></extra>")
      ) %>%
      layout(
        xaxis = list(
          title = "Proporção de mulheres que realizaram mamografia (%)", 
          range = c(23, 101), 
          tickvals = seq(25, 100, by = 25)
        ),
        yaxis = list(
          title = "", 
          #range = c(-1, 27),
          categoryorder = "array", 
          #  categoryarray = rev(mapa_uf_vetor),
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
            x0 = 25, x1 = 100,  # De 0 a 100 no eixo X
            y0 = -0.5, y1 = -0.5,  # Linha na base do gráfico
            line = list(color = "black", width = 0.5)
          )
        ),
        font = list(size = 14, color = "black"),
        margin = list(t = 10) 
      ) |>
      config(displayModeBar = FALSE)
    
  })
  
  
  output$razao_raca_cor_pardo_2019 <- renderText({
    
    tabela_raca_cor() |> 
      filter(variavel == "Raca_cor" & Ano == 2019) |>
      mutate(ord = 1:n()) |>
      filter(ord %in% c(1,2)) |>
      summarise(val = last(valor)/first(valor),
                val = format(round(val,2), decimal.mark = ",", nsmall = 2)) |>
      unlist()
    
  })
  
  output$razao_raca_cor_pardo_2013 <- renderText({
    
    #tabela_raca_cor() |> 
      tabela_resultados |>
      filter(variavel == "Raca_cor" & Ano == 2013) |>
      mutate(ord = 1:n()) |>
      filter(ord %in% c(1,2)) |>
      summarise(val = last(valor)/first(valor),
                val = format(round(val,2), decimal.mark = ",", nsmall = 2))|>
      unlist()
    
  })
  
  
  output$plot_exame_raca_cor <- renderPlotly({
    
    linecolor = plasma(n = 5, begin = 0.1, end = 0.9, direction = -1)[4]
    
    # Definir as cores da métrica usando a paleta "plasma"
    metrica_colors <- setNames(plasma(2, begin = 0.1, end = 0.9, direction = -1), 
                               c("2013", "2019"))
    
    tabela_percentil_exame_filtrada_rc = tabela_raca_cor() |>
      filter(variavel == "Raca_cor") |>
      mutate(valor = valor*100,
             Ano = factor(Ano))
    
    tabela_segmento_exame_rc = tabela_raca_cor() |>
      filter(variavel == "Raca_cor") |>
      mutate(valor = valor*100) |>
      mutate(Ano = factor(Ano, levels = c(2013, 2019), labels = c("cmin", "cmax"))) |>
      pivot_wider(names_from = Ano, values_from = valor)
    
    plot_ly(height = 550) %>%
      add_trace(
        data = tabela_percentil_exame_filtrada_rc,
        x = ~valor, 
        y = ~categoria, 
        color = ~Ano,
        colors = metrica_colors,
        type = "scatter", mode = "markers",
        marker = list(size = 10, opacity = 0.9),
        hovertemplate = "none"
      ) %>%
      # Adiciona segmentos para representar os intervalos (mínimo-máximo)
      add_segments(
        data = tabela_segmento_exame_rc,
        x = ~cmin, xend = ~cmax,
        y = ~categoria, yend = ~categoria,
        line = list(color = linecolor, width = 2),
        showlegend = FALSE
      ) %>%
      # Adiciona pontos para cada métrica (percentis)
      add_trace(
        data = tabela_percentil_exame_filtrada_rc,
        x = ~valor, 
        y = ~categoria, 
        color = ~Ano,
        showlegend = FALSE,
        type = "scatter", mode = "markers",
        marker = list(size = 10, opacity = 0.9),
        hovertemplate = ~paste0("Ano: ", Ano,
                                "<br>", categoria, ": ", format(round(valor,1), decimal.mark = ",", nsmall = 2), "%<extra></extra>")
      ) %>%
      layout(
        xaxis = list(
          title = "Proporção de mulheres que realizaram mamografia (%)", 
          range = c(23, 101), 
          tickvals = seq(25, 100, by = 25)
        ),
        yaxis = list(
          title = "", 
          #range = c(-1, 27),
          categoryorder = "array", 
          #  categoryarray = rev(mapa_uf_vetor),
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
            x0 = 25, x1 = 100,  # De 0 a 100 no eixo X
            y0 = -0.5, y1 = -0.5,  # Linha na base do gráfico
            line = list(color = "black", width = 0.5)
          )
        ),
        font = list(size = 14, color = "black"),
        margin = list(t = 10) 
      ) |>
      config(displayModeBar = FALSE)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


