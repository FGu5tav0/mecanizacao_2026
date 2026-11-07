library(shiny)
library(bslib)
library(tidyverse)
library(DT)

`%||%` <- function(a, b) if (is.null(a)) b else a
fmt <- function(x, d = 2) {
  format(round(x, d), decimal.mark = ",", big.mark = ".", trim = TRUE)
}

# "12, 15, 20-30" -> inteiros; vazio -> integer(0); inválido -> NULL
parse_lista <- function(x) {
  x <- gsub("\\s", "", x %||% "")
  if (!nzchar(x)) {
    return(integer(0))
  }
  out <- unlist(lapply(strsplit(x, "[,;]")[[1]], function(it) {
    if (grepl("^\\d+-\\d+$", it)) {
      l <- as.integer(strsplit(it, "-")[[1]])
      seq(min(l), max(l))
    } else if (grepl("^\\d+$", it)) {
      as.integer(it)
    } else {
      NA_integer_
    }
  }))
  if (anyNA(out)) NULL else sort(unique(out))
}

# Valores de referência (editáveis): PMS em g, população em plantas/ha
culturas <- tribble(
  ~nome                 , ~pms  , ~pop    , ~esp , ~germ ,
  "Soja (graúda)"       , 160   ,  280000 ,   50 ,    85 ,
  "Milho (graúda)"      , 330   ,   60000 ,   70 ,    90 ,
  "Feijão (graúda)"     , 250   ,  240000 ,   50 ,    85 ,
  "Girassol (graúda)"   ,  60   ,   45000 ,   70 ,    85 ,
  "Trigo (miúda)"       ,  38   , 3000000 ,   17 ,    85 ,
  "Aveia-preta (miúda)" ,  25   , 3000000 ,   17 ,    80 ,
  "Sorgo (miúda)"       ,  25   ,  180000 ,   50 ,    85 ,
  "Milheto (miúda)"     ,   8   , 1200000 ,   20 ,    80 ,
  "Brachiaria (miúda)"  ,   7   , 1000000 ,   20 ,    60 ,
  "Canola (miúda)"      ,   3.5 ,  400000 ,   34 ,    90
)

ui <- page_sidebar(
  title = "Cálculo de sementes — miúdas e graúdas",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  sidebar = sidebar(
    width = 330,
    accordion(
      open = c("Cultura e semente", "Máquina"),
      multiple = TRUE,
      accordion_panel(
        "Cultura e semente",
        selectInput(
          "cultura",
          "Cultura (valores de referência)",
          culturas$nome
        ),
        numericInput("pms", "Peso de mil sementes — PMS (g)", 160, min = 0.1),
        numericInput("pop", "População desejada (plantas/ha)", 280000, min = 1),
        numericInput("esp", "Espaçamento entre linhas (cm)", 50, min = 1),
        numericInput("germ", "Germinação (%)", 85, min = 1, max = 100),
        numericInput("pureza", "Pureza (%)", 98, min = 1, max = 100),
        numericInput(
          "emerg",
          "Emergência/sobrevivência em campo (%)",
          90,
          min = 1,
          max = 100
        )
      ),
      accordion_panel(
        "Área e custo",
        numericInput("area", "Área (ha)", 10, min = 0),
        numericInput(
          "preco",
          "Preço da semente (R$/kg) — opcional",
          NA,
          min = 0
        )
      ),
      accordion_panel(
        "Máquina",
        numericInput("dia", "Diâmetro da roda motriz (cm)", 80, min = 10),
        numericInput("slip", "Patinagem (%)", 5, min = 0, max = 40),
        numericInput("linhas", "Nº de linhas", 8, min = 1)
      )
    )
  ),
  layout_columns(
    fill = FALSE,
    value_box("Sementes/ha", textOutput("vb_semha")),
    value_box("Dose (kg/ha)", textOutput("vb_kg")),
    value_box("Sementes por metro", textOutput("vb_semm")),
    value_box("Entre sementes (cm)", textOutput("vb_cm"))
  ),
  navset_card_tab(
    nav_panel("Dose e plantio", tableOutput("resumo")),
    nav_panel(
      "Graúdas: disco + engrenagens",
      layout_columns(
        col_widths = c(4, 8),
        div(
          textInput(
            "furos",
            "Furos dos discos disponíveis",
            "20, 24, 28, 36, 40"
          ),
          textInput("dentes", "Dentes das engrenagens", "12-30"),
          numericInput(
            "fixa",
            "Relação fixa adicional (roda → eixo)",
            1,
            min = 0.01,
            step = 0.01
          ),
          helpText(
            "Sementes/m = furos × (motora/movida) × relação fixa ÷ perímetro da roda (descontada a patinagem)."
          ),
          uiOutput("alvo_graudas")
        ),
        DTOutput("tab_graudas")
      )
    ),
    nav_panel(
      "Miúdas: aferição e vazão",
      layout_columns(
        col_widths = c(4, 8),
        div(
          numericInput("voltas", "Voltas da roda no teste", 10, min = 1),
          numericInput(
            "coletado",
            "Massa coletada no teste (g, todas as linhas)",
            NA,
            min = 0
          ),
          numericInput("vel", "Velocidade de trabalho (km/h)", 6, min = 0.1),
          numericInput(
            "caixa",
            "Capacidade da caixa de sementes (kg)",
            300,
            min = 1
          )
        ),
        tableOutput("tab_miudas")
      )
    ),
    nav_panel(
      "PMS",
      layout_columns(
        col_widths = c(5, 7),
        div(
          textAreaInput(
            "pesos",
            "Pesos de 8 amostras de 100 sementes (g)",
            "15,9; 16,1; 16,0; 15,8; 16,2; 16,0; 15,9; 16,1",
            rows = 3
          ),
          actionButton(
            "usar_pms",
            "Usar este PMS",
            class = "btn-primary btn-sm"
          )
        ),
        tableOutput("tab_pms")
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$cultura, {
    c <- filter(culturas, nome == input$cultura)
    updateNumericInput(session, "pms", value = c$pms)
    updateNumericInput(session, "pop", value = c$pop)
    updateNumericInput(session, "esp", value = c$esp)
    updateNumericInput(session, "germ", value = c$germ)
  })

  calc <- reactive({
    req(input$pms, input$pop, input$esp, input$germ, input$pureza, input$emerg)
    fator <- input$germ / 100 * input$pureza / 100 * input$emerg / 100
    sem_ha <- input$pop / fator
    list(
      fator = fator,
      sem_ha = sem_ha,
      kg_ha = sem_ha * input$pms / 1e6,
      sem_m = sem_ha * (input$esp / 100) / 1e4
    )
  })

  output$vb_semha <- renderText(fmt(calc()$sem_ha, 0))
  output$vb_kg <- renderText(fmt(calc()$kg_ha, 2))
  output$vb_semm <- renderText(fmt(calc()$sem_m, 2))
  output$vb_cm <- renderText(fmt(100 / calc()$sem_m, 1))

  output$resumo <- renderTable(
    {
      k <- calc()
      r <- tribble(
        ~Item                                      , ~Valor                                                ,
        "Valor cultural (germinação × pureza)"     , paste0(fmt(input$germ * input$pureza / 100, 1), " %") ,
        "Fator total (inclui emergência em campo)" , paste0(fmt(k$fator * 100, 1), " %")                   ,
        "Sementes necessárias por hectare"         , fmt(k$sem_ha, 0)                                      ,
        "Sementes por m²"                          , fmt(k$sem_ha / 1e4, 1)                                ,
        "Sementes por metro de linha"              , fmt(k$sem_m, 2)                                       ,
        "Distância média entre sementes"           , paste0(fmt(100 / k$sem_m, 1), " cm")                  ,
        "Dose"                                     , paste0(fmt(k$kg_ha, 2), " kg/ha")                     ,
        "Total para a área"                        , paste0(fmt(k$kg_ha * input$area, 1), " kg")
      )
      if (isTruthy(input$preco)) {
        r <- bind_rows(
          r,
          tibble(
            Item = "Custo estimado de semente",
            Valor = paste0("R$ ", fmt(k$kg_ha * input$area * input$preco, 2))
          )
        )
      }
      r
    },
    striped = TRUE,
    width = "100%"
  )

  # ---- Graúdas ----
  perimetro <- reactive({
    req(input$dia, !is.na(input$slip))
    pi * input$dia / 100 * (1 - input$slip / 100) # m efetivos por volta
  })

  output$alvo_graudas <- renderUI({
    div(
      class = "alert alert-info py-2",
      paste0("Alvo: ", fmt(calc()$sem_m, 2), " sementes/m")
    )
  })

  output$tab_graudas <- renderDT({
    furos <- parse_lista(input$furos)
    dentes <- parse_lista(input$dentes)
    validate(
      need(
        !is.null(furos) && length(furos) > 0,
        "Lista de furos inválida (ex.: 20, 24, 36)."
      ),
      need(
        !is.null(dentes) && length(dentes) > 0,
        "Lista de dentes inválida (ex.: 12-30)."
      )
    )
    alvo <- calc()$sem_m
    d <- expand_grid(Furos = furos, Motora = dentes, Movida = dentes) |>
      mutate(
        `Relação` = Motora / Movida * input$fixa,
        `Sementes/m` = Furos * `Relação` / perimetro(),
        `Espaçamento (cm)` = 100 / `Sementes/m`,
        `Erro (%)` = (`Sementes/m` / alvo - 1) * 100
      ) |>
      arrange(abs(`Erro (%)`), Furos) |>
      head(15)
    datatable(
      d,
      rownames = FALSE,
      options = list(dom = "tp", pageLength = 8, order = list())
    ) |>
      formatRound(
        c("Relação", "Sementes/m", "Espaçamento (cm)", "Erro (%)"),
        3,
        mark = ".",
        dec.mark = ","
      )
  })

  # ---- Miúdas ----
  output$tab_miudas <- renderTable(
    {
      taxa <- calc()$kg_ha
      larg <- input$linhas * input$esp / 100
      area_teste <- input$voltas * perimetro() * larg # m²
      massa <- taxa * 0.1 * area_teste # g (1 kg/ha = 0,1 g/m²)
      r <- tribble(
        ~Item                               , ~Valor                                                   ,
        "Dose alvo"                         , paste0(fmt(taxa, 2), " kg/ha")                           ,
        "Largura de trabalho"               , paste0(fmt(larg, 2), " m")                               ,
        "Área percorrida no teste"          , paste0(fmt(area_teste, 1), " m²")                        ,
        "Massa a coletar (todas as linhas)" , paste0(fmt(massa, 1), " g")                              ,
        "Massa a coletar por linha"         , paste0(fmt(massa / input$linhas, 1), " g")               ,
        "Vazão em campo"                    , paste0(fmt(taxa * larg * input$vel / 600, 2), " kg/min") ,
        "Rendimento operacional"            , paste0(fmt(larg * input$vel / 10, 2), " ha/h")           ,
        "Área por abastecimento"            , paste0(fmt(input$caixa / taxa, 1), " ha")
      )
      if (isTruthy(input$coletado)) {
        real <- input$coletado / (0.1 * area_teste)
        r <- bind_rows(
          r,
          tribble(
            ~Item                       , ~Valor                                        ,
            "Dose real (pelo teste)"    , paste0(fmt(real, 2), " kg/ha")                ,
            "Desvio em relação ao alvo" , paste0(fmt((real / taxa - 1) * 100, 1), " %")
          )
        )
      }
      r
    },
    striped = TRUE,
    width = "100%"
  )

  # ---- PMS ----
  pms_calc <- reactive({
    v <- suppressWarnings(as.numeric(strsplit(
      gsub(",", ".", input$pesos),
      "[;\\s]+",
      perl = TRUE
    )[[1]]))
    v <- v[!is.na(v)]
    validate(need(
      length(v) >= 2,
      "Informe ao menos 2 pesos, separados por ponto e vírgula."
    ))
    list(pms = mean(v) * 10, cv = sd(v) / mean(v) * 100, n = length(v))
  })

  output$tab_pms <- renderTable(
    {
      p <- pms_calc()
      tibble(
        Item = c(
          "Repetições",
          "PMS (média × 10)",
          "Coeficiente de variação",
          "Avaliação"
        ),
        Valor = c(
          p$n,
          paste0(fmt(p$pms, 1), " g"),
          paste0(fmt(p$cv, 2), " %"),
          if (p$cv <= 4) {
            "Dentro do limite (CV ≤ 4%)"
          } else {
            "CV alto: refaça a pesagem"
          }
        )
      )
    },
    striped = TRUE,
    width = "100%"
  )

  observeEvent(
    input$usar_pms,
    updateNumericInput(session, "pms", value = round(pms_calc()$pms, 1))
  )
}

shinyApp(ui, server)
