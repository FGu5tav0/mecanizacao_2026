library(shiny)
library(bslib)
library(tidyverse)
library(DT)

# ---- Funções auxiliares -------------------------------------------------

# Aceita "2", "2,5", "5/2" ou "3:1"
parse_razao <- function(x) {
  x <- gsub("\\s", "", x %||% "")
  x <- gsub(",", ".", x, fixed = TRUE)
  x <- gsub(":", "/", x, fixed = TRUE)
  partes <- strsplit(x, "/", fixed = TRUE)[[1]]
  nums <- suppressWarnings(as.numeric(partes))
  if (length(nums) == 0 || anyNA(nums)) {
    return(NA_real_)
  }
  if (length(nums) == 1) {
    return(nums)
  }
  if (length(nums) == 2 && nums[2] != 0) {
    return(nums[1] / nums[2])
  }
  NA_real_
}

# Aceita "12, 15, 20-30, 40". Vazio -> integer(0); inválido -> NULL
parse_dentes <- function(x) {
  x <- gsub("\\s", "", x %||% "")
  if (!nzchar(x)) {
    return(integer(0))
  }
  itens <- strsplit(x, "[,;]")[[1]]
  out <- lapply(itens, function(it) {
    if (grepl("^\\d+-\\d+$", it)) {
      lim <- as.integer(strsplit(it, "-")[[1]])
      seq(min(lim), max(lim))
    } else if (grepl("^\\d+$", it)) {
      as.integer(it)
    } else {
      NA_integer_
    }
  })
  out <- unlist(out)
  if (anyNA(out)) {
    return(NULL)
  }
  sort(unique(out[out >= 3]))
}

fmt <- function(x, d = 4) {
  format(round(x, d), decimal.mark = ",", trim = TRUE)
}

# Contorno de uma engrenagem (dentes quadrados) com raio primitivo = z/2
engrenagem_poly <- function(z, cx, nome) {
  th <- seq(0, 2 * pi, length.out = z * 16 + 1)
  rr <- (z / 2) * if_else(sin(z * th) > 0, 1.07, 0.93)
  tibble(x = cx + rr * cos(th), y = rr * sin(th), nome = nome)
}

cor_melhor <- "#1B7837"

# ---- UI -----------------------------------------------------------------
ui <- page_sidebar(
  title = "Buscador de relações de engrenagens",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  sidebar = sidebar(
    width = 340,
    accordion(
      open = c("Alvo", "Engrenagens"),
      multiple = TRUE,
      accordion_panel(
        "Alvo",
        textInput(
          "alvo",
          "Relação desejada",
          value = "2",
          placeholder = "ex.: 2,5 ou 5/2 ou 3:1"
        ),
        radioButtons(
          "conv",
          "Convenção da relação",
          choices = c(
            "Motora / movida (saída ÷ entrada de rotação)" = "dir",
            "Movida / motora (redução, ex.: 3:1 = 3)" = "inv"
          )
        ),
        numericInput(
          "rpm",
          "Rotação de entrada (rpm) — opcional",
          value = NA,
          min = 0
        )
      ),
      accordion_panel(
        "Engrenagens",
        radioButtons(
          "tipo",
          "Tipo de trem",
          choices = c(
            "Simples (2 engrenagens)" = "simples",
            "Composto (4 engrenagens)" = "composto"
          )
        ),
        sliderInput(
          "faixa",
          "Faixa de dentes",
          min = 4,
          max = 80,
          value = c(6, 30),
          step = 1
        ),
        textInput(
          "lista",
          "Ou use só as que você tem (opcional)",
          value = "",
          placeholder = "ex.: 12, 15, 20-30, 40"
        ),
        helpText("Se preencher a lista, ela substitui a faixa.")
      ),
      accordion_panel(
        "Filtros e exibição",
        numericInput(
          "n_destaque",
          "Quantas combinações mostrar",
          value = 5,
          min = 1,
          max = 50,
          step = 1
        ),
        numericInput(
          "tol",
          "Tolerância máxima de erro (%) — 0 desativa",
          value = 0,
          min = 0,
          step = 0.1
        ),
        numericInput(
          "soma_max",
          "Soma máx. de dentes por par (compacidade)",
          value = NA,
          min = 6
        ),
        checkboxInput(
          "so_diferentes",
          "Ignorar pares com mesmo nº de dentes",
          FALSE
        ),
        radioButtons(
          "modo",
          "Estilo do mapa",
          choices = c(
            "Destaque do alvo" = "destaque",
            "Mapa de calor (escala log)" = "calor"
          )
        )
      )
    ),
    downloadButton("baixar_csv", "Tabela (CSV)", class = "btn-sm"),
    downloadButton("baixar_png", "Mapa (PNG)", class = "btn-sm")
  ),
  layout_columns(
    fill = FALSE,
    value_box("Melhor combinação", textOutput("vb_trem")),
    value_box("Relação obtida", textOutput("vb_rela")),
    value_box("Erro", textOutput("vb_erro")),
    value_box("Rotação de saída", textOutput("vb_rpm"))
  ),
  navset_card_tab(
    nav_panel(
      "Resultados",
      layout_columns(
        col_widths = c(7, 5),
        div(
          DTOutput("tabela"),
          helpText(
            "Clique em uma linha para ver o diagrama e destacá-la no mapa."
          )
        ),
        plotOutput("diagrama", height = "330px")
      )
    ),
    nav_panel(
      "Mapa",
      uiOutput("info_clique"),
      plotOutput("mapa", height = "780px", click = "clique_mapa")
    ),
    nav_panel(
      "Ajuda",
      div(
        class = "p-3",
        h5("Como usar"),
        tags$ul(
          tags$li(
            "Digite a relação desejada como decimal (2,5), fração (5/2) ou razão (3:1)."
          ),
          tags$li(
            "Use a lista de engrenagens para buscar só entre as que você tem no estoque."
          ),
          tags$li(
            "No trem composto, a engrenagem movida do 1º par e a motora do 2º par ficam no mesmo eixo."
          ),
          tags$li(
            "A soma de dentes é proporcional à distância entre eixos (mesmo módulo): quanto menor, mais compacto."
          ),
          tags$li(
            "Cada linha da tabela mostra o trem como motora/movida; no composto: par 1 × par 2."
          )
        ),
        h5("Convenção"),
        p(
          "Com motora/movida, relação < 1 reduz a rotação e > 1 aumenta. ",
          "A rotação de saída é rpm de entrada × motora/movida."
        )
      )
    )
  )
)

# ---- Server -------------------------------------------------------------
server <- function(input, output, session) {
  # ---- Entradas tratadas ----
  alvo <- reactive({
    a <- parse_razao(input$alvo)
    validate(need(
      !is.na(a) && a > 0,
      "Relação inválida. Use, por exemplo: 2, 2,5, 5/2 ou 3:1"
    ))
    a
  })

  inv <- reactive(identical(input$conv, "inv"))
  alvo_int <- reactive(if (inv()) 1 / alvo() else alvo())

  engrenagens <- reactive({
    txt <- input$lista %||% ""
    if (!nzchar(trimws(txt))) {
      return(input$faixa[1]:input$faixa[2])
    }
    g <- parse_dentes(txt)
    validate(need(
      !is.null(g),
      "Lista inválida. Use por exemplo: 12, 15, 20-30, 40"
    ))
    validate(need(
      length(g) >= 2,
      "Informe ao menos 2 engrenagens (mín. 3 dentes)."
    ))
    g
  })

  pares <- reactive({
    g <- engrenagens()
    d <- expand_grid(motora = g, movida = g) |>
      mutate(rela = motora / movida)
    if (input$so_diferentes) {
      d <- filter(d, motora != movida)
    }
    if (isTruthy(input$soma_max)) {
      d <- filter(d, motora + movida <= input$soma_max)
    }
    validate(need(nrow(d) > 0, "Nenhum par satisfaz os filtros."))
    d
  })

  # ---- Busca ----
  resultados <- reactive({
    p <- pares()
    t_int <- alvo_int()
    k <- input$n_destaque
    validate(need(!is.na(k) && k >= 1, "Informe quantas combinações mostrar."))

    if (input$tipo == "simples") {
      r <- p |>
        transmute(
          z1 = motora,
          z2 = movida,
          z3 = NA_integer_,
          z4 = NA_integer_,
          rela
        )
    } else {
      p1 <- arrange(p, rela)
      P <- nrow(p1)
      validate(need(
        P <= 8000,
        "Pares demais para o trem composto: reduza a faixa, use a lista ou a soma máxima."
      ))
      lr <- log2(p1$rela)
      # busca vizinhos mais próximos: para cada par 1, procura o par 2 ideal
      pos <- findInterval(log2(t_int) - lr, lr)
      offs <- seq(-k + 1, k)
      jj <- outer(pos, offs, "+")
      cand <- tibble(
        i = rep(seq_len(P), times = length(offs)),
        j = as.vector(jj)
      ) |>
        filter(j >= 1, j <= P) |>
        mutate(i2 = pmin(i, j), j2 = pmax(i, j)) |>
        distinct(i2, j2)
      r <- tibble(
        z1 = p1$motora[cand$i2],
        z2 = p1$movida[cand$i2],
        z3 = p1$motora[cand$j2],
        z4 = p1$movida[cand$j2],
        rela = p1$rela[cand$i2] * p1$rela[cand$j2]
      )
    }

    alvo_v <- alvo()
    r <- r |>
      mutate(
        rela_disp = if (inv()) 1 / rela else rela,
        erro = abs(log2(rela) - log2(t_int)),
        erro_r = round(erro, 9),
        erro_pct = (rela_disp / alvo_v - 1) * 100,
        soma = z1 + z2 + coalesce(z3, 0L) + coalesce(z4, 0L)
      )

    if (isTruthy(input$tol) && input$tol > 0) {
      r <- filter(r, abs(erro_pct) <= input$tol)
    }

    r <- if (input$tipo == "simples") {
      r |> slice_min(erro_r, n = k, with_ties = TRUE) |> arrange(erro_r, soma)
    } else {
      r |> arrange(erro_r, soma) |> head(k)
    }
    validate(need(nrow(r) > 0, "Nenhuma combinação dentro da tolerância."))
    r
  })

  tab <- reactive({
    rpm <- input$rpm
    resultados() |>
      mutate(
        Trem = if_else(
          is.na(z3),
          paste0(z1, "/", z2),
          paste0(z1, "/", z2, " × ", z3, "/", z4)
        ),
        Efeito = case_when(
          abs(rela - 1) < 1e-9 ~ "Direta (1:1)",
          rela < 1 ~ "Reduz rotação",
          TRUE ~ "Aumenta rotação"
        ),
        rpm_saida = if (isTruthy(rpm)) rpm * rela else NA_real_
      )
  })

  tab_vis <- reactive({
    t <- tab() |>
      transmute(
        Trem,
        `Relação` = rela_disp,
        `Erro (%)` = erro_pct,
        `Soma de dentes` = soma,
        Efeito,
        `Saída (rpm)` = rpm_saida
      )
    if (!isTruthy(input$rpm)) {
      t <- select(t, -`Saída (rpm)`)
    }
    t
  })

  sel <- reactive({
    r <- tab()
    i <- input$tabela_rows_selected
    i <- if (length(i) == 0 || i[1] > nrow(r)) 1 else i[1]
    r[i, ]
  })

  # ---- Cartões de resumo ----
  output$vb_trem <- renderText(tab()$Trem[1])
  output$vb_rela <- renderText(fmt(tab()$rela_disp[1], 4))
  output$vb_erro <- renderText(paste0(fmt(tab()$erro_pct[1], 3), " %"))
  output$vb_rpm <- renderText({
    v <- tab()$rpm_saida[1]
    if (is.na(v)) "—" else paste(fmt(v, 1), "rpm")
  })

  # ---- Tabela ----
  output$tabela <- renderDT({
    t <- tab_vis()
    dt <- datatable(
      t,
      rownames = FALSE,
      selection = list(mode = "single", selected = 1),
      options = list(
        pageLength = 10,
        dom = "tip",
        order = list(),
        language = list(
          info = "_START_–_END_ de _TOTAL_",
          infoEmpty = "Sem resultados",
          emptyTable = "Nenhuma combinação encontrada",
          paginate = list(previous = "Anterior", `next` = "Próxima")
        )
      )
    ) |>
      formatRound("Relação", 4, mark = ".", dec.mark = ",") |>
      formatRound("Erro (%)", 3, mark = ".", dec.mark = ",")
    if ("Saída (rpm)" %in% names(t)) {
      dt <- formatRound(dt, "Saída (rpm)", 1, mark = ".", dec.mark = ",")
    }
    dt
  })

  # ---- Diagrama do trem selecionado ----
  output$diagrama <- renderPlot(
    {
      s <- sel()
      composto <- !is.na(s$z3)
      nm <- if (composto) {
        c("Motora 1", "Movida 1", "Motora 2", "Movida 2")
      } else {
        c("Motora", "Movida")
      }
      x2 <- (s$z1 + s$z2) / 2
      g <- tibble(nome = nm[1:2], z = c(s$z1, s$z2), x = c(0, x2))
      if (composto) {
        g <- bind_rows(
          g,
          tibble(
            nome = nm[3:4],
            z = c(s$z3, s$z4),
            x = c(x2, x2 + (s$z3 + s$z4) / 2)
          )
        )
      }
      pal <- setNames(
        c("#4393C3", "#92C5DE", "#D6604D", "#F4A582")[seq_along(nm)],
        nm
      )

      # maiores primeiro, para as menores ficarem visíveis por cima
      g_ord <- arrange(g, desc(z))
      polys <- pmap_dfr(list(g_ord$z, g_ord$x, g_ord$nome), engrenagem_poly) |>
        mutate(nome = factor(nome, levels = g_ord$nome))

      lab <- mutate(g, y = 0)
      if (composto) {
        off <- 0.12 * min(s$z2, s$z3)
        lab$y[lab$nome == nm[2]] <- off
        lab$y[lab$nome == nm[3]] <- -off
      }

      ggplot(polys, aes(x, y, group = nome, fill = nome)) +
        geom_polygon(color = "grey25", alpha = 0.65, linewidth = 0.3) +
        geom_text(
          data = lab,
          aes(x, y, label = z),
          inherit.aes = FALSE,
          fontface = "bold",
          size = 5
        ) +
        scale_fill_manual(values = pal, breaks = nm, name = NULL) +
        coord_equal() +
        labs(
          title = paste("Trem:", s$Trem),
          subtitle = paste0(
            "Relação ",
            fmt(s$rela_disp, 4),
            " | erro ",
            fmt(s$erro_pct, 3),
            " %"
          ),
          caption = "Raios proporcionais ao nº de dentes (mesmo módulo)"
        ) +
        theme_void() +
        theme(legend.position = "bottom")
    },
    res = 96
  )

  # ---- Mapa ----
  mapa_dados <- reactive({
    pares() |>
      mutate(
        rela_disp = if (inv()) 1 / rela else rela,
        lr = log2(rela_disp),
        cor_texto = if_else(abs(lr) > 1.3, "white", "black")
      )
  })

  mapa <- reactive({
    d <- mapa_dados()
    g <- engrenagens()
    tam <- min(4.5, max(1.6, 67.5 / length(g)))
    res <- resultados()

    if (input$tipo == "simples") {
      keys <- transmute(res, motora = z1, movida = z2)
      mel_keys <- res |>
        filter(erro_r == min(erro_r)) |>
        transmute(motora = z1, movida = z2)
      legenda <- paste0(
        "Melhor combinação: ",
        paste0(mel_keys$motora, "/", mel_keys$movida, collapse = ", "),
        " (erro de ",
        fmt(res$erro_pct[1], 3),
        " %)"
      )
    } else {
      s <- sel()
      keys <- bind_rows(
        tibble(motora = s$z1, movida = s$z2),
        tibble(motora = s$z3, movida = s$z4)
      )
      mel_keys <- keys
      legenda <- paste0(
        "Trem selecionado: ",
        s$Trem,
        " (erro de ",
        fmt(s$erro_pct, 3),
        " %)"
      )
    }
    dest <- inner_join(d, distinct(keys), by = c("motora", "movida"))
    mel <- inner_join(d, distinct(mel_keys), by = c("motora", "movida"))

    if (input$modo == "calor") {
      lim <- max(max(abs(d$lr)), 0.1)
      p <- ggplot(d, aes(x = motora, y = movida)) +
        geom_tile(aes(fill = lr), color = "white", linewidth = 0.3) +
        geom_text(
          aes(label = fmt(rela_disp, 2), color = cor_texto),
          size = tam
        ) +
        scale_color_identity() +
        scale_fill_gradient2(
          low = "#2166AC",
          mid = "white",
          high = "#B2182B",
          midpoint = 0,
          limits = c(-lim, lim),
          name = "Relação",
          breaks = log2(c(0.25, 0.5, 1, 2, 4)),
          labels = c("0,25", "0,5", "1", "2", "4")
        ) +
        geom_tile(data = dest, fill = NA, color = "black", linewidth = 0.8) +
        geom_tile(data = mel, fill = NA, color = cor_melhor, linewidth = 1.6)
    } else {
      p <- ggplot(d, aes(x = motora, y = movida)) +
        geom_tile(fill = NA, color = "grey90", linewidth = 0.3) +
        geom_text(
          aes(label = fmt(rela_disp, 2)),
          color = "grey55",
          size = tam
        ) +
        geom_tile(data = dest, fill = NA, color = "grey30", linewidth = 0.6) +
        geom_text(
          data = dest,
          aes(label = fmt(rela_disp, 2)),
          color = "black",
          size = tam,
          fontface = "bold"
        ) +
        geom_tile(
          data = mel,
          fill = cor_melhor,
          color = "black",
          linewidth = 1.2
        ) +
        geom_text(
          data = mel,
          aes(label = fmt(rela_disp, 2)),
          color = "white",
          size = tam,
          fontface = "bold"
        )
    }

    p +
      scale_x_continuous(breaks = g, expand = c(0, 0)) +
      scale_y_continuous(breaks = g, expand = c(0, 0)) +
      coord_fixed() +
      labs(
        title = "Relações de transmissão entre engrenagens",
        subtitle = paste0(
          if (inv()) "Movida / motora" else "Motora / movida",
          " | Alvo: ",
          fmt(alvo(), 4)
        ),
        x = "Engrenagem motora (dentes)",
        y = "Engrenagem movida (dentes)",
        caption = legenda
      ) +
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5)
      )
  })

  output$mapa <- renderPlot(mapa(), res = 96)

  output$info_clique <- renderUI({
    req(input$clique_mapa)
    pt <- nearPoints(
      mapa_dados(),
      input$clique_mapa,
      xvar = "motora",
      yvar = "movida",
      threshold = 25,
      maxpoints = 1
    )
    req(nrow(pt) == 1)
    e <- (pt$rela_disp / alvo() - 1) * 100
    div(
      class = "alert alert-info py-2 mb-2",
      sprintf(
        "Motora %d dentes / movida %d dentes → relação %s (erro de %s %% em relação ao alvo)",
        pt$motora,
        pt$movida,
        fmt(pt$rela_disp, 4),
        fmt(e, 3)
      )
    )
  })

  # ---- Downloads ----
  output$baixar_csv <- downloadHandler(
    filename = \() "melhores_combinacoes.csv",
    content = function(file) write_excel_csv2(tab_vis(), file)
  )

  output$baixar_png <- downloadHandler(
    filename = \() "mapa_engrenagens.png",
    content = function(file) {
      ggsave(file, mapa(), width = 10, height = 9, dpi = 300, bg = "white")
    }
  )
}

shinyApp(ui, server)
