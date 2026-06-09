library(tidyverse)

dados <- tribble(
  ~cultura    , ~periodo  , ~hours_per_acre , ~produtividade ,

  "Milho"     , "1915-19" ,  34.2           ,   25.9         ,
  "Milho"     , "1925-29" ,  30.3           ,   26.3         ,
  "Milho"     , "1935-39" ,  28.1           ,   26.1         ,
  "Milho"     , "1945-49" ,  19.2           ,   36.1         ,
  "Milho"     , "1955-59" ,   9.9           ,   48.7         ,
  "Milho"     , "1965-69" ,   5.8           ,   78.5         ,
  "Milho"     , "1974-78" ,   3.7           ,   87.8         ,

  "Sorgo"     , "1925-29" ,  17.5           ,   16.8         ,
  "Sorgo"     , "1935-39" ,  13.1           ,   12.8         ,
  "Sorgo"     , "1945-49" ,   8.8           ,   17.8         ,
  "Sorgo"     , "1955-59" ,   5.9           ,   29.2         ,
  "Sorgo"     , "1965-69" ,   4.2           ,   52.9         ,
  "Sorgo"     , "1974-78" ,   3.9           ,   50.8         ,

  "Trigo"     , "1915-19" ,  13.6           ,   13.9         ,
  "Trigo"     , "1925-29" ,  10.5           ,   14.1         ,
  "Trigo"     , "1935-39" ,   8.8           ,   13.2         ,
  "Trigo"     , "1945-49" ,   5.7           ,   16.9         ,
  "Trigo"     , "1955-59" ,   3.8           ,   22.3         ,
  "Trigo"     , "1965-69" ,   2.9           ,   27.5         ,
  "Trigo"     , "1974-78" ,   2.9           ,   30.0         ,

  "Feno"      , "1915-19" ,  13.0           ,    1.25        ,
  "Feno"      , "1925-29" ,  12.0           ,    1.22        ,
  "Feno"      , "1935-39" ,  11.3           ,    1.24        ,
  "Feno"      , "1945-49" ,   8.4           ,    1.35        ,
  "Feno"      , "1955-59" ,   6.0           ,    1.61        ,
  "Feno"      , "1965-69" ,   3.8           ,    1.97        ,
  "Feno"      , "1974-78" ,   3.5           ,    2.15        ,

  "Batata"    , "1915-19" ,  73.8           ,   56.9         ,
  "Batata"    , "1925-29" ,  73.1           ,   68.4         ,
  "Batata"    , "1935-39" ,  69.7           ,   70.3         ,
  "Batata"    , "1945-49" ,  68.5           ,  117.8         ,
  "Batata"    , "1955-59" ,  53.1           ,  178.1         ,
  "Batata"    , "1965-69" ,  45.1           ,  212.8         ,
  "Batata"    , "1974-78" ,  38.3           ,  257.0         ,

  "Beterraba" , "1915-19" , 125             ,    9.6         ,
  "Beterraba" , "1925-29" , 109             ,   10.9         ,
  "Beterraba" , "1935-39" ,  98             ,   11.6         ,
  "Beterraba" , "1945-49" ,  85             ,   13.6         ,
  "Beterraba" , "1955-59" ,  51             ,   17.4         ,
  "Beterraba" , "1965-69" ,  33             ,   17.5         ,
  "Beterraba" , "1974-78" ,  26             ,   19.7         ,

  "Algodão"   , "1915-19" , 105             ,  168           ,
  "Algodão"   , "1925-29" ,  96             ,  171           ,
  "Algodão"   , "1935-39" ,  99             ,  226           ,
  "Algodão"   , "1945-49" ,  83             ,  273           ,
  "Algodão"   , "1955-59" ,  66             ,  428           ,
  "Algodão"   , "1965-69" ,  30             ,  484           ,
  "Algodão"   , "1974-78" ,  10             ,  462           ,

  "Tabacco"   , "1915-19" , 353             ,  803           ,
  "Tabacco"   , "1925-29" , 370             ,  772           ,
  "Tabacco"   , "1935-39" , 415             ,  886           ,
  "Tabacco"   , "1945-49" , 460             , 1176           ,
  "Tabacco"   , "1955-59" , 475             , 1541           ,
  "Tabacco"   , "1965-69" , 427             , 1960           ,
  "Tabacco"   , "1974-78" , 259             , 2049           ,

  "Soja"      , "1915-19" ,  19.9           ,   13.9         ,
  "Soja"      , "1925-29" ,  15.9           ,   12.6         ,
  "Soja"      , "1935-39" ,  11.8           ,   18.5         ,
  "Soja"      , "1945-49" ,   8.0           ,   19.6         ,
  "Soja"      , "1955-59" ,   5.2           ,   22.7         ,
  "Soja"      , "1965-69" ,   4.8           ,   25.8         ,
  "Soja"      , "1974-78" ,   3.7           ,   27.8
)

dados$periodo <- factor(
  dados$periodo,
  levels = c(
    "1915-19",
    "1925-29",
    "1935-39",
    "1945-49",
    "1955-59",
    "1965-69",
    "1974-78"
  )
)


dados_long <- dados |>
  pivot_longer(
    c(hours_per_acre, produtividade),
    names_to = "variavel",
    values_to = "valor"
  )

ggplot(dados_long, aes(periodo, valor, group = cultura)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  facet_grid(
    variavel ~ cultura,
    scales = "free_y"
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# dados_long <- dados |>
#   pivot_longer(
#     c(hours_per_acre, produtividade),
#     names_to = "variavel",
#     values_to = "valor"
#   )

ggplot(dados, aes(periodo, produtividade, group = cultura)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  facet_wrap(
    ~cultura,
    scales = "free_y",
    # ncol = 1
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    x = "Ano",
    y = "Produção (bushels)",
    color = "Crop"
  ) +
  ggview::canvas(width = 7, height = 5, dpi = 600) -> p1

p1

ggview::save_ggplot(file = "aula_1/imagens/producao_e_ano.jpeg", plot = p1)


ggplot(dados, aes(periodo, hours_per_acre, group = cultura)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  facet_wrap(
    ~cultura,
    scales = "free_y",
    # ncol = 1
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    x = "Ano",
    y = "Tempo de trabalho por acre (~0.40 Ha)",
    color = "Crop"
  ) +
  ggview::canvas(width = 7, height = 5, dpi = 600) -> p1

p1

ggview::save_ggplot(file = "aula_1/imagens/tempo_e_ano.jpeg", plot = p1)
