library(ggplot2)
library(dplyr)

# -----------------------------
# Dados aproximados da figura
# -----------------------------

dados <- data.frame(
  rpm = c(
    900,
    1000,
    1100,
    1200,
    1300,
    1400,
    1500,
    1600,
    1700,
    1800,
    1900,
    2000,
    2100,
    2200,
    2300,
    2400,
    2450,
    2500
  ),
  potencia = c(
    40,
    45,
    51,
    57,
    63,
    69,
    73,
    77,
    80,
    83,
    85,
    85,
    84,
    82,
    78,
    70,
    50,
    0
  ),
  torque = c(
    420,
    430,
    440,
    450,
    455,
    458,
    457,
    450,
    440,
    430,
    420,
    400,
    380,
    350,
    315,
    270,
    150,
    0
  )
)


# -----------------------------
# Gráfico
# -----------------------------

ggplot(dados, aes(x = rpm)) +

  # Potência
  geom_line(
    aes(y = potencia),
    linewidth = 1.5,
    color = "#12366B"
  ) +

  # Torque convertido para a escala da potência
  geom_line(
    aes(y = torque / 5.55),
    linewidth = 1.5,
    color = "#11A8D8"
  ) +

  # Linhas verticais da reserva de rotação
  geom_vline(
    xintercept = c(1300, 2000),
    linetype = "dotted",
    linewidth = 0.8
  ) +

  # Linha horizontal da reserva de torque
  geom_segment(
    aes(
      x = 1300,
      xend = 2000,
      y = 71,
      yend = 71
    ),
    linetype = "dotted",
    linewidth = 0.8
  ) +

  # -----------------------------
  # Setas
  # -----------------------------

  # Reserva de torque
  annotate(
    "segment",
    x = 1300,
    xend = 1300,
    y = 71,
    yend = 83,
    linewidth = 1,
    arrow = arrow(
      ends = "both",
      length = unit(0.25, "cm")
    )
  ) +

  annotate(
    "text",
    x = 1180,
    y = 86,
    label = "Reserva de Torque",
    size = 5,
    hjust = 0
  ) +

  # Reserva de rotação
  annotate(
    "segment",
    x = 1300,
    xend = 2000,
    y = 17,
    yend = 17,
    linewidth = 1,
    arrow = arrow(
      ends = "both",
      length = unit(0.25, "cm")
    )
  ) +

  annotate(
    "text",
    x = 1650,
    y = 13,
    label = "Reserva de rotação",
    size = 5
  ) +

  # -----------------------------
  # Escalas
  # -----------------------------

  scale_x_continuous(
    limits = c(800, 2800),
    breaks = c(800, 1300, 1800, 2300, 2800),
    expand = c(0, 0)
  ) +

  scale_y_continuous(
    name = "Potência (kW)",
    limits = c(0, 90),
    breaks = seq(0, 90, 10),

    sec.axis = sec_axis(
      ~ . * 5.55,
      name = "Torque (N·m)",
      breaks = seq(0, 500, 50)
    )
  ) +

  labs(
    title = "Curva do motor",
    x = "Rotação (rpm)"
  ) +

  # -----------------------------
  # Tema
  # -----------------------------

  theme_classic(base_size = 14) +

  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 20
    ),

    axis.title.x = element_text(
      face = "bold"
    ),

    axis.title.y = element_text(
      face = "bold"
    ),

    axis.title.y.right = element_text(
      face = "bold"
    ),

    axis.text = element_text(
      color = "black"
    ),

    panel.grid.major.y = element_line(
      color = "grey80",
      linewidth = 0.4
    ),

    legend.position = "none"
  )

ggsave(filename = "torque_potencia.jpeg", width = 12, height = 7, dpi = 600)
