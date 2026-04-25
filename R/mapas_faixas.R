mapa_faixas <- function(base_dados,
                        variavel,
                        nome_legenda = "Cobertura (%)",
                        ano = 2020) {

  library(dplyr)
  library(ggplot2)
  library(geobr)

  # --- carregar mapas ---
  mun <- read_municipality(year = ano)
  mun$code_muni <- substr(mun$code_muni, 1, nchar(mun$code_muni) - 1)
  mun$code_muni <- as.numeric(mun$code_muni)

  estados <- read_state(code_state = "all", year = ano)

  # --- preparar base ---
  df <- base_dados %>%
    right_join(mun, by = "code_muni") %>%
    mutate(
      valor = {{ variavel }},
      faixa = cut(valor * 100,
                  breaks = c(0, 12, 24, 36, 48, 60, 100),
                  include.lowest = TRUE,
                  labels = c("0–12%", "12–24%", "24–36%",
                             "36–48%", "48–60%", "60–100%"))
    )

  # --- plot ---
  ggplot(df) +
    geom_sf(aes(fill = faixa, geometry = geom),
            color = NA) +

    scale_fill_manual(
      name = nome_legenda,
      values = c(
        "0–12%"   = "#fee5d9",
        "12–24%"  = "#fcbba1",
        "24–36%"  = "#fc9272",
        "36–48%"  = "#fb6a4a",
        "48–60%"  = "#de2d26",
        "60–100%" = "#a40f15"
      ),
      na.value = "#eeeeee",
      drop = FALSE
    ) +

    geom_sf(data = estados,
            fill = NA,
            color = "#7F7F7F",
            size = 0.1) +

    theme_minimal() +
    theme(
      legend.position = "right",
      plot.title = element_text(size = 9, hjust = 0.5),
      plot.caption = element_text(size = 9, hjust = 0.5),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 9),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank()
    )
}
