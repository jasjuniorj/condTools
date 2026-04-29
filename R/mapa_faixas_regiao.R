#' @export
mapa_faixas_regiao <- function(base_dados,
                               variavel,
                               regiao = NULL,
                               nome_legenda = "Taxa de Acompanhamento",
                               ano = 2020) {

  library(dplyr)
  library(ggplot2)
  library(geobr)

  # --- mapas ---
  mun <- read_municipality(year = ano)
  #mun$code_muni <- substr(mun$code_muni, 1, nchar(mun$code_muni) - 1)
  #mun$code_muni <- as.numeric(mun$code_muni)

  estados <- read_state(code_state = "all", year = ano)

  # --- base ---
  df <- base_dados %>%
    right_join(mun, by = "code_muni") %>%
    mutate(valor = {{ variavel }})

  # --- filtrar região (se houver) ---
  if (!is.null(regiao)) {
    df <- df %>% filter(abbrev_region == regiao)
    estados <- estados %>% filter(abbrev_region == regiao)
  }

  # --- faixas ---
  df <- df %>%
    mutate(faixa = cut(valor * 100,
                       breaks = c(0, 12, 24, 36, 48, 60, 100),
                       include.lowest = TRUE,
                       labels = c("0–12%", "12–24%", "24–36%",
                                  "36–48%", "48–60%", "60–100%")))

  # --- plot ---
  p <- ggplot(df) +
    geom_sf(aes(fill = faixa, geometry = geom), color = NA) +

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
            color = "#8C8C8C",
            size = 0.2) +

    theme_minimal() +
    theme(
      legend.position = "right",
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank()
    )

  return(p)
}
