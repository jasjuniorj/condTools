#' @export
mapa_faixas <- function(base_dados,
                        variavel,
                        Periodo,
                        codigoibge,
                        nome_legenda = "Taxa de Acompanhamento",
                        ano = 2020) {

  library(dplyr)
  library(ggplot2)
  library(geobr)


  # --- carregar mapas ---
  mun <- read_municipality(year = ano)
  estados <- read_state(code_state = "all", year = ano)

  # --- preparar base ---
  df <- base_dados %>%
    filter(DS_PERIODO == Periodo) %>%
    rename(code_muni=codigoibge) %>%
    right_join(mun, by = "code_muni") %>%
    mutate(
      valor = {{ variavel }},
      faixa = cut(valor,
                  breaks = c(0, 50, 60, 70, 80, 90, 95, 100),
                  include.lowest = TRUE,
                  labels = c("0-50%", "50-60%", "60-70%",
                             "70-80%", "80-90%", "90-95%", "95-100%"))
    )

  # --- plot ---
  ggplot(df) +
    geom_sf(aes(fill = faixa, geometry = geom), color = NA) +
    scale_fill_manual(
      name = nome_legenda,
      values = c(
        "0-50%"   = "#bcffdd",
        "50-60%"  = "#a4f0a7",
        "60-70%"  = "#7dd980",
        "70-80%"  = "#02ba02",
        "80-90%"  = "#069525",
        "90-95%"  = "#095747",
        "95-100%" = "#053c32"
      ),
      na.value = "#eeeeee",
      drop = FALSE
    ) +
    geom_sf(data = estados, fill = NA, color = "#7F7F7F", size = 0.1) +
    theme_minimal()
}

