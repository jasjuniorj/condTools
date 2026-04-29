#' @export
mapa_faixas_regiao <- function(base_dados,
                               variavel,
                               codigoibge,
                               Periodo = NULL,
                               regiao = NULL,
                               nome_legenda = "Taxa de Acompanhamento",
                               ano = 2020) {

  library(dplyr)
  library(ggplot2)
  library(geobr)

  # --- mapas (PADRONIZANDO TIPO) ---
  mun <- read_municipality(year = ano) %>%
    mutate(code_muni = as.character(code_muni))

  estados <- read_state(code_state = "all", year = ano)

  # --- preparar base ---
  df <- base_dados %>%
    { if (!is.null(Periodo)) filter(., DS_PERIODO == Periodo) else . } %>%
    rename(code_muni = {{ codigoibge }}) %>%
    mutate(code_muni = as.character(code_muni)) %>%
    right_join(mun, by = "code_muni") %>%
    mutate(valor = {{ variavel }})

  # --- filtrar região (ARGUMENTO CONTROLANDO O MAPA) ---
  if (!is.null(regiao)) {
    df <- df %>% filter(abbrev_region %in% regiao)
    estados <- estados %>% filter(abbrev_region %in% regiao)
  }

  # --- faixas ---
  df <- df %>%
    mutate(
      faixa = cut(valor,
                  breaks = c(0, 50, 60, 70, 80, 90, 95, 100),
                  include.lowest = TRUE,
                  labels = c("0-50%", "50-60%", "60-70%",
                             "70-80%", "80-90%", "90-95%", "95-100%"))
    )

  # --- plot ---
  p <- ggplot(df) +
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

    geom_sf(data = estados,
            fill = NA,
            color = "#7F7F7F",
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
