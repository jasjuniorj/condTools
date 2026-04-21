#' Verifica valores ausentes
#'
#' @param df data.frame
#' @return data.frame com contagem de NA
#' @export
check_na <- function(df) {
  data.frame(
    variavel = names(df),
    n_na = sapply(df, function(x) sum(is.na(x))),
    prop_na = sapply(df, function(x) mean(is.na(x)))
  )
}
