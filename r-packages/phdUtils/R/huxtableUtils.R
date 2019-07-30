library(huxtable)

#' A tidy article theme for huxtables that works with google docs
#'
#' Once copied and pasted table needs to have leading spaces removed in google docs
#' @param ... a huxtable object
#' @keywords huxtable
#' @import huxtable
#' @export
#' @examples
#' defaultTableLayout(huxtable(dataframe %>% select("col 1 title"=col1))
defaultTableLayout = function(...) {
  return( theme_article(...) %>%
    set_width("400pt") %>%
    set_wrap(TRUE) %>%
    set_all_padding(everywhere,everywhere,2) %>%
    set_valign(everywhere,everywhere,"top") )
}

#' save labelled dataframe to html and tex file silently
#'
#' @param labelledDataFrame e.g. dataframe %>% select("col 1 title"=col1)
#' @param filename file of desired output without extension.
#' @keywords omop
#' @export
#' @examples
#' getOmop()
saveTable = function(labelledDataframe, filename) {
  tmp <- defaultTableLayout(huxtable(
    labelledDataframe,
    add_colnames = TRUE
  ))
  quick_html(tmp,file=paste0(filename,'.html'),open=FALSE)
  quick_latex(tmp,file=paste0(filename,'.tex'),open=FALSE)
}
