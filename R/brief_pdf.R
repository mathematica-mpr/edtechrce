#' Title
#'
#' @param html
#'
#' @return
#' @export
#'
#' @examples
brief_pdf <- function(html) {

  writeLines(html, 'brief.html')

  cmd <- sprintf('xvfb-run --server-args="-screen 0, 1024x768x24" wkhtmltopdf brief.html brief.pdf')

  system(cmd)
}
