#' cut data
#' @examples
#' cut5.data <- dataCut(gen.data, cut_size = 5)
#' @export
dataCut <- function(.data, cut_size) {

  # cut(x = .data[,1], breaks = cut_size, labels = F)

  cut.data <- apply(.data, 2, function(x) cut(x = x, breaks = cut_size, labels = F))

  cut.data <- data.frame(cut.data)

  return(cut.data)
}
