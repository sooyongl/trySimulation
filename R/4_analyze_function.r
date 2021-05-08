#' run a single replication
#' @examples
#' .fit <- analysis(cut5.data)
#' @export
analysis <- function(.data) {

  n_indi <- dim(.data)[2]/2

  for_f1 <- paste0("X", 1:n_indi)
  for_f2 <- paste0("X", (n_indi+1):(2*n_indi))

  for_f1 <- paste(for_f1, collapse = " + ")
  for_f2 <- paste(for_f2, collapse = " + ")

  .model <- glue('
      F1 =~ {for_f1}
      F2 =~ {for_f2}

      F1 ~~ F2
  ')

  .fit <- lavaan::sem(model = .model,
                      data  = .data )

  # summary(.fit)
  return(.fit)
}

#' extract estimates
#' @examples
#' extractEst(.fit)
#' @export
extractEst <- function(.fit) {

  parameterestimates(.fit) %>%
    # filter(str_detect(op, "~~") & str_detect(lhs, "F1") & str_detect(rhs, "F2")) %>%
    filter(str_detect(op, "=~") | str_detect(lhs, "F1|F2")) %>%
    filter(se != 0) %>%
    mutate(type = paste0(lhs, op, rhs)) %>%
    dplyr::select(type, est, pvalue)
}
