#' @title Plot a portfolio pie.
#'
#' @description
#' \code{portfolio.pie} plots a pie chart of a portfolio given a specific scenario set
#'
#' @export
portfolio.pie <- function(portfolio, scenario.set, main="Portfolio") {
  pie_labels <- names(scenario.set)
  pie_labels[which(portfolio == 0)] <- NA
  pie(portfolio, labels=pie_labels, main=main)
}
