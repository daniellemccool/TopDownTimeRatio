
#' Mean filter
#'
#' @param coord A vector of coordinates over which to apply a mean filter
#' @param n The number of values to average
#'
#' @return A vector of mean-averaged coordinates
#' @export
#'

meanFilter <- function(coord, n=5){
  filter(coord, rep(1/n, n), sides=2)
}


#' Median filter
#'
#' @param coord A vector of coordinates over which to apply a mean filter
#' @param n The number of values to average (best when odd-numbered)
#'
#' @return A vector of median-averaged coordinates
#' @export
#'

medianFilter <- function(coord, n=5){
  runmed(coord, n)
}
