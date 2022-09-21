#' Split the difference when it comes to difftimes
#'
#' Averages out time differences between successive locations. This is useful
#' in calculating the time-weighted Radius of Gyration, as it provides a method
#' of using both the first and last locations. This assumes that the location
#' is measured at a given time period and will account for half of the time
#' difference occurring between this location and the one immediately preceding,
#' as well as half the time difference occurring between this location and the
#' one immediately following.
#'
#' @param timestamp a duration, period, difftime or interval
#'
#' @return the averaged difftime of same length
#' @importFrom lubridate time_length
splitDiffTime <- function(timestamp){
  N       <- length(timestamp)

  timedif    <- lubridate::time_length(timestamp - shift(timestamp))
  timedifsec <- (timedif + shift(timedif, -1)) / 2
  timedifsec[c(1L, N)] <- c(timedif[2L] / 2L, timedif[N] / 2L)
  timedifsec
}

#' Radius of Gyration
#'
#' Calculates the time-weighted radius of Gyration provided a data.table
#' containing latitude, longitude and a timestamp. This is the root-mean-square
#' time-weighted average of all locations. Weighting by time is provided to
#' adjust for unequal frequency of data collection.
#'
#' Time-weighted RoG is defined as
#' \loadmathjax
#' \mjdeqn{ \sqrt{\frac{\sum_i{w_j \times dist([\overline{lon}, \overline{lat}], [lon_j, lat_j]})}{\sum_i{w_j}}}}{sqrt(1/sum(w_j) * sum(w_j * dist(|mean_lon, mean_lat|, |lon_j, lat_j|)^2))}
#' Where
#' \loadmathjax
#' \mjdeqn{\overline{lon} = \frac{ \sum_j w_j lon_j}{\sum_j w_j} \quad \textrm{and} \quad \overline{lat} = \frac{ \sum_j w_j lat_j}{\sum_j w_j}}{mean_lon = sum(w_j * lon_j)/sum(w_j) and mean_lat = sum(w_j * lat_j)/sum(w_j)}
#' And the weighting element w_j represents half the time interval during which a location was recorded
#' \loadmathjax
#' \mjdeqn{w_j = \frac{t_{j+1} - t_{j - 1}}{2}}{w_j = (t_j+1 - t_j-1)/2}
#'
#' @param lat_col Time-ordered vector of latitudes
#' @param lon_col Time-ordered vector of longitudes
#' @param timestamp Timestamps associatd with the latitude/longitude pairs
#' @param dist_measure Passed through to geodist::geodist_vec, One of
#' "haversine" "vincenty", "geodesic", or "cheap" specifying desired method of
#' geodesic distance calculation.
#'
#' @return Time-weighted radius of gyration
#' @export
#' @importFrom geodist geodist_vec
#' @importFrom stats weighted.mean
#' @import data.table
#' @import mathjaxr
#' @examples
#' # Inside a data.table
#' dt <- data.table::data.table(
#'   lat = c(1, 1, 1, 1, 1),
#'   lon = c(1, 1.5, 4, 1.5, 2),
#'   timestamp = c(100, 200, 300, 600, 900)
#' )
#' dt[, radiusOfGyrationDT(lat, lon, timestamp)]
#' # As vectors
#' radiusOfGyrationDT(
#'   c(1, 1, 1, 1, 1),
#'   c(1, 1.5, 4, 1.5, 2),
#'   c(100, 200, 300, 600, 900)
#'   )
radiusOfGyrationDT <- function(lat_col, lon_col, timestamp, dist_measure = "geodesic") {

  timedifsec <- splitDiffTime(timestamp)
  N          <- length(timedifsec)
  meanlat    <- weighted.mean(lat_col, timedifsec)
  meanlon    <- weighted.mean(lon_col, timedifsec)
  dist    <-
    geodist::geodist_vec(
      x1 = rep(meanlon, N),
      y1 = rep(meanlat, N),
      x2 = lon_col,
      y2 = lat_col,
      paired = TRUE,
      measure = dist_measure
    )

  sqrt(1/sum(timedifsec) * sum(timedifsec * dist^2))
}

`.` <- list

