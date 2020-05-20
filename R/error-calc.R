#' Calculate the Single Segment Synchronous Error on a \code{tdtr} object
#'
#' \code{singleSegmentSynchError} solves for the average distance with respect
#' to time between the approximated segment returned by the top-down time ratio
#' algorithm and the 'true' segment formed by two adjacent recorded points.
#'
#'
#'
#'
#' @param res Segmented results from \code{tdtr}
#' @param tol Tolerance for zero comparisons. May vary with time scale.
#' @return Adds by reference a column with segment error as given in Meratnia & de By \(2004\)
#' @export

singleSegmentSynchError <- function(res, tol = 1e-24){
  res[, lat1 := lat]
  res[, lon1 := lon]
  res[, lat2 := shift(lat, -1)]
  res[, lon2 := shift(lon, -1)]
  # res[, t1 := timestamp_numeric]
  res[, t1 := (timestamp_numeric - timestamp_numeric[1])]
  res[, t2 := shift(t1, -1)]
  res[, alat1 := adjusted_lat]
  res[, alon1 := adjusted_lon]
  res[, alat2 := shift(adjusted_lat, -1)]
  res[, alon2 := shift(adjusted_lon, -1)]

  res[, dx1 := lon1 - alon1]
  res[, dy1 := lat1 - alat1]
  res[, dx2 := lon2 - alon2]
  res[, dy2 := lat2 - alat2]

  res[, c1 := ((dx1 - dx2)^2 + (dy1 - dy2)^2)]
  res[, c2 := 2 * ((dx2 *t1 - dx1*t2) * (dx1-dx2) + (dy2*t1 - dy1*t2) * (dy1-dy2))]
  res[, c3 := (dx2*t1 - dx1*t2)^2 + (dy2*t1 - dy1*t2)^2]
  res[, c4 := t2 - t1]
  res[, discr := c2^2 - 4*c1*c3]
  force(res)
  res[, case := NULL]
  res[c1 < tol, case := "directseg"]
  res[c1 < tol, segment_err := sqrt(c3)/(c4)]

  res[ discr < tol &
        c1 > 0 &
        dx1 < tol &
        dy1 < tol,
        case := "sharedendpoint"]

  res[discr < tol &
        c1 > 0 &
        dx1 < tol &
        dy1 < tol,
        segment_err := .5*sqrt(dx2^2+dy2^2)]

  res[discr < tol &
         c1 > 0 &
         dx2 < tol &
         dy2 < tol,
        case := "sharedendpoint"]
  res[discr < tol &
        c1 > 0 &
        dx2 < tol &
        dy2 < tol,
        segment_err := .5*sqrt(dx1^2 + dy1^2)]

  res[discr < 0 & is.na(case),
        case := "general"]
  res[discr < 0 & case == "general",
        t1int := ((2 * c1 * t1 + c2)/(4*c1)) * sqrt(c1 * t1^2 + c2*t1 + c3) -
          discr*asinh((2 * c1 * t1 + c2) / sqrt(4 * c1 * c3 - c2^2))]

  res[discr < 0 & case == "general",
        t2int := ((2 * c1 * t2 + c2)/(4*c1)) * sqrt(c1 * t2^2 + c2*t2 + c3) -
          discr*asinh((2 * c1 * t2 + c2) / sqrt(4 * c1 * c3 - c2^2))]
  res[discr < 0 & case == "general", segment_err := 1/c4^2 * ((t2int) - (t1int))]
  cols.to.remove <- c("c1", "c2", "c3", "c4", "t1", "t2", "dx1", "dx2", "dy1", "dy2",
                      "discr", "t1int", "t2int", "alat1", "alat2", "alon1", "alon2")
  set(res, j = cols.to.remove,
      value = rep(NULL, length(cols.to.remove)))[]
}
