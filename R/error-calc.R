isZero <- Vectorize(function(x) isTRUE(all.equal(x, 0, tolerance = 1e-26)))

singleSegmentSynchError <- function(res){
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
  res[, .(dx1, dy1, dx2, dy2)]

  res[, c1 := ((dx1 - dx2)^2 + (dy1 - dy2)^2)]
  res[, c2 := 2 * ((dx2 *t1 - dx1*t2) * (dx1-dx2) + (dy2*t1 - dy1*t2) * (dy1-dy2))]
  res[, c3 := (dx2*t1 - dx1*t2)^2 + (dy2*t1 - dy1*t2)^2]
  res[, c4 := t2 - t1]
  res[, detr := c2^2 - 4*c1*c3]

  res[isZero(c1), case := "directseg"]
  res[isZero(c1), segment_err := sqrt(c3)/(c4)]

  res[isZero(detr) & c1 > 0 & (dx1 == 0) & (dy1 == 0),
        case := "sharedendpoint"]
  res[isZero(detr) & c1 > 0 & (isZero(dx1)) & (isZero(dy1)),
        segment_err := .5*sqrt(dx2^2+dy2^2)]

  res[isZero(detr) & c1 > 0 & isZero(dx2) & isZero(dy2),
        case := "sharedendpoint"]
  res[isZero(detr) & c1 > 0 & isZero(dx2) & isZero(dy2),
        segment_err := .5*sqrt(dx1^2 + dy1^2)]

  res[detr < 0 & is.na(case),
        case := "general"]
  res[case == "general",
        t1int := ((2 * c1 * t1 + c2)/(4*c1)) * sqrt(c1 * t1^2 + c2*t1 + c3) -
          detr*asinh((2 * c1 * t1 + c2) / sqrt(4 * c1 * c3 - c2^2))]

  res[case == "general",
        t2int := ((2 * c1 * t2 + c2)/(4*c1)) * sqrt(c1 * t2^2 + c2*t2 + c3) -
          detr*asinh((2 * c1 * t2 + c2) / sqrt(4 * c1 * c3 - c2^2))]
  res[case == "general", segment_err := 1/c4^2 * ((t2int) - (t1int))]
  cols.to.remove <- c("c1", "c2", "c3", "c4", "t1", "t2", "dx1", "dx2", "dy1", "dy2",
                      "detr", "t1int", "t2int", "alat1", "alat2", "alon1", "alon2")
  set(res, j = cols.to.remove,
      value = rep(NULL, length(cols.to.remove)))
}
