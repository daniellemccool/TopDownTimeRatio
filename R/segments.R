#' Get Segments
#'
#' Extract segment info from the segmented data.table.
#'
#' Segment location information can be either in lat/lon coordinates, or
#' expressed in terms of distance for a more anonymous presentation of small
#' trajectories. (Full anonymity is not guaranteed as sufficiently long
#' trajectories with small error parameters can provide enough data to match
#' against a map.)
#'
#' @param data data.table returned from function tdtr()
#' @param coord.type return actual coordinates, relative distance, or both (see
#' Details)
#' @param group separate by group, default is FALSE
#' @return data.table with segments only, containing information about the start
#' and end locations, start and end time and distance covered by the segment
#' @export
#' @examples
#' df <- data.frame(entity_id = rep(1, 12),
#'    timestamp = c(1, 2, 4, 10, 14, 18, 20, 21, 24, 25, 28, 29),
#'    lon = c(5.1299311, 5.129979, 5.129597, 5.130028, 5.130555, 5.131083,
#'            5.132101, 5.132704, 5.133326, 5.133904, 5.134746, 5.135613),
#'    lat = c(52.092839, 52.092827, 52.092571, 52.092292, 52.092076, 52.091821,
#'            52.091420, 52.091219, 52.091343, 52.091651, 52.092138, 52.092698))
#' # First generate segments
#' res30 <- tdtr(df,
#'      group_col = NULL,
#'      max_error = 30)
#' # Then extract a data.table of segments
#' getSegments(res30)
#'
#' # Calculating distance instead of coordinates
#' segs <- getSegments(res30, coord.type = "distance")
#' segs
#' plot(c(0, 700), c(0, 200), col = "white",
#'      xlab = "East-West distance",
#'      ylab = "North-South distance")
#' with(segs,
#'      segments(seg_start_lon_dist, seg_start_lat_dist,
#'       seg_end_lon_dist, seg_end_lat_dist))


getSegments <- function(data, coord.type = c("coordinate", "distance", "both"), group = FALSE){

  # Fix 'no visible vinding for global variable'
  # https://github.com/Rdatatable/data.table/issues/850#issuecomment-259466153
  segment_start = seg_start_lon = seg_start_lat = seg_start_time = seg_end_lon =
    seg_end_lat = seg_end_time = segdist = .id = entity_id = NULL

  coord.type <- match.arg(coord.type, c("coordinate", "distance",
                                        "both"))
  segs <- data[(segment_start)]

    set(segs,
      j = "segdist",
      value = geodist(cbind(lon = segs[["seg_start_lon"]], lat = segs[["seg_start_lat"]]),
                      cbind(lon = segs[["seg_end_lon"]], lat = segs[["seg_end_lat"]]),
                      paired = TRUE, measure = "haversine"))

  if (group == TRUE) {
    segs <- unique(segs[, .(seg_start_lon, seg_start_lat, seg_start_time,
                            seg_end_lon, seg_end_lat, seg_end_time, segdist, .id, entity_id)])
  } else {
    segs <- unique(segs[, .(seg_start_lon, seg_start_lat, seg_start_time,
                            seg_end_lon, seg_end_lat, seg_end_time, segdist, entity_id)])
  }
  if (coord.type == "coordinate") {
    return(segs[])
  }
  else {
    convertCoordsToDist(segs, c("seg_start_lat", "seg_end_lat"))
    convertCoordsToDist(segs, c("seg_start_lon", "seg_end_lon"))
  }
  if (coord.type == "both") {
    return(segs[])
  }
  else if (coord.type == "distance") {
    set(segs, j = c("seg_start_lat", "seg_start_lon", "seg_end_lat",
                    "seg_end_lon"), value = NULL)
    segs[]
  }
}

convertCoordsToDist <- function(data, coord_cols){
  mincoord = NULL # Fix 'no visible vinding for global variable' https://github.com/Rdatatable/data.table/issues/850#issuecomment-259466153

  distFromMin <- function(coord, mincoord){
    geodist::geodist(data.table(longitude = 0, latitude = coord),
                     data.table(longitude = 0, latitude = mincoord), paired = TRUE, measure = "haversine")
  }


  data[, mincoord := min(.SD, na.rm = TRUE), .SDcols = coord_cols]
  data[, `:=`(paste0(coord_cols, "_dist"), lapply(.SD, distFromMin, mincoord = mincoord)), .SDcols = coord_cols]
  data[, mincoord := NULL][]

}

#' Extract segment info with extra information from the segmented data.table
#'
#' @param data data.table returned from function /code{tdtr}
#' @param coord.type return actual coordinates, relative distance, or both
#' @param group Separate by group, default is FALSE
#' @return data.table with segments only
#' @export
getSegsExtra <- function(data, coord.type = c("coordinate", "distance", "both"), group = FALSE){
  coord.type <- match.arg(coord.type, c("coordinate", "distance",
                                        "both"))

  # Fix 'no visible vinding for global variable' https://github.com/Rdatatable/data.table/issues/850#issuecomment-259466153
  gap = speed2 = calcmps = speed = pbdiff = lat = lon = timestamp = bearing =
    segment_id = segment_start = seg_start_lon = seg_start_lat =
    seg_start_time = seg_end_lon = seg_end_lat = seg_end_time = segdist =
    avgspeed = maxspeed = varspeed = avgpbdif = varpbdif = bearvar = avgmps =
    maxmps = varmps = perc0spd = N2 = rog = .id = entity_id = NULL

  data <- copy(data)
  data[!(gap), `:=`(
    avgspeed = mean(speed2, na.rm = TRUE),
    maxspeed = max(speed2, na.rm = TRUE),
    varspeed = var(speed2, na.rm = TRUE),
    avgmps   = mean(calcmps, na.rm = TRUE),
    maxmps   = max(calcmps, na.rm = TRUE),
    varmps   = var(calcmps, na.rm = TRUE),
    perc0spd = sum(speed == 0)/.N,
    avgpbdif = mean(pbdiff, na.rm = TRUE),
    varpbdif = var(pbdiff, na.rm = TRUE),
    rog      = radiusOfGyrationDT(lat, lon, timestamp),
    bearvar  = CircStats::circ.disp(CircStats::rad(na.omit(bearing)))$var,
    N2       = .N < 3
  ), segment_id]

  segs <- data[!(gap) & (segment_start)]
  set(segs,
      j = "segdist",
      value = geodist(cbind(lon = segs[["seg_start_lon"]], lat = segs[["seg_start_lat"]]),
                      cbind(lon = segs[["seg_end_lon"]], lat = segs[["seg_end_lat"]]),
                      paired = TRUE, measure = "haversine"))

  if (group == TRUE) {
    segs <- unique(segs[, .(seg_start_lon, seg_start_lat, seg_start_time,
                            seg_end_lon, seg_end_lat, seg_end_time, segdist,
                            avgspeed, maxspeed, varspeed, avgpbdif, varpbdif, bearvar,
                            avgmps, maxmps, varmps, perc0spd, N2, rog,
                            .id, entity_id, segment_id)])
  } else {
    segs <- unique(segs[, .(seg_start_lon, seg_start_lat, seg_start_time,
                            seg_end_lon, seg_end_lat, seg_end_time, segdist,
                            avgmps, maxmps, varmps, perc0spd, N2, rog,
                            avgspeed, maxspeed, varspeed, avgpbdif, varpbdif, bearvar, entity_id, segment_id)])
  }
  if (coord.type == "coordinate") {
    return(segs[])
  }
  else {
    convertCoordsToDist(segs, c("seg_start_lat", "seg_end_lat"))
    convertCoordsToDist(segs, c("seg_start_lon", "seg_end_lon"))
  }
  if (coord.type == "both") {
    return(segs[])
  }
  else if (coord.type == "distance") {
    set(segs, j = c("seg_start_lat", "seg_start_lon", "seg_end_lat",
                    "seg_end_lon"), value = NULL)
    segs[]
  }
}

## Add error to this function

## Add segment id

## fix segment dist
