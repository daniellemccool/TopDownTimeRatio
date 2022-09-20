#' Extract segment info from the segmented data.table
#'
#' @param data data.table returned from function /code{tdtr}
#' @param coord.type return actual coordinates, relative distance, or both
#' @param group separate by group, default is FALSE
#' @return data.table with segments only
#' @export

getSegments <- function(data, coord.type = c("coordinate", "distance", "both"), group = FALSE){
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

  distFromMin <- function(coord, mincoord){
    geodist::geodist(data.table(longitude = 0, latitude = coord),
                     data.table(longitude = 0, latitude = mincoord), paired = TRUE, measure = )
  }


  data[, mincoord := min(.SD), .SDcols = coord_cols]
  data[, `:=`(paste0(coord_cols, "_dist"), lapply(.SD, distFromMin, mincoord = mincoord)), .SDcols = coord_cols]
  data[, mincoord := NULL][]

}

convertCoordsToDist <- function(data, coord_cols){

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
    bearvar  = circ.disp(rad(na.omit(bearing)))$var,
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
