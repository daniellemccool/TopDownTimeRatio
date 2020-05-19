#' Extract segment info from the segmented data.table
#'
#' @param data data.table returned from function /code{tdtr}
#' @param coord.type return actual coordinates, relative distance, or both
#' @return data.table with segments only
#' @export

getSegments <- function(data, coord.type = c("coordinate", "distance", "both")){
  coord.type <-match.arg(coord.type, c("coordinate", "distance", "both"))

  segs <- data[segment_start == TRUE]
  segs[, segdist := geodist::geodist(data.table(lon = seg_start_lon, lat = seg_start_lat), data.table(lon = seg_end_lon, lat = seg_end_lat), paired = TRUE, measure = "geodesic")]
  segs <- unique(segs[, .(seg_start_lon, seg_start_lat, seg_start_time, seg_end_lon, seg_end_lat, seg_end_time, segdist)])


  if(coord.type == "coordinate"){
    return(segs[])
  } else{
    convertCoordsToDist(segs, c("seg_start_lat", "seg_end_lat"))
    convertCoordsToDist(segs, c("seg_start_lon", "seg_end_lon"))
  }

  if(coord.type == "both"){
    return(segs[])
  } else if (coord.type == "distance"){
    set(segs, j = c("seg_start_lat", "seg_start_lon",
                    "seg_end_lat", "seg_end_lon"),
        value = NULL)
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

## Add error to this function

## Add segment id

## fix segment dist
