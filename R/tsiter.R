#' @import data.table
#' @importFrom geodist geodist
NULL

#' Set up a data.table for iterative segmentation
#'
#' @param data A data.frame or data.table containing lat, lon and timestamp
#' @return A data.table with numeric timestamp, and an initial segment
#' @export
#' @importFrom geodist geodist

setup <- function(data) {
    # library(data.table)
    setDT(data)
    data[, `:=`(timestamp_numeric, as.numeric(timestamp))]
    set(data, j = c("segment_start", "segment_end"), value = FALSE)
    data[1, `:=`(segment_start, TRUE)]
    data[nrow(data), `:=`(segment_end, TRUE)]
    data[, `:=`(c("adjusted_lat", "adjusted_lon"), .(lat, lon))]
    data[, `:=`(segment_id, cumsum(segment_start))]
    setkey(data, segment_id)

    segstarts <- data[, .I[1], segment_id]$V1
    data[data[segstarts], `:=`(seg_start_lat = i.lat, seg_start_lon = i.lon, seg_start_time = i.timestamp_numeric)]

    segends <- data[, .I[.N], segment_id]$V1
    data[data[segends], `:=`(seg_end_lat = i.lat, seg_end_lon = i.lon, seg_end_time = i.timestamp_numeric)]

    data[, `:=`(seg_dur = seg_end_time - seg_start_time, seg_dist_lat = seg_end_lat - seg_start_lat,
                seg_dist_lon = seg_end_lon - seg_start_lon)]

    data[, `:=`(perc_of_seg_dur, (timestamp_numeric - seg_start_time)/seg_dur)]


    data[!(segment_start | segment_end), `:=`(adjusted_lat = seg_start_lat + (perc_of_seg_dur *
                                                                                  seg_dist_lat), adjusted_lon = seg_start_lon + (perc_of_seg_dur * seg_dist_lon))]

    data[, `:=`(dist, geodist::geodist(x = data.table(lon, lat), y = data.table(longitude = adjusted_lon,
                                                                       latitude = adjusted_lat), paired = TRUE))]

}

#' Perform one iteration of segmentation
#'
#' @param data data.table that has been setup by \code{setup}
#' @return data.table with one additional segment and error distance
#' @export


iterate <- function(data) {
    data[dist == max(dist), `:=`(segment_start = TRUE, segment_end = TRUE, adjusted_lon = lon, adjusted_lat = lat)]

    data[, `:=`(segment_id, cumsum(segment_start))]
    data[, `:=`(seg_end_id, shift(segment_id, fill = 1))]

    setorder(data, timestamp_numeric)

    data[, `:=`(seg_start_lat = lat[1],
                seg_start_lon = lon[1],
                seg_start_time = timestamp_numeric[1]),
         segment_id]

    data[, `:=`(seg_end_lat = lat[.N],
                seg_end_lon = lon[.N],
                seg_end_time = timestamp_numeric[.N]),
         seg_end_id]

    data[segment_end == TRUE, `:=`(seg_end_lat = lat[.N],
                                    seg_end_lon = lon[.N],
                                    seg_end_time = timestamp_numeric[.N]),
         segment_id]


    data[, `:=`(seg_dur = seg_end_time - seg_start_time, seg_dist_lat = seg_end_lat - seg_start_lat,
        seg_dist_lon = seg_end_lon - seg_start_lon)]

    data[, `:=`(perc_of_seg_dur, (timestamp_numeric - seg_start_time)/seg_dur)]


    data[!(segment_start | segment_end), `:=`(adjusted_lat = seg_start_lat + (perc_of_seg_dur *
        seg_dist_lat), adjusted_lon = seg_start_lon + (perc_of_seg_dur * seg_dist_lon))]



    data[, `:=`(dist, geodist(x = data.table(lon, lat), y = data.table(longitude = adjusted_lon,
        latitude = adjusted_lat), paired = TRUE))]

    data[, `:=`(segment_id, cumsum(segment_start))]

}


#' Perform Top-Down Time Ratio segmentation
#'
#' @param data is a data.frame or data.table with timestamp, lat and lon
#' @param col_names named list with column names for timestamp, lat and lon
#' @param max_segs with maximum number of segments allowed default 5000
#' @param n_segs used to generate a specific number of segments
#' @param max_error used as stopping criteria
#' @return data.table with segment information
#' @export

tdtr <- function(data,
                 col_names = list(timestamp_col = "timestamp",
                                  latitude_col = "lat",
                                  longitude_col = "lon"),
                 max_segs = 5000,
                 n_segs = max_segs,
                 max_error = 200){
    setnames(data, col_names$timestamp_col, "timestamp")
    setnames(data, col_names$latitude_col, "lat")
    setnames(data, col_names$longitude_col, "lon")

    data <- TopDownTimeRatio::setup(data)

    i <- 1
    while(max(data$dist) > 200 & (i < max_segs) & (i < n_segs)){
        iterate(data)
        i <- i + 1
    }

    data
}


# data <- setup(data) data <- iterate(data) i <- 1 while(max(data$dist) > 200 & i < 2000){ data <-
# iterate(data) i <- i + 1 print(i) }
