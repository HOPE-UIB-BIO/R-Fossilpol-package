#' @title Extract data from shapefile and add to points
#' @param data_source Data.frame containing `long` and `lat`
#' @param shapefile Shapefile to use
#' @return Data.frame with additional columns
#' @description Extract data from shapefile and add to points provided based
#' on the data `lat` and `long`
#' @export
geo_assign_shapefile <-
  function(data_source,
           shapefile) {
    RUtilpol::check_class("data_source", "data.frame")

    RUtilpol::check_col_names("data_source", c("lat", "long"))

    RUtilpol::check_class("shapefile", c("sf", "SpatialPolygonsDataFrame"))

    # extract the coordinates system
    data_coord <-
      data_source %>%
      dplyr::select(long, lat) %>%
      as.data.frame()

    # transform into Spatial polygon
    sp::coordinates(data_coord) <- ~ long + lat

    # if 'shapefile' loaded using 'sf' package, transfer back to 'sp'
    if (
      "sf" %in% class(shapefile)
    ) {
      shapefile <- sf::as_Spatial(shapefile)
    }

    # silence this line as CRS seems to be obsolete
    # sp::proj4string(data_coord) <- rgdal::CRSargs(sp::CRS(sp::proj4string(shapefile)))

    # assumes projection are the same
    suppressWarnings(
      sp::proj4string(data_coord) <- sp::proj4string(shapefile)
    )

    # combine the extracted values and original data
    data_res <-
      data_source %>%
      dplyr::bind_cols(sp::over(x = data_coord, y = shapefile)) %>%
      tibble::as_tibble()

    return(data_res)
  }
