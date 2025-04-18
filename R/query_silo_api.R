
#' Query the SILO API
#'
#' Use \CRANpkg{crul} to query the SILO API.
#'
#' @param .station_code A `character` string of the \acronym{BOM} station code
#'   for the station of interest.
#' @param .longitude A `numeric` value for the geographic longitude of interest
#'   in decimal degrees.
#' @param .latitude A `numeric` value for the geographic latitude of interest
#'   in decimal degrees.
#' @param .start_date A `character` string representing the beginning of the
#'   range to query in the format 'yyyy-mm-dd' (ISO8601).  Will return data
#'   inclusive of this date.
#' @param .end_date A `character` string representing the end of the range query
#'   in the format 'yyyy-mm-dd' (ISO8601).  Will return data inclusive of this
#'   range.
#' @param .values A `character` string with the type of weather data to
#'   return.
#' @param .format A `character` string indicating which format to return from
#'   the API, either `apsim`, `csv` or `near`.
#' @param .api_key A valid e-mail address
#' @param .dataset A SILO dataset, either "PatchedPoint" or "DataDrill".
#'
#' @return A `data.table` of data for manipulating before returning to the user.
#'
#' @noRd
#' @autoglobal
#' @keywords internal

.query_silo_api <- function(.station_code = NULL,
                            .longitude = NULL,
                            .latitude = NULL,
                            .start_date = NULL,
                            .end_date = NULL,
                            .values = NULL,
                            .format,
                            .radius = NULL,
                            .api_key = NULL,
                            .dataset) {

  base_url <- "https://www.longpaddock.qld.gov.au/cgi-bin/silo/"

  end_point <- data.table::fcase(
    .dataset == "PatchedPoint",
    "PatchedPointDataset.php",
    .dataset == "DataDrill",
    "DataDrillDataset.php"
  )

  if (.dataset == "PatchedPoint" && .format == "csv") {
    silo_query_list <- list(
      station = as.integer(.station_code),
      start = as.character(.start_date),
      finish = as.character(.end_date),
      format = .format,
      comment = paste(.values, collapse = ""),
      username = .api_key,
      password = "api_request"
    )
  } else if (.dataset == "DataDrill" && .format == "csv") {
    silo_query_list <- list(
      longitude = .longitude,
      latitude = .latitude,
      start = as.character(.start_date),
      finish = as.character(.end_date),
      format = .format,
      comment = paste(.values, collapse = ""),
      username = .api_key,
      password = "api_request"
    )
  } else if (.dataset == "PatchedPoint" && .format == "apsim") {
    silo_query_list <- list(
      station = as.integer(.station_code),
      start = as.character(.start_date),
      finish = as.character(.end_date),
      format = .format,
      username = .api_key
    )
  } else if (.dataset == "PatchedPoint" && .format == "near") {
    silo_query_list <- list(
      station = .station_code,
      radius = .radius,
      format = .format
    )
    } else {
    silo_query_list <- list(
      longitude = .longitude,
      latitude = .latitude,
      start = as.character(.start_date),
      finish = as.character(.end_date),
      format = .format,
      username = .api_key
    )
    }

  client <-
    crul::HttpClient$new(url = sprintf("%s%s", base_url, end_point))
  response <- client$get(query = silo_query_list)

  # check responses for errors
  # check to see if request failed or succeeded
  # - a custom approach this time combining status code,
  #   explanation of the code, and message from the server
  if (response$status_code > 201) {
    mssg <- response$parse("UTF-8")
    x <- response$status_http()
    stop("HTTP (", x$status_code, ") - ", x$explanation, "\n", mssg,
         call. = FALSE)
  }
  response$raise_for_status()

  # the API won't return proper responses for malformed requests, so, we check
  # for the word "Sorry" and parse the response to the user if something slips
  # through our user checks.
  if (grepl("Sorry", response$parse("UTF8")) ||
      grepl("Request Rejected", response$parse("UTF8"))) {
    stop(call. = FALSE,
         gettext(response$parse("UTF8")),
         domain = NA)
  }

  if (.format == "apsim") {
    met_file_path <- tempfile()
    on.exit(unlink(tempfile()))
    writeLines(text = response$parse("UTF8"), con = met_file_path)
    apsim <- readLines(met_file_path)
    data <- data.table::fread(file = met_file_path,
                              skip = grep(pattern = "^year",
                                          x = apsim) + 2)
    apsim_met <- apsimx::as_apsim_met(
      x = data,
      filename = "weather.met.weather",
      site = trimws(sub("^.*=", "",
                        apsim[grep(pattern = "^!station name =", x = apsim)])),
      latitude = trimws(sub("^.*=", "",
                            apsim[grep(pattern = "^latitude =", x = apsim)])),
      longitude = trimws(sub("^.*=", "",
                             apsim[grep(pattern = "^longitude =", x = apsim)])),
      colnames = scan(
        text = apsim[grep(pattern = "year", x = apsim)],
        what = '',
        quiet = TRUE
      ),
      units = scan(
        text = apsim[grep(pattern = "\\(\\)", x = apsim)],
        what = '',
        quiet = TRUE
      ),
      comments = apsim[grep(pattern = "^!", x = apsim)]
    )
    return(apsim_met)
  }

  response_data <- data.table::fread(response$parse("UTF8"))

  # return the response if we're just looking for the nearest stations
  if (.format == "near") {
    data.table::setnames(
      x = response_data,
      old = c(
        "Number",
        "Station name",
        "Latitude",
        "Longitud",
        "Stat",
        "Elevat.",
        "Distance (km)"
      ),
      new = c(
        "station_code",
        "station_name",
        "latitude",
        "longitude",
        "state",
        "elev_m",
        "distance_km"
      ),
      skip_absent = TRUE
    )

    response_data[, station_code :=
                    as.factor(as.character(sprintf("%06d", station_code)))]
    response_data[, station_name := trimws(.strcap(x = station_name))]
    response_data[, owner := "BOM"]
    response_data[, distance_km := round(distance_km, 1)]
    data.table::setkey(response_data, "station_code")
    data.table::setcolorder(response_data, c(1:2, 4:3, 5:6, 8, 7))
    return(response_data)
  }

  response_data[, elev_m :=
                  trimws(
                    gsub("elevation=", "",
                         response_data$metadata[grep("elevation",
                                                     response_data$metadata)]))]

  data.table::setnames(response_data, old = "YYYY-MM-DD", new = "date")
  response_data[, year := lubridate::year(date)]
  response_data[, month := lubridate::month(date)]
  response_data[, day := lubridate::day(date)]
  response_data[, extracted :=
                  lubridate::as_date(trimws(gsub(
                    "extracted=", "",
                    response_data$metadata[grep(
                      "extracted", response_data$metadata)]
                  )))]

  if (.dataset == "PatchedPoint") {

    response_data[, station_code :=
                    as.factor(as.character(sprintf("%06d", station)))]
    response_data[, station := NULL]

    # apparently some of the data don't have station names, who knew?
    if (any(grepl("name", response_data$metadata))) {
      response_data[, station_name :=
                      .strcap(trimws(gsub("name=", "",
                                          response_data$metadata[grep("name",
                                            response_data$metadata)])))]
    } else {
      response_data[, station_name := NA]
    }

    response_data[, latitude :=
                    as.numeric(trimws(
                      gsub("latitude=", "",
                           response_data$metadata[grep(
                             "latitude", response_data$metadata)])))]
    response_data[, longitude :=
                    as.numeric(trimws(gsub("longitude=", "",
                                response_data$metadata[grep(
                                  "longitude", response_data$metadata)])))]
    .check_silo_codes(response_data)
  }

  # put columns in alphabetical order, then move others to front
  data.table::setcolorder(response_data, c(order(names(response_data))))

  data.table::setcolorder(
    response_data,
    c(
      "longitude",
      "latitude",
      "elev_m",
      "date",
      "year",
      "month",
      "day",
      "extracted"
    )
  )

  # provide some standard names between DPIRD and SILO for easy merging where
  # data values are shared
  # not all columns are renamed, but all are listed for clarity
  data.table::setnames(response_data,
                       old = c(
                         "station_code",
                         "station_name",
                         "longitude",
                         "latitude",
                         "elev_m",
                         "date",
                         "year",
                         "month",
                         "day",
                         "extracted",
                         "daily_rain",
                         "daily_rain_source",
                         "et_morton_actual",
                         "et_morton_actual_source",
                         "et_morton_potential",
                         "et_morton_potential_source",
                         "et_morton_wet",
                         "et_morton_wet_source",
                         "et_short_crop",
                         "et_short_crop_source",
                         "et_tall_crop",
                         "et_tall_crop_source",
                         "evap_comb",
                         "evap_comb_source",
                         "evap_morton_lake",
                         "evap_morton_lake_source",
                         "evap_pan",
                         "evap_pan_source",
                         "evap_syn",
                         "evap_syn_source",
                         "max_temp",
                         "max_temp_source",
                         "min_temp",
                         "min_temp_source",
                         "mslp",
                         "mslp_source",
                         "radiation",
                         "radiation_source",
                         "rh_tmax",
                         "rh_tmax_source",
                         "rh_tmin",
                         "rh_tmin_source",
                         "vp",
                         "vp_deficit",
                         "vp_deficit_source",
                         "vp_source"
                       ),
                       new = c(
                         "station_code",
                         "station_name",
                         "longitude",
                         "latitude",
                         "elev_m",
                         "date",
                         "year",
                         "month",
                         "day",
                         "extracted",
                         "rainfall",
                         "rainfall_source",
                         "et_morton_actual",
                         "et_morton_actual_source",
                         "et_morton_potential",
                         "et_morton_potential_source",
                         "et_morton_wet",
                         "et_morton_wet_source",
                         "et_short_crop",
                         "et_short_crop_source",
                         "et_tall_crop",
                         "et_tall_crop_source",
                         "evap_comb",
                         "evap_comb_source",
                         "evap_morton_lake",
                         "evap_morton_lake_source",
                         "evap_pan",
                         "evap_pan_source",
                         "evap_syn",
                         "evap_syn_source",
                         "air_tmax",
                         "air_tmax_source",
                         "air_tmin",
                         "air_tmin_source",
                         "mslp",
                         "mslp_source",
                         "radiation",
                         "radiation_source",
                         "rh_tmax",
                         "rh_tmax_source",
                         "rh_tmin",
                         "rh_tmin_source",
                         "vp",
                         "vp_deficit",
                         "vp_deficit_source",
                         "vp_source"
                       ),
                       skip_absent = TRUE
  )

  response_data[, metadata := NULL]
  response_data <- response_data[!is.na(response_data$date), ]
  response_data[, date := as.Date(date)]
  return(response_data[])
}


#' Check SILO Data Codes
#'
#' Checks if any SILO data codes for interpolated data are present in the
#'   requested station observation data. If any such codes are found, a message
#'   will be reported with a suggestion to check the data source columns.  See
#'   `get_patched_point()` documentation for further details on codes and
#'   references.
#'
#' @param dt A `data.table`, defaults to the SILO API query result object from
#'   `.query_silo_api()`.
#'
#' @return An `invisible(NULL)`. This function returns no value, only a friendly
#'   message. It is used for checking and reporting the presence of interpolated
#'   data codes in the station observation data (for API queries performed using
#'   a station_code/code).
#'
#' @noRd
#' @autoglobal
#' @keywords internal

.check_silo_codes <- function(dt) {
  # these are the only cols that we need to be concerned about being
  # interpolated
  primary_cols <- c(
    "daily_rain_source",
    "max_temp_source",
    "min_temp_source",
    "vp_source",
    "evap_pan_source"
  )

  dt <-
    dt[, .SD, .SDcols = primary_cols[primary_cols %in% names(dt)]]

  if (ncol(dt) > 0 && any(dt[, lapply(
      X = .SD,
      FUN = function(col)
        all(stats::na.omit(col == 0)))])) {
      # Report message
      message(
        "You have requested station observation data but some rows in this\n",
        "dataset have data codes for interpolated data.\n",
        "Check the 'data_source' columns and `get_patched_point()` or\n",
        "`get_data_drill()` documentation for further details on codes and\n",
        "references.\n"
      )
    }
  return(invisible(NULL))
}
