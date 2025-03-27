#' Impute weather data to hourly
#'
#' @details
#'   Use daily data from weatherOz functions to estimate hourly termperature,
#'   relative humidity, and vapor pressure.
#'   The imputation function uses a sin function to fit the daily maximum and
#'   minimum values to estimate hourly data.
#'
#' @param w data.table output from \package{weatherOz} `get_patched_point()`
#' @param tm_max_hour
#' @param tm_min_hour
#'
#' @return a [data.table::data.table()] with the original input weather data
#'   expanded to hourly as described in get_data function.
#'   Non imputed values are duplicated for each hour and additional columns
#'   named as follows.
#'
#'   * ...
#'   * `time`, POSIXct formatted hourly time
#'   * `vp`, vapour pressure in hPa
#'   * `vpd` (vapour pressure deficit),
#'   * `tm` (temperature in degrees celcius),
#'   * `rh` (relative humidity %),
#' @autoglobal
#' @export
#'
#' @examples
#' \dontrun{
# Retrieved data requires an API key to get data
#' wd <- get_data_drill(
#'   latitude = -27.85,
#'   longitude = 150.05,
#'   start_date = "20221001",
#'   end_date = "20221201",
#'   api_key = "your_api_key"
#' )
#' wi <- impute_hourly(wd)
#' }
#'
#' @author Paul Melloy, \email{paul.melloy@@csiro.au}
impute_hourly <- function(w,
                          tm_max_hour = 14,
                          tm_min_hour = 4) {


  # create an hourly data.table with dates
  #  and remove last line which does not exist in original data
  wh <- data.table::data.table(
    time = seq(as.POSIXct(data.table::first(w$date)),
               as.POSIXct(data.table::last(w$date) + 1),
               by = "hour"
               ))[,date := as.Date(time)][-.N]

  wm <-data.table::merge.data.table(wh,w, by = "date")

  wm[, c("tm", "rh") := list(
    round(impute_diurnal(h = data.table::hour(time),
                         max_obs = air_tmax,
                         min_obs = air_tmin,
                         max_hour = tm_max_hour,
                         min_hour = tm_min_hour),digits = 3),
    round(impute_diurnal(h = data.table::hour(time),
                   max_obs = rh_tmin,
                   min_obs = rh_tmax,
                   max_hour = tm_max_hour,
                   min_hour = tm_min_hour),digits = 3)
            )
     ][,c("vpd", "vp") :=
         list((100-rh)/100 * (exp (1.8096 + (17.269425 * tm)/(237.3 + tm))),
              (exp (1.8096 + (17.269425 * tm)/(237.3 + tm))) * (rh/100))]



}
