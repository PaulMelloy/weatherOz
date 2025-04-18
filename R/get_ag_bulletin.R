
#' Get a BOM Agriculture Bulletin
#'
#' **Defunct:** This function is defunct as of version 2.0.0 because the
#'  underlying BOM agricultural forecast bulletin service is no longer available.
#'
#' Fetch the \acronym{BOM} agricultural bulletin information for a specified
#'   station or stations.
#'
#' @param state Australian state or territory as full name or postal code.
#'   Fuzzy string matching via [base::agrep()] is done.  Defaults to `AUS`
#'   returning all state bulletins, see Details for more.
#'
#' @details Allowed state and territory postal codes, only one state per request
#' or all using 'AUS'.
#'  \describe{
#'    \item{AUS}{Australia, returns forecast for all states, NT and ACT}
#'    \item{ACT}{Australian Capital Territory (will return NSW)}
#'    \item{NSW}{New South Wales}
#'    \item{NT}{Northern Territory}
#'    \item{QLD}{Queensland}
#'    \item{SA}{South Australia}
#'    \item{TAS}{Tasmania}
#'    \item{VIC}{Victoria}
#'    \item{WA}{Western Australia}
#'  }
#'
#' @return
#'  A data frame as a `weatherOz_tbl` object (inherits and is fully compatible
#'    with [data.table::data.table()]) of Australia \acronym{BOM}
#'    agricultural bulletin information.
#'
#' @note Data and Information Use
#' Please note the copyright notice and disclaimer,
#'   <http://www.bom.gov.au/other/copyright.shtml> related to the use of this
#'   information. Users of this information are deemed to have read and
#'   accepted the conditions described therein.
#'
#' @examplesIf interactive()
#'
#' get_ag_bulletin(state = "QLD")
#'
#' @references
#' Agricultural observations are retrieved from the Australian Bureau of
#'   Meteorology (\acronym{BOM}) Weather Data Services Agriculture Bulletins,\cr
#'   <http://www.bom.gov.au/catalogue/observations/about-agricultural.shtml>.
#'
#' And also,
#'
#' Australian Bureau of Meteorology (\acronym{BOM})) Weather Data Services
#'   Observation of Rainfall, \cr
#'   <http://www.bom.gov.au/climate/how/observations/rain-measure.shtml>.
#'
#' Station location and other metadata are sourced from the Australian Bureau of
#'   Meteorology (\acronym{BOM}) webpage, Bureau of Meteorology Site Numbers:\cr
#'   <http://www.bom.gov.au/climate/cdo/about/site-num.shtml>.
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}, and Paul
#'   Melloy, \email{paul@@melloy.com.au}
#'
#' @seealso [parse_ag_bulletin]
#'
#' @family BOM
#' @family data fetching
#' @autoglobal
#' @keywords internal

get_ag_bulletin <- function(state = "AUS") {
  .Defunct(msg = "The function 'get_ag_bulletin' is defunct because
  the BOM agricultural forecast bulletin service is no longer available.")

  # see internal_functions.R for these functions
  the_state <- .check_states(state)

  # `NULL` is used for functionality with parse_coastal_forecast(),
  # this just creates `location` with a string of
  # <ftp://ftp.bom.gov.au/anon/gen/fwo>
  location <- .validate_filepath(filepath = NULL)

  bulletin_out <-
    .return_bulletin(file_loc = location, cleaned_state = the_state)

  return(
    structure(
      bulletin_out,
      class = union("weatherOz_tbl", class(bulletin_out)),
      state = unique(bulletin_out[, "state"]),
      product_id = unique(bulletin_out[, "product_id"]),
      tbl_type = "ag_bulletin"
    )
  )
}

# Ag bulletin functions for get() and parse() ----------------------------------
#' Fetches and Returns the Ag Bulletin
#'
#' @param file_loc Either a local file path or a valid FTP URL
#' @param cleaned_state A text string containing the state(s) to download that
#'   has been validated.
#'
#' @return A `data.table` containing the BOM's ag bulletin
#' @keywords Internal
#' @noRd
#'
.return_bulletin <- function(file_loc, cleaned_state) {
  # create vector of XML files
  AUS_XML <- c(
    "IDN65176.xml",
    # NSW
    "IDD65176.xml",
    # NT
    "IDQ60604.xml",
    # QLD
    "IDS65176.xml",
    # SA
    "IDT65176.xml",
    # TAS
    "IDV65176.xml",
    # VIC
    "IDW65176.xml"  # WA
  )
  if (cleaned_state != "AUS") {
    xml_url <- .create_bom_file(AUS_XML,
                                .the_state = cleaned_state,
                                .file_loc = file_loc)
    bulletin_out <- .parse_bulletin(xml_url)
    if (is.null(bulletin_out)) {
      return(invisible(NULL))
    }
    return(bulletin_out[])
  } else {
    file_list <- sprintf("%s/%s", file_loc, AUS_XML)
    bulletin_out <-
      lapply(X = file_list, FUN = .parse_bulletin)
    bulletin_out <- data.table::rbindlist(bulletin_out, fill = TRUE)
    return(bulletin_out[])
  }
}

#' Parse the XML Bulletin to Something Useful
#'
#' @param xml_url The location of the XML file to be parsed
#' @return A `data.table`
#' @keywords Internal
#' @autoglobal
#' @noRd
#'
.parse_bulletin <- function(xml_url) {
  # load the XML from ftp
  if (substr(xml_url, 1, 3) == "ftp") {
    xml_object <- .get_url(remote_file = xml_url)
    if (is.null(xml_object)) {
      return(invisible(NULL))
    }
  } else {
    # load the XML from local
    xml_object <- xml2::read_xml(xml_url)
  }
  # get definitions (and all possible value fields to check against)
  definition_attrs <- xml2::xml_find_all(xml_object, "//data-def")
  definition_attrs <- xml2::xml_attrs(definition_attrs)
  definition_attrs <-
    lapply(definition_attrs, function(x)
      x[[1]][[1]])
  # get the actual observations and create a data table
  observations <- xml2::xml_find_all(xml_object, ".//d")
  out <- data.table::data.table(
    obs_time_local = xml2::xml_find_first(observations, ".//ancestor::obs") |>
      xml2::xml_attr("obs-time-local"),
    obs_time_utc = xml2::xml_find_first(observations, ".//ancestor::obs") |>
      xml2::xml_attr("obs-time-utc"),
    time_zone = xml2::xml_find_first(observations, ".//ancestor::obs") |>
      xml2::xml_attr("time-zone"),
    site = xml2::xml_find_first(observations, ".//ancestor::obs") |>
      xml2::xml_attr("site"),
    station = xml2::xml_find_first(observations, ".//ancestor::obs") |>
      xml2::xml_attr("station"),
    observation = observations |>
      xml2::xml_attr("t"),
    values = observations |>
      xml2::xml_text("t"),
    product_id = substr(basename(xml_url),
                        1,
                        nchar(basename(xml_url)) - 4)
  )
  out <- data.table::dcast(
    out,
    product_id + obs_time_local + obs_time_utc + time_zone + station + site ~
      observation,
    value.var = "values"
  )
  # check that all fields are present, if not add missing col with NAs
  missing <-
    setdiff(unlist(definition_attrs), names(out[, -c(1:5)]))
  if (length(missing) != 0) {
    out[, eval(missing) := NA]
  }

  # tidy up the cols
  refcols <- c(
    "product_id",
    "state",
    "station",
    "site",
    "obs_time_local",
    "obs_time_utc",
    "time_zone",
    "r",
    "tn",
    "tx",
    "twd",
    "ev",
    "tg",
    "sn",
    "solr",
    "t5",
    "t10",
    "t20",
    "t50",
    "t1m",
    "wr"
  )
  # set col classes
  # factor
  out[, c(1, 6) := lapply(.SD, function(x)
    as.factor(x)),
    .SDcols = c(1, 6)]
  # dates
  out[, obs_time_local := gsub("T", " ", obs_time_local)]
  out[, obs_time_utc := gsub("T", " ", obs_time_utc)]
  out[, c(2:3) := lapply(.SD, function(x)
    as.POSIXct(x,
               origin = "1970-1-1",
               format = "%Y%m%d %H%M")),
    .SDcols = c(2:3)]
  # set "Tce" to 0.01
  out[, r := gsub("Tce", "0.01", r)]
  # set numeric cols
  out[, 7:20 := lapply(.SD, as.numeric),
      .SDcols = 7:20]


  # add state column
  out[, state := data.table::fcase(
    product_id == "IDN65176",
    "NSW",
    product_id == "IDD65176",
    "NT",
    product_id == "IDQ60604",
    "QLD",
    product_id == "IDS65176",
    "SA",
    product_id == "IDT65176",
    "TAS",
    product_id == "IDV65176",
    "VIC",
    product_id == "IDW65176",
    "WA"
  )]

  data.table::setcolorder(out, refcols)
  # return from main function
  return(out)
}
