% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_ag_bulletin.R
\name{get_ag_bulletin}
\alias{get_ag_bulletin}
\title{Get a BOM Agriculture Bulletin}
\usage{
get_ag_bulletin(state = "AUS")
}
\arguments{
\item{state}{Australian state or territory as full name or postal code.
Fuzzy string matching via \code{\link[base:agrep]{base::agrep()}} is done.  Defaults to \code{AUS}
returning all state bulletins, see Details for more.}
}
\value{
A data frame as a \code{weatherOz_tbl} object (inherits and is fully compatible
with \code{\link[data.table:data.table]{data.table::data.table()}}) of Australia \acronym{BOM}
agricultural bulletin information.
}
\description{
\strong{Defunct:} This function is defunct as of version 2.0.0 because the
underlying BOM agricultural forecast bulletin service is no longer available.
}
\details{
Fetch the \acronym{BOM} agricultural bulletin information for a specified
station or stations.

Allowed state and territory postal codes, only one state per request
or all using 'AUS'.
\describe{
\item{AUS}{Australia, returns forecast for all states, NT and ACT}
\item{ACT}{Australian Capital Territory (will return NSW)}
\item{NSW}{New South Wales}
\item{NT}{Northern Territory}
\item{QLD}{Queensland}
\item{SA}{South Australia}
\item{TAS}{Tasmania}
\item{VIC}{Victoria}
\item{WA}{Western Australia}
}
}
\note{
Data and Information Use
Please note the copyright notice and disclaimer,
\url{http://www.bom.gov.au/other/copyright.shtml} related to the use of this
information. Users of this information are deemed to have read and
accepted the conditions described therein.
}
\examples{
\dontshow{if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}

get_ag_bulletin(state = "QLD")
\dontshow{\}) # examplesIf}
}
\references{
Agricultural observations are retrieved from the Australian Bureau of
Meteorology (\acronym{BOM}) Weather Data Services Agriculture Bulletins,\cr
\url{http://www.bom.gov.au/catalogue/observations/about-agricultural.shtml}.

And also,

Australian Bureau of Meteorology (\acronym{BOM})) Weather Data Services
Observation of Rainfall, \cr
\url{http://www.bom.gov.au/climate/how/observations/rain-measure.shtml}.

Station location and other metadata are sourced from the Australian Bureau of
Meteorology (\acronym{BOM}) webpage, Bureau of Meteorology Site Numbers:\cr
\url{http://www.bom.gov.au/climate/cdo/about/site-num.shtml}.
}
\seealso{
\link{parse_ag_bulletin}

Other BOM: 
\code{\link{find_forecast_towns}()},
\code{\link{get_available_imagery}()},
\code{\link{get_available_radar}()},
\code{\link{get_coastal_forecast}()},
\code{\link{get_precis_forecast}()},
\code{\link{get_radar_imagery}()},
\code{\link{get_satellite_imagery}()},
\code{\link{parse_ag_bulletin}()},
\code{\link{parse_coastal_forecast}()},
\code{\link{parse_precis_forecast}()}

Other data fetching: 
\code{\link{get_coastal_forecast}()},
\code{\link{get_data_drill}()},
\code{\link{get_data_drill_apsim}()},
\code{\link{get_dpird_apsim}()},
\code{\link{get_dpird_extremes}()},
\code{\link{get_dpird_minute}()},
\code{\link{get_dpird_summaries}()},
\code{\link{get_patched_point}()},
\code{\link{get_patched_point_apsim}()},
\code{\link{get_precis_forecast}()},
\code{\link{get_radar_imagery}()},
\code{\link{get_satellite_imagery}()}
}
\author{
Adam H. Sparks, \email{adamhsparks@gmail.com}, and Paul
Melloy, \email{paul@melloy.com.au}
}
\concept{BOM}
\concept{data fetching}
\keyword{internal}
