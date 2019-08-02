\name{tcmat.create}
\alias{tcmat.create}

\title{
Creation of a transport costs matrix
}

\description{
Creation of a transport costs matrix (airline distance, car travel time) 
}

\usage{
tcmat.create(origins.id, origins.addr, 
destinations.id, destinations.addr, 
tc.type = "airline", tc.unit = "km", 
or.addr.format = "stradr", de.addr.format = "stradr",
tc.constant = 0, query.delay = 0, show_proc = FALSE)
}

\arguments{
  \item{origins.id}{
ID of the origins
}
  \item{origins.addr}{
Address of the origins, either street addresses (e.g. "Markgrafenstrasse 68, Freiburg, Germany") or coordinates (X_LON;X_LAT, e.g.: "7,82218;47,99387")
}
  \item{destinations.id}{
ID of the destinations
}
  \item{destinations.addr}{
Address of the destinations, either street addresses (e.g. "Markgrafenstrasse 68, Freiburg, Germany") or coordinates (X_LON;X_LAT, e.g.: "7,82218;47,99387")
}
  \item{tc.type}{
Type of transportat costs: \code{"airline"} (airline distance) or \code{"street"} (car travel time)
}
  \item{tc.unit}{
If \code{tc.type = "airline"}: unit of distance ("m", "km" or "miles")
}
  \item{or.addr.format}{
Origins address format. Default: \code{"stradr"} (street addresses))
}
  \item{de.addr.format}{
Destinations address format. Default: \code{"stradr"} (street addresses))
}
  \item{tc.constant}{
Adding a constant to the calculated/queried values (default: 0)
}
  \item{query.delay}{
Delay within OSM geocoding querys (unit: seconds, default: 0)
}
  \item{show_proc}{
Logical argument that indicates if the function prints messages about the state of process during the work 
}
}

\details{
Creation of a \code{tcmat list} object (Transport costs matrix) for further use in \code{huff} and \code{mcimat.create}. Included geocoding of street addresses (from \code{geocode_OSM} in \pkg{tmaptools}) and travel time query (from \code{ormsTable} in \pkg{osrm}) or the calculation of airline distances (from \code{dist.mat} in \pkg{REAT}).
}

\value{
A \code{tcmat list} (invisible) containing the following components:
\item{coords_origins }{A \code{data.frame} containing the coordinates (lat, lon) of the origins}
\item{coords_destinations }{A \code{data.frame} containing the coordinates (lat, lon) of the destinations}
\item{tcmat }{A \code{data.frame} containing the transport costs matrix}
\item{tc.mode }{A \code{list} containing information about the transportation costs matrix}
}

\references{
Giraud, T. (2018): osrm: Interface Between R and the OpenStreetMap-Based Routing Service OSRM. R package version 3.1.1. \url{https://CRAN.R-project.org/package=osrm}.
  
Tennekes, M. (2018): tmaptools: Thematic Map Tools. R package version 2.0-1. \url{https://CRAN.R-project.org/package=tmaptools}.

Wieland, T. (2017): \dQuote{Market Area Analysis for Retail and Service Locations with MCI}. In: \emph{The R Journal}, \bold{9}, 1, p. 298-323. \url{https://journal.r-project.org/archive/2017/RJ-2017-020/RJ-2017-020.pdf}.

Wieland, T. (2018): \dQuote{Competitive locations of grocery stores in the local supply context - The case of the urban district Freiburg-Haslach}. In: \emph{European Journal of Geography}, \bold{9}, 3, p. 98-115.

Wieland, T. (2019): REAT: Regional Economic Analysis Toolbox. R package version 3.0.0. \url{https://CRAN.R-project.org/package=REAT}.
}

\author{
Thomas Wieland
}

\note{
This function is wrapper of the functions \code{geocode_OSM} (for geocoding street addresses using OSM Nominatim) from the \pkg{tmaptools} package (Tennekes 2018), \code{osrmTable} (for creating travel time matrices between points while interfacing the table OSRM service) from the  \pkg{osrm} package (Giraud 2018) and \code{dist.mat} (for creating airline distances between points) from the \pkg{REAT} package (Wieland 2018). The OSM-related functions rely on the usage of a running OSRM service! For more information, see the related package documentations. 
}

\seealso{
\code{\link{huff}}, \code{\link{mcimat.create}}
}

\examples{
data(HaslachSurvey)
# survey raw data (Store choices and purchases)
data(HaslachDistricts)
# IDs and information about customer origins
data(HaslachStores)
# IDs and information about destinations (grocery stores)

\dontrun{
tcmat_haslach_airline <- tcmat.create(origins.id = HaslachDistricts$WO, 
origins.addr = HaslachDistricts$WO_Adr, destinations.id = HaslachStores$LM, 
destinations.addr = HaslachStores$LM_Adr_zus, 
tc.type = "airline", tc.unit = "km",
addr.format = "stradr", tc.constant = 0, show_proc = TRUE)
# Creation of a transport costs matrix with airline distances
# saving as list object "tcmat_haslach_airline"
# Needs internet access and accesses OpenStreetMap server(s)
# If server not available, the function will produce an error

tcmat_haslach_drvtime <- tcmat.create(origins.id = HaslachDistricts$WO, 
origins.addr = HaslachDistricts$WO_Adr, destinations.id = HaslachStores$LM, 
destinations.addr = HaslachStores$LM_Adr_zus,
tc.type = "street", addr.format = "stradr", 
tc.constant = 0, show_proc = TRUE)
# Creation of a transport costs matrix with car driving times
# saving as list object "tcmat_haslach_drvtime"
# Needs internet access and accesses OpenStreetMap server(s)
# If server not available, will produce an error
}
}