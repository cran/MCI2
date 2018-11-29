\name{huff.newdest}
\alias{huff.newdest}

\title{
Huff Model market simulation with a new destination
}
\description{
Adding a new location and (re-)calculating local market shares and total market areas of locations using the probabilistic market area model by Huff
}
\usage{
huff.newdest(huffmodel, newdest.id, newdest.addr, newdest.attract, 
newdestaddr.format = "stradr", distval = NULL)
}

\arguments{
  \item{huffmodel}{
Object (\code{list}) created by the function \code{huff} (Huff Model)
}
  \item{newdest.id}{
Code of the new destination
}
  \item{newdest.addr}{
(Street) address of the new destination
}
  \item{newdest.attract}{
Attraction value of the new destination
}
  \item{newdestaddr.format}{
Address format (default: street address)
}
  \item{distval}{
Optional: numerical vector containing the transportation costs values with respect to the new destination and all origins in the Huff Model object (\code{huffmodel})
}
}
\details{
Adding a new destination (e.g. new store) to a given location system (Huff Model object). This function computes local market shares (\eqn{p_{ij}}) and total market areas (\eqn{T_j}) according to the Huff Model.
}
\value{
A \code{huffmodel list} (invisible) containing the following components:
\item{huffmat }{Huff interaction matrix (\code{data frame}), also containing the local market shares (\code{p_ij})}
\item{hufftotal }{Total location market areas (\code{data frame}), also containing the total market areas (\code{T_j})}
\item{params }{A \code{matrix} containing the user-defined weighting functions and the corresponding weighting parameters}
\item{coords }{A \code{list} containing the coordinates and additional information of the origins (\code{coords_origins}) and the destinations (\code{coords_destinations}), inherited from the \code{tcmat} object}
\item{tc.mode }{A \code{list} containing information about the transportation costs matrix, inherited from the \code{tcmat} object}
}
\references{
Huff, D. L. (1962): \dQuote{Determination of Intra-Urban Retail Trade Areas}. Los Angeles : University of California.

Huff, D. L. (1963): \dQuote{A Probabilistic Analysis of Shopping Center Trade Areas}. In: \emph{Land Economics}, \bold{39}, 1, p. 81-90.

Huff, D. L. (1964): \dQuote{Defining and Estimating a Trading Area}. In: \emph{Journal of Marketing}, \bold{28}, 4, p. 34-38.

Loeffler, G. (1998): \dQuote{Market areas - a methodological reflection on their boundaries}. In: \emph{GeoJournal}, \bold{45}, 4, p. 265-272.

Wieland, T. (2015): \dQuote{Nahversorgung im Kontext raumoekonomischer Entwicklungen im Lebensmitteleinzelhandel - Konzeption und Durchfuehrung einer GIS-gestuetzten Analyse der Strukturen des Lebensmitteleinzelhandels und der Nahversorgung in Freiburg im Breisgau}. Projektbericht. Goettingen : GOEDOC, Dokumenten- und Publikationsserver der Georg-August-Universitaet Goettingen. \url{http://webdoc.sub.gwdg.de/pub/mon/2015/5-wieland.pdf}.

Wieland, T. (2017): \dQuote{Market Area Analysis for Retail and Service Locations with MCI}. In: \emph{The R Journal}, \bold{9}, 1, p. 298-323. \url{https://journal.r-project.org/archive/2017/RJ-2017-020/RJ-2017-020.pdf}.
}

\author{
Thomas Wieland
}

\note{
The function is a wrapper of \code{huff.shares} and \code{shares.total} of the \pkg{MCI} package, also integrating the \code{tcmat.create} function. For further information see the \pkg{MCI} documentation and the corresponding RJ paper (Wieland 2017).
}

\seealso{
\code{\link{huff}}, \code{\link{huff.updest}}, \code{\link{tcmat.create}}
}

\examples{
# Compilation of tcmat list from existing datasets:
# (Results from the tcmat.create function)
data(Haslach_tcmatAirline)
# airline distances
data(Haslach_coords_origins)
# Coordinates of origins
data(Haslach_coords_destinations)
# Coordinates of destinationes

# Component "tc.mode":
Airline_tc.mode <- list()
Airline_tc.mode$tc.type = "airline"
Airline_tc.mode$tc.unit = "km"
Airline_tc.mode$tc.constant = 0

# tcmat with airline distances
# Compilation as a list:
tcmat_haslach_airline <- list(tcmat = Haslach_tcmatAirline,
coords_origins = Haslach_coords_origins,
coords_destinations = Haslach_coords_destinations,
tc.mode = Airline_tc.mode)

Drvtime_tc.mode <- list()
Drvtime_tc.mode$tc.type = "street"
Drvtime_tc.mode$tc.unit = "min"
Drvtime_tc.mode$tc.constant = 0

data(Haslach_tcmatDrvtime)
# car driving times

# tcmat with car driving times
# Compilation as a list:
tcmat_haslach_drvtime <- list(tcmat = Haslach_tcmatDrvtime,
coords_origins = Haslach_coords_origins,
coords_destinations = Haslach_coords_destinations,
tc.mode = Drvtime_tc.mode)

data(HaslachDistricts)
# IDs and information about customer origins
data(HaslachStores)
# IDs and information about destinations (grocery stores)

huffmodel1 <- huff (HaslachDistricts$WO, HaslachDistricts$WO_Einwohner, 
HaslachStores$LM, HaslachStores$LM_VKF, 
tcmat = tcmat_haslach_airline)
# Huff Model with standard parameters
# uses given transport costs matrix (tcmat_haslach_airline)

huffmodel1a <- huff (HaslachDistricts$WO, HaslachDistricts$WO_Einwohner, 
HaslachStores$LM, HaslachStores$LM_VKF, 
tcmat = tcmat_haslach_airline,
atype = "pow", gamma = 0.9, dtype = "pow", lambda = -2.1)
# Alternative weighting parameters

\dontrun{
huffmodel2 <- huff.newdest(huffmodel1, newdest.id = "LM_NEU",
newdest.addr = "Bettackerstrasse 3, Freiburg, Germany", 
newdest.attract = 1500)
# Adding a new destination with a given street address
# Recalculation of the Huff Model
# Needs internet access and accesses OpenStreetMap server(s)
# If server not available, the function will produce an error
}

huffmodel3 <- huff.updest (huffmodel1, dest.id = "LM01", 
dest.attrac = 1200)
# Update the attraction value of one grocery stores
# Recalculation of the Huff Model

\dontrun{
model.export(huffmodel3, "interactionmatrix", "totals")
# export as CSV files
}
}