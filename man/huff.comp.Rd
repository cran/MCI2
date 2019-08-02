\name{huff.comp}
\alias{huff.comp}

\title{
Compare two Huff models
}
\description{
Comparison of two existing Huff model objects with respect to changes in total market areas
}
\usage{
huff.comp(huffmodel1, huffmodel2)
}

\arguments{
  \item{huffmodel1}{
Object (\code{list}) created by the function \code{huff} (Huff Model)
}
  \item{huffmodel2}{
Object (\code{list}) created by the function \code{huff} (Huff Model)
}
}

\value{
A \code{data frame} (invisible) with one row for each \eqn{j} destination and their total market area values from Huff model 1 and 2, respectively, and the corresponding total, share and percentage changes
}

\references{
Huff, D. L. (1962): \dQuote{Determination of Intra-Urban Retail Trade Areas}. Los Angeles : University of California.

Huff, D. L. (1963): \dQuote{A Probabilistic Analysis of Shopping Center Trade Areas}. In: \emph{Land Economics}, \bold{39}, 1, p. 81-90.

Huff, D. L. (1964): \dQuote{Defining and Estimating a Trading Area}. In: \emph{Journal of Marketing}, \bold{28}, 4, p. 34-38.

Loeffler, G. (1998): \dQuote{Market areas - a methodological reflection on their boundaries}. In: \emph{GeoJournal}, \bold{45}, 4, p. 265-272.

Wieland, T. (2015): \dQuote{Nahversorgung im Kontext raumoekonomischer Entwicklungen im Lebensmitteleinzelhandel - Konzeption und Durchfuehrung einer GIS-gestuetzten Analyse der Strukturen des Lebensmitteleinzelhandels und der Nahversorgung in Freiburg im Breisgau}. Projektbericht. Goettingen : GOEDOC, Dokumenten- und Publikationsserver der Georg-August-Universitaet Goettingen. \url{http://webdoc.sub.gwdg.de/pub/mon/2015/5-wieland.pdf}.

Wieland, T. (2017): \dQuote{Market Area Analysis for Retail and Service Locations with MCI}. In: \emph{The R Journal}, \bold{9}, 1, p. 298-323. \url{https://journal.r-project.org/archive/2017/RJ-2017-020/RJ-2017-020.pdf}.

Wieland, T. (2018): \dQuote{Competitive locations of grocery stores in the local supply context - The case of the urban district Freiburg-Haslach}. In: \emph{European Journal of Geography}, \bold{9}, 3, p. 98-115.
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{huff}}, \code{\link{huff.newdest}}, \code{\link{huff.updest}}
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

huff.comp(huffmodel1, huffmodel1a)
# Comparinf results of huffmodel1 and huffmodel1a

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