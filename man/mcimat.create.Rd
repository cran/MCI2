\name{mcimat.create}
\alias{mcimat.create}

\title{
Creation of a MCI interaction matrix
}
\description{
Creation of an interaction matrix with local market shares (\eqn{p_{ij}}) of each location (\eqn{j}) in each customer origin (\eqn{i}) based on the frequencies in the raw data (e.g. household or POS survey). 

}
\usage{
mcimat.create(rawdata, origins.id, destinations.id, ..., tcmat, 
origvar.data = NULL, origvardata.id = NULL, 
destvar.data = NULL, destvardata.id = NULL, 
remOrig = NULL, remDest = NULL, corObserved = 0, 
remNA = TRUE)
}

\arguments{
  \item{rawdata}{
Raw data (\code{data.frame}), e.g. customer survey
}
  \item{origins.id}{
Vector of customer origins
}
  \item{destinations.id}{
Vector of destinations (stores, locations)
}
  \item{\dots}{
other numeric variables in the raw data which were observed and shall be used to calculate market shares (e.g. expenditures)
}
  \item{tcmat}{
Object (\code{list}) created by the function \code{tcmat.create} (Transportation costs matrix)
}
  \item{origvar.data}{
Optional: additional data (\code{data.frame}) concerning the customer origins
}
  \item{origvardata.id}{
Optional: customer origins in the additional origins data
}
  \item{destvar.data}{
Optional: additional data (\code{data.frame}) concerning the destinations
}
  \item{destvardata.id}{
Optional: destinations in the additional destinations data
}
  \item{remOrig}{
Optional: vector of origins to be removed from the analysis
}
  \item{remDest}{
Optional: vector of destinations to be removed from the analysis
}
  \item{corObserved}{
numeric value which is added to the absolute values before calculating market shares (default: 0)
}
  \item{remNA}{
Logical argument that indicates if \code{NA} values of the origins and destinations are removed or not
}
}
\details{
This function creates a Multiplicative Competitive Interaction (MCI) Model interaction matrix for further use in the function \code{mci}.
}
\value{
A \code{mcimat list} (invisible) containing the following components:
\item{mcimat }{MCI interaction matrix (\code{data frame}), also containing the local market shares (\code{p_ij})}
\item{coords }{A \code{list} containing the coordinates and additional information of the origins (\code{coords_origins}) and the destinations (\code{coords_destinations}), inherited from the \code{tcmat} object}
\item{tc.mode }{A \code{list} containing information about the transportation costs matrix, inherited from the \code{tcmat} object}
\item{mci.cormode }{A \code{list} containing information about the processing mode of the interaction matrix (removed origins/destinations etc.)}
}
\references{
Huff, D. L./Batsell, R. R. (1975): \dQuote{Conceptual and Operational Problems with Market Share Models of Consumer Spatial Behavior}. In: \emph{Advances in Consumer Research}, \bold{2}, p. 165-172.

Huff, D. L./McCallum, D. (2008): \dQuote{Calibrating the Huff Model Using ArcGIS Business Analyst}. \emph{ESRI White Paper}, September 2008. \url{https://www.esri.com/library/whitepapers/pdfs/calibrating-huff-model.pdf}

Nakanishi, M./Cooper, L. G. (1974): \dQuote{Parameter Estimation for a Multiplicative Competitive Interaction Model - Least Squares Approach}. In: \emph{Journal of Marketing Research}, \bold{11}, 3, p. 303-311.

Nakanishi, M./Cooper, L. G. (1982): \dQuote{Simplified Estimation Procedures for MCI Models}. In: \emph{Marketing Science}, \bold{1}, 3, p. 314-322. 

Wieland, T. (2017): \dQuote{Market Area Analysis for Retail and Service Locations with MCI}. In: \emph{The R Journal}, \bold{9}, 1, p. 298-323. \url{https://journal.r-project.org/archive/2017/RJ-2017-020/RJ-2017-020.pdf}.

Wieland, T. (2018): \dQuote{Competitive locations of grocery stores in the local supply context - The case of the urban district Freiburg-Haslach}. In: \emph{European Journal of Geography}, \bold{9}, 3, p. 98-115.
}
\author{
Thomas Wieland
}
\note{
The function is a wrapper of \code{ijmatrix.create} from the \pkg{MCI} package. For further information see the \pkg{MCI} documentation and the corresponding RJ paper (Wieland 2017).
}

\seealso{
\code{\link{mci}}, \code{\link{tcmat.create}}
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

data(HaslachSurvey)
# survey raw data (Store choices and purchases)
data(HaslachDistricts)
# IDs and information about customer origins
data(HaslachStores)
# IDs and information about destinations (grocery stores)

# Preparing raw data (HaslachSurvey)
HaslachSurvey_prepared <- rawdata.prep (cols.below1 = 
list(HaslachSurvey$LM1_Text, HaslachSurvey$LM2_Text),
cols.below2 = list(HaslachSurvey$LM1E, HaslachSurvey$LM2E),
cols.keep = list(HaslachSurvey$WO),
colnames.new = c("LM", "LME", "Wohnort"))
# "WO" and "Wohnort" = origin ID
# "LM1_Text", "LM2_Text" and "LM" = destination IDs (grocery stores)
# "LM1E", "LM2E" and "LME" = grocery store purchases

# Creating a MCI interaction matrix
# based on raw data of a survey (HaslachSurvey_prepared)
# and a tcmat list object
mcimat_haslach <- mcimat.create(rawdata = HaslachSurvey_prepared, 
origins.id = "Wohnort", destinations.id = "LM", "LME", 
tcmat = tcmat_haslach_airline, 
remOrig = c("SBXXX", "SB613"), corObserved = 0.1,
origvar.data = HaslachDistricts, origvardata.id = "WO",
destvar.data = HaslachStores, destvardata.id = "LM")

# MCI model based on empirical local market shares
# two explanatory variables: distance (d_ij), store size (LM_VKF)
mcimodel_haslach <- mci (mcimat_haslach, "p_ij", "d_ij", "LM_VKF", 
show_proc = TRUE)
}