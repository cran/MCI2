\name{mci.sim}
\alias{mci.sim}

\title{
MCI model market simulation
}

\description{
Market simulation using the MCI model
}

\usage{
mci.sim(mcimodel, origins.pot, ..., 
mcitrans = "lc", interc = NULL)
}

\arguments{
  \item{mcimodel}{
Object (\code{list}) created by the function \code{mci} 
}
  \item{origins.pot}{
The column representing the customer potential in the origins in the interaction matrix in \code{mcimodel}
}
  \item{\dots}{
Explanatory variables (partial utilites) and their corresponding weighting parameters (variable1, parameter1, variable2, parameter2, ...)
}
  \item{mcitrans}{
Type of MCI transformation: Log-centering transformation (\code{mcitrans = "lc"}), or, e.g. in the case of using dummy variables, inverse log-centering transformation (\code{mcitrans = "ilc"})
}
  \item{interc}{
Intercept to be included
}

}
\details{
This function calculates a market simulation using the Multiplicative Competitive Interaction (MCI) Model based on a given MCI model.
}

\value{
A \code{mcimodel list} (invisible) containing the following components:
\item{mcimat }{MCI interaction matrix (\code{data frame}), also containing the local market shares (\code{p_ij})}
\item{mcitotal }{Total location market areas (\code{data frame}), also containing the total market areas (\code{T_j})}
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
The function is a wrapper of \code{mci.shares} and \code{shares.total} of the \pkg{MCI} package. For further information see the \pkg{MCI} documentation and the corresponding RJ paper (Wieland 2017).
}

\seealso{
\code{\link{mci}}, \code{\link{mcimat.create}}
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

if (!require("MCI")) install.packages("MCI")
# Needed for function var.asdummy from package MCI 

# Integration of chains as dummy variables
chaindummies <- var.asdummy(HaslachStores$LM_Name)
HaslachStores <- cbind(HaslachStores, chaindummies)

mcimat_haslach2 <- mcimat.create (rawdata = HaslachSurvey_prepared,
origins.id = "Wohnort", destinations.id = "LM", "LME", 
tcmat = tcmat_haslach_airline, 
remOrig = c("SBXXX", "SB613"), remDest = "LM00", corObserved = 0.1,
origvar.data = HaslachDistricts, origvardata.id = "WO",
destvar.data = HaslachStores, destvardata.id = "LM")

mcimodel_haslach2 <- mci (mcimat = mcimat_haslach2, shares = "p_ij_obs_LME", 
"d_ij", "LM_VKF", "Aldi_DUMMY", "Edeka_DUMMY", "Netto_DUMMY", 
"Lidl_DUMMY", "Real_DUMMY")
# same analysis as above, but including chain dummies

mcisim_haslach2 <- mci.sim (mcimodel_haslach2, origins.pot = "WO_Einwohner", 
"d_ij", -3.1627045, "LM_VKF", 4.5001070,
"Aldi_DUMMY", 0.6012442, "Edeka_DUMMY", 0.4078891, "Netto_DUMMY", -0.4179182, 
"Lidl_DUMMY", 1.3272989, "Real_DUMMY", -2.8156859,
mcitrans = "ilc", interc = NULL)
# MCI simulation based on mcimodel_haslach2
# "d_ij", -3.1627045, "LM_VKF", 4.5001070, "Aldi_DUMMY", 0.6012442, ...
# = explanatory variables and their corresponding weighting parameters
# mcitrans = "ilc": inverse log-centering transformation
}