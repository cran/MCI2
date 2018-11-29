\name{mci}
\alias{mci}

\title{
Fitting the MCI model
}
\description{
This function fits the MCI model based on a given MCI interaction matrix.
}
\usage{
mci(mcimat, shares, ..., no.intercept = TRUE, 
mci.weighting = FALSE, mci.weights = NULL, 
show_proc = FALSE)
}

\arguments{
  \item{mcimat}{
Object (\code{list}) created by the function \code{mcimat.create} (Creating an MCI interaction matrix based on raw data)
}
  \item{shares}{
Column containing the local market shares
}
  \item{\dots}{
Explanatory variables (partial utilites)
}
  \item{no.intercept}{
Logical argument that indicates if an intercept is included into the model
}
  \item{mci.weighting}{
Logical argument that indicates if weighted least squares (WLS) should be used for fitting the model
}
  \item{mci.weights}{
If \code{mci.weighting = TRUE}: optional weighting vector for the WLS fitting. If \code{mci.weights = NULL}, the reciprocals of the residuals are used as weightings
}
  \item{show_proc}{
Logical argument that indicates if the function prints messages about the state of process during the work 
}
}
\details{
This function calculates a Multiplicative Competitive Interaction (MCI) Model based on a given interaction matrix.
}
\value{
A \code{mcimodel list} (invisible) containing the following components:
\item{regdata }{Log-centering transformed interaction matrix (\code{data frame})}
\item{mcimodel_coef }{A \code{matrix} containing the regression results (parameters, std. errors, t statistics,...)}
\item{mcimodel_stat }{A \code{matrix} containing the regression model diagnostics (R-squared, adj. R-squared, F statistic,...)}
}
\references{
Huff, D. L./Batsell, R. R. (1975): \dQuote{Conceptual and Operational Problems with Market Share Models of Consumer Spatial Behavior}. In: \emph{Advances in Consumer Research}, \bold{2}, p. 165-172.

Huff, D. L./McCallum, D. (2008): \dQuote{Calibrating the Huff Model Using ArcGIS Business Analyst}. \emph{ESRI White Paper}, September 2008. \url{https://www.esri.com/library/whitepapers/pdfs/calibrating-huff-model.pdf}

Nakanishi, M./Cooper, L. G. (1974): \dQuote{Parameter Estimation for a Multiplicative Competitive Interaction Model - Least Squares Approach}. In: \emph{Journal of Marketing Research}, \bold{11}, 3, p. 303-311.

Nakanishi, M./Cooper, L. G. (1982): \dQuote{Simplified Estimation Procedures for MCI Models}. In: \emph{Marketing Science}, \bold{1}, 3, p. 314-322. 

Wieland, T. (2017): \dQuote{Market Area Analysis for Retail and Service Locations with MCI}. In: \emph{The R Journal}, \bold{9}, 1, p. 298-323. \url{https://journal.r-project.org/archive/2017/RJ-2017-020/RJ-2017-020.pdf}.
}
\author{
Thomas Wieland
}
\note{
The function is a wrapper of \code{mci.transmat} and \code{mci.fit} of the \pkg{MCI} package. For further information see the \pkg{MCI} documentation and the corresponding RJ paper (Wieland 2017).
}

\seealso{
\code{\link{mcimat.create}}
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
cols.keep = HaslachSurvey$WO,
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