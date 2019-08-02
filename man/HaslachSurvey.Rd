\name{HaslachSurvey}
\alias{HaslachSurvey}
\docType{data}
\title{
Freiburg-Haslach: customer survey
}
\description{
Part of a point of sale survey (n=235) in Freiburg-Haslach with respect to grocery shopping behavior.
}
\usage{data("HaslachSurvey")}
\format{
  A data frame with 470 observations on the following 24 variables.
  \describe{
    \item{\code{DATUM}}{a numeric vector containing the datum code}
    \item{\code{UHRZEIT}}{a numeric vector containing the time code}
    \item{\code{BEFRSTANDORT2}}{a numeric vector containing the sample point code}
    \item{\code{LMHAEUF}}{a numeric vector containing the weekly frequency of grocery shopping}
    \item{\code{LM1}}{a numeric vector containing the store code of the last grocery shopping trip}
    \item{\code{LM1_Text}}{a character vector containing the store code (character) of the last grocery shopping trip, corresponding to variable \code{LM1} in the \code{HaslachStores} dataset}
    \item{\code{LM1A}}{a character vector containing other shopping destinations on grocery shopping trips (not coded)}
    \item{\code{LM1E}}{a numeric vector containing the amount of purchases corresponding to the last grocery shopping trip}
    \item{\code{LM2}}{a numeric vector containing the store code of the second to the last grocery shopping trip}
    \item{\code{LM2_Text}}{a character vector containing the store code (character) of the second to the the last grocery shopping trip, corresponding to variable \code{LM2} in the \code{HaslachStores} dataset}
    \item{\code{LM2A}}{a character vector containing other shopping destinations on grocery shopping trips (not coded)}
    \item{\code{LM2E}}{a numeric vector containing the amount of purchases corresponding to the second to the last grocery shopping trip}
    \item{\code{ZUFR_LM}}{a numeric vector containing customer satisfaction scores (1=best, ... 6=worst) with respect to local grocery stores supply}
    \item{\code{ZUFR_LME}}{a numeric vector containing customer satisfaction scores (1=best, ... 6=worst) with respect to local grocery stores accessibility}
    \item{\code{ZUFR_APO}}{a numeric vector containing customer satisfaction scores (1=best, ... 6=worst) with respect to local pharmacies supply}
    \item{\code{ZUFR_EHSO}}{a numeric vector containing customer satisfaction scores (1=best, ... 6=worst) with respect to other types of local retailing}
    \item{\code{ZUFR_BANK}}{a numeric vector containing customer satisfaction scores (1=best, ... 6=worst) with respect to local bank supply}
    \item{\code{WOHNSTANDORT}}{a numeric vector containing the district code}
    \item{\code{WO}}{a character vector containing the district code, corresponding to variable \code{WO} in the \code{HaslachDistricts} dataset}
    \item{\code{ALTERKAT}}{a numeric vector containing the code of age categoory}
    \item{\code{GESCHL}}{a numeric vector containing the gender code}
    \item{\code{BERUF}}{a numeric vector containing the code of the respondent's working status}
    \item{\code{HHPERS}}{a numeric vector containing the household size}
    \item{\code{HHKIND}}{a numeric vector containing the no. of children in the household}
  }
}
\source{
Own survey (June 2018). Own postprocessing.

Wieland, T. (2018): \dQuote{Competitive locations of grocery stores in the local supply context - The case of the urban district Freiburg-Haslach}. In: \emph{European Journal of Geography}, \bold{9}, 3, p. 98-115.
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

\keyword{datasets}