\name{model.export}
\alias{model.export}

\title{
Output Huff Model results
}
\description{
Output Huff Model results as .csv file(s)
}
\usage{
model.export(huffmodel, mat.filename, total.filename, 
decimal = ".", colsep = ";", mat.ascrosstab = TRUE)
}

\arguments{
  \item{huffmodel}{
Object (\code{list}) created by the function \code{huff} (Huff Model)
}
  \item{mat.filename}{
File name for the interaction matrix output (no suffix)
}
  \item{total.filename}{
File name for the total market areas output (no suffix)
}
  \item{decimal}{
the string to use for decimal points (as in \code{write.table})
}
  \item{colsep}{
the field separator string (as in \code{write.table})
}
  \item{mat.ascrosstab}{
Logical argument that indicates if the interaction matrix should be stored in the form of a crosstab (for further use in GIS)
}
}

\value{
Two .csv files (interaction matrix, total market areas)
}

\author{
Thomas Wieland
}
\note{
The function is a wrapper of \code{ijmatrix.crosstab} of the \pkg{MCI} package and \code{write.table} from \pkg{utils}. For further information see the package documentations and the corresponding RJ paper (Wieland 2017).
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