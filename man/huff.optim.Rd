\name{huff.optim}
\alias{huff.optim}

\title{
Huff Model optimization
}

\description{
This function fits the Huff Model using a local optimization of attraction algorithm
}

\usage{
huff.optim(huffmodel, location.dataset, location.id, location.total,
tolerance = 5, iterations = 3, show_proc = TRUE)
}

\arguments{
  \item{huffmodel}{
Object (\code{huffmodel list}) object created by the function \code{huff} (Huff Model)
}
  \item{location.dataset}{
A \code{data frame} containing the destinations and the corresponding observed total market areas, \eqn{T_{j,obs}} (e.g. annual turnover)
}
  \item{location.id}{
Name of the column in \code{location.dataset} containing the destination IDs
}
  \item{location.total}{
Name of the column in \code{location.dataset} containing the destinations' observed total market areas, \eqn{T_{j,obs}} (e.g. annual turnover)
}
  \item{tolerance}{
Accepted value of absolute percentage error between observed (\eqn{T_{j,obs}}) and estimated total values (\eqn{T_{j,exp}}) to skip a local optimization of location \eqn{j}
}
  \item{iterations}{
a single numeric value for the desired number of iterations
}
  \item{show_proc}{
logical argument that indicates if the function prints messages about the state of process during the work
}
}

\details{
In many cases, only total empirical market areas (e.g. annual turnover) of the destinations/locations can be used for market area estimation. This function fits the \emph{Huff Model} not by estimating the parameters but by optimizing the attraction variable (transport cost weighting by \eqn{\lambda} is given) using an optimization algorithm introduced and explained in Wieland (2017a) and Wieland (2017b).
}

\value{
A \code{huffmodel list} (invisible) containing the following components:
\item{huffmat }{Huff interaction matrix (\code{data frame}), also containing the local market shares (\code{p_ij})}
\item{hufftotal }{Total location market areas (\code{data frame}), also containing both the empirical and the estimated total market areas (\code{T_j} and \code{T_j_obs}, respectively) as well as their difference (\code{T_j_diff}) and the new attraction values (\code{A_opt})}
\item{diag }{A \code{data frame} containing several model diagnoses for each iteration}
\item{params }{A \code{matrix} containing the user-defined weighting functions and the corresponding weighting parameters}
\item{coords }{A \code{list} containing the coordinates and additional information of the origins (\code{coords_origins}) and the destinations (\code{coords_destinations}), inherited from the \code{tcmat} object}
\item{tc.mode }{A \code{list} containing information about the transportation costs matrix, inherited from the \code{tcmat} object}
}

\references{
Wieland, T. (2017a): \dQuote{Market Area Analysis for Retail and Service Locations with MCI}. In: \emph{The R Journal}, \bold{9}, 1, p. 298-323. \url{https://journal.r-project.org/archive/2017/RJ-2017-020/RJ-2017-020.pdf}.

Wieland, T. (2017b): \dQuote{Raum- und standortoekonomische Optimierungsmodelle in Open-Source-Umgebungen - Implementation und Anwendungsmoeglichkeiten im Kontext der Einzelhandels- und Versorgungsforschung}. In: Schrenk, M./Popovich, V./Zeile, P./Elisei, P./Beyer, C. (eds.): REAL CORP 2017. PANTA RHEI - A World in Constant Motion. Proceedings of 22nd International Conference on Urban Planning, Regional Development and Information Society. p. 463-473. \url{https://programm.corp.at/cdrom2017/papers2017/CORP2017_98.pdf}. 
}

\author{
Thomas Wieland
}

\note{
The function is a wrapper of \code{huff.attrac} from the \pkg{MCI} package. For further information see the \pkg{MCI} documentation and the corresponding RJ paper (Wieland 2017).
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

huffmod_total <- huffmodel1$hufftotal
# Total values from the Huff Model

huff.optim(huffmodel1, location.dataset = huffmod_total, 
location.id = "j_dest", location.total = "T_j")
# Using calculated total market areas for optimization

# now adding random errors:
rnderr <- runif(8, min=-1000, max=1500)
huffmod_total2 <- huffmod_total
huffmod_total2$T_j <- huffmod_total2$T_j+rnderr 

# New optimization:
# now saving as huffmodel list object
huffmodel_opt <- huff.optim(huffmodel1, location.dataset = huffmod_total2, 
location.id = "j_dest", location.total = "T_j")

huffmodel_opt
# complete huffmodel list object

huffmodel_opt$huffmat
# local market shares
}