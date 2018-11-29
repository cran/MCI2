\name{MCI2-package}
\alias{MCI2-package}
\alias{MCI2}
\docType{package}
\title{
Market Area Models for Retail and Service Locations
}
\description{
This package provides several function for market area modeling with respect to retail and service locations (for a review, see Wieland 2017). 
The package \pkg{MCI2} has two purposes:

1) A user-friendly wrapper for the functions in the package \pkg{MCI} (Wieland 2017) concerning the Huff Model and the Multiplicative Competitive Interaction Model. For more information, see the \pkg{MCI} package documentation and the related RJ paper (Wieland 2017).

2) A user-friendly way to create transport costs matrices, including the usage of OSM (OpenStreetMap)-related APIs. The included function \code{tcmat.create} is a convenient wrapper of the functions \code{geocode_OSM} (for geocoding street addresses using OSM Nominatim) from the \pkg{tmaptools} package (Tennekes 2018), \code{osrmTable} (for creating travel time matrices between points while interfacing the table OSRM service) from the \pkg{osrm} package (Giraud 2018) and \code{dist.mat} (for creating airline distances between points) from the \pkg{REAT} package (Wieland 2018). For more information, see the related package documentations.
}

\author{
Thomas Wieland

Maintainer: Thomas Wieland \email{thomas.wieland.geo@googlemail.com}
}

\references{
Giraud, T. (2018): osrm: Interface Between R and the OpenStreetMap-Based Routing Service OSRM. R package version 3.1.1. \url{https://CRAN.R-project.org/package=osrm}.
  
Tennekes, M. (2018): tmaptools: Thematic Map Tools. R package version 2.0-1. \url{https://CRAN.R-project.org/package=tmaptools}.

Wieland, T. (2017): \dQuote{Market Area Analysis for Retail and Service Locations with MCI}. In: \emph{The R Journal}, \bold{9}, 1, p. 298-323. \url{https://journal.r-project.org/archive/2017/RJ-2017-020/RJ-2017-020.pdf}.

Wieland, T. (2018): REAT: Regional Economic Analysis Toolbox. R package version 2.1.0. \url{https://CRAN.R-project.org/package=REAT}.
}