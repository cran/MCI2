\name{to.tcmat}
\alias{to.tcmat}

\title{
Creation of a transport costs matrix
}

\description{
Creation of a \code{tcmat list} object from a given transport costs table 
}

\usage{
to.tcmat(dataset, colname.from, colname.to, colname.tc)
}

\arguments{
  \item{dataset}{
A \code{data frame} containing the transport costs table
}
  \item{colname.from}{
Name of the column containing the origin IDs
}
  \item{colname.to}{
Name of the column containing the destination IDs
}
  \item{colname.tc}{
Name of the column containing the transport costs (e.g. airline distances, travel times)
}
}

\details{
Creation of a \code{tcmat list} object (Transport costs matrix) from a given table for further use in \code{huff} and \code{mcimat.create}. 
}

\value{
A \code{tcmat list} (invisible) containing the following components:
\item{coords_origins }{A \code{data.frame}}
\item{coords_destinations }{A \code{data.frame}}
\item{tcmat }{A \code{data.frame} containing the transport costs matrix}
\item{tc.mode }{A \code{list} containing information about the transportation costs matrix}
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{huff}}, \code{\link{mcimat.create}}, \code{\link{tcmat.create}}
}

\examples{
data(Haslach_tcmatDrvtime)
# Travel times from the Haslach example
Haslach_tcmat <- to.tcmat (Haslach_tcmatDrvtime, "from", "to", "tc")
# Creating new tcmat object
Haslach_tcmat
}