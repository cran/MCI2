\name{rawdata.prep}
\alias{rawdata.prep}

\title{
Preparation of survey raw data
}

\description{
Preparation of survey raw data for further use in a MCI interaction matrix.
}

\usage{
rawdata.prep(cols.below1, cols.below2, cols.keep, colnames.new)
}

\arguments{
  \item{cols.below1}{
First list of columns in a survey data frame to arrange one below the other (e.g. destination IDs)
}
  \item{cols.below2}{
Second list of columns in a survey data frame to arrange one below the other (e.g. obtained destination purchases)
}
  \item{cols.keep}{
List of columns in a survey data frame that must be kept for all data (e.g. IDs of customer origins)
}
  \item{colnames.new}{
Character vector of new column names
}

}
\details{
This function re-orders survey raw data for further use in the function \code{mcimat.create}. Sometimes, in a household survey, two or three destinations/stores are obtained instead of one, always arranged in different columns. This function re-orders survey raw data in single columns for further use in the function \code{mcimat.create}.
}

\value{
A \code{data frame} containing three columns named corresponding to \code{colnames.new}
}

\references{
Huff, D. L./Batsell, R. R. (1975): \dQuote{Conceptual and Operational Problems with Market Share Models of Consumer Spatial Behavior}. In: \emph{Advances in Consumer Research}, \bold{2}, p. 165-172.

Wieland, T. (2017): \dQuote{Market Area Analysis for Retail and Service Locations with MCI}. In: \emph{The R Journal}, \bold{9}, 1, p. 298-323. \url{https://journal.r-project.org/archive/2017/RJ-2017-020/RJ-2017-020.pdf}.

Wieland, T. (2018): \dQuote{Competitive locations of grocery stores in the local supply context - The case of the urban district Freiburg-Haslach}. In: \emph{European Journal of Geography}, \bold{9}, 3, p. 98-115.
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{mcimat.create}}
}

\examples{
data(HaslachSurvey)
# survey raw data (Store choices and purchases)

# Preparing raw data (HaslachSurvey)
HaslachSurvey_prepared <- rawdata.prep (cols.below1 = 
list(HaslachSurvey$LM1_Text, HaslachSurvey$LM2_Text),
cols.below2 = list(HaslachSurvey$LM1E, HaslachSurvey$LM2E),
cols.keep = list(HaslachSurvey$WO),
colnames.new = c("LM", "LME", "Wohnort"))
# "WO" and "Wohnort" = origin ID
# "LM1_Text", "LM2_Text" and "LM" = destination IDs (grocery stores)
# "LM1E", "LM2E" and "LME" = grocery store purchases

HaslachSurvey_prepared
}