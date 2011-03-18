% About the gSummary function. 

\name{gSummary}
\alias{gSummary}
\title{Summarize Results}

\description{Provides a rapid summary of the metainformation in a set of wells analyzed by \code{gAnalysis} to provide a check for gChoose etc.}

\usage{gSummary(g.analysis)}

\arguments{
  \item{g.analysis}{
  An object of class \code{"gAnalysis"} which contains metainformation and derived parameters for a set of growth curves chosen by \code{gChoose}.
  }

  \item{control}{
  An object of class \code{"gControl"} providing options for \code{gSummary}, specifically \code{control$no.display} to supress metainformation categories.
  }
}


\details{
This function should be used as a simple user-check for for the contents of an object returned from \code{gAnalysis}, for example to check whether an environmental perterbation was included inadvertantly.
}

\value{
\item{to.display}{A list of metainformation categories and attributes with their number of occurences. NAs are supressed.} 
}


\author{Goodwin Gibbins}

\references{Draws on the Grofit Package, \url{http://www.jstatsoft.org/v33/i07/paper}}

\seealso{\link{GCAF}, \link{gAnalysis}}

\examples{
#gSummary(g.analysis)
}