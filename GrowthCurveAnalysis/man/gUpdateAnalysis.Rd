% About the gUpdateAnalysis function. 

\name{gUpdateAnalysis}
\alias{gUpdateAnalysis}
\title{Updates Saved gAnalysis Objects}

\description{Allows for a database-wide update of saved gAnalysis objects which should be used for quicker analysis of sets of curves.} 

\usage{gUpdateAnalysis(replace.old=FALSE, replace.id="Test", control=gControl())}

\arguments{
  \item{replace.old}{
  Logical indicating whether to save old gAnalysis file in an Archive folder or write over.
  }

  \item{replace.id}{
	A name to append to Analysis for archived \code{gAnalysis} class objects, preferably date in YYYYMMDD form.
  }
  
  \item{control}{
	An object of class \code{"gControl"} providing options for \code{gUpdateAnalysis}.
  }
}


\details{
This function has not been integrated into the GCAF pathway yet - ultimately, the saved \code{gAnalysis} objects would be searched and used for analysis such as \code{gSlice} rather than performing \code{gAnalysis} again.
}


\author{Goodwin Gibbins}

\references{Draws on the Grofit Package, \url{http://www.jstatsoft.org/v33/i07/paper}}

\seealso{\code{\link{GCAF}}, \code{\link{gAnalysis}}.}

\examples{
#gUpdateAnalysis() #This takes A LONG TIME to run, as it performs gAnalysis on every curve in the database.
}