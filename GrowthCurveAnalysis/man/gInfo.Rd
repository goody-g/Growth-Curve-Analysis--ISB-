% About the gInfo function. Add arguments as they come up in other functions.

\name{gInfo}
\alias{gInfo}
\title{Gather Metainformation for a Curve}

\description{As part of the \code{gAnalysis} function, adds metainformation from the database file associated with a chosen curve.}

\usage{gInfo(date=20080422, initials="FYL", well.number=156)}

\arguments{
  \item{date}{
  The date of the experiment for the curve to find information for. One part of the three part specification of a curve.
  }

  \item{initials}{
  The intials of the experimenter - another part of the three part specification of a curve.
  }
  
   \item{well.number}{
  The well number of a curve - the final part of the three part specification of a curve.
  }
  
   \item{control}{
  An object of class \code{"gControl"} providing options for \code{gInfo}.
  }
}


\details{
This function acts exclusively as part of the \code{gAnalysis} pathway, gathering metainformation to include with the parameters in the analysis.
}

\value{
\item{g.info}{A list of class \code{gInfo} with metainformation categories and attributes} 
}


\author{Goodwin Gibbins}

\references{Draws on the Grofit Package, \url{http://www.jstatsoft.org/v33/i07/paper}}

\seealso{\code{\link{gAnalysis}}}

\examples{
gInfo(date=20080422, initials="FYL", well.number=156)
}