% About the gSplineFit function. Add arguments as they come up in other functions.

\name{gSplineFit}
\alias{gSplineFit}
\title{Estimate Parameters from a Spline Fitted Curve}

\description{A collection of estimating functions which provide parameters to gAnalysis.} 

\usage{gSplineFit(time, data, control=gControl(), for.bs=F)}

\arguments{

  \item{time}{
  The raw time data supplied by the results file.
  }
  
  \item{data}{
  The raw data supplied by the results file.
  }
  
  \item{control}{
  An object of class \code{"gControl"} providing options for \code{gSplineFit}.
  }

   \item{for.bs}{
	Logical, indicating whether the parameters are being estimated for bootstrapping or regular estimation, and so what specifications should be used for \code{smooth.spline}.
	}
  
}


\details{
This function, along with gInfo, provide the basis for the analysis carried out under gAnalysis. 

To add new parameters, functions should be written which take time, date etc data and return parameters. These functions can then be easily added to \code{gSplineFit} following the directsion in the documentation and source code.
}

\value{
\item{spline.fit}{An object containing estimated parameters and original data to be incorporated into \code{g.analysis}.} 
}


\author{Goodwin Gibbins}

\references{Draws on the Grofit Package, \url{http://www.jstatsoft.org/v33/i07/paper}}

\seealso{\code{\link{GCAF}}, \code{\link{gAnalysis}}, \code{\link{gInfo}}}

\examples{
#gSplineFit(time, data)
}