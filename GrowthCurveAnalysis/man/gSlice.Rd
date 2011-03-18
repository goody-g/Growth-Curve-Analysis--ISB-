% About the gSlice function. Add arguments as they come up in other functions.

\name{gSlice}
\alias{gSlice}
\title{View Slices of Parameter Space}

\description{Create a set of scatter plots to visualize distribution of parameters.}

\usage{gSlice(g.analysis, compare=NULL, name="slice", control=gControl())}

\arguments{
  \item{g.analysis}{
  An object of class \code{"gAnalysis"} containing the analysis of a set of wells to analyze.
  }

  \item{compare}{
  A list of two character vectors, one \code{compare$info} and one \code{compare$param} of attributes to list on the \code{gSlice} graph. If null, \code{control$compare} is used.
  }
  
  \item{svg}{
  A logical object stating whether to output to R or an svg.
  }
  
  
  \item{name}{
  The name of the saved slice file if an SVG output is made.
  }
  
  \item{control}{
   An object of class \code{"gControl"} providing options for \code{gSlice}. The colors for the \code{gSlice} graph are specified by \code{control$color.by}.
  }
 
}


\details{
This function provides a visualization of parameters for the validation of parameter estimation. Clumpings by color indicate more sucessful parameters - if estimated parameters fit graphs from similar environmental conditions, they are more likely to be accessing something meaningful in the biology. This visualization also alows the developer to notice outliers in the parameters and investigate them using the svg plot.
}



\author{Goodwin Gibbins}

\references{Draws on the Grofit Package, \url{http://www.jstatsoft.org/v33/i07/paper}}

\seealso{\code{\link{GCAF}}, \code{\link{gAnalysis}}, \code{\link{plot.gAnalysis}}}

\examples{
#Prepare g.analysis
matches <- list(Background="NRC-1", Date=20080415, Media="CM")
chosen.wells <- gChoose(matches)
g.analysis <- gAnalysis(chosen.wells)

#Visualze the Slice
gSlice(g.analysis)
}