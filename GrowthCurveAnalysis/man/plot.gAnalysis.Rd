% About the plot.gAnalysis function. Add arguments as they come up in other functions.

\name{plot.gAnalysis}
\alias{plot.gAnalysis}
\title{Plot gAnalysis Objects}

\description{Plots growth curves and analysis in a variety of formats such as single well views with parameters and metainformation, all wells, and svgs.}

\usage{plot(g.analysis, 1)}

\arguments{
  \item{g.analysis.all}{
  An object of class \code{gAnalysis} with the analysis of a set of growth curves to be visualized.
  }
  
  \item{well}{
  The well to display in a single well view (based on the number in the list of gAnalysis) or "all" if want a graph of all curves.
  }

  \item{name}{
  A name to save the svg of for an SVG plot.
  }
  
  \item{svg}{
  Logical, indicating whether to output to R or save an SVG plot.
  }
    
  \item{graph.log}{
  Logical, indicating whether to graph on a log axis, where T is for a log axis.
  }
   
  \item{control}{
  An object of class \code{"gControl"} providing options for \code{plot.gAnalysis}. 
  }
  
}


\details{
This function provides several plot styles for objects of class gAnalysis, most helpfully quick visual summary of a single curve and all serves in a set in an R window.

The plots need to be developed along with the the functions. Currently, graph.log doesn't work with SVG, SVG only works with "all" plots.
}


\author{Goodwin Gibbins}

\references{Draws on the Grofit Package, \url{http://www.jstatsoft.org/v33/i07/paper}}

\seealso{\code{\link{GCAF}}, \code{\link{gAnalysis}}, \code{\link{gSlice}}}

\examples{
#plot(g.analysis, well=12)

#plot(g.analysis, well="all", graph.log=T)

#plot(g.analysis, well="all", svg=T)
}