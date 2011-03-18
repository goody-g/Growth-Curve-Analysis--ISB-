% About the gAnalysis function. Add arguments as they come up in other functions. TODO: Links aren't working.

\name{gAnalysis}
\alias{gAnalysis}
\title{Analyze a Set of Growth Curves.}

\description{Analyze (find the parameters and information for) a set of growth curves chosen by \code{\link{gChoose}}.} 

\usage{gAnalysis(chosen.wells, control = my.control)}

\arguments{
  \item{chosen.wells}{
  The location of the database folder. Updated by user upon initial download of GCAF functions.
  }

  \item{export.folder}{
  The location of a folder for exported \code{gAnalysis} spreadsheets from \code{\link{gExport}}.
  }
  
  \item{control}{
  An object of class \code{gControl} cotaining control options for gAnalysis and connected functions. See \code{\link{gControl}}.
  }
  
}


\details{
This function organizes parameter (\code{gSplineFit}) and information gathering (\code{gInfo}) for the GCAF package. Analyis of a set of experiments chosen by \code{gChoose} can then be supplied to \code{\link{gSummary}}, \code{\link{gExport}}, \code{\link{plot.gAnalyis}} or \code{\link{gSlice}} for interpretation and further analysis.
}

\value{
\item{g.analysis.all}{A list object containing analysis of each well supplied to the \code{gAnalysis} function.} 
}


\author{Goodwin Gibbins}

\references{Draws on concepts from the Grofit Package, \url{http://www.jstatsoft.org/v33/i07/paper}}

\seealso{\code{\link{GCAF}}, \code{\link{gControl}}, \code{\link{gSplineFit}}, \code{\link{gInfo}}, \code{\link{gSummary}}, \code{\link{gExport}}, \code{\link{plot.gAnalysis}}, \code{\link{gSlice}}}

\examples{
# Choose appropriate well
matches <- list(Background="NRC-1", Date=20080415, Media="CM")
chosen.wells <- gChoose(matches)

#Analyze Wells - this can take a while, depending on the complexity of gSplineFit and the number of wells. (ex, 1sec per well)

nrc1.20080415 <- gAnalysis(chosen.wells)

#Check that the right wells were chosen, etc
gSummary(nrc1.20080415)

#View plots of all wells or single wells
plot(nrc1.20080415, "all")
plot(nrc1.20080415, 1)

#Export using gExport to continue analysis outside of GCAF
}