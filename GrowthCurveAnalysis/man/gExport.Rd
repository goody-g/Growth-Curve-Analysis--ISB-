% About the gExport function.

\name{gExport}
\alias{gExport}
\title{Export gAnalysis Objects}

\description{Exports analyzed growth curve data for further analysis outside of GCAF.} 

\usage{gExport(g.analysis, name="CMura3")}

\arguments{
  \item{g.analysis}{
  An object or list of objects of class gAnalysis to export.
  }

  \item{name}{
   A character vector consisting of a name for the exported analysis. Default is "LastExport".
	}
	
	\item{control}{
	An object of class \code{"gControl"} providing options for \code{gExport}, specifically the designated export folder.
	}
  
}

\details{
This function provides a connection between the GCAF functions and other external analysis techniques. Exporting in a versitile the derived parameters for chosen wells is an important ability of the GCAF functions.
}


\value{
\item{LastExport.txt}{An exported spreadsheet containing metainformation and parameters for each curve included in the \code{g.analysis} input.} 
}


\author{Goodwin Gibbins}

\references{Draws on the Grofit Package, \url{http://www.jstatsoft.org/v33/i07/paper}}

\seealso{\code{\link{GCAF}}, \code{\link{gAnalysis}}}

\examples{
#Prepare gAnalysis 
matchesCDM <- list(Media="CDM", Overexpression=NA, Background="NRC-1")
chosen.wells.CDM <- gChoose(matchesCDM)
#g.analysis.cdm <- gAnalysis(chosen.wells.CDM) # takes a while to run.

matchesCM <- list(Media="CM", Overexpression=NA, Background="NRC-1")
chosen.wells.CM <- gChoose(matchesCM)
#g.analysis.cm <- gAnalysis(chosen.wells.CM)  # takes a while to run.

# Export both the analyzed sets
#gExport(list(g.analysis.cdm, g.analysis.cm))

}