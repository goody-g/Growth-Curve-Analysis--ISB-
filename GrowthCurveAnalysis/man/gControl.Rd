% About the gControl function. Add arguments as they come up in other functions.

\name{gControl}
\alias{gControl}
\title{Define available GCAF options}

\description{Returns an object of class \code{gControl} with options for \pkg{GCAF} functions, either providing defaults or user-defined options.}

\usage{gControl(color.by="Date", spar=.4)}

\arguments{
  \item{root.folder}{
  The location of the database folder. Updated by user upon initial download of GCAF functions.
  }

  \item{export.folder}{
  The location of a folder for exported \code{gAnalysis} spreadsheets from \link{gExport}.
  }
  
  \item{spar}{
  The smoothing parameter to use with smooth.spline in \link{gSplineFit}
  }
  
  \item{cv.spline}{
  Logical, indicates whether to use ordinary (\code{TRUE}) or the faster 'generalized' cross-validation (\code{FALSE}) in \code{smooth.spline()}.
  }
  
  \item{cv.bs}{
  Logical, indicates whether to use ordinary (\code{TRUE}) or the faster 'generalized' cross-validation (\code{FALSE}) in \code{smooth.spline()} for bootstrapping.
  }

  
  \item{num.bs.samp}{
  Number of bootstrapping samples to run as part of analysis.
  }
  
  
  \item{color.by}{
  The meta-information to color a well according to, for example, Background, Date etc. in \link{gSlice}.
  }
  
  \item{num.bs.samp}{
  Number of bootstrapping samples to run as part of analysis.
  }
  
  \item{export.folder}{
  The location of a folder for exported \code{gAnalysis} spreadsheets from \link{gExport}.
  } 
  
   \item{no.display}{
  Information parameters to supress in gSummary.
  } 
  
  
   \item{export.folder}{
  Information and parameters to supress on plots.
  } 
  
}


\details{
This function can be either edited to easily change defaults or passed options for changing options in single runs. Any new options which are written into the package should be added to the this function to simplify maitenence.

The function also makes a list of all the experiments based on the appropriately-named folders in the database folder

When adding new control options, be sure to add to list of outputs!

}

\value{
\item{g.control}{An object containing all the options listed in the arguments and also a list of date and initials for every experiment in the database as defined by \code{}.} 
}


\author{Goodwin Gibbins}

\references{Draws on the Grofit Package, \url{http://www.jstatsoft.org/v33/i07/paper}}

\seealso{\link{GCAF}, \link{gAnalysis}}

\examples{
# Contains default settings
def.control <- gControl()

#Or can be updated by user for a specific run.
my.control <- gControl(num.bs.samp=30, color.by="Media", spar=.3)
}