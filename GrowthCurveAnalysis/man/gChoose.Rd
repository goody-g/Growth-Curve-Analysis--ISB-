% About the gChoosefunction. 

\name{gChoose}
\alias{gChoose(time, data, control=gControl())}
\title{Choose Matching Growth Curves by Metainformation}

\description{Finds the \code{Date, Initials, Well.Number} coordinates for every growth curve with metainformation matching the user input.}

\usage{gChoose(matches, not.matches, control=gControl())}

\arguments{
  \item{matches}{
  A named list with vectors of matches for each attribute, named per the database columns. Vectors with multiple elements are treated as "or", as in "gc with temperature 37 or not designated".
  }

  \item{not.matches}{
  A named list with vectors describing growth curves to exclude, named per the database columns. Vectors with multiple elements are treated as "nor" as in "growth curves from neither 20080422 nor 20080415".
  }
    
  \item{control}{
  An object of class \code{"gControl"} providing options for \code{gChoose}.
  }

  }


\details{
This function is the first step in the GCAF pathway. It finds wells matching user-defined information parameters to be analyzed using gAnalysis. 

TODO: The program has some bugs which need to be worked out - it runs into problems with certain matches or not-matches options (see program file). 
}

\value{
\item{chosen.wells}{An object containing Initials, Date and Well Number identification for the chosen set of wells to be analyzed by gAnalysis.} 
}


\author{Goodwin Gibbins}

\references{Draws on the Grofit Package, \url{http://www.jstatsoft.org/v33/i07/paper}}

\seealso{\code{\link{GCAF}}, \code{\link{gAnalysis}}}

\examples{
#Choose all ura3 wells in CM without any knockouts.
gChoose(matches = list(Media="CM", Background="ura3", Knockout=NA))
}