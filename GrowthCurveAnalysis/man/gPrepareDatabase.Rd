% About the gPrepareDatabase function. Add arguments as they come up in other functions.

\name{gPrepareDatabase}
\alias{gPrepareDatabase}
\title{Prepares a Pre-Database File}

\description{Prepares a pre-database file with well number, label, date and intials information to be completed by user. See manual.} 

\usage{gPrepareDatabase("FYL", 20080422)}

\arguments{
  \item{initials}{
  The initials of the experimenter, which must match with the initials on the layout file and experiment folder and be consistent with past experiments.
  }

  \item{date}{
  The date chosen to represent the experiment, which must match with the date on the layout file and experiment folder.
  }
  
  \item{control}{
  An object of class \code{"gControl"} which provides options for \code{gPrepareDatabase} such as the root folder of the database.
  }
}


\details{
This function simplifies adding a database entry to the growth curve database associated with the GCAF package.

The layout file and results file (for formatting see documentation) are saved in the \code{control$root.folder} location at \code{Database/Layouts/LayoutII########.txt} and \code{Database/########II/ResultsII########.txt} where \code{II} are the initials and \code{#} is the date.

Running \code{gPrepareDatabase} will then create a barebones database file in the folder with the results file which should be edited using Excel or similar.

The gPrepareDatabase will not write over existing database files, so delete the original manually if you wish to replace.
}

\value{
\item{Database}{A basic database file is saved in the appropriate folder for further editing with a spreadsheet editor (Excel).} 
}


\author{Goodwin Gibbins}

\references{Draws on the Grofit Package, \url{http://www.jstatsoft.org/v33/i07/paper}}

\seealso{\code{\link{GCAF}}, \code{\link{gAnalysis}}}

\examples{
# After having created the layout file and experiment folder in the appropriate locations (see documentation):
gPrepareDatabase(initials="ST", date=20090511)

#Then open the database file and complete information using Excel or a similar spreadsheet editor.
}