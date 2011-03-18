%Copied from Grofit package.

\name{low.integrate}
\Rdversion{1.1}
\alias{low.integrate}
\title{
Function to estimate the area under a curve given as respecting x and y(x) values
}
\description{
Approximates the integral of function by calculating the area under a smoothed spline fitted to the given \code{x} and \code{y(x)} values.
}
\usage{
low.integrate(x, y)
}
\arguments{
  \item{x}{
Numeric vector.
}
  \item{y}{
Numeric vector, corresponding to \code{x}.
}
}
\details{
The function uses the the R internal function \code{\link{smooth.spline}}.

NOTE: This function was originally part of the Grofit package and has been copied for use in new package. This documentation is also from the Grofit package. In GCAF it is used in \code{gSplineFit}.
}
\value{
Area under the smoothed spline.
}

\references{Copied from the Grofit Package, \url{http://www.jstatsoft.org/v33/i07/paper}}

\seealso{
\code{\link{smooth.spline}}, \code{\link{gSplineFit}}
}