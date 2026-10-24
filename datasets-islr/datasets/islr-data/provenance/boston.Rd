\name{Boston}
\alias{Boston}
\docType{data}
\title{Boston Data}
\description{A data set containing housing values in 506 suburbs of Boston.}
\usage{Boston}
\format{
  A data frame with 506 rows and 13 variables.
  \describe{
    \item{\code{crim}}{per capita crime rate by town.}    
    \item{\code{zn}}{  proportion of residential land zoned for lots over 25,000 sq.ft. }
    \item{\code{indus}}{proportion of non-retail business acres per town.  }
\item{\code{chas}}{Charles River dummy variable (= 1 if tract bounds river; 0 otherwise). }
\item{\code{nox}}{nitrogen oxides concentration (parts per 10 million).}
\item{\code{rm}}{average number of rooms per dwelling.}
\item{\code{age}}{proportion of owner-occupied units built prior to 1940.}
\item{\code{dis}}{weighted mean of distances to five Boston employment centres.}
\item{\code{rad}}{index of accessibility to radial highways.}
\item{\code{tax}}{full-value property-tax rate per $10,000.}
\item{\code{ptratio}}{pupil-teacher ratio by town.}
\item{\code{lstat}}{lower status of the population (percent).}
\item{\code{medv}}{median value of owner-occupied homes in $1000s.}
  }
}
\source{This dataset was obtained from, and is slightly modified from, the Boston dataset that is part of the MASS library. 
References are available in the MASS library. }
\references{
  James, G., Witten, D., Hastie, T., and Tibshirani, R. (2013)
  \emph{An Introduction to Statistical Learning with applications in R},
  \url{https://www.statlearning.com},
  Springer-Verlag, New York
}
\examples{
lm(medv ~ crim + rm, data=Boston)
}
\keyword{datasets}
