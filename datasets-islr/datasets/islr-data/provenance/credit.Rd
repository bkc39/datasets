\name{Credit}
\alias{Credit}
\docType{data}
\title{Credit Card Balance Data
%%   ~~ data name/kind ... ~~
}
\description{A simulated data set containing information on 400 customers.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{Credit}
\format{
  A data frame with 400 observations on a number of variables.
  \describe{
    \item{\code{Income}}{Income in $1,000's}    
    \item{\code{Limit}}{Credit limit}
    \item{\code{Rating}}{Credit rating}
    \item{\code{Cards}}{Number of credit cards}
    \item{\code{Age}}{Age in years}  
    \item{\code{Education}}{Education in years}
    \item{\code{Own}}{A factor with levels \code{No} and \code{Yes} indicating whether the individual owns a home} 
    \item{\code{Student}}{A factor with levels \code{No} and \code{Yes}
      indicating whether the individual is a student}
    \item{\code{Married}}{A factor with levels \code{No} and \code{Yes}
      indicating whether the individual is married}
    \item{\code{Region}}{A factor with levels \code{East}, \code{South}, and \code{West}
      indicating the individual's geographical location}
    \item{\code{Balance}}{Average credit card balance in $.}
  }
}
\source{Simulated data. Many thanks to Albert Kim for helpful suggestions, and for 
 supplying a draft of the man documentation page on
  Oct 19, 2017.
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
  James, G., Witten, D., Hastie, T., and Tibshirani, R. (2021)
  \emph{An Introduction to Statistical Learning with applications in R,
    Second Edition},
  \url{https://www.statlearning.com},
  Springer-Verlag, New York
}
\examples{
summary(Credit)
lm(Balance ~ Student + Limit, data=Credit)
}
\keyword{datasets}
