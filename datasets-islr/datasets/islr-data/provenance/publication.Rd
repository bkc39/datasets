\name{Publication}
\alias{Publication}
\docType{data}
\title{Time-to-Publication Data}
\description{Publication times for 244 clinical trials funded by the National Heart, Lung, and Blood Institute. }
\usage{Publication}
\format{
  A data frame with 244  observations, each representing a clinical trial, and 9 variables:
  \describe{
    \item{\code{posres}}{Did the trial produce a positive (significant) result? 1=Yes, 0=No.}    
    \item{\code{multi}}{Did the trial involve multiple centers? 1=Yes, 0=No.}    
    \item{\code{clinend}}{Did the trial focus on a clinical endpoint? 1=Yes, 0=No.}    
    \item{\code{mech}}{Funding mechanism within National Institute of Health: a qualitative variable.}    
    \item{\code{sampsize}}{Sample size for the trial.}    
    \item{\code{budget}}{Budget of the trial, in millions of dollars.}    
    \item{\code{impact}}{Impact of the trial; this is related to the number of publications.}    
\item{\code{time}}{Time to publication, in months.}
\item{\code{status}}{Whether or not the trial was published at \code{time}: 1=Published, 0=Not yet published.}
  }
}
\source{
Gordon, Taddei-Peters, Mascette, Antman, Kaufmann, and Lauer.
Publication of trials funded by the National Heart, Lung, and Blood Institute. 
New England Journal of Medicine, 369(20):1926-1934, 2013.
}
\references{
  James, G., Witten, D., Hastie, T., and Tibshirani, R. (2021)
  \emph{An Introduction to Statistical Learning with applications in R,
    Second Edition},
  \url{https://www.statlearning.com},
  Springer-Verlag, New York
}
\keyword{datasets}
