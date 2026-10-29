\name{NYSE}
\alias{NYSE}
\docType{data}
\title{New York Stock Exchange Data
%%   ~~ data name/kind ... ~~
}
\description{Data consisting of the Dow Jones returns, log trading volume, and log volatility
for the New York Stock Exchange over a 20 year period}
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
\usage{Portfolio}
\format{
  A data frame with 6,051 observations and 6 variables:
  \describe{
    \item{\code{date}}{Date}
    \item{\code{day_of_week}}{Day of the week}
    \item{\code{DJ_return}}{Return for Dow Jones Industrial Average}
    \item{\code{log_volume}}{Log of trading volume}
    \item{\code{log_volatility}}{Log of volatility}
    \item{\code{train}}{For the first 4,281 observations, this is set to TRUE}
  }
}
\source{B. LeBaron and A. Weigend (1998), IEEE Transactions on Neural Networks 9(1): 213-220.
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
attach(NYSE)
plot(log_volatility)
}
\keyword{datasets}
