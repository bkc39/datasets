\name{Fund}
\alias{Fund}
\docType{data}
\title{Fund Manager Data
%%   ~~ data name/kind ... ~~
}
\description{A simulated data set containing the returns for 2,000 hedge fund managers.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{Fund}
\format{
A data frame containing the returns of 2,000 hedge fund managers over 50 months.
}
\source{Simulated data.
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
t.test(Fund$Manager1, mu=0)
}
\keyword{datasets}
