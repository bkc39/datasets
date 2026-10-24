\name{BrainCancer}
\alias{BrainCancer}
\docType{data}
\title{Brain Cancer Data}
%%   ~~ data name/kind ... ~~
\description{A 
data set consisting of survival times for patients diagnosed with brain cancer. 
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{BrainCancer}
\format{
  A data frame with 88 observations and 8 variables:
  \describe{
    \item{\code{sex}}{Factor with levels "Female" and "Male"}    
    \item{\code{diagnosis}}{Factor with levels "Meningioma",
      "LG glioma", "HG glioma", and "Other".}    
    \item{\code{loc}}{Location factor with levels "Infratentorial" and "Supratentorial".}    
    \item{\code{ki}}{Karnofsky index.}    
    \item{\code{gtv}}{Gross tumor volume, in cubic centimeters.}    
    \item{\code{stereo}}{Stereotactic method factor with levels "SRS"
      and "SRT".}    
    \item{\code{status}}{Whether the patient is still alive at the end of the study: 0=Yes, 1=No.}    
    \item{\code{time}}{Survival time, in months.}    
  }
}
\source{
I. Selingerova, H. Dolezelova, I. Horova, S. Katina, and J. Zelinka.
Survival of patients with primary brain tumors: Comparison of two
statistical approaches. PLoS One, 11(2):e0148733, 2016. \url{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4749663/}
}
\references{
  James, G., Witten, D., Hastie, T., and Tibshirani, R. (2021)
  \emph{An Introduction to Statistical Learning with applications in R,
    Second Edition},
  \url{https://www.statlearning.com},
  Springer-Verlag, New York
}
\keyword{datasets}
