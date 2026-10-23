\name{Bikeshare}
\alias{Bikeshare}
\docType{data}
\title{Bike sharing data
%%   ~~ data name/kind ... ~~
}
\description{This data set contains the hourly and daily count of rental bikes between years 2011 and 2012 in 
Capital bikeshare system, along with weather and seasonal information.


%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{Bikeshare}
\format{
  A data frame with 8645 observations on a number of variables.
  \describe{
    \item{\code{season}}{Season of the year, coded as Winter=1, Spring=2, Summer=3, Fall=4.}
\item{\code{mnth}}{Month of the year, coded as a factor.}    
\item{\code{day}}{Day of the year, from 1 to 365}     
\item{\code{hr}}{Hour of the day, coded as a factor from 0 to 23.}    
\item{\code{holiday}}{Is it a holiday? Yes=1, No=0.}
\item{\code{weekday}}{Day of the week, coded from 0 to 6, where Sunday=0, Monday=1, Tuesday=2, etc.}    
\item{\code{workingday}}{Is it a work day? Yes=1, No=0.}    
\item{\code{weathersit}}{Weather, coded as a factor.}    
\item{\code{temp}}{Normalized temperature in Celsius. The values are derived via (t-t_min)/(t_max-t_min), t_min=-8, t_max=+39.}    
\item{\code{atemp}}{Normalized feeling temperature in Celsius. The values are derived via (t-t_min)/(t_max-t_min), t_min=-16, t_max=+50.}    
\item{\code{hum}}{ Normalized humidity. The values are divided to 100 (max).}    
\item{\code{windspeed}}{Normalized wind speed. The values are divided by 67 (max).}    
\item{\code{casual}}{Number of casual bikers.}    
\item{\code{registered}}{Number of registered bikers.}    
\item{\code{bikers}}{Total number of bikers.}    
  }
}
\source{The UCI Machine Learning Repository \url{https://archive.ics.uci.edu/ml/datasets/bike+sharing+dataset}
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
lm(bikers~hr, data=Bikeshare)
}
\keyword{datasets}
