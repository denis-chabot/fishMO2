% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotR2.R
\name{plotR2}
\alias{plotR2}
\title{Make 3 plots involving the $"R"^2$ for each MO2 measurement for a single
individual fish or other water-breathing animal.}
\usage{
plotR2(
  r2,
  time,
  mo2,
  r2lab = expression(r^2),
  Timelab = "Time",
  mo2lab = expression(M[O[2]]),
  minR2 = NULL,
  cex = 0.7,
  ...
)
}
\arguments{
\item{r2}{Vector or data frame column holding the R2 for each measurement
of oxygen uptake.}

\item{time}{Vector or data frame column holding the time, in POSIXct or
numeric (hours) for each measurement of oxygen uptake.}

\item{mo2}{Vector or data frame column holding the oxygen uptake
measurements.}

\item{r2lab}{Character string to label a plot axis involving R2.}

\item{Timelab}{Character string to label a plot axis involving time.}

\item{mo2lab}{Character string to label a plot axis involving oxygen
uptake. The user can create a more sophisticated lable with
\verb{makeO2lab} and use it here.}

\item{minR2}{If an estimate of what minR2, the minimum acceptable R2 for an
oxygen uptake measurement, already exists, it can be entered here and it
will be shown on the plots.}

\item{cex}{cex, as in other plot functions in R.}

\item{\dots}{Other parameters understood by the \verb{plot} function.}
}
\value{
Three plots are produced.
}
\description{
Make 3 plots (R2 vs time, frequency distribution of R2s, R2s vs MO2) to
help selecting the minimum R2 used in selecting valid oxygen uptake
measurements for a single individual.
}
\details{
This function draws 3 plots to assess the quality of the oxygen uptake
measurements during an intermittent-flow respirometry experiment. R2 is
used here as a proxy for linearity, itself used as a demonstration of good
mixing and absence of leaks in the respirometer. Typically a minimum
acceptable value of R2 is selected, called minR2 here. Measurements of
oxygen uptake (MO2) with a R2 greater or equal to minR2 are deemed
acceptable, the others are rejected. These plots can be used to assess
minR2, useful to calculate SMR and other variables, or to assess the
validity and usefulness of an existing minR2 and modify it if required.
}
\examples{

data(codSDA)
plotR2(codSDA$r2, codSDA$DateTime, codSDA$MO2cor, minR2=0.95) 
# this produced 3 plots, you likely see only the last one

# to see all plots at once
def.par <- par(no.readonly = TRUE) # save default, for resetting...
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
plotR2(codSDA$r2, codSDA$DateTime, codSDA$MO2cor, minR2=0.95) 
par(def.par)  #- reset to default

}
\references{
%% ~put references to the literature/web site here ~
}
\seealso{
\code{\link{plot}}
}
\author{
Denis Chabot, Institut Maurice-Lamontagne, Department of Fisheries
and Oceans.
}
