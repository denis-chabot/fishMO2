% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcSDA.R
\name{calcSDA}
\alias{calcSDA}
\title{Calculate Specific Dynamic Action for a fish or aquatic invertebrate using
non-parametric quantile regression.}
\usage{
calcSDA(
  X,
  Y,
  my.smr,
  tau = 0.2,
  lambda = 24,
  postfeed.acclim = 6,
  tol = 0.05,
  tol.type = c("proportion", "constant"),
  MO2.time.unit = c("hour", "min", "day"),
  X.time.unit = c("hour", "min", "day")
)
}
\arguments{
\item{X}{A numeric vector or data frame column containing the time relative
to the meal (which it at time zero) for each oxygen uptake measurement.
Measurements obtained before the meal, if present, have negative X values.
See \verb{X.time.unit}.}

\item{Y}{A numeric vector or data frame column containing oxygen uptake
measurements.}

\item{my.smr}{The Standard Metabolic Rate (SMR) for the animal. Same units
as \verb{Y}.}

\item{tau}{A parameter that varies between 0 and 1. As with a quantile, the
\verb{rqss} function fits a curve that results in the probability of the
response, Y, of falling below the line is tau, and of falling above is 1 −
tau. If SMR has been calculated using a quantile, it is logical to make
tau == p, and often p is 0.2.}

\item{lambda}{A penalty parameter used by \verb{rqss} to control the
flexibility of the fitted curve. Assuming that X is in hours, this should
be a number of hours greater than the period of activity cycles. Typically
such cycles are circadian and values of lambda between 18 and 30 work
well.}

\item{postfeed.acclim}{Duration (in same units as X axis, typically hours)
of a period, right after feeding, which is skipped before fitting the SDA
curve with \verb{rqss}. See \verb{Details}.}

\item{tol}{How close to SMR should the SDA curve return to declare SDA
complete. Ideally the curve returns to SMR (tol == 0). In practice, small
fluctuations in MO2 around SMR, or experiments that end a bit too early
for the fit to return to SMR will result in curve that do not return to
SMR and SDA will have no end, no duration, and the magnitude will not be
reliable. A solution is to allow for a tolerance about SMR to declare that
SDA has ended. A good value is 5\% of SMR, expressed as a proportion
(0.05).}

\item{tol.type}{If \verb{proportion}, the value set by \verb{tol} is
interpreted as a percent of SMR. If set to \verb{constant}, a set value
(same scale as MO2) is added to SMR.}

\item{MO2.time.unit}{Time unit over which MO2 is expressed, \verb{hour, min,
day}. Required to integrate the area under the curve bound by SDA and SMR.}

\item{X.time.unit}{Time can be in \verb{hour, min, day} units, but will be
transformed into hours within this function.}
}
\value{
A list with two object:

  \item{sda.fit}{ A data frame containing the fitted values of SDA every
  0.25 hour, which can then be used to plot SDA. The variables of this data
  frame are: \describe{ \item{time}{ A numeric in hours, spaced every 0.25 h
  except between time 0 and time \code{postfeed.acclim}.  } \item{pred}{
  Predicted MO2 (including SMR).  } \item{net}{ Predicted MO2 (after
  removing SMR).  } \item{status}{ Status of each predicted MO2 value,
  either \code{start, sda, post-sda}.  } } }

  \item{sda.var}{ A list that contains the calculated parameters of SDA:
  \describe{ \item{duration}{ Duration of SDA in hours.  } \item{peak.time}{
  Time in hours when the peak of SDA is reached, with 0 == time of feeding.
  } \item{peak}{ Maximum MO2 due to SDA (including SMR). Same unit as
  \code{Y}.  } \item{peak.net}{ Maximum MO2 due to SDA (after removing SMR).
  Same unit as \code{Y}.  } \item{magnitude}{ Area under the curve
  representing SDA and above the straight horizontal line representing SMR.
  Same units as \code{Y} without the time component.  } \item{end.MO2}{ If
  the fit reached SMR + \code{tol}, SDA has ended and the value of tol is
  entered, expressed as a proportion of SMR. If the SDA has not ended
  properly, the difference between the lowest observed value of SDA and SMR
  is shown, expressed as a proportion of SMR.  } \item{SMR}{ The value of
  SMR used to calculate SDA.  } \item{tau}{ The value of \code{tau} used to
  calculate SDA.  } \item{lambda}{ The value of \code{lambda} used to
  calculate SDA.  } } }
}
\description{
Calculate the peak value, the time-to-peak, the duration and the magnitude
of the Specific Dynamic Action (SDA, i.e., Heat Increment of Feeding,
post-prandial metabolic rate, etc.) from oxygen uptake data recorded with a
fed fish for which the standard metabolic rate (SMR) is known. SMR is
required to calculate the magnitude of SDA (area bounded by the SDA curve
and the SMR). A non-parametric quantile regression is used to fit the SDA
curve, a technique that is robust against changes in activity level during
digestion.
}
\details{
The \verb{rqss} function of the \verb{quantreg} package is used for this
non-parametric quantile regression that results in a curve that tracks the
bottom of the MO2 values on a plot of MO2 x time, keeping a proportion
\verb{tau} of values below the line. Thus the same assumption is made than
when estimating SMR: SMR is not taken as the lowest value of MO2 that can be
observed, but as a normal distribution describing the MO2 when an animal is
fasted and quiet, and some values are below the mean of this distribution.
In practice, when measuring SMR, a method that fits several normal
distributions, should reveal SMR, which is the lowest of these
distributions. In practice, it does not work in all cases (See Chabot et al.
2016b) and simply assuming that 20\% of the MO2 values are below SMR works
well. This same principle is used to fit a curve tracing SDA. Without
ignoring a few hours at the beginning of SDA, it is difficult to get a fit
that begins close to SMR at time zero. The practical, if not particularly
elegant, solution is to start the fit a few hours hours after the meal and
to force a straight line from SMR at time zero, to the beginning of the SDA
fit at time \verb{postfeed.acclim}.
}
\section{Warning }{
 The parameters tau and lambda are not independent from
  each other. If changing one, check if the results are acceptable,
  otherwise adjust the second parameter.
}

\examples{

# using juvenile cod data included in the package, minimum acceptable r2 is
# 0.96 and SMR is 81.2; see calcSMR for how it was obtained
data("codSDA")
sda.data = subset(codSDA, r2 >= 0.96)
# Logtime_hr is time in hours relative to time of feeding, a required variable.
# MO2cor is in micromoles per min per kg and the "cor" in the name means that
# the MO2 values soon after feeding were corrected for the effect of the feeding procedure

SDA= calcSDA(X=sda.data$Logtime_hr, Y=sda.data$MO2cor, my.smr=81.2,
               tau=0.2, lambda=30,
	           postfeed.acclim=3, tol=0.05, tol.type="proportion",
	           MO2.time.unit="min", X.time.unit="hour")
names(SDA)
SDA$sda.var

}
\references{
Chabot, Denis, Koenker, Roger and Farrell, Anthony P. (2016a)
  The measurement of the specific dynamic action in fishes.  \emph{Journal
  of Fish Biology} 88, 152-172.

  Chabot, Denis, Steffensen, John F. and Farrell, Anthony P. (2016b) The
  determination of the standard metabolic rate in fishes.  \emph{Journal of
  Fish Biology} 88, 81-121.

  Koenker, Roger (2005) Quantile regression.  Cambridge University Press,
  Cambridge. 366 p.
}
\seealso{
\code{\link{calcSMR}}, \code{\link{rqss}}
}
\author{
Denis Chabot, Institut Maurice-Lamontagne, Department of Fisheries
  and Oceans.
}
