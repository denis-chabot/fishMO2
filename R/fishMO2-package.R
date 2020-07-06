

##' Oxygen uptake data from a juvenile Atlantic cod before and after ingestion
##' of a meal
##'
##' Oxygen uptake data from a juvenile (22.9 g) Atlantic cod (\emph{Gadus
##' morhua}) measured by intermittend-flow respirometry before and after
##' ingestion of a meal (about 3.5\% body mass, experimental diet). This is to
##' illustrate \verb{calcSDA} and \verb{plotSDA}.
##'
##' Intermittent flow respirometry and a 2.56 L respirometer were used to
##' measure oxygen uptake in this juvenile cod (salinity of 27⋅6 ± 1⋅7, 11⋅8 ±
##' 0⋅1∘ C, mean ± s.d.). The fish was fasted for 66 h before being placed into
##' the respirometer to insure it was in post-absorptive state, a requirement
##' to measure its standard metabolic rate. After 3 days, it was taken out of
##' the respirometer, anasthetised, a tube was inserted in its stomach and
##' water was flushed in its stomach (sham feeding). Approximately 24 h later,
##' the same procedure was repeated but water was replaced by a paste made of
##' the experimental pellets and water. Approximately 3.5\% of body mass was
##' fed to the fish, which was then returned to the respirometers. Oxygen
##' uptake was measured for an additional 4 days.
##'
##' This fish was placed into the respirometer on the 2009-01-05 and removed on
##' 2009-01-13. The first MO2 after feeding was measured 16:01:12 on
##' 2009-01-09, which was taken at time zero. The first and last MO2 measures
##' were obtained at -97.22 and 88.84 h. Sham-feeding was completed at -25.65
##' h. Examination of the raw data suggested that only MO2 measurements with a
##' $r^2$ > 0.96 should be considered valid. This particular fish acclimated
##' slowly to the respirometer and it is recommended to exclude the first 24-h
##' of data before calculating SMR. Data recorded during the first 10-h after
##' sham-feeding or after feeding should also be excluded when calculating SMR.
##'
##' @name codSDA
##' @docType data
##' @format A data frame with 947 observations on the following 5 variables:
##' \describe{ \item{DateTime}{a POSIXct vector indicating date and
##' time for each measurement} \item{Logtime_hr}{a numeric vector
##' indicating the time, in hours, relative to feeding. Measurements obtained
##' before feeding have a negative value for this variable}
##' \item{MO2}{a numeric vector representing oxygen uptake, in
##' micromoles of oxygen per min per kg of body mass}
##' \item{MO2cor}{same as MO2, except for the first 10 hours after the
##' meal: these measurements were corrected for the increase in MO2 resulting
##' from the feeding procedure. The correction was based on an exponential
##' decay function fitted to data obtained after a sham feeding that took place
##' the day prior to feeding.} \item{r2}{a numeric vector representing
##' the coefficient of determination R^2 of the regression between dissolved
##' oxygen and time for a given value of MO2} }
##' @references Chabot, Denis, Koenker, Roger and Farrell, Anthony P. (2016)
##' The measurement of the specific dynamic action in fishes.  \emph{Journal of
##' Fish Biology} 88, 152-172.
##' @source Collected by Denis Chabot at the Maurice-Lamontagne Institute,
##' Department of Fisheries and Oceans, Canada.
##' @keywords datasets
##' @examples
##'
##' data(codSDA)
##' plot(MO2~DateTime, data=codSDA)
##'
NULL





##' Oxygen uptake data from an adult Greenland halibut to illustrate the
##' determination of SMR and O2crit
##'
##' Oxygen uptake data from an adult (1.563 kg) Greenland halibut
##' (\emph{Reinhardtius hippoglossoides}) measured by intermittend-flow
##' respirometry. During the last few hours of the experiment, the ambient
##' dissolved oxygen was slowly reduced until the fish was no longer able to
##' sustain its Standard Metabolic Rate (SMR), so as to calculate its critical
##' oxygen level (O2crit).
##'
##' Results of an experiment with a Greenland halibut \emph{Reinhardtius
##' hippoglossoides} of 1.563 kg placed in a 48.43 L respirometer. Oxygen
##' uptake was measured by intermittent-flow respirometry for about 2.5 days.
##' These measurements allowed the determination of the Standard Metabolic Rate
##' (SMR) by the method described in Chabot et al. 2016, using the
##' \verb{calcSMR} function from this package. At this point, dissolved oxygen
##' was decreased slowly until the fish could no longer sustain its SMR. The
##' \verb{calcO2crit} function from this package can be used to determine
##' O2crit (in \% saturation).
##'
##' @name GrHalO2crit
##' @docType data
##' @format A data frame with 220 observations on the following 6 variables:
##' \describe{ \item{DateTime}{a POSIXct vector indicating date and
##' time for each measurement} \item{Logtime_hr}{a numeric vector
##' indicating the time, in hours, relative to feeding. Measurements obtained
##' before feeding have a negative value for this variable}
##' \item{MO2}{a numeric vector representing oxygen uptake, in
##' micromoles of oxygen per min per kg of body mass} \item{r2}{a
##' numeric vector representing the coefficient of determination R^2 of the
##' regression between dissolved oxygen and time for a given value of MO2}
##' \item{Aver_O2}{a numeric vector representing the average level of
##' dissolved oxygen (in kPa) in the respirometer during each measurement of
##' MO2} \item{DO}{a numeric vector representing the equivalent of
##' Aver_O2 in \% saturation units} }
##' @references Chabot, D., Steffensen, J. F. and Farrell, A. P. (2016) The
##' determination of the standard metabolic rate in fishes.  \emph{Journal of
##' Fish Biology} 88, 81-121.
##'
##' Claireaux, Guy and Chabot, Denis (2016) Responses by fishes to
##' environmental hypoxia: integration through Fry's concept of aerobic
##' metabolic scope.  \emph{Journal of Fish Biology} 88, 232-251.
##' doi:doi:10.1111/jfb.1283
##' @source Collected by Denis Chabot at the Maurice-Lamontagne Institute,
##' Department of Fisheries and Oceans, Canada.
##' @keywords datasets
##' @examples
##'
##' data(GrHalO2crit)
##' plot(MO2~DateTime, data=GrHalO2crit)
##'
NULL





##' Calculate and plot the standard metabolic rate (SMR), the critical oxygen
##' level (O2crit) and the specific dynamic action (SDA) and related variables
##' in fishes and crustaceans
##'
##' Package to calculate and plot the metabolic rate of water-breathers such as
##' fish and crustaceans. In particular, functions are provided to calculate
##' the Standard Metabolic Rate (SMR) from multiple measurements of oxygen
##' uptake obtained from a single animal. In experiments where ambient
##' dissolved oxygen was slowly decreased until the animal could no longer
##' sustain its SMR, O2crit can be calculated. Specific Dynamic Action (SDA)
##' can also be calculated and plotted for experiments measuring oxygen uptake
##' before and after a meal. Conversion between different oxygen concentration
##' units is provided, functions to calculate oxygen solubility and saturation
##' values will be added soon.
##'
##'
##' @name fishMO2-package
##' @aliases fishMO2-package fishMO2
##' @docType package
##' @author Denis Chabot
##'
##' Maintainer: Denis Chabot \email{chabot.denis@@gmail.com}
##' @references Chabot, D., Steffensen, J. F. and Farrell, A. P. (2016) The
##' determination of the standard metabolic rate in fishes.  \emph{Journal of
##' Fish Biology} 88, 81-121.
##'
##' Claireaux, Guy and Chabot, Denis (2016) Responses by fishes to
##' environmental hypoxia: integration through Fry's concept of aerobic
##' metabolic scope.  \emph{Journal of Fish Biology} 88, 232-251.
##' doi:doi:10.1111/jfb.1283
##' @keywords package
NULL



