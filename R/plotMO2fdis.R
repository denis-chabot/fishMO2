##' Produce a histogram of the frequency distribution of oxygen uptake values
##' for an animal and show the normal distributions that were fitted to these
##' data.
##'
##' Produce a histogram of the frequency distribution of oxygen uptake values
##' for an animal and show the normal distributions that were fitted to these
##' data. The user can then add lines representing different estimates of
##' Standard Metabolic Rate.
##'
##' Uses \code{hist} to produce the histogram of the frequency distribution,
##' \code{mclustBIC} to fit a mixture of normal distribution to these data and
##' calculate the line showing these distributions.
##'
##' @param X A vector or data frame column representing multiple measurements
##' of oxygen uptake (MO2) for a single animal. Values that are untrustworthy
##' (poor R2) or that were recorded when the animal could not have been at SMR
##' (such as early after it was placed in the respirometer, or soon after it
##' was fed) should have been removed from X.
##' @param g Range of normal distribution mixtures to try fit to the data,
##' default to 1--4.
##' @param Breaks The number of possible bars on the histogram.
##' @param mo2lab Label for the X axis, which is the range of observed MO2
##' values. The default is minimalist, the user can use \code{makeO2lab} to
##' produce a more sophisticated label prior to calling \code{plotMO2fdis}.
##' @param Border.col The colour of the border around the bars.
##' @param Bar.col The colour used to fill the bars.
##' @param ColNormdis The colour of the line tracking the normal distributions
##' fitted to the data.
##' @param \dots Other possible parameters for \code{hist}.
##' @return A plot as well as the mean of each normal distribution.
##' @note %% ~~further notes~~
##' @author Denis Chabot, Maurice-Lamontagne Institute, DFO, Canada.
##' @seealso \code{\link{calcSMR}}
##' @references Chabot, D., Steffensen, J. F. and Farrell, A. P. (2016) The
##' determination of the standard metabolic rate in fishes.  \emph{Journal of
##' Fish Biology} 88, 81-121.
##' @keywords ~kwd1 ~kwd2
##' @examples
##'
##' mo2 = c(rnorm(25, 150, 5), rnorm(50, 90, 3), rnorm(100, 50, 2))
##' plotMO2fdis(mo2)
##'
##' # with real data, Greenland halibut
##' # remove a 10-h acclimation period, as recommended in Chabot et al. (2016)
##' # also remove the last 11 h, when DO was decreasing to calculate O2crit.
##' # there were no low R2s that required removal
##' data(GrHalO2crit)
##' smrData = subset(GrHalO2crit, DateTime >= DateTime[1] + 10*60*60 &
##'                  DateTime <= DateTime[nrow(GrHalO2crit)] - 11*60*60)
##' plotMO2fdis(smrData$MO2)
##' # show some estimates of SMR as vertical lines
##' smr = calcSMR(smrData$MO2)
##' abline(v=smr$mlnd, col="black")
##' abline(v=smr$quant[4], col="blue")
##' abline(v=smr$low10pc, col="red")
##' legend("topright", c("MLND", "q_0.2", "low10pc"), lty=1,
##'        col=c("black","blue","red"), bty="n", y.intersp=1.2)
##' # note that the CV of MLND is huge and the MLND is likely to overestimate SMR,
##' #    the quantile with p==0.2 is preferable.
##'
##' # with real data, cod
##' data("codSDA")
##' # The fish was disturbed once for a sham-feeding, and 25.65 h later, it was fed.
##' # Some data after sham-feeding and all data post-feeding need to be excluded
##' #    when calculating SMR
##' # There are 2 time variables, one in POSIXct and one in hours, with zero at time of feeding.
##' # Tine in hours is used to filter undesired periods
##' start = -97.22  # first MO2
##' sham = -25.65   # time of sham-feeding
##' acclim = 24     # nb of hours to exclude because of handling stress, this fish took a long time to calm down
##' sham.acclim = 10  # nb of hours after sham-feeding with elevated MO2
##' minR2 = 0.96
##'
##' smr.data = subset(codSDA, r2 > minR2 &
##'                   ((Logtime_hr >= (start + acclim) & Logtime_hr < sham ) |
##'                   (Logtime_hr >= (sham + sham.acclim) & Logtime_hr < 0))
##'                   )
##' dim(smr.data)
##' plotMO2fdis(smr.data$MO2)
##' # show some estimates of SMR as vertical lines
##' smr = calcSMR(smr.data$MO2)
##' abline(v=smr$mlnd, col="black")
##' abline(v=smr$quant[4], col="blue")
##' abline(v=smr$low10pc, col="red")
##' legend("topright", c("MLND", "q_0.2", "low10pc"), lty=1, col=c("black","blue","red"), bty="n", y.intersp=1.2)
##' # note that the CV of MLND is small and the MLND is a good estimate of SMR.
##' smr$CVmlnd
##'
##' @export plotMO2fdis
##' @importFrom mclust Mclust mclustBIC dens
plotMO2fdis = function(X, g=1:4, Breaks=100, mo2lab=expression(M[O[2]]),
                       Border.col = grey(0.5), Bar.col=grey(0.92),
                       ColNormdis = "blue", ...){
    u = sort(X)
	hist(u, breaks=Breaks, freq=F, main="", border=Border.col, col=Bar.col,
	     xlab=mo2lab, ...)
	rug(u, ticksize = 0.01, quiet = TRUE)

	myBIC <- mclust::mclustBIC(X, G=g)
	myModel <- summary(myBIC, X)
	newX <- seq(from = min(X), to = max(X), length = 500)
	Dens <- dens(modelName = myModel$modelName, data = newX,
				parameters = myModel$parameters)
	lines(newX, Dens, col=ColNormdis)
	myModel$parameters$mean
}
