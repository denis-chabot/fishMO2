##' Make 3 plots involving the $"R"^2$ for each MO2 measurement for a single
##' individual fish or other water-breathing animal.
##' 
##' Make 3 plots (R2 vs time, frequency distribution of R2s, R2s vs MO2) to
##' help selecting the minimum R2 used in selecting valid oxygen uptake
##' measurements for a single individual.
##' 
##' This function draws 3 plots to assess the quality of the oxygen uptake
##' measurements during an intermittent-flow respirometry experiment. R2 is
##' used here as a proxy for linearity, itself used as a demonstration of good
##' mixing and absence of leaks in the respirometer. Typically a minimum
##' acceptable value of R2 is selected, called minR2 here. Measurements of
##' oxygen uptake (MO2) with a R2 greater or equal to minR2 are deemed
##' acceptable, the others are rejected. These plots can be used to assess
##' minR2, useful to calculate SMR and other variables, or to assess the
##' validity and usefulness of an existing minR2 and modify it if required.
##' 
##' @param r2 Vector or data frame column holding the R2 for each measurement
##' of oxygen uptake.
##' @param time Vector or data frame column holding the time, in POSIXct or
##' numeric (hours) for each measurement of oxygen uptake.
##' @param mo2 Vector or data frame column holding the oxygen uptake
##' measurements.
##' @param r2lab Character string to label a plot axis involving R2.
##' @param Timelab Character string to label a plot axis involving time.
##' @param mo2lab Character string to label a plot axis involving oxygen
##' uptake. The user can create a more sophisticated lable with
##' \verb{makeO2lab} and use it here.
##' @param minR2 If an estimate of what minR2, the minimum acceptable R2 for an
##' oxygen uptake measurement, already exists, it can be entered here and it
##' will be shown on the plots.
##' @param cex cex, as in other plot functions in R.
##' @param \dots Other parameters understood by the \verb{plot} function.
##' @return Three plots are produced.
##' @author Denis Chabot, Institut Maurice-Lamontagne, Department of Fisheries
##' and Oceans.
##' @seealso \code{\link{plot}}
##' @references %% ~put references to the literature/web site here ~
##' @examples
##' 
##' data(codSDA)
##' plotR2(codSDA$r2, codSDA$DateTime, codSDA$MO2cor, minR2=0.95) 
##' # this produced 3 plots, you likely see only the last one
##' 
##' # to see all plots at once
##' def.par <- par(no.readonly = TRUE) # save default, for resetting...
##' layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
##' plotR2(codSDA$r2, codSDA$DateTime, codSDA$MO2cor, minR2=0.95) 
##' par(def.par)  #- reset to default
##' 
##' @export plotR2
plotR2 <- function(r2, time, mo2, r2lab=expression(r^2), Timelab="Time", mo2lab=expression(M[O[2]]), minR2=NULL, cex=0.7, ...) {
    # First plot
    plot(r2~time, xlab=Timelab, ylab=r2lab, cex=0.7, xaxt="n", ...)
    if("POSIXct" %in% class(time)){
        r = range(time)
        ndays = as.numeric(difftime(r[2],r[1], units="days"))
        t <- as.POSIXct(round(r, "days"))
        t[1] = t[1] - 24*60*60
        t[2] = t[2] + 24*60*60
        if(ndays<1.5){
            axis.POSIXct(1, at=seq(t[1], t[2], by=6*60*60), format="%H", tck=-0.014)
        } else { # longer session
            axis.POSIXct(1, at = seq(t[1], t[2], by = "days"), format = "%m-%d")
            axis(1, at=seq(t[1], t[2], by = 6*60*60), labels=F, tck = -0.007)
        }
    } else {  # numeric X, must be hours
        axis(1, at=seq(-120, 240, 24), labels=seq(-120, 240, 24), tick=T, tck=-0.014)
        axis(1, at=seq(-120, 240, 6), labels=F, tck = -0.007)
    }
    if(!is.null(minR2)) abline(h=minR2, col="red")  # if a first estimate of minR2 already exists, show it
    
    # Second plot
    hist(r2, breaks=seq(0,1,0.01), freq=F, main="", bty="l", border=grey(0.5), xlab=r2lab)
    if(!is.null(minR2)) abline(v=minR2, col="red")
    
    # Third plot
    plot(r2~mo2, xlab=mo2lab, ylab=r2lab, cex=0.7)
    if(!is.null(minR2)) abline(h=minR2, col="red")
}
