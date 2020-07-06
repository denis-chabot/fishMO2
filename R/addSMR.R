##' Add SMR to an existing plot of oxygen uptake values as a function of time.
##'
##' Displays a horizontal line representing the Standard Metabolic Rate on top
##' of a plot of MO2 vs time. A label can be shown optionally.
##'
##' A horizontal line representing SMR is added to the existing plot.
##'
##' @param SMR The SMR, a constant obtained previously, usually obtained with
##'   \code{calcSMR}.
##' @param SMRcol A colour for the horizontal line.
##' @param showLabel Boolean, \code{TRUE} displays the "SMR" label as well as
##'   the value of SMR.
##' @param SMRcex The size of the text label that can be shown next to the line,
##'   at the left of the plot.
##' @param \dots Other graphics parameters that work with lines, such as
##'   \code{lwd} for line width, \code{lty} for line type.
##' @note %% ~~further notes~~
##' @author Denis Chabot, Institut Maurice-Lamontagne, Department of Fisheries
##'   and Oceans.
##' @seealso \code{\link{calcSMR}}, \code{\link{plotMO2}}, \code{\link{par}}
##' @keywords ~kwd1 ~kwd2
##' @examples
##'
##' data("GrHalO2crit")
##' # remove a 10-h acclimation period; also remove the last 11 h, when DO was
##' #    decreasing to calculate O2crit. There were no low R^2s that required removal
##' smr.indices = GrHalO2crit$DateTime >= GrHalO2crit$DateTime[1] + 10*60*60 &
##'               GrHalO2crit$DateTime <= GrHalO2crit$DateTime[nrow(GrHalO2crit)] - 11*60*60
##' smr = calcSMR(GrHalO2crit$MO2[smr.indices])  # contains several estimations of
##'                                              # SMR, need to chose one
##' SMR = as.numeric(ifelse(smr$CVmlnd > 5.4, smr$quant[4], smr$mlnd))
##' SMR     # as recommended in Chabot et al. 2016
##' GrHalO2crit$pch = ifelse(smr.indices, 1, 0)
##' GrHalO2crit$col = ifelse(smr.indices, "blue", grey(0.5))
##' plotMO2(GrHalO2crit$DateTime, GrHalO2crit$MO2, mo2="dotital", o2="umol",
##'         t="min", m="kg", showO2 = FALSE,
##'         Xlab = "Time (month-day)", pch=GrHalO2crit$pch, col=GrHalO2crit$col)
##' legend("topright", c("Used to calculate SMR", "Not used to calculate SMR"),
##'        pch=c(1,0), col=c("blue", grey(0.5)), bty="n")
##' addSMR(SMR)
##'
##'
##' @export addSMR
##' @importFrom graphics par plot rect text
##'
addSMR <- function(SMR, SMRcol="orange", SMRcex=0.85, showLabel=T, ...) {
        coord = par("usr")  # in future version, check if a plot exists and
                            # exit if there is none
        abline(h=SMR, col=SMRcol, ...)
        if(showLabel) {
            text(coord[1], SMR, paste("SMR = ", round(SMR,1), sep=""),
                 adj=c(-0.1,1.3), cex=SMRcex) }
}
