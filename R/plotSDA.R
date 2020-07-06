##' Add SDA fit and SDA parameters to an existing plot of oxygen uptake values
##' as a function of time.
##'
##' Displays the SDA line fitted by \code{calcSDA} on top of a plot of MO2 vs
##' time. Also displayed is coloured representation of the magnitude of SDA (the
##' area between the fitted line and the horizontal line representing SMR) and
##' the values of peak, time-to-peak, duration and magnitude.
##'
##' Add SDA details to an existing plot of MO2 x time.
##'
##' @param my.sda Object created by \code{calcSDA}.
##' @param show.par Boolean, if \code{TRUE}, displays in upper-left of plot the
##'   values of tau and lambda that were used to obtain the fitted line.
##' @param show.var Boolean, if \code{TRUE}, displays the values of peak MO2
##'   (net, i.e. after removal of SMR), time to peak, duration and magnitude of
##'   SDA.
##' @param show.smr.line Boolean, \code{TRUE} displays a horizontal line at SMR.
##' @param show.feeding Boolean, \code{TRUE} displays a vertical line at time
##'   zero to illustrate feeding time.
##' @param show.mag.arrow Boolean, \code{TRUE} displays an arrow from the word
##'   "magnitude" to the shaded area representing the area under the curve.
##' @param peak.lab.pos If NULL, the function tries to calculate a position for
##'   the label associated with the peak of SDA. A (x, y) pair can be provided
##'   if the calculated position is inadequate.
##' @param mag.lab.pos If NULL, the function tries to calculate a position for
##'   the label associated with the magnitude of SDA. A (x, y) pair can be
##'   provided if the calculated position is inadequate.
##' @param smr.lab.pos Although a horizontal line depicting SMR is shown, the
##'   value of SMR is only shown if requested, either in one string below SMR
##'   (\code{1L}) or in two parts, one above and one below SMR (\code{2L}).
##' @param lty.smr Line type for SMR, same meaning as \code{lty} for any line in
##'   R.
##' @param col.smr Colour of the horizontal line representing SMR.
##' @param lty.feeding Line type for time of feeding, same meaning as \code{lty}
##'   for any line in R.
##' @param col.feeding Colour of the vertical line representing feeding time.
##' @param Cex.par Size of the text for tau and lambda. Same use as \code{cex}
##'   parameter in \code{text}.
##' @param Cex.sda Size of the text for the SDA parameters. Same use as
##'   \code{cex} parameter in \code{text}
##' @param col.sda Colour of the shading representing SDA magnitude.
##' @param \dots Other graphics parameters may work.
##' @note %% ~~further notes~~
##' @section Warning: The automatic positioning of SMR and of the SDA parameters
##'   works best when plots have a longer X axis than Y axis. On square plots
##'   you will probably have to position these labels yourself using the
##'   arguments \code{peak.lab.pos} and \code{mag.lab.pos}. On plots showing
##'   data before and after feeding, the space to draw an arrow associated with
##'   magnitude (\code{show.mag.arrow}) is restricted and the result is often
##'   unattractive, it may be necessary to set this argument to \code{FALSE}.
##' @author Denis Chabot, Institut Maurice-Lamontagne, Department of Fisheries
##'   and Oceans.
##' @seealso \code{\link{calcSDA}}, \code{\link{plotMO2}}, \code{\link{text}}
##' @references Chabot, Denis, Koenker, Roger and Farrell, Anthony P. (2016) The
##'   measurement of the specific dynamic action in fishes.  \emph{Journal of
##'   Fish Biology} 88, 152-172.
##'
##'   Chabot, Denis, Steffensen, John F. and Farrell, Anthony P. (2016) The
##'   determination of the standard metabolic rate in fishes.  \emph{Journal of
##'   Fish Biology} 88, 81-121.
##'
##'   Koenker, Roger (2005) Quantile regression.  Cambridge University Press,
##'   Cambridge. 366 p.
##' @keywords ~kwd1 ~kwd2
##' @examples
##'
##' data("codSDA")
##' sda.data = subset(codSDA, r2 >= 0.96)
##' # Logtime_hr is time in hours relative to time of feeding, a required variable.
##' # MO2cor is in micromoles per min per kg and the "cor" in the name means that
##' #    the MO2 values soon after feeding were corrected for the effect of the
##' #    feeding procedure
##'
##' SDA= calcSDA(X=sda.data$Logtime_hr, Y=sda.data$MO2cor, my.smr=81.2,
##'                tau=0.2, lambda=30,
##' 	           postfeed.acclim=3, tol=0.05, tol.type="proportion",
##' 	           MO2.time.unit="min", X.time.unit="hour")
##'
##' par(mar=c(4, 4, 0.1, 0.1)+0.1, mgp=c(1.8,0.5,0))
##' plotMO2(sda.data$Logtime_hr, sda.data$MO2cor, mo2="dotital", o2="umol", t="min", m="kg",
##'     showO2 = FALSE, Xlab = "Time relative to feeding (hr)", ylim=c(50, 250))
##' plotSDA(SDA)
##' # not enough space to show the arrow associated with magnitude, better to hide it
##' plotMO2(sda.data$Logtime_hr, sda.data$MO2cor, mo2="dotital", o2="umol", t="min", m="kg",
##'     showO2 = FALSE, Xlab = "Time relative to feeding (hr)", ylim=c(50, 250))
##' plotSDA(SDA, show.mag.arrow=FALSE)
##'
##' # same data, but focus on post-feeding data and the SDA
##' plotMO2(sda.data$Logtime_hr, sda.data$MO2cor, mo2="dotital", o2="umol", t="min", m="kg",
##'     showO2 = FALSE, Xlab = "Time relative to feeding (hr)", ylim=c(50, 250), xlim=c(0,100))
##' plotSDA(SDA, smr.lab.pos="1L")
##'
##' # SMR label is illegible, better not to display it
##' # Default position for peak not great, force a new position
##' plotMO2(sda.data$Logtime_hr, sda.data$MO2cor, mo2="dotital", o2="umol", t="min", m="kg",
##'     showO2 = FALSE, Xlab = "Time relative to feeding (hr)", ylim=c(50, 250), xlim=c(0,100))
##' plotSDA(SDA, smr.lab.pos="none", peak.lab.pos=c(40,150))
##'
##'
##'
##' @export plotSDA
##' @importFrom graphics par plot rect text abline axis axis.POSIXct hist
##'   lines polygon rug segments strheight strwidth
##' @importFrom grDevices grey rgb
##' @importFrom stats anova coefficients lm na.omit predict quantile sd time

plotSDA <- function(my.sda, show.par=T, show.var=T, show.smr.line=T,
                    show.feeding=T, show.mag.arrow=T,
                    peak.lab.pos=NULL, mag.lab.pos=NULL,
                    smr.lab.pos=c("2L", "1L", "none"),
                    lty.smr =1, col.smr="orange",
                    lty.feeding=1, col.feeding="seagreen", Cex.par=0.8, Cex.sda=0.9,
                    col.sda=rgb(1,0,0,0.2),  ...) {

    smr.lab.pos = match.arg(smr.lab.pos)
    sda.var = my.sda$sda.var
    sda.fit <- my.sda$sda.fit
    # sda.short = subset(sda.fit, status %in% c("start","sda")) # to draw the SDA polygon
    sda.short = sda.fit[sda.fit$status %in% c("start","sda"),]
    # this needs improvements, but works
    # required to properly format different scales
    rnd = ifelse(sda.var$peak.net <= 1, 3, ifelse(sda.var$peak.net <= 10, 2, 1))

    coords = par("usr")
    deltax = coords[2] - coords[1]
    deltay = coords[4] - coords[3]
    deltax2 = coords[2] - sda.var$peak.time
    deltay2 = coords[4] - sda.var$peak

    # if requested, indicate the values of tau and lambda in upper left corner
    if(show.par){
        text(par("usr")[1], par("usr")[4],
             substitute(list(tau, lambda) == group("(",list(x,y),")"),
             list(x=sda.var$tau, y=sda.var$lambda)), 	adj = c(-0.1,1.1), cex=Cex.par)
    }

    # if requested show SMR line
    if(show.smr.line) abline(h=sda.var$SMR, lty=lty.smr, col=col.smr)
    # if requested, print "SMR" along with its value
    if(smr.lab.pos == "1L") {
        smr.text = as.expression(substitute(SMR ==l, list(l=round(sda.var$SMR,rnd))))
        text(coords[1] + 0.01*deltax, sda.var$SMR, smr.text, adj = c(0,1.5), cex=Cex.sda )
    } else if(smr.lab.pos=="2L"){
        text(coords[1], sda.var$SMR, "SMR", adj=c(-0.1,1.3), cex=Cex.sda)
        text(coords[1], sda.var$SMR, round(sda.var$SMR,1), adj=c(-0.1,-0.3), cex=Cex.sda)
    }

    if(show.feeding){
        abline(v=0, lty=lty.feeding, col=col.feeding)
    }

    # to draw a polygon representing SDA
    new.x <- c(sda.short$time, sda.short$time[length(sda.short$time)], sda.short$time[1])
    new.y <- c(sda.short$pred, sda.var$SMR, sda.var$SMR)
    polygon(new.x, new.y, col=col.sda)

    if(show.var){
        # add peak label and arrow
        # if the user has not specified the coordinates of the label for the peak of SDA,
        #   automatically determine a position that will be likely acceptable
        #   note: the user can specify the coordinates in peak.lab.pos as a x and y pair
        #   with c(x, y)
        arrow.start = c(sda.var$peak.time+0.005*deltax, sda.var$peak+0.005*deltay)
        if (is.null(peak.lab.pos)) {
            peak.lab.pos = c(sda.var$peak.time+0.25*deltax2, sda.var$peak+0.2*deltay2)
        }

        # place SDA variables on plot
        # peak & peak time
        peak.net.text = as.expression(substitute("peak (net)"==m,
                                      list(m=round(sda.var$peak.net,rnd))))
        peak.time.text = paste("at", round(sda.var$peak.time,1), "h")
        Arrows(arrow.start[1], arrow.start[2], peak.lab.pos[1], peak.lab.pos[2],
               arr.length = 0.2, code=1, arr.adj=1)    # arrow toward peak
        text(peak.lab.pos[1], peak.lab.pos[2], peak.net.text, adj = c(-0.05,0.3), cex=Cex.sda )
        text(peak.lab.pos[1], peak.lab.pos[2], peak.time.text, adj = c(-0.4,1.5), cex=Cex.sda )

        # duration
        duration.text = paste0("duration = ", round(sda.var$duration,1), " h")
        text(sda.var$duration/2, sda.var$SMR, duration.text, adj = c(0.5,1.5), cex=Cex.sda)

        # magnitude and arrow
        if (is.null(mag.lab.pos)) {
            mag.lab.pos = c(sda.var$duration/2, sda.var$SMR)
        }
        magnitude.text = as.expression(substitute(magnitude ==z,
                                       list(z=round(sda.var$magnitude,rnd-1))))
        text(mag.lab.pos[1], mag.lab.pos[2], magnitude.text,
             cex=Cex.sda, adj = c(0.5,2.5) )  # one fewer signif. digit than peak
        arrowstart.x = sda.var$duration/2 - strwidth(magnitude.text)/2  #+ strwidth("i")
        arrowstart.y = sda.var$SMR - strheight(magnitude.text)*1.8
        arrowhead.x = ifelse(arrowstart.x > 3, min(sda.var$peak.time, arrowstart.x),
                             sda.var$peak.time)
        arrowhead.y = sda.var$SMR + sda.var$peak.net/2
        if(show.mag.arrow){
            Arrows(arrowhead.x, arrowhead.y, arrowstart.x, arrowstart.y, arr.length = 0.2,
                   code=1, arr.adj=1)    # arrow toward peak
            }
        } # end show variables
}	# end function
