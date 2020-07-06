##' Plot oxygen uptake values as a function of dissolved oxygen and show
##' O2crit.
##' 
##' Takes an object created by \verb{calcO2crit}. This object is self-contained
##' and includes the original data, SMR and O2crit. This functions produces a
##' plot showing the original values of oxygen uptake as a function of
##' dissolved oxygen, along with an horizontal line representing SMR and a
##' regression line fitted to those oxygen uptake values deemed to be
##' conforming (related to dissolved oxygen). The O2crit is shown as the
##' intersection of the two lines.
##' 
##' Produces a plot illustrating how O2crit was calculated for a given animal,
##' based on data obtained by intermittent-flow respirometry. Typically the
##' data set includes multiple measurements of oxygen uptake in conditions that
##' are compatible with the determination of the Standard Metabolic Rate (SMR).
##' Then the ambient level of dissolved oxygen is slowly decreased until the
##' animal can no longer sustain its SMR. The only data required are contained
##' in an object created by \verb{calcO2crit}. Depending on the number of
##' values of oxygen uptake identified as conforming (proportional) to
##' dissolved oxygen level and below SMR, on the necessity (or not) to include
##' a value of oxygen uptake far above SMR to increase the slope and on the
##' type of regression fitted to these values the color and type of regression
##' line changes. For ordinary LS regression, without added points: blue line,
##' red symbols; ordinary LS regression, with an added point: blue line, red
##' symbols, except orange for the added value; regression through origin,
##' green dotted line, red symbols.
##' 
##' @param o2critobj Object created by \verb{calcO2crit}. It is a list that
##' contains the original data, in addition to the SMR and O2crit.
##' @param plotID Can be used to add a label (e.g., A, B, C, a, b, c) to the
##' upper-left corner of a plot. For a finer control, produce the plot without
##' plotID and add it manually afterward.
##' @param o2lab Character string to be used to label the X-axis.
##' @param mo2lab Character string to be used to label the Y-axis. The user can
##' create an object with \verb{makeO2lab} and use it here for a more
##' sophisticated result than the default.
##' @param smr.cex The size (cex) of the SMR value, shown on the plot.
##' @param o2crit.cex The size (cex) of the O2crit value, shown on the plot.
##' @param plotID.cex The size (cex) of the plot label, if shown on the plot.
##' @param Transparency Some of the colours are transparent, which is not
##' supported by all graphics devices. For \verb{postscript} and other such
##' devices, set \verb{Transparency} to FALSE and similar but not transparent
##' colours will be used instead.
##' @param \dots Additional graphics parameters accepted by \verb{plot}.
##' @return Produces a plot.
##' @note %% ~~further notes~~
##' @section Warning : For R plotting devices that do not support transparency,
##' set Transparency to FALSE.
##' @author Denis Chabot, Institut Maurice-Lamontagne, Department of Fisheries
##' and Oceans.
##' @seealso \code{\link{calcO2crit}}, \code{\link{calcSMR}}
##' @references Chabot, Denis, Steffensen, John F. and Farrell, Anthony P.
##' (2016) The determination of the standard metabolic rate in fishes.
##' \emph{Journal of Fish Biology} 88, 81-121.
##' 
##' Claireaux, Guy and Chabot, Denis (2016) Responses by fishes to
##' environmental hypoxia: integration through Fry's concept of aerobic
##' metabolic scope.  \emph{Journal of Fish Biology} 88, 232-251.
##' doi:doi:10.1111/jfb.1283
##' @examples
##' 
##' data("GrHalO2crit")
##' # remove a 10-h acclimation period; also remove the last 11 h, when DO was decreasing to calculate O2crit. There were no low R^2s that required removal
##' smrData = subset(GrHalO2crit, DateTime >= DateTime[1] + 10*60*60 & DateTime <= DateTime[nrow(GrHalO2crit)] - 11*60*60)
##' smr = calcSMR(smrData$MO2)  # contains several estimations of SMR, need to chose one
##' SMR = as.numeric(ifelse(smr$CVmlnd > 5.4, smr$quant[4], smr$mlnd)); SMR # as recommended in Chabot et al. 2016
##' MyO2crit = calcO2crit(GrHalO2crit, SMR)
##' plotO2crit(MyO2crit)
##' plotO2crit(MyO2crit, plotID="A")
##' 
##' @export plotO2crit
plotO2crit <- function(o2critobj, plotID="", o2lab="Dissolved oxygen (% sat.)",
    mo2lab=expression(M[O[2]]), smr.cex=0.9, o2crit.cex=0.9, plotID.cex=1.2, Transparency=T, ...)
{
    smr = o2critobj$SMR
    if(Transparency) Col=c(rgb(0,0,0,0.7), "red", "orange") else Col=c(grey(0.3), "red", "orange")
    Data=o2critobj$origData
    Data$Color = Col[1]
    Data$Color[o2critobj$lethalPoints] = Col[2]
    Data$Color[o2critobj$AddedPoints] = Col[3]
    # ordinary LS regression, without added points: blue line, red symbols
    # ordinary LS regression, with added points: blue line, red & orange symbls
    # regression through origin, green dotted line, red symbols
    line.color = ifelse(o2critobj$Method=="LS_reg", "blue", "darkgreen")
    line.type = ifelse(o2critobj$Method=="LS_reg", 1, 3)
    limX = c(0, max(Data$DO))
    limY = c(0, max(Data$MO2))
    plot(MO2~DO, data=Data, xlim=limX, ylim=limY, col=Data$Color, xlab=o2lab, 
       ylab=mo2lab, ...)
    coord <- par("usr")
    if(plotID != ""){
    text(0, coord[4], plotID, cex=plotID.cex, adj=c(0,1.2))    
    }
    
    abline(h=smr, col="orange")
    text(coord[1], smr, "SMR", adj=c(-0.1,1.3), cex=smr.cex)
    text(coord[1], smr, round(smr,1), adj=c(-0.1,-0.3), cex=smr.cex)
    if(!is.na(o2critobj$o2crit)) {
    	abline(o2critobj$mod, col=line.color, lty=line.type)
    	segments(o2critobj$o2crit, smr, o2critobj$o2crit, coord[3], 
             col=line.color, lwd=1)
    text(x=o2critobj$o2crit, y=0, o2critobj$o2crit, col=line.color, 
         cex=o2crit.cex, adj=c(-0.1,0.5))
    	} 
}
