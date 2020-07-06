##' Calculate Specific Dynamic Action for a fish or aquatic invertebrate using
##' non-parametric quantile regression.
##'
##' Calculate the peak value, the time-to-peak, the duration and the magnitude
##' of the Specific Dynamic Action (SDA, i.e., Heat Increment of Feeding,
##' post-prandial metabolic rate, etc.) from oxygen uptake data recorded with a
##' fed fish for which the standard metabolic rate (SMR) is known. SMR is
##' required to calculate the magnitude of SDA (area bounded by the SDA curve
##' and the SMR). A non-parametric quantile regression is used to fit the SDA
##' curve, a technique that is robust against changes in activity level during
##' digestion.
##'
##' The \verb{rqss} function of the \verb{quantreg} package is used for this
##' non-parametric quantile regression that results in a curve that tracks the
##' bottom of the MO2 values on a plot of MO2 x time, keeping a proportion
##' \verb{tau} of values below the line. Thus the same assumption is made than
##' when estimating SMR: SMR is not taken as the lowest value of MO2 that can be
##' observed, but as a normal distribution describing the MO2 when an animal is
##' fasted and quiet, and some values are below the mean of this distribution.
##' In practice, when measuring SMR, a method that fits several normal
##' distributions, should reveal SMR, which is the lowest of these
##' distributions. In practice, it does not work in all cases (See Chabot et al.
##' 2016b) and simply assuming that 20\% of the MO2 values are below SMR works
##' well. This same principle is used to fit a curve tracing SDA. Without
##' ignoring a few hours at the beginning of SDA, it is difficult to get a fit
##' that begins close to SMR at time zero. The practical, if not particularly
##' elegant, solution is to start the fit a few hours hours after the meal and
##' to force a straight line from SMR at time zero, to the beginning of the SDA
##' fit at time \verb{postfeed.acclim}.
##'
##' @param X A numeric vector or data frame column containing the time relative
##'   to the meal (which it at time zero) for each oxygen uptake measurement.
##'   Measurements obtained before the meal, if present, have negative X values.
##'   See \verb{X.time.unit}.
##' @param Y A numeric vector or data frame column containing oxygen uptake
##'   measurements.
##' @param my.smr The Standard Metabolic Rate (SMR) for the animal. Same units
##'   as \verb{Y}.
##' @param tau A parameter that varies between 0 and 1. As with a quantile, the
##'   \verb{rqss} function fits a curve that results in the probability of the
##'   response, Y, of falling below the line is tau, and of falling above is 1 −
##'   tau. If SMR has been calculated using a quantile, it is logical to make
##'   tau == p, and often p is 0.2.
##' @param lambda A penalty parameter used by \verb{rqss} to control the
##'   flexibility of the fitted curve. Assuming that X is in hours, this should
##'   be a number of hours greater than the period of activity cycles. Typically
##'   such cycles are circadian and values of lambda between 18 and 30 work
##'   well.
##' @param postfeed.acclim Duration (in same units as X axis, typically hours)
##'   of a period, right after feeding, which is skipped before fitting the SDA
##'   curve with \verb{rqss}. See \verb{Details}.
##' @param tol How close to SMR should the SDA curve return to declare SDA
##'   complete. Ideally the curve returns to SMR (tol == 0). In practice, small
##'   fluctuations in MO2 around SMR, or experiments that end a bit too early
##'   for the fit to return to SMR will result in curve that do not return to
##'   SMR and SDA will have no end, no duration, and the magnitude will not be
##'   reliable. A solution is to allow for a tolerance about SMR to declare that
##'   SDA has ended. A good value is 5\% of SMR, expressed as a proportion
##'   (0.05).
##' @param tol.type If \verb{proportion}, the value set by \verb{tol} is
##'   interpreted as a percent of SMR. If set to \verb{constant}, a set value
##'   (same scale as MO2) is added to SMR.
##' @param MO2.time.unit Time unit over which MO2 is expressed, \verb{hour, min,
##'   day}. Required to integrate the area under the curve bound by SDA and SMR.
##' @param X.time.unit Time can be in \verb{hour, min, day} units, but will be
##'   transformed into hours within this function.
##' @return A list with two object:
##'
##'   \item{sda.fit}{ A data frame containing the fitted values of SDA every
##'   0.25 hour, which can then be used to plot SDA. The variables of this data
##'   frame are: \describe{ \item{time}{ A numeric in hours, spaced every 0.25 h
##'   except between time 0 and time \code{postfeed.acclim}.  } \item{pred}{
##'   Predicted MO2 (including SMR).  } \item{net}{ Predicted MO2 (after
##'   removing SMR).  } \item{status}{ Status of each predicted MO2 value,
##'   either \code{start, sda, post-sda}.  } } }
##'
##'   \item{sda.var}{ A list that contains the calculated parameters of SDA:
##'   \describe{ \item{duration}{ Duration of SDA in hours.  } \item{peak.time}{
##'   Time in hours when the peak of SDA is reached, with 0 == time of feeding.
##'   } \item{peak}{ Maximum MO2 due to SDA (including SMR). Same unit as
##'   \code{Y}.  } \item{peak.net}{ Maximum MO2 due to SDA (after removing SMR).
##'   Same unit as \code{Y}.  } \item{magnitude}{ Area under the curve
##'   representing SDA and above the straight horizontal line representing SMR.
##'   Same units as \code{Y} without the time component.  } \item{end.MO2}{ If
##'   the fit reached SMR + \code{tol}, SDA has ended and the value of tol is
##'   entered, expressed as a proportion of SMR. If the SDA has not ended
##'   properly, the difference between the lowest observed value of SDA and SMR
##'   is shown, expressed as a proportion of SMR.  } \item{SMR}{ The value of
##'   SMR used to calculate SDA.  } \item{tau}{ The value of \code{tau} used to
##'   calculate SDA.  } \item{lambda}{ The value of \code{lambda} used to
##'   calculate SDA.  } } }
##' @section Warning : The parameters tau and lambda are not independent from
##'   each other. If changing one, check if the results are acceptable,
##'   otherwise adjust the second parameter.
##' @author Denis Chabot, Institut Maurice-Lamontagne, Department of Fisheries
##'   and Oceans.
##' @seealso \code{\link{calcSMR}}, \code{\link{rqss}}
##' @references Chabot, Denis, Koenker, Roger and Farrell, Anthony P. (2016a)
##'   The measurement of the specific dynamic action in fishes.  \emph{Journal
##'   of Fish Biology} 88, 152-172.
##'
##'   Chabot, Denis, Steffensen, John F. and Farrell, Anthony P. (2016b) The
##'   determination of the standard metabolic rate in fishes.  \emph{Journal of
##'   Fish Biology} 88, 81-121.
##'
##'   Koenker, Roger (2005) Quantile regression.  Cambridge University Press,
##'   Cambridge. 366 p.
##' @examples
##'
##' # using juvenile cod data included in the package, minimum acceptable r2 is
##' # 0.96 and SMR is 81.2; see calcSMR for how it was obtained
##' data("codSDA")
##' sda.data = subset(codSDA, r2 >= 0.96)
##' # Logtime_hr is time in hours relative to time of feeding, a required variable.
##' # MO2cor is in micromoles per min per kg and the "cor" in the name means that
##' # the MO2 values soon after feeding were corrected for the effect of the feeding procedure
##'
##' SDA= calcSDA(X=sda.data$Logtime_hr, Y=sda.data$MO2cor, my.smr=81.2,
##'                tau=0.2, lambda=30,
##' 	           postfeed.acclim=3, tol=0.05, tol.type="proportion",
##' 	           MO2.time.unit="min", X.time.unit="hour")
##' names(SDA)
##' SDA$sda.var
##'
##' @export calcSDA
##' @importFrom quantreg rqss qss
##' @importFrom Hmisc trap.rule
##' @importFrom shape Arrows
calcSDA <- function(X, Y, my.smr, tau=0.2, lambda=24, postfeed.acclim=6,
             tol=0.05, tol.type=c("proportion", "constant"),
             MO2.time.unit=c("hour", "min", "day"), X.time.unit=c("hour", "min", "day")) {
        my.data = data.frame(X, Y)
        my.data <- na.omit(my.data)
        names(my.data)[1:2]= c("time", "MO2")
        # time axis must be in hours. If it is not, we transform it.
        if(X.time.unit != "hour"){
            if(X.time.unit == "min") {my.data$time = my.data$time/60
            } else {my.data$time = my.data$time * 24}
        }

        my.data = subset(my.data, X >= 0)

        time.step = 0.25 # in hours, we predict SDA every 15 min, 0.25 h
        my.data.2 <- subset(my.data, time >= postfeed.acclim - time.step)
            # shorter dataset, after removal of the data that are too influenced by
            # the feeding procedure. The rqss fit will use this shorter data set
            # To be able to predict a SDA right after the end of the period excluded
            #   by the user, it is necessary to include at least one value of MO2
            #   recorded before. Thus we start with post.feed.acclim - time.step
        pred.x <- data.frame(time=seq(postfeed.acclim, max(my.data.2$time), time.step))
            # X to predict, in 0.25 h steps
        # model MO2 to calculate SDA
        fitsda <- rqss(MO2 ~ qss(time, lambda=lambda), tau=tau, data=my.data.2)
        pred <- predict(fitsda, pred.x)
        sda <- data.frame(time=pred.x$time, pred)
        sda$net <- sda$pred - my.smr

        # ASSUMPTION: duration > 10 h, necessary to determine end of SDA. This is
        #    because I calculated "net" above
        #    and I look for when net becomes very small. But net can also be very
        #    small at the beginning of SDA.
        #    This function has been used by people who did not wait for MO2 to
        #    return to SMR before ending the experiment and therefore
        #    "net" at the end can be a greater value than at the beginning.
        #    By assuming that the end comes at least 10 h after feeding time
        #    I avoid the problem

        if(tol.type == "proportion") {  # tolerance can be in % of SMR or a fixed value
            endPoint = tol*my.smr
        } else {endPoint = tol}         # endPoint is net, i.e. above SMR

        end <- subset(sda, time>(max(10, postfeed.acclim)) & net < endPoint)[1,1]
        end.mo2 <- tol  # to indicate that we did return to expected endPoint,
                        # otherwise this gets changed below
        if(is.na(end)){
            sda.end = subset(sda, time>max(10, postfeed.acclim))
            sda.stop = subset(sda.end, net == min(net))
            end = sda.stop$time  # time for lowest post-feeding MO2
            if(tol.type == "proportion") {
                closest = sda.stop$net / my.smr
                # SDA stopped at X% above smr, normally 5%.
                # When we do not reach the expected value, we use the lowest reached
                end.mo2 <- round(closest,3)
            } else {
                end.mo2 = sda.stop$net
            }
        }

        if (postfeed.acclim > 0) {
            # if a few hours were excluded immediately after feeding
            # we need to add a line to the sda dataframe for time zero
            dummy = data.frame(time=0, pred=my.smr, net=0)
            sda = rbind(dummy,sda)
        }

        duringSDA = sda$net>0 & sda$time<end

        sda$status[duringSDA] = "sda"
        sda$status[sda$time==0] = "start"
        sda$status[is.na(sda$status)] = "post-sda"
        sda.short = subset(sda, status %in% c("start","sda"))
        if(nrow(sda.short)==0) { # something went wrong, we have no usable SDA
            sda.dur.h = NA
            sda.peak = data.frame(time=NA, pred=NA, net=NA)
            area.net=NA
            end.mo2=NA
        } else {	# normal situation
            sda.dur.h <- max(sda.short$time)
            sda.peak <- sda.short[sda.short$pred == max(sda.short$pred),1:3]

            # to compute area under the curve
            area.net <- trap.rule(sda.short$time, sda.short$net)
            # if time base of MO2 is in minutes instead of hours, multiply by 60
            if(MO2.time.unit == "min"){
                area.net = area.net*60
            }
            if(MO2.time.unit == "day"){
                area.net = area.net/24
            }
         } # end sda data not empty

        the.sda <- data.frame(c(
            duration=sda.dur.h,
            peak=sda.peak,
            magnitude=area.net,
            end.MO2=as.numeric(end.mo2),
            SMR=my.smr,
            tau=tau,
            lambda=lambda))
        names(the.sda)[which(names(the.sda) == "peak.pred" )] = "peak"
        rq.out <- list(sda.fit=sda, sda.var=the.sda)
        rq.out             # output a list with SDA curve and SDA variables
}	# end function
