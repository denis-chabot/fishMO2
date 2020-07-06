##' Calculates SMR using different methods,
##'
##' Calculates Standard Metabolic Rate (SMR) using several methods, the user
##' then look into the resulting object and picks the result(s) of his/her
##' choice.
##'
##' This function accepts a vector of oxygen uptake values recorded from a
##' single animal over a period of a few hours to a few days and calculates
##' Standard Metabolic Rate (SMR) according to several methods reviewed by
##' Chabot et al. (2016). NAs are excluded.
##'
##' @param Z A vector of oxygen uptake (MO2) values for a single animal
##'   resulting from intermittent-flow or open-flow respirometry.
##' @param q p-values for the quantiles to be calculated.
##' @param G The maximum number of distributions to fit to the data with the
##'   MLND method.
##' @return Returns a list containing several estimations of SMR and some
##'   additional results.
##'
##'   The details of the output components are as follows:
##'
##'   \item{mlnd}{The Mean of the Lowest Normal Distribution, calculated by
##'   \verb{Mclust}.}
##'
##'   \item{quant}{ A vector of quantile values, with length = number of values
##'   in parameter \verb{q}.  }
##'
##'   \item{low10}{ The mean of the 10 lowest oxygen uptake values (vector Z).
##'   } \item{low10pc}{ The mean of the 10\% lowest oxygen uptake values (vector
##'   Z), after removing the 5 lowest values, which are assumed to be outliers
##'   (abnormally low values).  } \item{low10_2pc}{ The mean of the 10 lowest
##'   oxygen uptake values (vector Z), after removing the 2\% lowest values,
##'   which are assumed to be outliers (abnormally low values).  } \item{cl}{ A
##'   vector of same length as Z containing the number of the distribution
##'   assigned to each oxygen uptake value by \verb{Mclust}. This can be useful
##'   to assign different plot symbols to oxygen uptake values that belong to
##'   different normal distributions.  } \item{theSMRdistr}{ The number of the
##'   normal distribution that is used to estimate SMR. Normally this is 1 (the
##'   lowest normal distribution). In some rare cases, \verb{Mclust} identifies
##'   a small normal distribution with only a few aberrently low values. This
##'   distribution does not represent SMR. As an attempt to prevent this small
##'   distribution (if present) from being used to estimate SMR, the script
##'   requires that the lowest normal distribution containing at least 10\% of
##'   the oxygen uptake values be used as SMR.  } \item{CVmlnd}{ The coefficient
##'   of variation (in \% of MLND) of the oxygen uptake values assigned to the
##'   lowest normal distribution. Chabot et al. (2016) use this to assess
##'   whether the MLND method should be used to estimate SMR or not. When it
##'   cannot (CV > 5.4\%), they recommend using the quantile method with p =
##'   0.2.  }
##'
##'   Results are in same units as the elements of Z (e.g., oxygen units (mg,
##'   ml, mmol, µmol, etc.) per time unit (min, hr), per whole individual or per
##'   mass unig (kg, g, etc.)
##' @author Denis Chabot, Maurice-Lamontagne Institute, DFO, Canada.
##' @references Chabot, D., Steffensen, J. F. and Farrell, A. P. (2016) The
##'   determination of the standard metabolic rate in fishes.  \emph{Journal of
##'   Fish Biology} 88, 81-121.
##' @examples
##'
##' mo2 = c(rnorm(25, 150, 5), rnorm(50, 90, 3), rnorm(100, 50, 2))
##' smr = calcSMR(mo2)
##' smr
##' SMR = as.numeric(ifelse(smr$CVmlnd > 5.4, smr$quant[4], smr$mlnd)) # as recommended in Chabot et al. 2016
##'
##' # with real data, Greenland halibut
##' data(GrHalO2crit)
##' # to ensure that the data are shown in the time zone where the experiment
##' # took place, regardless of your computer's time zone:
##' attr(GrHalO2crit$DateTime, "tzone") = "EST"
##' plotMO2(GrHalO2crit$DateTime, GrHalO2crit$MO2, mgp=c(2,0.5,0))
##'
##' # remove a 10-h acclimation period, as recommended in Chabot et al. (2016);
##' # also remove the last 11 h, when DO was decreasing to calculate O2crit.
##' # There were no low R^2s that required removal
##' smrData = subset(GrHalO2crit, DateTime >= DateTime[1] + 10*60*60 &
##'                  DateTime <= DateTime[nrow(GrHalO2crit)] - 11*60*60)
##' smr = calcSMR(smrData$MO2); smr
##' # as recommended in Chabot et al. 2016
##' SMR = as.numeric(ifelse(smr$CVmlnd > 5.4, smr$quant[4], smr$mlnd)); SMR
##' abline(h=SMR, col="orange")
##'
##' # with real data, cod
##' data("codSDA")
##' # There was a sham-feeding 25.65 h before feeding. On average the
##' # sham-feeding increased MO2 for about 10 h.
##' # This period, as well as after the meal and during the first period in the
##' # respirometer should be removed before calculating SMR
##' # This fish took a long time to calm down, the acclimation period was set
##' # to 24 h
##' start = -97.22  # first MO2
##' sham = -25.65   # time of sham-feeding
##' acclim = 24     # nb of hours to exclude because of handling stress,
##'                 # this fish took a long time to calm down
##' sham.acclim = 10  # nb of hours after sham-feeding with elevated MO2
##' minR2 = 0.96
##'
##' smr.data = subset(codSDA, r2 > minR2 &
##'                   ((Logtime_hr >= (start + acclim) & Logtime_hr < sham ) |
##'                   (Logtime_hr >= (sham + sham.acclim) & Logtime_hr < 0))
##'                   )
##' dim(smr.data)
##' SMR = calcSMR(smr.data$MO2cor, q=0.2)
##' bestSMR = ifelse(SMR$CVmlnd > 5.4, SMR$quant, SMR$mlnd)
##' bestSMR
##' plotMO2(codSDA$DateTime, codSDA$MO2cor, mgp=c(2,0.5,0), col="grey")
##' points(smr.data$DateTime, smr.data$MO2cor, col="blue")
##' abline(h=bestSMR, col="orange")

##'
##' @export calcSMR
##' @importFrom mclust Mclust mclustBIC summaryMclustBIC
calcSMR = function(Z, q=c(0.05,0.1,0.15,0.2,0.25,0.3,0.4,0.5), G=1:4){
    # requireNamespace(mclust)    # I use :: below, but the Mclust function
                                # itself calls other functions in package mclust
    Z = Z[!is.na(Z)]  # remove NAs
    u = sort(Z)
    the.Mclust <- mclust::Mclust(Z,  G=G)
    cl <- the.Mclust$classification
    # sometimes, a small distribution of outliers is identified which does not constitute SMR
    # when class 1 contains more than 10% of cases, it is assumed to contain SMR,
    # otherwise class 2 is assumed to contain SMR
    cl2 <- as.data.frame(table(cl))
    cl2$cl <- as.numeric(levels(cl2$cl))
    valid <- cl2$Freq>=0.1*length(cl)
    the.cl <- min(cl2$cl[valid])
    left.distr <- Z[the.Mclust$classification==the.cl]
    mlnd = the.Mclust$parameters$mean[the.cl]
    popID = the.cl
    CVmlnd = sd(left.distr)/mlnd * 100
    quant=quantile(u, p=q)
    low10=mean(u[1:10])
    if(length(u) >= 35) { # to have at least 3 values for SMR
        low10pc = mean(u[6:(5 + round(0.1*(length(u)-5)))])  # remove 5 outliers, keep lowest 10% of the rest, average
    } else low10pc = NA
    twopc = round(0.02*N(u),0)
    low10_2pc = mean(u[twopc+1:twopc+11])  # remove 2% of values as outliers, keep 10 lowest to average

    Z = list(mlnd=mlnd, quant=quant, low10=low10, low10pc=low10pc, low10_2pc = low10_2pc, cl=cl, theSMRdistr=popID, CVmlnd=CVmlnd)
    return(Z)
}
