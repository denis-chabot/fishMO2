###
# final version for O2crit paper






##' Calculate the critical oxygen level (O2crit) of a fish or aquatic
##' invertebrate.
##'
##' This requires that the Standard Metabolic Rate of the animal be known.
##' Typically all data available for animal, such as those obtained to estimate
##' MMr and SMR and those obtained to estimate O2crit, are used together,
##' because this can improve the estimation of O2crit in some cases.
##'
##' At first glance, estimating O2crit is an easy task: MO2 becomes lower than
##' SMR and proportional to DO at very low DO levels. A regression between MO2
##' and DO can be fitted to these low MO2 values, and the intersection of this
##' regression line with the horizontal line is O2crit.
##'
##' Indeed the code to calculate O2crit in an experiment when DO decreased
##' slowly, many MO2 values are available during the period of DO decrease, and
##' in particular, there is a MO2 value close to SMR near the putative O2crit.
##'
##' This function tries to salvage experiments that are suboptimal, either
##' because of manipulation error or lack of cooperation from the animal. To
##' achieve this, some assumptions are made. First, the regression line
##' calculated for the MO2 values, when extended to high DO levels, must pass
##' above or on the highest MO2 values. Second, positive intercepts are not
##' possible (i.e. no fish or invertebrate should have a MO2 > 0 at 0\% sat.)
##'
##' It is up to the user to decide if the data are too poor for the
##' determination of O2crit to be usable. But at least, there is less user
##' interpretation in calculating O2crit in such circumstances.
##'
##' This function is not meant to estimate O2crit from experiments when the
##' maximum metabolic rate (MMR) is measured in normoxia and at progressively
##' declining DO levels, until the level of no excess activity (MMR = SMR) is
##' reached, although it may work with such data: I did not have datasets to
##' check this out.
##'
##' Typically, the same experiment is used to estimate both the SMR (and even
##' MMR) and O2crit. The conditions for the determination of SMR are documented
##' elsewhere (Chabot et al., 2016). Briefly, the fish should be acclimated to
##' the respirometer and to the experimental temperature, in postabsorptive
##' state and protected from stimuli that may elicit increases in MO2. The
##' experimental conditions should remain the same for the determination of
##' O2crit, except for the progressively declining DO level. If the fish’s SMR
##' has been determined in a separate experiment, it is possible to estimate
##' O2crit from a shorter experiment in which DO is declining from normoxia to
##' below O2crit within a few hours. However, it is advantageous to determine
##' SMR and O2crit during a single experiment because the elevated values of
##' MO2 obtained when the fish is first placed into the respirometer are useful
##' to check the quality of the O2crit determination.
##'
##' This function has been updated and simplified in 2018 and the description
##' in an appendix to Claireaux and Chabot (2016) is no longer accurate.
##'
##' @param Data Data.frame with at least these 2 variables: MO2 (values of
##' oxygen uptake) and DO (Dissolved oxygen measured with each value of oxygen
##' uptake). DO can only have \% saturation units with the current version of
##' the function.
##' @param SMR A single value representing SMR [not the list created by
##' \verb{calcSMR}].
##' @param lowestMO2 A low value of MO2 which suggest that the animal's oxygen
##' consumption may be limited by hypoxia; values lower than SMR are not rare
##' in normoxia, so a value that is rarely observed in normoxia is required. It
##' can be entered by the user, otherwise the quantile (p = 0.05) of MO2 values
##' observed in normoxia (DO > 80\% sat.) is used automatically.
##' @return Returns a list containing the estimated values of O2crit and
##' additional results that can be useful to assess the quality of the estimate
##' and in producing plots.
##'
##' The details of the output components are as follows:
##'
##' \item{o2crit}{The estimated value of O2crit.}
##'
##' \item{SMR}{ The value of SMR used in estimating O2crit, same as entered in
##' the function call.  }
##'
##' \item{$origData}{ The original data.frame with some new variables added.
##' This is used by plotO2crit.  }
##'
##' \item{$Method}{ Indicates if a standard least-square regression (LS_reg)
##' was used to estimate O2crit. When the intercept is positive, indicating a
##' positive MO2 when DO is zero, which does not make sense, a regression
##' passing through the origin is used instead (through_origin).  }
##'
##' \item{$mod}{ The output of the lm function used to estimate the regression
##' between MO2 and DO.  }
##'
##' \item{r2}{ The R2 of the regression MO2 and DO.  }
##'
##' \item{P}{ The p-level of the regression MO2 and DO.  }
##'
##' \item{lethalPoints}{ The index of the MO2 values identified as conforming
##' to ambient DO.  }
##'
##' \item{AddedPoints}{ The index of the MO2 value added to the regression, if
##' necessary, to insure that it cleared MMR.  }
##' @section Warning : Any observation with missing values is removed. If
##' variables that are not used to calculate O2crit (any variable other than
##' MO2 and DO) are frequently missing when the main variables are present, it
##' is better to remove these variables from \verb{Data} before calling
##' \verb{calcO2crit}.
##' @author Denis Chabot, Maurice-Lamontagne Institute, DFO, Canada.
##' @references Claireaux, Guy and Chabot, Denis (2016) Responses by fishes to
##' environmental hypoxia: integration through Fry's concept of aerobic
##' metabolic scope.  \emph{Journal of Fish Biology} 88, 232-251.
##' doi:doi:10.1111/jfb.1283
##' @examples
##'
##' Data = data.frame(
##'     MO2 = c(rnorm(25, 150, 5), rnorm(50, 90, 3), rnorm(100, 50, 2),
##'           50*c(0.9, 0.8, 0.7)),
##'     DO = c(rnorm(168, 90, 3), seq(80, 18, length.out=10))
##' )
##' smr = calcSMR(Data$MO2[1:168])  # 10 values recorded when DO was decreasing
##'                                 # SHOULD NOT be included when calculating SMR
##' SMR = as.numeric(ifelse(smr$CVmlnd > 5.4, smr$quant[4], smr$mlnd))
##'                                      # as recommended in Chabot et al. 2016
##' MyO2crit = calcO2crit(Data, SMR)
##' MyO2crit$o2crit # to display only O2crit, without the other elements of the list object
##'
##' # with real data
##' data(GrHalO2crit)
##' # this dataset already contains the two required variables with the two
##' #    required names, MO2 and DO
##' # first calculate SMR
##' # remove a 10-h acclimation period and the last 11 h, when DO was decreasing
##' #    to calculate O2crit. There were no low R^2s that required removal
##' smrData = subset(GrHalO2crit, DateTime >= DateTime[1] + 10*60*60 &
##' DateTime <= DateTime[nrow(GrHalO2crit)] - 11*60*60)
##' smr = calcSMR(smrData$MO2); smr
##' SMR = as.numeric(ifelse(smr$CVmlnd > 5.4, smr$quant[4], smr$mlnd))
##' SMR # as recommended in Chabot et al. 2016
##' # ready to calculate O2crit
##' MyO2crit = calcO2crit(GrHalO2crit, SMR)
##' MyO2crit$o2crit
##'
##' @export calcO2crit
calcO2crit <- function(Data, SMR, lowestMO2=NA)
{
    # programmed by Denis Chabot, Institut Maurice-Lamontagne, DFO, Canada
    # first version written in June 2009
    # last updated in July 2018
    # updated 2018-12-16 to improve how functions from other packages are handled

    # require(segmented)
    Data = na.omit(Data)
    method = "LS_reg"  # will become "through_origin" if intercept is > 0
    if(is.na(lowestMO2)) lowestMO2 = quantile(Data$MO2[Data$DO >= 80], p=0.05)

    Data = na.omit(Data)
    method = "LS_reg"
    if (is.na(lowestMO2)) lowestMO2 = quantile(Data$MO2[Data$DO >= 80], p = 0.05)

    # Step 1 Pivot
    geqSMR = Data$MO2 >= lowestMO2
    pivDO = min(Data$DO[geqSMR])  # = the lowest DO where MO2 >= ~ SMR was observed

    # Step 2 Segmented regression (new in v0.42)
    # prepare a data.frame that only contains data at or slightly above SMR,
    # here defined as all data < SMR+20%
    # I subsample the data in normoxia, otherwise they can overwhelm the data in hypoxia
    prop.of.SMR = 1.2  # move this to function parameters?
    dfs1 = subset(Data, MO2<= prop.of.SMR*SMR & DO >= 80)
    max.to.keep = 25
    if(nrow(dfs1) > max.to.keep) {
        keep = sample(1:nrow(dfs1), max.to.keep)  # select 25 rows from the dataframe
        dfs1 = dfs1[keep,]
    }
    dfs2 = subset(Data, MO2<= prop.of.SMR*SMR & DO < 80)
    DataForSegm = rbind(dfs1, dfs2)
    rm(dfs1, dfs2)
    modl =lm(MO2~DO, data=DataForSegm)
    startBreak = min(DataForSegm$DO) + (max(DataForSegm$DO) - min(DataForSegm$DO))/4

    # some very misbehaved datasets can make it impossible for the segmented
    # regression to be computed, we want to continue
    # despite the error
    er = function(e) out=NA

    breakDO <- tryCatch({
        mods=segmented::segmented(modl, ~DO, psi=startBreak)
        mods$psi[2]
    }, error = er)

    # Step 3 Choose from one of the above
    pivotDO = min(pivDO, breakDO, na.rm=T)

    # Step 4 Select data at DO < pivot to calculate regression in "conforming"
    # portion of the data
    lethal = Data$DO < pivotDO
    N_under_SMR = sum(lethal)
    if(is.na(N_under_SMR)) N_under_SMR = 0

    pivotMO2 = Data$MO2[Data$DO == pivotDO]
    if (N_under_SMR < 3) {  # fewer than 3 DO values are below SMR, we try
                            # incorporationg points above putative O2crit
                            # O2crit thus derived is at your own risk
        missing = 3 - N_under_SMR
        not.lethal = Data$DO[geqSMR]
        DOlimit = max(sort(not.lethal)[1:missing])
        addedPoints = Data$DO <= DOlimit
        lethal = lethal | addedPoints
    }

    # to handle cases with few data close to SMR and protracted series of
    # values < SMR at low and intermediate DO
    # there are cases (Schouman S4R3, S5R4) when DO drops below SMR, then comes
    # back up at a lower DO, to finally drop for good
    # keeping all the values < SMR does not work well
    # this distinguishes v042 and v042b
    Data_lethal_zone = Data[lethal, ]
    DO_range_conf = as.numeric(range(Data_lethal_zone$DO))
    Delta = DO_range_conf[2] - DO_range_conf[1]
    if(Delta > 15 & N_under_SMR >6) {
        lethal = Data$DO < (DO_range_conf[2] - Delta/2)
        rm(Data_lethal_zone)
    }

    Mod_conf = lm(MO2 ~ DO, data = Data[lethal, ])

    # Step 5 regression line must not lie below MMR or any MO2 > SMR
    predMO2 = as.numeric(predict(Mod_conf, data.frame(DO=Data$DO)))
    Data$delta = (Data$MO2-predMO2)/predMO2 * 100 # residuals set to zero
    # when below pivotDO
    Data$delta[Data$DO < pivotDO | lethal] = 0
    tol = 0 # any positive residual is unacceptable
    HighValues = Data$delta > tol
    Ranks = rank(-1*Data$delta)
    HighMO2 = HighValues & Ranks == min(Ranks)    # keep largest residual
    if (sum(HighValues) > 0) {
        nblethal = sum(lethal)
        Data$W = NA
        Data$W[lethal]=1/nblethal
        Data$W[HighMO2] = 2  # v0.42 was 1 heavy weight, we force the regression
                             # to go through this high value
        Mod_conf = lm(MO2~DO, weight=W, data=Data[lethal | HighMO2,])
    } # end search for positive residuals

    Coef = coefficients(Mod_conf)

    #Step 6, check for positive intercept
    AboveOrigin = Coef[1] > 0
    # if it is, we use a regression that goes through the origin
    if (AboveOrigin){
        Mod_conf = lm(MO2~DO -1, data=Data[lethal,])
        Coef = c(0, coefficients(Mod_conf)) # need to add the intercept (0)
        #  manually to have a pair of coefficients
        method = "through_origin"
        HighMO2 = rep(FALSE, nrow(Data)) # did not use the additional value
        # from Step 4
    }

    po2crit = as.numeric(round((SMR - Coef[1]) / Coef[2], 1))
    sum_mod = summary(Mod_conf)
    anov_mod = anova(Mod_conf)
    O2CRIT = list(o2crit = po2crit, SMR = SMR, origData = Data, Method = method,
                  mod = Mod_conf, r2 = sum_mod$r.squared, P = anov_mod$"Pr(>F)",
                  lethalPoints = which(lethal),
                  AddedPoints = which(HighMO2))
    return(O2CRIT)
} # end function

###

