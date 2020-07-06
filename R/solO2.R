##' Solubility of oxygen in fresh- and seawater
##'
##' Calculates the amount of oxygen that dissolves in water according to
##' temperature, salinity and atmospheric pressure, when water and air are in
##' equilibrium (100\% saturation). Different oxygen units are allowed.
##' Solubility can be calculated per liter (common in physiology) or per kg
##' (common in oceanography) of water.
##'
##' Different units of oxygen concentration are allowed, different units for
##' quantity of water (1 liter or one kg), and equations from different two
##' authors are allowed. If total gas pressure is not 101.325, solubility will
##' be calculated for the pressure requested by the user.
##'
##' The equations provided by García and Gordon (1992) are used by default,
##' they are supposedly the best available for seawater below 5 °C. Equations
##' of Benson and Krause (1980, 1984) are provided for comparison with the
##' results from DOTABLES program at http://water.usgs.gov/software/DOTABLES/.
##' For T varying from 0 to 15°C, P from 99 to 102 kPa, S=0 and 28 and values
##' rounded to 2 decimals, author set to "Benson", results match what DOTABLES
##' does.
##'
##' @param t A vector of temperature values in degrees Celsius
##' @param s A vector of salinity values
##' @param o2Unit The desired oxygen units for solubility calculation (mg, ml,
##' µmol)
##' @param waterUnit Unit of water used for solubility calculation (liters or
##' kg)
##' @param p Total gas pressure above the water body (kPa)
##' @param author Equations from 2 authors can be used, Garcia or Benson (see
##' references)
##' @return Solubility value(s) of oxygen for requested temperatures and
##' salinities, in desired oxygen and water units, adjusted to pressure
##' requested by user.
##' @note %% ~~further notes~~
##' @section Warnings: It is assumed that vectors \code{t} and \code{s} have
##' the same length, or that one of the two is a constant value. This is not
##' checked in the present version, the user must make sure it is the case.
##' @author Denis Chabot
##' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
##' @references Benson B. B. and Krause D. Jr. (1980) The concentration and
##' isotopic fractionation of gases dissolved in freshwater in equilibrium with
##' the atmosphere. 1. Oxygen.  \emph{Limnology and Oceanography}, 25, 662-671.
##'
##' Benson B. B. and Krause D. Jr. (1984) The concentration and isotopic
##' fractionation of oxygen dissolved in freshwater and seawater in equilibrium
##' with the atmosphere.  \emph{Limnology and Oceanography}, 29, 620-632.
##'
##' García, H. E. and Gordon, L.I. (1992) Oxygen solubility in seawater: better
##' fitting equations.  \emph{Limnology and Oceanography} 37, 1307-1312.
##' @keywords ~kwd1 ~kwd2
##' @examples
##'
##' solO2(t=5, s=28, o2Unit="mg")
##' # should give 10.5928 mg O2 per liter
##'
##' # to compute % saturation
##'
##' obsDO = 8 # observed DO in mg per liter
##' Sol = solO2(t=10, s=28, o2Unit="mg")
##' sat = obsDO / Sol * 100
##' sat
##'
##' @export solO2
solO2 <- function (t, s = 0, o2Unit = c("mg", "ml", "umol"),
                   waterUnit = c("l", "kg"),
                   p = 101.325,
                   author = c("Garcia", "Benson"))
{
    T = 273.15 + t
    Ts <- log((298.15 - t)/T)
    theta <- 0.000975 - (1.426e-05 * t) + (6.436e-08 * t^2)
    author <- match.arg(author)
    o2Unit <- match.arg(o2Unit)
    waterUnit = match.arg(waterUnit)
    if (waterUnit == "l") {
        switch(author, Garcia = {
            A0 <- 2.00907
            A1 <- 3.22014
            A2 <- 4.0501
            A3 <- 4.94457
            A4 <- -0.256847
            A5 <- 3.88767
            B0 <- -0.00624523
            B1 <- -0.00737614
            B2 <- -0.010341
            B3 <- -0.00817083
            C0 <- -4.88682e-07
            sol <- exp(A0 + A1 * Ts + A2 * Ts^2 + A3 * Ts^3 +
                           A4 * Ts^4 + A5 * Ts^5 + s * (B0 + B1 * Ts + B2 *
                           Ts^2 + B3 * Ts^3) + C0 * s^2)
            switch(o2Unit, mg = {
                sol = convO2conc(sol, from = "ml", to = "mg")
            }, ml = {
                sol = sol
            }, umol = {
                sol = convO2conc(sol, from = "ml", to = "umol")
            })
        }, Benson = {
            switch(o2Unit, mg = {
                a0 <- -139.34411
            }, ml = {
                a0 <- -139.70012
            }, umol = {
                a0 <- -135.90205
            })
            a1 <- 157570.1
            a2 <- 66423080
            a3 <- 1.2438e+10
            a4 <- 8.621949e+11
            b0 <- 0.017674
            b1 <- 10.754
            b2 <- 2140.7
            sol = exp(a0 + a1/T - a2/T^2 + a3/T^3 - a4/T^4 -
                          s * (b0 - b1/T + b2/T^2))
        })
    }
    else {
        switch(author, Garcia = {
            A0 <- 5.80871
            A1 <- 3.20291
            A2 <- 4.17887
            A3 <- 5.10006
            A4 <- -0.0986643
            A5 <- 3.80369
            B0 <- -0.00701577
            B1 <- -0.00770028
            B2 <- -0.0113864
            B3 <- -0.00951519
            C0 <- -2.75915e-07
            sol <- exp(A0 + A1 * Ts + A2 * Ts^2 + A3 * Ts^3 +
                           A4 * Ts^4 + A5 * Ts^5 + s * (B0 + B1 * Ts + B2 *
                           Ts^2 + B3 * Ts^3) + C0 * s^2)
            switch(o2Unit, mg = {
                sol = convO2conc(sol, from = "umol", to = "mg")
            }, ml = {
                sol = convO2conc(sol, from = "umol", to = "ml")
            }, umol = {
                sol = sol
            })
        }, Benson = {
            switch(o2Unit, mg = {
                a0 <- -138.74202
            }, ml = {
                a0 <- -139.09803
            }, umol = {
                a0 <- -135.29996
            })
            a1 <- 1.572288e+5
            a2 <- 6.637149e+7
            a3 <- 1.243678e+10
            a4 <- 8.621061e+11
            b0 <- 0.020573
            b1 <- 12.142
            b2 <- 2363.1
            sol = exp(a0 + a1/T - a2/T^2 + a3/T^3 - a4/T^4 -
                          s * (b0 - b1/T + b2/T^2))
        })
    }
    Patm <- p/101.325
    Pv <- wvp(t)/101.325
    Pressure_correction <- Patm * ((1 - Pv/Patm) * (1 - theta *
                           Patm))/((1 - Pv) * (1 - theta))
    sol2 = Pressure_correction * sol
    sol2
}
