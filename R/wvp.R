##' Water vapor pressure as a function of water temperature and salinity
##' 
##' Calculates water vapor pressure, given water temperature and salinity,
##' using a variety of methods. This is required to convert partial pressure of
##' oxygen into saturation or concentration units and vice-versa.
##' 
##' Output: water vapor pressure above fresh or sea water in kPa.
##' 
##' Note: this function is not intended to calculate vapor pressure above ice.
##' Also, water vapor pressure is not affected by atmospheric pressure
##' 
##' Methods for vapor pressure calculation above fresh water:
##' 
##' \describe{ \item{IAPWS}{ This is the 'official' formulation from the
##' International Association for the Properties of Water and Steam (Wagner &
##' Pruss 1993), Eq. 2.5 p. 399. The valid range of this formulation is 273.16
##' <= K <= 647.096 and is based on the ITS90 temperature scale. }
##' 
##' \item{BensonKrause}{ Eq. 23 of Benson and Krause, 1980 }
##' 
##' \item{GreenCarritt}{ Green & Carritt 1967 (see also table 2 Benson & Krause
##' 1984) without salinity correction, in atm converted to kPa }
##' 
##' \item{WMO2008}{ Annex 4B, Guide to Meteorological Instruments and Methods
##' of Observation, WMO Publication No 8, 7th edition, Geneva, 2008. }
##' 
##' \item{Antoine}{ http://www.watervaporpressure.com }
##' 
##' \item{Weiss}{ Weiss & Price 1980 eq 10 p. 350, in atm converted to kPa }
##' 
##' \item{GoffGratch}{ Smithsonian Meteorological Tables, 5th edition, p. 350,
##' 1984, in hPa then converted to kPa } }
##' 
##' Methods for salinity correction: \describe{ \item{Chabot}{ My own
##' polynomial fit passing through origin to Robinson 1954's data, Table II p.
##' 451 gives values of delta vp for different chlorinities. Chlorinity changed
##' into salinity according to Lyman (1969) }
##' 
##' \item{Green}{ From table 2 in Benson & Krause (1984) }
##' 
##' \item{none}{ Removes salinity correction, same as leaving \code{s} to its
##' default value of zero } }
##' 
##' @param t Temperature in °C
##' @param s Salinity in parts per thousand
##' @param method One of "IAPWS", "BensonKrause", "GreenCarritt","WMO2008",
##' "Antoine", "Weiss", "GoffGratch". See Details.
##' @param sal_method One of "Chabot", "Green" or "none". See Details.
##' @return A vector of same length as parameter t
##' @note This assumes that air temperature above the water surface is that of
##' the water. The effect of salinity is negligible, but it is nevertheless
##' included in the calculation if the user provides salinity.
##' @section Warnings: \code{s} must be of length 1 or same length as t. This
##' is not checked by the function at present so errors could occur if this is
##' not respected.
##' @author Denis Chabot
##' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
##' @references Benson B.B. and Krause D.Jr. (1980) The concentration and
##' isotopic fractionation of gases dissolved in freshwater in equilibrium with
##' the atmosphere. 1. Oxygen.  \emph{Limnology and Oceanography}, 25, 662-671.
##' 
##' Benson B.B. and Krause D.Jr. (1984) The concentration and isotopic
##' fractionation of oxygen dissolved in freshwater and seawater in equilibrium
##' with the atmosphere.  \emph{Limnology and Oceanography}, 29, 620-632.
##' 
##' Green, E. J. and Carritt, D. E. (1967) New tables for oxygen saturation of
##' seawater.  \emph{Journal of Marine Research}, 25, 140-147.
##' 
##' Lyman, J. (1969) Redefinition of salinity and chlorinity.  \emph{Limnology
##' and Oceanography}, 27, 928-929.
##' 
##' Robinson R. A. (1954) The vapour pressure and osmotic equivalence of sea
##' water.  \emph{Journal of the Marine Biological Association of the United
##' Kingdom}, 33, 449-455.
##' 
##' Wagner, W. and Pruss, A. (1993) International Equations for the Saturation
##' Properties of Ordinary Water Substance.  Revised According to the
##' International Temperature Scale of 1990. Addendum to J. Phys. Chem. Ref.
##' Data 16, 893 (1987) \emph{Journal of Physical and Chemical Reference Data},
##' 22, 783.  http://dx.doi.org/10.1063/1.555926.
##' 
##' Wagner, W. & Pruss, A. (2002) The IAPWS formulation 1995 for the
##' thermodynamic properties of ordinary water substance for general and
##' scientific use.  \emph{Journal of Physical and Chemical Reference Data},
##' 31(2), 387-535.
##' @examples
##' 
##' wvp(t=c(0,5,10))
##' wvp(t=c(0,5,10), s=35)
##' 
##' @export wvp
wvp <-
function(t, s=0, 
		method=c("IAPWS", "BensonKrause", "GreenCarritt","WMO2008", "Antoine", "Weiss", "GoffGratch"),
		sal_method = c("Chabot", "Green", "none")
		) {
	Temp_K <- 273.15 + t	# Most formulas use T in [K]
	                        # Formulas using [C] use the variable t
	method <- match.arg(method)
	switch(method,
		IAPWS = {
				Tc = 647.096				# Temperature at the critical point in °K
				Pc = 22064					# Vapor pressure at the critical point in kPa
				a1 = -7.85951783
				a2 = 1.84408259
				a3 = -11.7866497
				a4 = 22.6807411
				a5 = -15.9618719
				a6 = 1.80122502
				tau = 1 - Temp_K/Tc
				ln_p_pc = (a1*tau + a2*tau^1.5 + a3*tau^3 + a4*tau^3.5 + a5*tau^4 + a6*tau^7.5)*Tc/Temp_K
				ln_p = ln_p_pc + log(Pc)
				wvp0 = exp(ln_p)
		},
		BensonKrause = {
				a1 = 11.8571 
				a2 = 3840.70
				a3 = 216961
			    vp = exp(a1 - (a2/Temp_K) - (a3/Temp_K^2)) 		# in atm
				wvp0 = 101.325 * vp							 	# in kPa 
		},
		GreenCarritt = {
			a1 = 18.1973
			a2 = 373.16
			a3 = 3.1813e-7
			a4 = 26.1205
			a5 = 1.8726e-2
			a6 = 8.03945
			a7 = 5.02802
		    wvp0 <- 101.325 * exp(a1 * (1-a2/Temp_K) + a3 *(1-exp(a4*(1- Temp_K/a2)))
		             - a5 * (1 - exp(a6 * (1-a2/Temp_K))) + a7 *log(a2/Temp_K))
		},
		WMO2008 = {
			a1 = 6.112
			a2 = 17.62
			a3 = 243.12
			vp = a1 * exp(a2 * Temp_K/(a3 + Temp_K))		# hPa
			wvp0 = vp/10									# kPa
		},
		Antoine = {
			# Antoine equation, http://www.watervaporpressure.com = Temp in °C
			a1 = 8.07131
			a2 = 1730.63
			a3 = 233.426
			vp = 10^(a1 - (a2/(a3+t)))		# mm Hg
			wvp0 = vp * 0.1333224			# kPa
		},
		Weiss = {
			a1 = 24.4543
			a2 = 67.4509
			a3 = 4.8489
			vp = a1 - a2*(100/Temp_K) - a3*log(Temp_K/100) 	# in atm
			wvp0 = 101.325 * vp					 			# in kPa
		},
		GoffGratch = {
		    Ts = 373.16       # steam point temperature in K
		    ews   = 1013.246  # saturation pressure at steam point temperature, normal atmosphere
		    a1 = -7.90298
		    a3 = 5.02808
		    a4 = 1.3816*10^-7
		    a5 = 11.344
		    a6 = 8.1328*10^-3
		    a7 = -3.49149
		    log10ew = a1 * (Ts/Temp_K -1) + a3*log10(Ts/Temp_K) - a4*(10^(a5*(1-Temp_K/Ts))) +
		        a6 * (10^(a7*(Ts/Temp_K -1)) -1) + log10(ews)
		    ew = 10^log10ew		# hPa
		    wvp0 = ew/10		# kPa
		}
	)
	# salinity correction
	sal_method = match.arg(sal_method)
	switch(sal_method,		# quality of salinity correction can be checked by comparison with Robinson's data, Water_vapor_pressure_decrease_for_seawater.Rda
		Chabot = {
			a1 = 5.197e-04
			a2 = 1.226e-08
			wvpS = wvp0 * (1 - (a1*s + a2*s^3) )
		},
		Green = {
			a1 = 5.37e-4
			wvpS = wvp0 * (1 - a1 * s)
		},
		none = {
		    wvpS = wvp0
	    }
	)
	wvpS
}
