##' Show night time on an existing plot.
##'
##' Add shaded areas as background on an existing plot where the X-axis is
##' time, to indicate night time. This function relies on package
##' \verb{StreamMetabolism} to obtain the time of sunrise and sunset for each
##' day shown on the plot. Normally, this function is not called directly by
##' the user, but is used by other functions in the package.
##'
##' This function can only be called after the user has produced a plot AND the
##' plot's X-axis is in POSIXct time units or in hours. It adds shading for the
##' parts of the plot that represent night-time. This only makes sense for
##' plots with a X-axis representing a sufficient number of hours to have both
##' day-time and night-time represented on the plot.
##'
##' The locations used by the author are hard-coded in the package (see
##' argument Site), as well as the time zones corresponding to these sites
##' (argument TimeZone). Any Site and TimeZone can be used, but the user must
##' enter the lat-long info for Site and the time zone argument as required by
##' other date-time objects in R.
##'
##' @param startD First day in dataset, in Date format.
##' @param endD Last day in dataset, in Date format.
##' @param Site Numeric vector with 2 values, latitude and longitude of the
##' site where the respirometry experiment took place. Positions in decimal
##' degrees, i.e., 46.162 degrees. Longitudes West shown as negative numbers.
##' Four positions are pre-defined (IML, La_Rochelle, Sète, Helsingor) and can
##' be called by name.
##' @param TimeZone Time zone (using same conventions as in \verb{strftime}),
##' for the site where the respirometry experiment took place. Some time zones
##' are pre-defined: IMLcivil, EuropeTZ, IMLst, EuropeTZst. The first two
##' switch to daylight time in summer, the last two remain on standard time
##' year-round).
##' @param coords Coordinates of the 4 corners of the existing plot, obtained
##' by doing coords =par("usr") before calling this function.
##' @param XunitsOut Type of X coordinates to output towards the existing plot.
##' \verb{POSIXct} by default, the alternative choice is \verb{hours}, when the
##' X-axis of the plot is in hours relative to the beginning of the experiment
##' or to a specific moment, such as the time of feeding.
##' @param zero The POSIXct date-time corresponding to "hour zero" on the plot.
##' Only used if XunitsOut is \verb{hours}.
##' @param night.col The choice of colour for the shading representing night
##' time. By default a very light and partially transparent grey specified in
##' RGB, but can be replaced by any colour by the user.
##' @return None, draws shaded rectangles representing night-time on an
##' existing plot with time on the X-axis.
##' @section Warning : By default, the shading is semi-transparent (as defined
##' by \verb{rgb} with an alpha parameter for transparency). Some graphics
##' devices do not support transparency (e.g., \verb{postscript}. In this case,
##' and in any case where \verb{night.col} is not transparent, any data point
##' occurring at night and already drawn on the plot will be obscured by the
##' shading. In this case, draw the plot's frame but not the data points, then
##' draw the night-shading, and only then add the data points (i.e. after
##' calling \verb{drawNights}).
##'
##' Time zones can be an issue when analyzing data with a computer located in a
##' different time zone than where the data were collected. R accounts
##' correctly for different time zones but what you get is the data displayed
##' correctly for your time zone. So it could be day-time for the fish, but
##' night-time for you, and it is your time that prevails, unless you take
##' precautions. See what happens when you go through the examples with codSDA
##' dataset and you remove the line
##' @author Denis Chabot, Institut Maurice-Lamontagne, Department of Fisheries
##' and Oceans.
##' @seealso See Also as \code{\link{rgb}}, \code{\link{sunrise.set}},
##' \code{\link{timezones}}
##' @examples
##'
##' StartD = as.POSIXct("2016-05-05 07:00", tz=myTZs$FRcivil)
##' EndD = as.POSIXct("2016-05-12 08:00", tz=myTZs$FRcivil)
##' Y = 1:5
##' X = seq(StartD, EndD, length.out = 5)
##' plot(Y~X)
##' coords =par("usr")
##' drawNights(StartD, EndD, Site=Sites$La_Rochelle, TimeZone=myTZs$FRcivil, coords=coords)
##'
##' # with real data
##' data(codSDA)  # data collected at Institut Maurice-Lamontagne
##' plotMO2(codSDA$DateTime, codSDA$MO2cor, mgp=c(2,0.5,0))
##' StartD = codSDA$DateTime[1]
##' EndD = codSDA$DateTime[nrow(codSDA)]
##' Coords = par("usr")
##' drawNights(StartD, EndD, Site=Sites$IML, TimeZone=myTZs$IMLst, coords=Coords)
##'
##' # this is pretty but could be wrong if your computer's time zone is not "EST"
##' # we want to see this in the time zone where the data were recorded
##'
##' attr(codSDA$DateTime, "tzone") = "EST"
##' plotMO2(codSDA$DateTime, codSDA$MO2cor, mgp=c(2,0.5,0))
##' StartD = codSDA$DateTime[1]
##' EndD = codSDA$DateTime[nrow(codSDA)]
##' Coords = par("usr")
##' drawNights(StartD, EndD, Site=Sites$IML, TimeZone=myTZs$IMLst, coords=Coords)
##' # notice how the night-time straddles "midnight" when we force the time zone to be "EST"
##'
##' @export drawNights
drawNights <- function(startD, endD, Site=Sites$IML, TimeZone=myTZs$IMLst, coords,
                       XunitsOut="POSIXct", zero=NA, night.col=rgb(0,0,0,0.08)) {
# You need the suggested package for this function
if (!requireNamespace("StreamMetabolism", quietly = TRUE)) {
            stop("Package \"StreamMetabolism\" needed for this function to work. Please install it.",
                 call. = FALSE)
        }

    nbDays = as.integer(endD - startD) + 2      # sunrise.set seems to start the day before my start date,
                                                # so I must add one day, then start date, then diff between start and end
	pp <- StreamMetabolism::sunrise.set(Site[1], Site[2], as.character(startD-1), timezone=TimeZone, nbDays)
	if (nrow(pp)>1 ) {
		lights_on = pp$sunrise[2:nrow(pp)]
		lights_off = pp$sunset[1:(nrow(pp)-1)]
#         Xcoords = as.POSIXct(coords[1:2], origin="1970-01-01", tz=TZone)
		if(XunitsOut=="POSIXct") {
			for(m in 1:length(lights_off)) {
				x = c(lights_off[m], lights_off[m], lights_on[m], lights_on[m])  # this uses local time zone!
                attr(x, "tzone") <- TimeZone  # new line added 2014-09-21
				y = c(coords[3], coords[4], coords[4], coords[3])
				polygon(x, y, col=night.col, border=NA)
				}  # end for m
		} else if (XunitsOut=="hours"){
			lights_off2 = as.numeric(difftime(lights_off, zero, units="hours"))
			lights_on2 = as.numeric(difftime(lights_on, zero, units="hours"))
			for(m in 1:length(lights_off2)) {
				x = c(lights_off2[m], lights_off2[m], lights_on2[m], lights_on2[m])
				y = c(coords[3], coords[4], coords[4], coords[3])
				polygon(x, y, col=night.col, border=NA)
				}  # end for m
		} # end XunitsOut
	} # end for if nrow
} # end function
