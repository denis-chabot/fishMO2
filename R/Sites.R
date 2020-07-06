##' List of lat-long for known sites.
##' 
##' List containing geographic position (latitude and longitude, in decimal
##' degrees) of sites where respirometry data were available to test this
##' package.
##' 
##' Can be used to specify location in \code{drawNights}. Latitude and
##' longitude are in decimal degrees.
##' 
##' @return A list with 6 elements (i.e. locations). For each element, lat and
##' long (in decimal degrees) are provided as a vector with 2 elements:
##' 
##' \item{La_Rochelle}{La Rochelle, France.} \item{Sete}{Sete, France.}
##' \item{IML}{IML (Institut Maurice-Lamontagne, Maurice Lamontagne Institute),
##' Fisheries and Oceans, QC, Canada.} \item{Helsingor}{Helsingør, Danemark.}
##' \item{Brest}{Brest, France.} \item{Vancouver}{Vancouver, BC, Canada.}
##' @examples
##' 
##' Sites$IML
##' 
##' @export Sites
Sites = list(La_Rochelle = c(46.162, -1.152), 
             Sete = c(43.4018, 3.6966), 
             IML = c(48.5833, -69.2), 
             Helsingor = c(56.0361, 12.6136), 
             Brest = c(48.39972, -4.483056), 
             Vancouver = c(49.24944, -123.1192)
)
