##' Conversion between oxygen concentration units.
##' 
##' Converts between different conversion units (mg, ml, micromol, mmol)
##' 
##' Conversion from one unit of oxygen concentration to another.
##' 
##' @param x A single value or a vector of values to be converted.
##' @param from Oxygen concentration unit of x. Must be one of "mg", "ml",
##' "umol" [for micromol] or "mmol".
##' @param to Desired oxygen concentration unit. Must be one of "mg", "ml",
##' "umol" [for micromol] or "mmol".
##' @return Returns a vector of converted values with same length as the vector
##' x.
##' @author Denis Chabot, Maurice-Lamontagne Institute, DFO, Canada.
##' @references García, H. E. and Gordon, L. I. (1992) Oxygen solubility in
##' seawater: better fitting equations.  \emph{Limnology and Oceanography} 37,
##' 1307-1312.
##' @examples
##' 
##' convO2conc(10)
##' convO2conc(10, from="mg", to="umol")
##' convO2conc(250, from="umol", to="mg")
##' 
##' @export convO2conc
convO2conc <- function(x, from=c("mg", "ml", "umol", "mmol"), to=c("umol", "mg", "ml", "mmol")){
    if (!is.numeric(x)) 
        stop("x must be numeric")    
    from <- match.arg(from)
    to <- match.arg(to)
    switch(from,
        mg = switch(to,
            ml = {
                z = x / 1.42905  # Garcia
            },
            umol = {
                z = x / 0.0319988   # Weast et al. 1986; Masterton et al. 1974; Wikipedia; Benson & Krause 1984
                                    # 31.9988 g / mole = 0.031988 mg / micromole
            },
            mmol = {
                z = x / 31.9988   # 31.9988 mg / mmol
            }),
        ml = switch(to,
            mg = {
                z = x * 1.42905  # Garcia
            },
            umol = {
                z = x / 0.0223916   # Garcia & Gordon 1992 22.3916 l/mol, but Benson & Krause 1980 & 1984 use 22.414
                                    # and Weast uses 0.022292 
            },
            mmol = {
                z = x / 22.3916 
            }),
        umol = switch(to,
            mg = {
                z = x * 0.0319988  # Weast et al. 1986; Benson & Krause 1984
                                   # http://www.helcom.fi/stc/files/CombineManual/PartB/ANNEXB-7.pdf
            },
            ml = {
                z = x * 0.0223916   # Garcia & Gordon 1992 22.3916 l/mol, but Benson & Krause 1980 & 1984 use 22.414
                                    # and Weast uses 0.022292 
            },
            mmol = {
                z = x / 1000 
            }),
        mmol = switch(to,
            mg = {
                z = x * 31.9988              
            },
            ml = {
                z = x * 22.3916 / 1000 
            },
            umol = {
                z = x * 1000 
            })
    )
    z
}
