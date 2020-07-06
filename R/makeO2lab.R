##' Make a formatted label for oxygen uptake (MO2), SDA or oxygen
##' concentration.
##'
##' Plot labels involving oxygen usually require subscript, superscript, italic
##' or dot formatting. This function facilitates the creation of such labels.
##'
##' The combination of R language and Tex commands required to obtain complex
##' labels can be difficult to master (and remember). This single function
##' allows the user to format a wide variety of labels dealing with MO2, SDA,
##' oxygen concentration to produce publication-quality figures.
##'
##' @param mo2 If the label is for oxygen uptake (see Type), selects the
##' formatting of the expression MO2, such as \verb{dotital} (adds a dot on
##' italics M to express a rate), \verb{dot}, \verb{ital} or \verb{plain}. The
##' expression MO2 can also by \verb{hidden}. Not used for SDA or concentration
##' labels.
##' @param o2 Oxygen units, either \verb{mg}, \verb{ug} (gives microg),
##' \verb{mmol}, \verb{umol} (gives micromol), \verb{ml} or \verb{mL}.
##' @param t Time unit, either \verb{hr} or \verb{min}.
##' @param m Mass unit for the animal, either \verb{kg}, \verb{g} or \verb{mg}.
##' Used for mass-specific MO2 or SDA, not used in "per individual"
##' measurements.
##' @param v Volume unit when dealing with oxygen concentration.
##' @param showO2 Logical, to hide or show \verb{O2} after the oxygen unit.
##' @param Type Select the type of label, either for mass-specific MO2
##' (\verb{MO2msp}), individual MO2 (\verb{MO2ind}), mass-specific SDA
##' magnitude (\verb{SDAmsp}), individual SDA magnitude (\verb{SDAind}) or
##' oxygen concentration (\verb{conc}).
##' @return Returns the formatted label.
##' @author Denis Chabot, Institut Maurice-Lamontagne, Fisheries and Oceans
##' Canada.
##' @examples
##'
##' # for a plot of MO2 as a function of time
##' mo2lab = makeO2lab(mo2="dotital", o2= "mg", t="hr", m="kg", v="l", showO2 = FALSE, Type="MO2msp")
##' plot(1:10, xlab="Time", ylab=mo2lab, mgp=c(2,0.5,0))
##'
##' @export makeO2lab
makeO2lab = function (mo2 = c("dotital", "dot", "ital", "plain", "hidden"),
                      o2 = c("mg", "ug", "mmol", "umol", "ml", "mL"),
                      t = c("hr", "min"),
                      m = c("kg", "g", "mg"),
                      v = c("l", "L"),
                      showO2 = F,
                      Type = c("MO2msp", "MO2ind", "SDAmsp", "SDAind", "conc"))
{
    mo2 <- match.arg(mo2)
    switch(mo2, plain = {
        labmo2 = quote(M[O[2]])
    }, ital = {
        labmo2 = quote(italic(M)[O[2]])
    }, dot = {
        labmo2 = quote(dot(M)[O[2]])
    }, dotital = {
        labmo2 = quote(italic(dot(M))[O[2]])
    }, hidden = {
        labmo2 = ""
    }
    )
    o2 <- match.arg(o2)
    switch(o2, mg = {
        labo2 = "mg"
    }, ug = {
        labo2 = quote(mu * "g")
    }, mmol = {
        labo2 = "mmol"
    }, umol = {
        labo2 = quote(mu * "mol")
    }, ml = {
        labo2 = "ml"
    }, mL = {
        labo2 = "mL"
    })
    if (showO2) {
        Type <- match.arg(Type)
        switch(Type, MO2ind = {
            Lab = substitute(a ~ "(" * b ~ O[2] ~ c^-1 * ")",
                             list(a = labmo2, b = labo2, c = t))
        }, MO2msp = {
            Lab = substitute(a ~ "(" * b ~ O[2] ~ c^-1 ~ d^-1 *
                                 ")", list(a = labmo2, b = labo2, c = t, d = m))
        }, SDAind = {
            Lab = substitute("SDA (" * b ~ O[2] * ")", list(b = labo2))
        }, SDAmsp = {
            Lab = substitute("SDA (" * b ~ O[2] ~ d^-1 * ")",
                             list(b = labo2, d = m))
        }, conc = {
            Lab = substitute("DO (" * b ~ O[2] ~ e^-1 * ")",
                             list(b = labo2, e = v))
        })
    }
    else {
        Type <- match.arg(Type)
        switch(Type, MO2ind = {
            Lab = substitute(a ~ "(" * b ~ c^-1 * ")", list(a = labmo2,
                                                            b = labo2, c = t))
        }, MO2msp = {
            Lab = substitute(a ~ "(" * b ~ c^-1 ~ d^-1 * ")",
                             list(a = labmo2, b = labo2, c = t, d = m))
        }, SDAind = {
            Lab = substitute("SDA (" * b * ")", list(b = labo2))
        }, SDAmsp = {
            Lab = substitute("SDA (" * b ~ d^-1 * ")", list(b = labo2,
                                                            d = m))
        }, conc = {
            Lab = substitute("DO (" * b ~ e^-1 * ")", list(b = labo2,
                                                           e = v))
        })
    }
    Lab
}
