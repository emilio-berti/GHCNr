.onLoad <- function(libname, pkgname) {
    packageStartupMessage(
        "This is GHCNr ",
        utils::packageDescription("GHCNr", fields="Version"),
        appendLF = TRUE
    )
}
