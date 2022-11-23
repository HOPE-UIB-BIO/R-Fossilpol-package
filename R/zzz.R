.onAttach <- function(lib, pkg)  {
    packageStartupMessage("RFossilpol loaded - version ",
                          utils::packageDescription("RFossilpol",
                                                    fields="Version"),
                          appendLF = TRUE)
}