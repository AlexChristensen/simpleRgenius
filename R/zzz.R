.onload <- function(libname, pkgname)
{
    library.dynam("simpleRgenius", package = pkgname, lib.loc = libname)
}

.onAttach <- function(libname, pkgname)
{
    msg <- paste("To get started, set up your Genius API token: <https://tinyurl.com/Genius-API-Setup>")
    
    packageStartupMessage(msg)
}
