.kinesis_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname)
{
  .kinesis_env$dataCols <- c("subjName","frameN", "time","deltaTime","trialN")
  libraries()
}

.onAttach <- function(libname, pkgname)
{
  descriptionfile <- system.file("DESCRIPTION", package = pkgname)
  descfile <- desc::desc(descriptionfile)
  ver <- tryCatch(
    as.character(utils::packageVersion(pkgname)),
    error = function(e) "unknown"
  )
  packageStartupMessage(paste0("#### ", pkgname, " v",
                               ver,
                               " 'Mambo' | ",
                               descfile$get_field("Date"),
                               " ####"))
}
