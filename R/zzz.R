.onLoad <- function(libname, pkgname)
{
  assign("kinesis_parameters", new.env(), .GlobalEnv)
  libraries()
}

.onAttach <- function(libname, pkgname)
{
  packageStartupMessage("#### KINESIS v 4.9 - 3 Aug 2023 ####")

  kinesis_parameters$dataCols <- c("subjName","frameN", "time","deltaTime","trialN")
}
