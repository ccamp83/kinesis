.onLoad <- function(libname, pkgname)
{
  assign("kinesis_parameters", new.env(), .GlobalEnv)
  libraries()
}

.onAttach <- function(libname, pkgname)
{
  packageStartupMessage("#### KINESIS v 4.3 - 10 Dec 2022 ####")

  kinesis_parameters$dataCols <- c("subjName","frameN", "time","deltaTime","trialN")
}
