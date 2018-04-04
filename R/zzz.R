.onLoad <- function(libname, pkgname)
{
  assign("kinesis_parameters", new.env(), .GlobalEnv)
}

.onAttach <- function(libname, pkgname)
{
  packageStartupMessage("#### KINESIS ####")

  kinesis_parameters$dataCols <- c("subjName","frameN", "time","deltaTime","trialN")
}
