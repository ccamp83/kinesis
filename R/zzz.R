.onLoad <- function(libname, pkgname)
{
  assign("kinesis_parameters", new.env(), .GlobalEnv)
  libraries()
}

.onAttach <- function(libname, pkgname)
{
  descriptionfile <- system.file("DESCRIPTION", package = "kinesis")
  descfile <- desc::desc(descriptionfile)

  packageStartupMessage(paste0("#### KINESIS v",
                               installed.packages()['kinesis','Version'],
                               " 'Mambo' | ",
                               descfile$get_field("Date"),
                               " ####"))

  kinesis_parameters$dataCols <- c("subjName","frameN", "time","deltaTime","trialN")
}
