#' Load and update all the required libraries of the kinesis package
#' @param update if TRUE, the function also updates the libraries to newest version (FALSE by default)
#' @export
libraries <- function(update=F)
{
  # retrieve the list of installed packages using installed.packages()
  packages <- levels(as.data.frame(installed.packages())$Package)

  # list of the required packages
  reqPackages <- c(
    "lattice",
    "Hmisc",
    "plyr",         # arrange dataset
    "ggplot2",      # generate plots
    # "fields",       # sreg function for smoothing
    # "gridExtra",    # generate plots
    # "grid",         # generate plots
    "rgl",          # opengl - ubuntu 14.04 requires "sudo apt-get install libX11-dev freeglut3 freeglut3-dev libxml2-dev" to install this package
    "roxygen2",     # generate package
    # "parallel",     # parallel computing
    "signal"        # butterworth filter
  )

  # check which of the required libraries have to be installed
  toBeInstalled <- reqPackages[reqPackages %in% packages==F]

  # install missing packages (if existing)
  if(length(toBeInstalled)!=0)
  {
    install.packages(toBeInstalled, repos = "https://cloud.r-project.org/")
  }

  # update packages if requested by user
  if(update)
  {
    update.packages(repos = "https://cloud.r-project.org/")
  }

  # load the required libraries
  for(i in 1:length(reqPackages))
  {
    library(reqPackages[i], character.only=T)
  }
}
