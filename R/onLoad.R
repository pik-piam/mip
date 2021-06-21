# taken from onLoad.R in https://github.com/pik-piam/gdx
.onLoad <- function(libname, pkgname) {
  # Set path to GAMS installation
  delimiter <- ifelse(Sys.info()["sysname"] == "Windows", ";", ":")
  gamspath <- grep("gams", strsplit(Sys.getenv("PATH"), delimiter)[[1]], value = TRUE, ignore.case = TRUE)
  gamspath <- grep("%", gamspath, value = TRUE, invert = TRUE)
  tmp <- NULL
  ok <- FALSE
  sink(textConnection("tmp", "w", local = TRUE)) # nolint
  for (path in gamspath) {
    if (gdxrrw::igdx(path) == 1) {
      ok <- TRUE
      break
    }
  }
  sink() # nolint
  if (!ok) packageStartupMessage(paste(tmp, collapse = "\n"))
}
