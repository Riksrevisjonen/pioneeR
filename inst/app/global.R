# Load packages
library(shiny)
library(bslib)
source('R/utils.R')

# Set Bootstrap version
bs_ver <- 5

enableBookmarking(store = 'server')

# Check if we're running locally or on a server
is_local <- !nzchar(Sys.getenv('SHINY_PORT'))

checkBalance <- function(df, id.var, time.var) {

  units <- unique(df[, id.var])
  time <- unique(df[, time.var])

  miss <- sapply(units, function(u) {
    unit.time <- unique(df[df[,id.var] == u, time.var])
    all(sapply(time, function(t) t %in% unit.time))
  })

  r <- list()

  if (!all(miss)) {
    r$data <- df[df[,id.var] %in% units[miss],]
    r$listwise <- TRUE
    r$message <- 'Data was not balanced, listwise deleting has been performed'
  } else {
    r$data <- df
    r$listwise <- FALSE
    r$message <- NULL
  }

  return(r)

}
