# Load packages
library(shiny)
library(bslib)
source('utils.R')

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

checkFile <- function(import) {

  check.missing <- sapply(1:nrow(import), function(i) all(is.na(import[i,])))

  error <- FALSE

  if (any(check.missing)) {
    min.miss <- min(which(check.missing))
    if (min.miss == 1 && sum(check.missing) == 1)
      import <- import[-1,]
    else if (min.miss == 1 && sum(check.missing) > 1)
      import <- import[2:(which(check.missing)[2] - 1),]
    else
      import <- import[1:(min.miss - 1),]
    error <- TRUE
  }

  for (i in 1:ncol(import)) {
    if (is.numeric(import[, i]))
      next()
    else if (all(is.na(suppressWarnings(as.numeric(import[, i])))))
      next()
    else
      import[,i] <- as.numeric(import[, i])
  }

  nums <- sapply(import, is.numeric)
  cols <- colnames(import[, nums])

  return(list(file = import, info = dim(import), cols = cols, error = error))

}

fileImport <- function(file, sheet = NULL, range = NULL, dec.point = NULL, stringsAsFactors = FALSE,
                       header = TRUE, fileEncoding = 'UTF-8') {

  if (!file.exists(file)) stop('Could not locate file with path ', file)

  ext <- tools::file_ext(file)

  import <- switch(
    ext,
    'xlsx' = as.data.frame(readxl::read_xlsx(file, sheet = sheet, range = range)),
    'xls' = as.data.frame(readxl::read_xls(file, sheet = sheet, range = range)),
    'dta' = as.data.frame(haven::read_dta(file)),
    'csv' = {
      opts <- list(file = file, stringsAsFactors = stringsAsFactors, header = header,
                   fileEncoding = fileEncoding)
      if (dec.point == '.' || is.null(dec.point))
        tryCatch({ do.call(read.csv, args = opts) }, error = function(e) { return(NULL) })
      else if (dec.point == ',')
        tryCatch({ do.call(read.csv2, args = opts) }, error = function(e) { return(NULL) })
      else
        NULL
    },
    'dta' = haven::read_dta(file),
    'rds' = {
      i <- readRDS(file) %>% as.data.frame()
    }
  )

  if (is.null(import))
    return(list(file = NULL, info = NULL, cols = NULL, error = TRUE))

  checkFile(import)

}
