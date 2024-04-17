#' Check user provided file
#' @noRd 
check_file <- function(file) {

  check_missing <- sapply(1:nrow(file), function(i) all(is.na(file[i,])))

  error <- FALSE

  if (any(check_missing)) {
    min_miss <- min(which(check_missing))
    if (min_miss == 1 && sum(check_missing) == 1)
      file <- file[-1,]
    else if (min_miss == 1 && sum(check_missing) > 1)
      file <- file[2:(which(check_missing)[2] - 1),]
    else
      file <- file[1:(min_miss - 1),]
    error <- TRUE
  }

  for (i in 1:ncol(file)) {
    if (is.numeric(file[, i]))
      next()
    else if (all(is.na(suppressWarnings(as.numeric(file[, i])))))
      next()
    else
      file[,i] <- suppressWarnings(as.numeric(file[, i]))
  }

  file

}

#' Check if a cell in Excel has a value
#' @noRd 
has_value <- function(x) {
  if (length(x) > 1L) return(sapply(x, has_value, USE.NAMES = FALSE))
  if (is.null(x) || is.na(x)) return(FALSE)
  if (!is.character(x)) x <- as.character(x)
  nzchar(x)
}
