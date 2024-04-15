# ---- Load datasets ---- #

# Simple example (taken from https://pystoned.readthedocs.io/en/latest/examples/DEA/dea_ref.html)
x <- as.matrix(c(100, 200, 300, 500, 100, 200, 600, 400, 550, 600))
y <- as.matrix(c(75, 100, 300, 400, 25, 50, 400, 260, 180, 240))
xref <- as.matrix(c(100, 300, 500, 100, 600))
yref <- as.matrix(c(75, 300, 400, 25, 400))

# Frontier 4.1
frontier41 <- readRDS('tests/testdata/frontier41.RDS')
f41_x <- as.matrix(frontier41[c('capital', 'labour')])
f41_y <- as.matrix(frontier41[c('output')])

# Norwegian District Courts
norCourts2018 <- readRDS('tests/testdata/norCourts2018.RDS')
nc_x <- as.matrix(norCourts2018[c('judges', 'case_workers', 'costs')])
nc_y <- as.matrix(norCourts2018[c(
  'civil_cases', 'criminal_case_single',
  'criminal_case_full_bench',
  'other_civil_cases')])

# Electric Plants
electricPlants <- readRDS('tests/testdata/electricPlants.RDS')
ecp_x <- as.matrix(electricPlants[c('labor', 'fuel', 'capital')])
ecp_y <- as.matrix(electricPlants[c('output')])

# Hospitals
hospitals <- readRDS('tests/testdata/hospitals.RDS')
hospitals_x <- as.matrix(hospitals[c('labor', 'capital')])
hospitals_y <- as.matrix(hospitals[c('inpatients', 'outpatients')])


# ---- Helper functions ---- #

#' Create DEA test data (using Benchmarking)
#' @param x A matrix with input values.
#' @param y A matrix with output values.
create_testdata <- function(x, y, xref = NULL, yref = NULL, ids) {
  orientations <- c('in', 'out')
  rts_ <- c('crs', 'vrs', 'drs', 'irs')
  dl <- list()
  for (a in seq_along(orientations)) {
    for (b in seq_along(rts_)) {
      tmp <- create_testdata_single(
        x, y, xref, yref, ids,
        rts = rts_[b],
        orientation = orientations[a])
      tmp <- list(tmp)
      names(tmp) <- paste0(orientations[a], '_', rts_[b])
      dl <- append(dl, tmp)
    }
  }
  dl
}

#' Create DEA test data (single)
#' @param x A matrix with input values.
#' @param y A matrix with output values.
#' @param rts Returns to scale
#' @param orientation Model orientation
create_testdata_single <- function(x, y, xref, yref, ids, rts, orientation) {
  eff <- Benchmarking::dea(x, y, XREF = xref, YREF = yref, RTS = rts, ORIENTATION = orientation)
  super_eff <- Benchmarking::sdea(x, y, RTS = rts, ORIENTATION = orientation)
  slack <- Benchmarking::slack(x, y, e = eff)
  colnames(eff$lambda) <- ids
  peers <- Benchmarking::peers(eff, NAMES = TRUE)

  dimnames(eff$lambda) <- NULL
  dimnames(super_eff$lambda) <- NULL
  dimnames(slack$lambda) <- NULL

  dl <- list(efficiency = list(eff = eff$eff, lambda = eff$lambda, objval = eff$objval, peers = peers),
             super_efficiency = list(eff = super_eff$eff, lambda = super_eff$lambda, objval = super_eff$objval),
             slack = list(eff = slack$eff, lambda = slack$lambda, objval = slack$objval, sum = slack$sum)
  )
  dl
}


# ---- Create testdata ---- #

results <- list(
  'simple' = create_testdata(x, y, xref, yref, ids = rownames(x)),
  'frontier41' = create_testdata(f41_x, f41_y, ids = frontier41$firm),
  'norCourts2018' = create_testdata(nc_x, nc_y, ids = norCourts2018$district_court),
  'electricPlants' = create_testdata(ecp_x, ecp_y, ids = electricPlants$plant),
  'hospitals' = create_testdata(hospitals_x, hospitals_y, ids = hospitals$firm_id)
)
saveRDS(results, 'tests/testdata/benchmarking-results.RDS')
