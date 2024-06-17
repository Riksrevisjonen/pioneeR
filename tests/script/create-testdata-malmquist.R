library(productivity)

# ---- Load datasets ---- #

chnEconomy <- readRDS('tests/testdata/chnEconomy.RDS')
Grifell_Lovell_1999 <- readRDS('tests/testdata/Grifell_Lovell_1999.RDS')

# ---- Helper functions ---- #

create_testdata_mlm <- function(data, input, output, id, time) {
  orientations <- c('in', 'out')
  rts_ <- c('crs', 'vrs', 'nirs', 'ndrs')
  dl <- list()
  for (a in seq_along(orientations)) {
    for (b in seq_along(rts_)) {
      tmp <- malm(
        data,
        id.var = id,
        time.var = time,
        orientation = orientations[a],
        rts = rts_[b],
        x.vars = input,
        y.vars = output,
        scaled = FALSE
      )
      tmp <- list(tmp)
      names(tmp) <- paste0(orientations[a], '_', rts_[b])
      dl <- append(dl, tmp)
    }
  }
  dl
}

# ---- Create testdata ---- #

results <- list(
  'chnEconomy' = create_testdata_mlm(
    chnEconomy, id = 'dmu', time = 'period',
    input = c('labor', 'capital'), output = 'giov'),
  'Grifell_Lovell_1999' = create_testdata_mlm(
    Grifell_Lovell_1999, id = 'dmu', time = 'year',
    input = 'x', output = 'y')
)
saveRDS(results, 'tests/testdata/productivity-results.RDS')
