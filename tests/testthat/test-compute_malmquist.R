# Load data
chnEconomy <- readRDS('../testdata/chnEconomy.RDS')
Grifell_Lovell_1999 <- readRDS('../testdata/Grifell_Lovell_1999.RDS')
results <- readRDS('../testdata/productivity-results.RDS')

test_that('compute_malmquist() works', {

  # --- China Economy 2005-2009 --- #

  # orientation in
  m <- as.numeric(results$chnEconomy$in_vrs$Changes$malmquist)
  tc <- as.numeric(results$chnEconomy$in_vrs$Changes$tech)
  ec <- as.numeric(results$chnEconomy$in_vrs$Changes$effch)
  obtech <- as.numeric(results$chnEconomy$in_vrs$Changes$obtech)
  ibtech <- as.numeric(results$chnEconomy$in_vrs$Changes$ibtech)
  matech <- as.numeric(results$chnEconomy$in_vrs$Changes$matech)
  pech <- as.numeric(results$chnEconomy$in_vrs$Changes$pure.inp.effch)
  sech <- as.numeric(results$chnEconomy$in_vrs$Changes$inp.scalech)

  res <- compute_malmquist(
    data = chnEconomy,
    id = 'dmu',
    time = 'period',
    input = c('labor', 'capital'),
    output = 'giov',
    orientation = 'in'
  )
  res <- as.data.frame(res)
  res <- res[!is.na(res$malmquist),]
  res <- res[order(res$time),]

  expect_equal(res$malmquist, m)
  expect_equal(res$tech, tc)
  expect_equal(res$effch, ec)
  expect_equal(res$obtech, obtech)
  expect_equal(res$ibtech, ibtech)
  expect_equal(res$matech, matech)
  expect_equal(res$pure_effch, pech)
  expect_equal(res$scale_effch, sech)

  # orientation out
  m <- as.numeric(results$chnEconomy$out_vrs$Changes$malmquist)
  tc <- as.numeric(results$chnEconomy$out_vrs$Changes$tech)
  ec <- as.numeric(results$chnEconomy$out_vrs$Changes$effch)
  obtech <- as.numeric(results$chnEconomy$out_vrs$Changes$obtech)
  ibtech <- as.numeric(results$chnEconomy$out_vrs$Changes$ibtech)
  matech <- as.numeric(results$chnEconomy$out_vrs$Changes$matech)
  pech <- as.numeric(results$chnEconomy$out_vrs$Changes$pure.out.effch)
  sech <- as.numeric(results$chnEconomy$out_vrs$Changes$out.scalech)

  res <- compute_malmquist(
    data = chnEconomy,
    id = 'dmu',
    time = 'period',
    input = c('labor', 'capital'),
    output = 'giov',
    orientation = 'out'
  )
  res <- as.data.frame(res)
  res <- res[order(res$time),]
  res <- res[!is.na(res$malmquist),]

  expect_equal(1/res$malmquist, m)
  expect_equal(1/res$tech, tc)
  expect_equal(1/res$effch, ec)
  expect_equal(1/res$obtech, obtech)
  expect_equal(1/res$ibtech, ibtech)
  expect_equal(1/res$matech, matech)
  expect_equal(1/res$pure_effch, pech)
  expect_equal(1/res$scale_effch, sech)

  # --- Grifell-Lovell 1999 --- #

  # orientation in
  m <- as.numeric(results$Grifell_Lovell_1999$in_vrs$Changes$malmquist)
  tc <- as.numeric(results$Grifell_Lovell_1999$in_vrs$Changes$tech)
  ec <- as.numeric(results$Grifell_Lovell_1999$in_vrs$Changes$effch)
  obtech <- as.numeric(results$Grifell_Lovell_1999$in_vrs$Changes$obtech)
  ibtech <- as.numeric(results$Grifell_Lovell_1999$in_vrs$Changes$ibtech)
  matech <- as.numeric(results$Grifell_Lovell_1999$in_vrs$Changes$matech)
  pech <- as.numeric(results$Grifell_Lovell_1999$in_vrs$Changes$pure.inp.effch)
  sech <- as.numeric(results$Grifell_Lovell_1999$in_vrs$Changes$inp.scalech)

  res <- compute_malmquist(
    data = Grifell_Lovell_1999,
    id = 'dmu',
    time = 'year',
    input ='x',
    output = 'y',
    orientation = 'in'
  )
  res <- as.data.frame(res)
  res <- res[order(res$time),]
  res <- res[!is.na(res$malmquist),]

  expect_equal(res$malmquist, m)
  expect_equal(res$tech, tc)
  expect_equal(res$effch, ec)
  expect_equal(res$obtech, obtech)
  expect_equal(res$ibtech, ibtech)
  expect_equal(res$matech, matech)
  expect_equal(res$pure_effch, pech)
  expect_equal(res$scale_effch, sech)

  # orientation out
  m <- as.numeric(results$Grifell_Lovell_1999$out_vrs$Changes$malmquist)
  tc <- as.numeric(results$Grifell_Lovell_1999$out_vrs$Changes$tech)
  ec <- as.numeric(results$Grifell_Lovell_1999$out_vrs$Changes$effch)
  obtech <- as.numeric(results$Grifell_Lovell_1999$out_vrs$Changes$obtech)
  ibtech <- as.numeric(results$Grifell_Lovell_1999$out_vrs$Changes$ibtech)
  matech <- as.numeric(results$Grifell_Lovell_1999$out_vrs$Changes$matech)
  pech <- as.numeric(results$Grifell_Lovell_1999$out_vrs$Changes$pure.out.effch)
  sech <- as.numeric(results$Grifell_Lovell_1999$out_vrs$Changes$out.scalech)

  res <- compute_malmquist(
    data = Grifell_Lovell_1999,
    id = 'dmu',
    time = 'year',
    input ='x',
    output = 'y',
    orientation = 'out'
  )
  res <- as.data.frame(res)
  res <- res[order(res$time),]
  res <- res[!is.na(res$malmquist),]

  expect_equal(1/res$malmquist, m)
  expect_equal(1/res$tech, tc)
  expect_equal(1/res$effch, ec)
  expect_equal(1/res$obtech, obtech)
  expect_equal(1/res$ibtech, ibtech)
  expect_equal(1/res$matech, matech)
  expect_equal(1/res$pure_effch, pech)
  expect_equal(1/res$scale_effch, sech)
})
