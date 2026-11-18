# These tests parse real COLONY 2.0.7.2 output (fixtures/colony-2.0.7.2, from a
# run on simulated data; see helper-simulate.R) and need no COLONY binary.
fixture <- function() paste0(test_path("fixtures", "colony-2.0.7.2"), "/")

test_that("get.colony.data parses 2.0.7.2 output, including the default filename", {
  # Default `filename` must pick the .DAT file and not e.g. *.Data files.
  x <- get.colony.data(fixture(), colonyVersion = "2.0.7.2")
  truth <- utils::read.csv(file.path(fixture(), "truth.csv"))
  expect_equal(nrow(x$offspring), nrow(truth))          # no spurious Unknown row
  expect_false("Unknown" %in% x$offspring$offspringID)
  expect_true("Unknown" %in% x$fathers$fatherID)          # parent tables keep it
  expect_true(all(c("maternity", "paternity", "pairwise.maternity",
                    "pairwise.paternity", "sibs", "pairwise.sibs") %in% names(x)))
  # comma-delimited parentage files and '#'-containing pairwise headers
  expect_named(x$paternity, c("OffspringID", "InferredDad1", "ProbDad1"))
  expect_true(all(c("OffspringID", "CandidateID", "Confidence") %in% names(x$pairwise.paternity)))
  expect_gt(ncol(x$pairwise.paternity), 3)
})

test_that("all accepted COLONY 2.0.7.x version strings give the same parse", {
  a <- get.colony.data(fixture(), colonyVersion = "2.0.7")
  for (v in c("2.0.7.1", "2.0.7.2")) {
    expect_equal(get.colony.data(fixture(), colonyVersion = v), a)
  }
  expect_error(get.colony.data(fixture(), colonyVersion = "9.9"), "only works")
})

test_that("a wrong colonyVersion fails loudly rather than returning garbage", {
  expect_error(suppressWarnings(get.colony.data(fixture(), colonyVersion = "2.0")))
})

test_that("get.parentage counts every offspring exactly once", {
  x <- get.colony.data(fixture(), colonyVersion = "2.0.7.2")
  truth <- utils::read.csv(file.path(fixture(), "truth.csv"))
  p <- get.parentage(x)
  expect_equal(sum(p$Freq), nrow(truth))
  # pairwise table: same number of offspring accounted for
  pw <- get.parentage(x, pairwise = TRUE)
  expect_equal(sum(pw$Freq), nrow(truth))
})

test_that("get.parentage and plotsibs tolerate objects with the old 'Unknown' offspring row", {
  data(testdata, package = "rcolony", envir = environment())
  expect_equal(sum(get.parentage(testdata)$Freq), nrow(testdata$offspring) - 1)
  grDevices::pdf(NULL); on.exit(grDevices::dev.off())
  expect_no_error(plotsibs(testdata, pairwise = FALSE))
})
