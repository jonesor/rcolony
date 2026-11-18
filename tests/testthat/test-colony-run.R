# End-to-end test against a real COLONY binary. COLONY cannot be shipped with
# the package, so this runs only when RCOLONY_EXEC points at the executable,
# e.g.  Sys.setenv(RCOLONY_EXEC = "~/Software/colony_2.0.7.2/colony2s.out")
# It takes roughly 40 s.

test_that("build -> run -> parse recovers a simulated pedigree with complex inputs", {
  skip_on_cran()
  skip_if_not_installed("withr")
  exec <- Sys.getenv("RCOLONY_EXEC")
  skip_if(exec == "" || !file.exists(exec), "RCOLONY_EXEC not set to a COLONY executable")
  exec <- normalizePath(exec)

  dir <- withr::local_tempdir()
  sim <- simulate_colony_data(file.path(dir, "sim"))
  tr <- sim$truth
  out <- build_input_scripted(sim, dir = dir, run.length = 1, precision = 1)

  # run.colony must leave the caller's working directory alone and write its
  # output next to the DAT file.
  wd0 <- getwd()
  utils::capture.output(ret <- run.colony(exec, out))
  expect_equal(ret, 0)
  expect_equal(getwd(), wd0)
  expect_true(file.exists(file.path(dir, "scripted.Paternity")))

  x <- get.colony.data(paste0(dir, "/"), colonyVersion = "2.0.7.2")

  # Parentage. Truth-based thresholds are deliberately loose: COLONY is
  # stochastic and this checks the pipeline works, not COLONY's accuracy.
  score <- function(assigned, truth) {
    ok <- !is.na(assigned)
    c(rate = mean(ok), accuracy = mean(assigned[ok] == truth[ok]))
  }
  m <- merge(tr, x$paternity, by.x = "offspring", by.y = "OffspringID", all.x = TRUE)
  s <- score(m$InferredDad1, m$father)
  expect_gte(s[["rate"]], 0.9);     expect_gte(s[["accuracy"]], 0.95)
  m <- merge(tr, x$maternity, by.x = "offspring", by.y = "OffspringID", all.x = TRUE)
  s <- score(m$InferredMum1[m$mother.in.candidates], m$mother[m$mother.in.candidates])
  expect_gte(s[["rate"]], 0.9);     expect_gte(s[["accuracy"]], 0.95)

  # Known dyads supplied through the wizard must be respected.
  known <- utils::read.table(sim$files$known.paternal, fill = TRUE, col.names = 1:3,
                             colClasses = "character", na.strings = "")
  known <- known[known$X1 != "0", ]
  p <- x$paternity
  for (i in seq_len(nrow(known))) {
    expect_equal(p$InferredDad1[p$OffspringID == known$X2[i]], known$X1[i])
  }

  # Full-sibship dyads against the true families.
  pairs <- t(utils::combn(tr$offspring, 2))
  same <- tr$family[match(pairs[, 1], tr$offspring)] == tr$family[match(pairs[, 2], tr$offspring)]
  fs <- subset(x$sibs, type == "Full" & Probability >= 0.5)
  key <- paste(pairs[, 1], pairs[, 2])
  inferred <- key %in% c(paste(fs$OffspringID1, fs$OffspringID2),
                         paste(fs$OffspringID2, fs$OffspringID1))
  expect_gte(sum(same & inferred) / sum(inferred), 0.9)   # precision
  expect_gte(sum(same & inferred) / sum(same), 0.8)       # recall
})
