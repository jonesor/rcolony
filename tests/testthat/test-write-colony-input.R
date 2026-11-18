# Compare write.colony.input() with the output of the interactive wizard
# (reference files in fixtures/wizard-golden, produced by build.colony.input()
# from the same simulated data), and test its checks directly. No COLONY
# binary is needed.

# Strip comments, blank lines and extra white space, so that only the content
# that COLONY reads is compared.
dat_content <- function(path) {
  x <- readLines(path)
  x <- sub("!.*$", "", x)
  x <- gsub("[[:space:]]+", " ", trimws(x))
  x <- x[x != ""]
  # compare numbers as numbers, so 0.0590 and 0.059 are the same
  vapply(strsplit(x, " "), function(tok) {
    num <- suppressWarnings(as.numeric(tok))
    paste(ifelse(is.na(num), tok, format(num, digits = 10)), collapse = " ")
  }, "")
}

sim_in <- function(dir) simulate_colony_data(file.path(dir, "sim"))

# The core call corresponding to each wizard scenario in helper-scenarios.R
core_args <- list(
  A = function(f) list(
    father.prob = 0.8, mother.prob = 0.8, known.paternal = f$known.paternal,
    known.maternal = f$known.maternal, paternity.threshold = 2,
    maternity.threshold = 2, excluded.paternities = f$excluded.paternity,
    sibship.prior = "weak", mean.sibship.size = c(2, 2),
    run.length = "medium", precision = "high", seed = 1234),
  B = function(f) list(father.prob = 0.8, mother.prob = 0.8, seed = 1234),
  C = function(f) list(
    father.prob = 0.5, mother.prob = 0.3, known.paternal = f$known.paternal,
    known.maternal = f$known.maternal, paternity.threshold = 0,
    maternity.threshold = 0, excluded.paternities = f$excluded.paternity,
    excluded.maternities = f$excluded.maternity,
    excluded.paternal.sibships = f$excluded.paternal.sibships,
    excluded.maternal.sibships = f$excluded.maternal.sibships,
    allele.freq = f$allele.freq, update.allele.freq = TRUE,
    species = "monoecious", inbreeding = TRUE, ploidy = "haplodiploid",
    male.monogamy = TRUE, female.monogamy = FALSE, clone.inference = TRUE,
    scale.sibship.size = FALSE, sibship.prior = "optimal",
    mean.sibship.size = c(3, 4), n.runs = 3, run.length = "long",
    monitor.by = "seconds", monitor.interval = 60, platform = "windows",
    likelihood = "pairwise", precision = "low", seed = 98765)
)

write_core <- function(sim, dir, scenario, ...) {
  f <- sim$files
  args <- c(list(file = file.path(dir, "core.DAT"), offspring = f$offspring,
                 markers = f$markers, fathers = f$fathers, mothers = f$mothers,
                 dataset.name = "scripted", output.name = "scripted"),
            core_args[[scenario]](f))
  do.call(write.colony.input, args)
}

test_that("write.colony.input reproduces the wizard's file for each scenario", {
  skip_if_not_installed("withr")
  for (sc in names(core_args)) {
    dir <- withr::local_tempdir()
    sim <- sim_in(dir)
    out <- write_core(sim, dir, sc)
    golden <- test_path("fixtures", "wizard-golden", paste0(sc, ".DAT"))
    expect_equal(dat_content(out), dat_content(golden), info = paste("scenario", sc))
  }
})

test_that("data frames, matrices and file paths are interchangeable", {
  skip_if_not_installed("withr")
  dir <- withr::local_tempdir()
  sim <- sim_in(dir); f <- sim$files
  rd <- function(p) utils::read.table(p, header = FALSE, colClasses = "character", fill = TRUE, col.names = 1:45, na.strings = "")
  a <- write.colony.input(file.path(dir, "a.DAT"), offspring = f$offspring, markers = f$markers,
                          fathers = f$fathers, mothers = f$mothers, father.prob = 0.8, mother.prob = 0.8,
                          known.paternal = f$known.paternal, paternity.threshold = 2,
                          project.info = FALSE)
  b <- write.colony.input(file.path(dir, "b.DAT"),
                          offspring = as.matrix(rd(f$offspring)[, 1:41]),
                          markers = rd(f$markers)[, 1:20],
                          fathers = rd(f$fathers)[, 1:41], mothers = rd(f$mothers)[, 1:41],
                          father.prob = 0.8, mother.prob = 0.8,
                          known.paternal = rd(f$known.paternal)[, 1:3], paternity.threshold = 2,
                          dataset.name = "a", project.info = FALSE)
  expect_equal(dat_content(a), dat_content(b))
})

test_that("allele frequencies can be given as a list", {
  skip_if_not_installed("withr")
  dir <- withr::local_tempdir()
  sim <- sim_in(dir); f <- sim$files
  tab <- readLines(f$allele.freq)
  ids <- strsplit(tab[c(TRUE, FALSE)], " "); fr <- strsplit(tab[c(FALSE, TRUE)], " ")
  lst <- Map(function(i, v) stats::setNames(as.numeric(v), i), ids, fr)
  a <- write.colony.input(file.path(dir, "a.DAT"), f$offspring, f$markers, allele.freq = f$allele.freq, project.info = FALSE)
  b <- write.colony.input(file.path(dir, "b.DAT"), f$offspring, f$markers, allele.freq = lst, dataset.name = "a", project.info = FALSE)
  expect_equal(dat_content(a), dat_content(b))
})

test_that("write.colony.input writes the project information file only when asked", {
  skip_if_not_installed("withr")
  dir <- withr::local_tempdir()
  sim <- sim_in(dir); f <- sim$files
  out <- write.colony.input(file.path(dir, "x.DAT"), f$offspring, f$markers, note = "hello")
  info <- file.path(dir, "x_ProjectInformation.txt")
  expect_true(file.exists(info))
  expect_match(paste(readLines(info), collapse = "\n"), "NOTE to the project : hello")
  write.colony.input(file.path(dir, "y.DAT"), f$offspring, f$markers, project.info = FALSE)
  expect_false(file.exists(file.path(dir, "y_ProjectInformation.txt")))
})

test_that("write.colony.input rejects bad input and writes nothing", {
  skip_if_not_installed("withr")
  dir <- withr::local_tempdir()
  sim <- sim_in(dir); f <- sim$files
  out <- file.path(dir, "bad.DAT")
  base <- list(file = out, offspring = f$offspring, markers = f$markers,
               fathers = f$fathers, father.prob = 0.8)
  bad <- function(..., pattern) {
    expect_error(do.call(write.colony.input, utils::modifyList(base, list(...))), pattern)
    expect_false(file.exists(out))
  }
  bad(father.prob = NULL, pattern = "father.prob")
  bad(father.prob = 1.5, pattern = "between 0 and 1")
  bad(offspring = utils::read.table(f$offspring, colClasses = "character")[, 1:10], pattern = "columns")
  bad(markers = f$fathers, pattern = "4 rows")
  bad(known.paternal = list("x"), pattern = "data frame")
  bad(known.paternal = data.frame(a = "M999", b = "O1"), paternity.threshold = 1, pattern = "not among the candidates")
  bad(known.paternal = data.frame(a = "M1", b = "NOPE"), pattern = "not in `offspring`")
  bad(known.paternal = data.frame(a = "M1", b = "O1"), paternity.threshold = 99, pattern = "threshold")
  bad(excluded.paternities = data.frame(a = "O1", b = "M999"), pattern = "not in the data")
  bad(sibship.prior = "weak", pattern = "mean.sibship.size")
  bad(dataset.name = "has space", pattern = "without spaces")
  bad(output.name = strrep("a", 21), pattern = "20 characters")
  bad(version = "2.0.5", pattern = "2.0.6.3")
  bad(seed = 1.5, pattern = "whole number")
  expect_error(write.colony.input(out, markers = f$markers), "offspring")
})

test_that("write.colony.input does not overwrite unless asked", {
  skip_if_not_installed("withr")
  dir <- withr::local_tempdir()
  sim <- sim_in(dir); f <- sim$files
  out <- file.path(dir, "o.DAT")
  write.colony.input(out, f$offspring, f$markers, project.info = FALSE)
  expect_error(write.colony.input(out, f$offspring, f$markers, project.info = FALSE), "overwrite")
  expect_no_error(write.colony.input(out, f$offspring, f$markers, project.info = FALSE, overwrite = TRUE))
})
