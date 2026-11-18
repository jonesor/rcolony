# The layout of the DAT file is described once (R/colony-format.R) and used by
# the writer and the reader. These tests check that they agree, and that older
# layouts are read correctly.
ns <- asNamespace("rcolony")
nrow_or_0 <- function(x) if (is.null(x)) 0 else nrow(x)

test_that("the layout spec reproduces the line numbers used by the original reader", {
  # The reader used to hard-code the line at which the offspring genotypes begin
  # when allele frequencies are unknown (22, 23, 26) and the line of the allele
  # frequency flag (11, 11, 14), found from real files of each version.
  n_header <- function(l) nrow(ns$colony.layout(l)$header)
  start <- function(l) n_header(l) + nrow(ns$colony.run.spec) + nrow(ns$colony.layout(l)$markers) + 1
  expect_equal(c(n_header("2.0"), n_header("2.0.3"), n_header("2.0.6")), c(11, 11, 14))
  expect_equal(c(start("2.0"), start("2.0.3"), start("2.0.6")), c(22, 23, 26))
  expect_equal(ns$colony.layout.name("2.0"), "2.0")
  expect_equal(ns$colony.layout.name("2.0.3"), "2.0.3")
  expect_equal(unique(vapply(c("2.0.6", "2.0.7", "2.0.7.1", "2.0.7.2"), ns$colony.layout.name, "")), "2.0.6")
  expect_error(ns$colony.layout.name("3.0"), "only works")
  expect_error(ns$colony.layout.name("nonsense"), "only works")
})

test_that("the reader recovers what the writer wrote, for each scenario", {
  skip_if_not_installed("withr")
  expected_header <- list(
    A = list(n.offspring = 61, n.loci = 20, seed = 1234, update.allele.freq = 0, species = 2,
             inbreeding = 0, ploidy = 0, mating = c(0, 0), clone.inference = 0,
             scale.sibship.size = 1, sibship.prior = c(1, 2, 2), known.allele.freq = 0),
    B = list(n.offspring = 61, n.loci = 20, seed = 1234, update.allele.freq = 0, species = 2,
             inbreeding = 0, ploidy = 0, mating = c(0, 0), clone.inference = 0,
             scale.sibship.size = 1, sibship.prior = c(0, 0, 0), known.allele.freq = 0),
    C = list(n.offspring = 61, n.loci = 20, seed = 98765, update.allele.freq = 1, species = 1,
             inbreeding = 1, ploidy = 1, mating = c(1, 0), clone.inference = 1,
             scale.sibship.size = 0, sibship.prior = c(4, 3, 4), known.allele.freq = 1))
  expected_run <- list(
    A = list(n.runs = 1, run.length = 2, monitor.by = 0, monitor.interval = 10000, platform = 0, likelihood = 1, precision = 3),
    C = list(n.runs = 3, run.length = 3, monitor.by = 1, monitor.interval = 60, platform = 1, likelihood = 0, precision = 1))
  expected_run$B <- expected_run$A

  for (sc in names(core_args)) {
    dir <- withr::local_tempdir()
    sim <- sim_in(dir); f <- sim$files
    r <- ns$read.colony.dat(write_core(sim, dir, sc), relationships = TRUE)
    info <- paste("scenario", sc)

    expect_equal(r$layout, "2.0.6", info = info)
    h <- r$header
    expect_equal(h[names(expected_header[[sc]])], expected_header[[sc]], info = info)
    expect_equal(h$dataset, "scripted"); expect_equal(h$output, "scripted")
    expect_equal(r$run, expected_run[[sc]], info = info)

    expect_equal(r$markers, stats::setNames(ns$colony.table(f$markers, "m"), 1:20), ignore_attr = TRUE, info = info)
    expect_equal(rownames(r$markers), c("id", "type", "dropout", "error"))
    expect_equal(r$offspring, ns$colony.table(f$offspring, "o"), info = info)
    expect_equal(r$fathers, ns$colony.table(f$fathers, "f"), info = info)
    expect_equal(r$mothers, ns$colony.table(f$mothers, "m"), info = info)
    expect_equal(r$n.candidates, c(nrow(r$fathers), nrow(r$mothers)))

    a <- core_args[[sc]](f)
    expect_equal(r$candidate.probabilities, c(a$father.prob, a$mother.prob), info = info)
    if (is.null(a$allele.freq)) {
      expect_null(r$allele.freq)
    } else {
      expect_equal(r$allele.freq$table, ns$check.allele.freq(f$allele.freq, 20)$table, info = info)
      expect_equal(r$allele.freq$n.alleles, rep(6, 20), ignore_attr = TRUE)
    }

    # known and excluded relationships
    ids_o <- r$offspring[[1]]; ids_f <- r$fathers[[1]]; ids_m <- r$mothers[[1]]
    kp <- ns$check.known(a$known.paternal, "k", ids_f, ids_o, "father")
    km <- ns$check.known(a$known.maternal, "k", ids_m, ids_o, "mother")
    expect_equal(r$known.paternities$dyads, kp$dyads, info = info)
    expect_equal(r$known.maternities$dyads, km$dyads, info = info)
    expect_equal(r$known.paternal.sibships, kp$sibships, info = info)
    expect_equal(r$known.maternal.sibships, km$sibships, info = info)
    expect_equal(r$known.paternities$threshold, if (nrow_or_0(kp$dyads) > 0) a$paternity.threshold else 0, info = info)
    ex <- function(x, allowed, what) ns$check.exclusions(x, "e", ids_o, allowed, what)
    expect_equal(r$excluded.paternities, ex(a$excluded.paternities, ids_f, "fathers"), info = info)
    expect_equal(r$excluded.maternities, ex(a$excluded.maternities, ids_m, "mothers"), info = info)
    expect_equal(r$excluded.paternal.sibships, ex(a$excluded.paternal.sibships, ids_o, "offspring"), info = info)
    expect_equal(r$excluded.maternal.sibships, ex(a$excluded.maternal.sibships, ids_o, "offspring"), info = info)
  }
})
# Make a file in an older layout from a current one, using the line numbers
# the original reader used (an independent description of the old layouts):
# 2.0.3 has no inbreeding, clone-inference or sibship-scaling lines (modern lines
# 8, 11 and 12), and 2.0 also has no allelic-dropout marker row.
legacy_dat <- function(modern, layout, one_token_prior = FALSE, sim) {
  x <- modern[!grepl("^[[:space:]]*$", modern)]
  n_cand <- sim$n.males + sim$n.females
  last <- 14 + 7 + 4 + sim$n.offspring + 2 + n_cand          # end of the candidate mothers
  x <- x[seq_len(last)]
  if (one_token_prior) x[13] <- "1 ! 0/1=Without/with sibship size prior"
  drop <- c(8, 11, 12, if (layout == "2.0") 14 + 7 + 3)       # the dropout row is the 3rd marker row
  x[-drop]
}

test_that("older layouts are detected and read", {
  skip_if_not_installed("withr")
  dir <- withr::local_tempdir()
  sim <- sim_in(dir); f <- sim$files
  modern <- readLines(write_core(sim, dir, "B"))
  for (layout in c("2.0.3", "2.0")) {
    for (one in c(FALSE, TRUE)) {
      x <- legacy_dat(modern, layout, one, sim)
      # the offspring genotypes start where the original reader looked for them
      expect_match(x[if (layout == "2.0") 22 else 23], "^O1 ")
      d <- file.path(dir, paste0("legacy-", layout, one)); dir.create(d)
      writeLines(x, file.path(d, "Colony2.DAT"))
      r <- ns$read.colony.dat(file.path(d, "Colony2.DAT"))
      info <- paste(layout, "one-token prior:", one)
      expect_equal(r$layout, layout, info = info)
      expect_equal(r$offspring[[1]], sim$truth$offspring, info = info)
      expect_equal(r$n.candidates, c(sim$n.males, sim$n.females), info = info)
      expect_equal(nrow(r$markers), if (layout == "2.0") 3 else 4, info = info)

      # through get.colony.data(), detected or insisted on
      got <- get.colony.data(paste0(d, "/"))
      expect_equal(got$offspring$offspringID, sim$truth$offspring, info = info)
      expect_equal(nrow(got$fathers), sim$n.males + 1, info = info)
      expect_equal(get.colony.data(paste0(d, "/"), colonyVersion = layout), got, info = info)
      # ... but the wrong layout is refused
      wrong <- if (layout == "2.0") "2.0.3" else "2.0.7.2"
      expect_error(get.colony.data(paste0(d, "/"), colonyVersion = wrong), "does not match")
    }
  }
})

test_that("get.colony.data detects the layout of the current format by default", {
  fix <- paste0(test_path("fixtures", "colony-2.0.7.2"), "/")
  expect_equal(get.colony.data(fix), get.colony.data(fix, colonyVersion = "2.0.7.2"))
})

test_that("IDs may contain characters other than letters and digits", {
  skip_if_not_installed("withr")
  dir <- withr::local_tempdir()
  sim <- sim_in(dir); f <- sim$files
  recode <- function(path, from, to) {
    x <- readLines(path); writeLines(sub(paste0("^", from), to, x), path)
  }
  recode(f$offspring, "O", "ind_"); recode(f$fathers, "M", "m-"); recode(f$mothers, "F", "f.")
  write.colony.input(file.path(dir, "Colony2.DAT"), f$offspring, f$markers, f$fathers, f$mothers,
                     father.prob = 0.8, mother.prob = 0.8, project.info = FALSE)
  got <- get.colony.data(paste0(dir, "/"))
  expect_setequal(got$offspring$offspringID, paste0("ind_", seq_len(sim$n.offspring)))
  expect_true(all(grepl("^ind_[0-9]+$", got$offspring$offspringID)))
  expect_true(all(grepl("^m-[0-9]+$", setdiff(got$fathers$fatherID, "Unknown"))))
  expect_true(all(grepl("^f\\.[0-9]+$", setdiff(got$mothers$motherID, "Unknown"))))
})

test_that("a file with no candidate parents is read correctly", {
  skip_if_not_installed("withr")
  dir <- withr::local_tempdir()
  sim <- sim_in(dir); f <- sim$files
  write.colony.input(file.path(dir, "Colony2.DAT"), f$offspring, f$markers, project.info = FALSE)
  got <- get.colony.data(paste0(dir, "/"))
  expect_equal(nrow(got$offspring), sim$n.offspring)
  expect_equal(got$fathers$fatherID, "Unknown")
  expect_equal(got$mothers$motherID, "Unknown")
  expect_equal(as.numeric(got$fathers$father.numID), 1)
})

test_that("the reader gives clear errors for files that do not fit", {
  skip_if_not_installed("withr")
  dir <- withr::local_tempdir()
  junk <- file.path(dir, "junk.DAT"); writeLines(c("hello", "world", "this is not a DAT file"), junk)
  expect_error(ns$read.colony.dat(junk), "does not match any known Colony input layout")

  sim <- sim_in(dir)
  modern <- write_core(sim, dir, "A")
  x <- readLines(modern)
  trunc <- file.path(dir, "trunc.DAT"); writeLines(x[1:60], trunc)
  expect_error(ns$read.colony.dat(trunc), "file ended")

  expect_error(ns$read.colony.dat(modern, layout = "2.0"), "does not match the layout")
  expect_error(get.colony.data(paste0(dir, "/"), filename = "junk.DAT"), "does not match any known")
})

test_that("lines that are only white space or only a comment are ignored", {
  skip_if_not_installed("withr")
  dir <- withr::local_tempdir()
  sim <- sim_in(dir)
  modern <- write_core(sim, dir, "A")
  x <- readLines(modern)
  y <- c("   ", x[1:5], "! a comment on its own", "        ", x[6:length(x)], "")
  p <- file.path(dir, "messy.DAT"); writeLines(y, p)
  expect_equal(ns$read.colony.dat(p, relationships = TRUE), ns$read.colony.dat(modern, relationships = TRUE))
})
