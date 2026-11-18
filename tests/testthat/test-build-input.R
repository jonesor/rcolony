test_that("build.colony.input writes a well-formed 2.0.7-format file with complex inputs", {
  skip_if_not_installed("withr")
  dir <- withr::local_tempdir()
  sim <- simulate_colony_data(file.path(dir, "sim"))
  out <- build_input_scripted(sim, dir = dir)
  expect_true(file.exists(out))

  x <- readLines(out)
  x <- x[!x %in% c("", " ")]
  header <- function(i) as.numeric(strsplit(trimws(x[i]), " +")[[1]][1])
  expect_equal(header(3), sim$n.offspring)
  expect_equal(header(4), sim$n.loci)
  # unknown allele frequencies, so the offspring block starts at line 26
  expect_equal(header(14), 0)
  expect_match(x[26], "^ *O1 ")
})

test_that("the generated input file round-trips through get.colony.data", {
  skip_if_not_installed("withr")
  dir <- withr::local_tempdir()
  sim <- simulate_colony_data(file.path(dir, "sim"))
  out <- build_input_scripted(sim, dir = dir)
  # Parse the input file as if it had been run (get.colony.data needs the
  # output files too, so only the header/ID parsing is checked here).
  x <- readLines(out); x <- x[!x %in% c("", " ")]
  ofs <- x[26:(25 + sim$n.offspring)]
  ids <- sub("^ *([A-Za-z0-9]+).*", "\\1", ofs)
  expect_equal(ids, sim$truth$offspring)
  pr <- strsplit(trimws(x[26 + sim$n.offspring]), " +")[[1]]
  expect_equal(as.numeric(pr[1:2]), c(0.8, 0.8))
  np <- strsplit(trimws(x[27 + sim$n.offspring]), " +")[[1]]
  expect_equal(as.numeric(np[1:2]), c(sim$n.males, sim$n.females))
})
