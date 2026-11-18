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

test_that("the wizard reproduces the reference files for each scenario", {
  # The references were made by the original, self-contained wizard (with the
  # excluded-sibship count corrected), before it was rewritten as a thin layer
  # over write.colony.input().
  skip_if_not_installed("withr")
  for (sc in names(wizard_scenarios)) {
    dir <- withr::local_tempdir()
    sim <- simulate_colony_data(file.path(dir, "sim"))
    out <- do.call(build_input_scripted, c(list(sim = sim, dir = dir), wizard_scenarios[[sc]]))
    golden <- test_path("fixtures", "wizard-golden", paste0(sc, ".DAT"))
    expect_equal(dat_content(out), dat_content(golden), info = paste("scenario", sc))
  }
})

test_that("the call the wizard prints reproduces the file it wrote", {
  skip_if_not_installed("withr")
  dir <- withr::local_tempdir()
  sim <- simulate_colony_data(file.path(dir, "sim"))
  res <- run_scripted(wizard_answers(sim, excluded.maternity = TRUE, allele.freq = TRUE), dir)
  shown <- attr(res, "output")
  start <- grep("^write.colony.input\\(", shown)
  expect_length(start, 1)
  end <- start + which(shown[start:length(shown)] == ")")[1] - 1
  call_text <- shown[start:end]
  # run the printed call, writing to a different file
  call_text <- sub(file.path(dir, "Colony2.DAT"), file.path(dir, "again.DAT"), call_text, fixed = TRUE)
  eval(parse(text = call_text))
  expect_equal(dat_content(file.path(dir, "again.DAT")), dat_content(res))
})

test_that("the wizard asks again after an invalid answer", {
  skip_if_not_installed("withr")
  dir <- withr::local_tempdir()
  sim <- simulate_colony_data(file.path(dir, "sim")); f <- sim$files
  q <- wizard_answers(sim)
  # a non-numeric answer where the number of offspring is asked for
  i <- which(names(q) == "scan1")[4]
  q <- append(q, c(scan1 = "not-a-number"), after = i - 1)
  # a file with the wrong layout where the marker file is asked for
  j <- which(names(q) == "path1")[1]
  q <- append(q, c(path1 = f$fathers), after = j - 1)
  # an offspring file whose number of rows does not match the number entered
  k <- which(names(q) == "path1")[3]
  q <- append(q, c(path1 = f$fathers), after = k - 1)
  res <- run_scripted(q, dir)
  w <- attr(res, "warnings")
  expect_match(w, "Please enter a number", all = FALSE)
  expect_match(w, "must have 4 rows", all = FALSE)
  expect_match(w, "does not equal the number provided", all = FALSE)
  # ... and it still produced the same file as an uninterrupted session
  expect_equal(dat_content(res), dat_content(test_path("fixtures", "wizard-golden", "A.DAT")))
})

test_that("a mismatch with the entered count is reported and the file is asked for again", {
  skip_if_not_installed("withr")
  dir <- withr::local_tempdir()
  sim <- simulate_colony_data(file.path(dir, "sim")); f <- sim$files
  q <- wizard_answers(sim)
  # known paternal: first give the excluded-paternity file, which has a different number of rows
  i <- which(names(q) == "path1")[5]   # markers, offspring, fathers, mothers, known paternal
  expect_equal(unname(q[i]), f$known.paternal)
  q <- append(q, c(path1 = f$excluded.paternity), after = i - 1)
  res <- run_scripted(q, dir)
  expect_true(length(attr(res, "warnings")) >= 1)
  expect_true(file.exists(res))
})

test_that("the wizard can be cancelled at the final prompt", {
  skip_if_not_installed("withr")
  dir <- withr::local_tempdir()
  sim <- simulate_colony_data(file.path(dir, "sim"))
  q <- wizard_answers(sim)
  q[length(q)] <- "2"
  res <- run_scripted(q, dir)
  expect_true(is.na(res))
  expect_false(file.exists(file.path(dir, "Colony2.DAT")))
})

test_that("the wizard replaces an existing file", {
  skip_if_not_installed("withr")
  dir <- withr::local_tempdir()
  sim <- simulate_colony_data(file.path(dir, "sim"))
  res <- run_scripted(wizard_answers(sim), dir)
  expect_true(file.exists(res))
  # a second run replaces the file rather than failing
  res2 <- run_scripted(wizard_answers(sim, seed = 99), dir)
  expect_equal(readLines(res2)[5], "99 ! I, Seed for random number generator")
})
