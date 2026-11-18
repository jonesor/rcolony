# build.colony.input() is an interactive wizard (scan(), utils::menu(),
# file.choose()). To test it non-interactively we replace those three calls with
# functions that pop answers from a queue. The queue must follow the order of
# the prompts; if it runs out, or a prompt receives the wrong kind of answer, the
# mock errors so a prompt-order change in the package is noticed.
build_input_scripted <- function(sim, dir, name = "Colony2.DAT",
                                 known.paternal = TRUE, known.maternal = TRUE,
                                 excluded.paternity = TRUE, seed = 1234,
                                 run.length = 2, sibship.prior = 1,
                                 known.threshold = 2, precision = 3) {
  f <- sim$files
  ans <- list(
    a_scan("scripted"), a_scan("scripted"), a_scan("A scripted test dataset"),
    a_scan(sim$n.offspring), a_scan(sim$n.loci), a_scan(seed),
    a_menu(1),  # not updating allele frequency
    a_menu(1),  # dioecious
    a_menu(1),  # no inbreeding
    a_menu(1),  # diploid
    a_menu(2),  # males polygamous
    a_menu(2),  # females polygamous
    a_menu(1),  # do not infer clones
    a_menu(1),  # scale full sibship size
    a_menu(sibship.prior + 1),
    if (sibship.prior > 0) list(a_scan(2), a_scan(2)),
    a_menu(1),  # unknown allele frequencies
    a_scan(1),  # number of runs
    a_menu(run.length),
    a_menu(1),  # monitor by iterate
    a_scan(10000),
    a_menu(2),  # non-Windows
    a_menu(1),  # full likelihood
    a_menu(precision),
    a_path(f$markers), a_path(f$offspring),
    a_scan(0.8), a_scan(sim$n.males), a_path(f$fathers),
    a_scan(0.8), a_scan(sim$n.females), a_path(f$mothers),
    a_scan(if (known.paternal) sim$n.known.paternal else 0),
    if (known.paternal) list(a_path(f$known.paternal), a_scan(known.threshold)),
    a_scan(if (known.maternal) sim$n.known.maternal else 0),
    if (known.maternal) list(a_path(f$known.maternal), a_scan(known.threshold)),
    a_scan(if (excluded.paternity) sim$n.excluded.paternity else 0),
    if (excluded.paternity) a_path(f$excluded.paternity),
    a_scan(0), a_scan(0), a_scan(0)  # excluded maternities and sibships: none
  )
  queue <- unlist(ans, recursive = TRUE, use.names = TRUE)
  kinds <- sub("[0-9]+$", "", names(queue))
  pos <- 0
  pop <- function(kind) {
    pos <<- pos + 1
    if (pos > length(queue)) stop("scripted wizard: ran out of answers")
    if (kinds[pos] != kind) {
      stop("scripted wizard: prompt ", pos, " expected a ", kinds[pos],
           " answer but the wizard asked for ", kind)
    }
    unname(queue[pos])
  }
  # Only console prompts (scan() without a file) are answered from the queue;
  # read.table() and friends call scan() on files and must reach the real one.
  real_scan <- base::scan
  testthat::local_mocked_bindings(
    scan = function(file = "", ...) {
      if (is.character(file) && identical(file, "")) pop("scan") else real_scan(file = file, ...)
    },
    file.choose = function(...) pop("path"),
    .package = "base"
  )
  testthat::local_mocked_bindings(menu = function(...) as.integer(pop("menu")),
                                  .package = "utils")
  withr::with_dir(dir, {
    utils::capture.output(build.colony.input(wd = dir, name = name))
  })
  invisible(file.path(dir, name))
}

a_scan <- function(x) stats::setNames(as.character(x), "scan1")
a_menu <- function(x) stats::setNames(as.character(x), "menu1")
a_path <- function(x) stats::setNames(as.character(x), "path1")
