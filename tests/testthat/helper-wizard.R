# build.colony.input() is an interactive wizard (scan(), utils::menu(),
# file.choose()). To test it non-interactively we replace those three calls with
# functions that pop answers from a queue. The queue must follow the order of
# the prompts; if it runs out, or a prompt receives the wrong kind of answer, the
# mock errors so a prompt-order change in the package is noticed.
#
# Menu answers are the 1-based position of the choice in the wizard's menu.
wizard_answers <- function(sim,
                                 known.paternal = TRUE, known.maternal = TRUE,
                                 excluded.paternity = TRUE,
                                 excluded.maternity = FALSE,
                                 excluded.paternal.sibships = FALSE,
                                 excluded.maternal.sibships = FALSE,
                                 allele.freq = FALSE,
                                 seed = 1234, run.length = 2, sibship.prior = 1,
                                 mean.sibship.size = c(2, 2),
                                 known.threshold = 2, precision = 3,
                                 update.allele.freq = 1, species = 1,
                                 inbreeding = 1, ploidy = 1, male.mating = 2,
                                 female.mating = 2, clone = 1, scaling = 1,
                                 n.runs = 1, monitor = 1, interval = 10000,
                                 platform = 2, likelihood = 1,
                                 father.prob = 0.8, mother.prob = 0.8) {
  f <- sim$files
  count_and_file <- function(use, n, file) {
    list(a_scan(if (use) n else 0), if (use) a_path(file))
  }
  ans <- list(
    a_scan("scripted"), a_scan("scripted"), a_scan("A scripted test dataset"),
    a_scan(sim$n.offspring), a_scan(sim$n.loci), a_scan(seed),
    a_menu(update.allele.freq), a_menu(species), a_menu(inbreeding),
    a_menu(ploidy), a_menu(male.mating), a_menu(female.mating),
    a_menu(clone), a_menu(scaling),
    a_menu(sibship.prior + 1),
    if (sibship.prior > 0) list(a_scan(mean.sibship.size[1]), a_scan(mean.sibship.size[2])),
    a_menu(if (allele.freq) 2 else 1),
    if (allele.freq) a_path(f$allele.freq),
    a_scan(n.runs), a_menu(run.length), a_menu(monitor), a_scan(interval),
    a_menu(platform), a_menu(likelihood), a_menu(precision),
    a_path(f$markers), a_path(f$offspring),
    a_scan(father.prob), a_scan(sim$n.males), a_path(f$fathers),
    a_scan(mother.prob), a_scan(sim$n.females), a_path(f$mothers),
    if (known.paternal) list(a_scan(sim$n.known.paternal), a_path(f$known.paternal), a_scan(known.threshold))
    else a_scan(0),
    if (known.maternal) list(a_scan(sim$n.known.maternal), a_path(f$known.maternal), a_scan(known.threshold))
    else a_scan(0),
    count_and_file(excluded.paternity, sim$n.excluded.paternity, f$excluded.paternity),
    count_and_file(excluded.maternity, sim$n.excluded.maternity, f$excluded.maternity),
    count_and_file(excluded.paternal.sibships, sim$n.excluded.paternal.sibships, f$excluded.paternal.sibships),
    count_and_file(excluded.maternal.sibships, sim$n.excluded.maternal.sibships, f$excluded.maternal.sibships),
    a_menu(1)  # "Write the input file"
  )
  unlist(ans, recursive = TRUE, use.names = TRUE)
}

# Run build.colony.input() answering its prompts from `queue` (a named character
# vector whose names say what kind of prompt each answer is for: scan1, menu1 or
# path1). Returns the path of the file, with the wizard's console output (and
# any warnings it gave) as attributes.
run_scripted <- function(queue, dir, name = "Colony2.DAT") {
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
  warns <- character()
  out <- withr::with_dir(dir, {
    withCallingHandlers(
      utils::capture.output(res <- build.colony.input(wd = dir, name = name)),
      warning = function(w) {warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning")})
  })
  if (pos < length(queue)) warning("scripted wizard: ", length(queue) - pos, " answers were not used")
  structure(if (is.null(res)) NA_character_ else file.path(dir, name),
            output = out, warnings = warns, returned = res)
}

# The usual case: answer a whole session as described by the arguments of
# wizard_answers().
build_input_scripted <- function(sim, dir, name = "Colony2.DAT", ...) {
  invisible(run_scripted(wizard_answers(sim, ...), dir, name))
}

a_scan <- function(x) stats::setNames(as.character(x), "scan1")
a_menu <- function(x) stats::setNames(as.character(x), "menu1")
a_path <- function(x) stats::setNames(as.character(x), "path1")
