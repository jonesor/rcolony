# Wizard answers for the scenarios used to compare write.colony.input() with
# the interactive wizard (see test-write-colony-input.R). Scenario A is the
# complex default, B has no optional sections, and C sets every option to a
# non-default value and includes every optional section.
wizard_scenarios <- list(
  A = list(),
  B = list(known.paternal = FALSE, known.maternal = FALSE,
           excluded.paternity = FALSE, sibship.prior = 0),
  C = list(allele.freq = TRUE, excluded.maternity = TRUE,
           excluded.paternal.sibships = TRUE, excluded.maternal.sibships = TRUE,
           update.allele.freq = 2, species = 2, inbreeding = 2, ploidy = 2,
           male.mating = 1, female.mating = 2, clone = 2, scaling = 2,
           sibship.prior = 4, mean.sibship.size = c(3, 4), n.runs = 3,
           run.length = 3, monitor = 2, interval = 60, platform = 1,
           likelihood = 2, precision = 1, father.prob = 0.5, mother.prob = 0.3,
           known.threshold = 0, seed = 98765)
)

# --- The same scenarios as calls to write.colony.input() (see test-write-colony-input.R)
sim_in <- function(dir) simulate_colony_data(file.path(dir, "sim"))

# The core call corresponding to each wizard scenario in helper-scenarios.R
core_args <- list(
  A = function(f) list(
    platform = "unix", father.prob = 0.8, mother.prob = 0.8, known.paternal = f$known.paternal,
    known.maternal = f$known.maternal, paternity.threshold = 2,
    maternity.threshold = 2, excluded.paternities = f$excluded.paternity,
    sibship.prior = "weak", mean.sibship.size = c(2, 2),
    run.length = "medium", precision = "high", seed = 1234),
  B = function(f) list(platform = "unix", father.prob = 0.8, mother.prob = 0.8, seed = 1234),
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

