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
