# Simulate a small, fully known pedigree and write the files that
# build.colony.input() asks for. Returns the file paths and the ground truth,
# so that tests can compare COLONY's inference with the truth.
#
# Design (chosen to exercise the less common input sections, not to be
# realistic): dioecious, polygamous, `n.loci` codominant loci with unequal
# allele frequencies, 14 full-sib families (some fathers reused, giving
# paternal half-sibs), a proportion of true parents missing from the candidate
# sets, a few known paternities, a known paternal and a known maternal sibship,
# some (true) excluded paternities, and a small genotyping-error rate.
simulate_colony_data <- function(dir, seed = 20251118, n.loci = 20,
                                 n.alleles = 6, error = 0.01,
                                 miss.prob = 0.2) {
  set.seed(seed)
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  freqs <- lapply(seq_len(n.loci), function(i) {
    f <- stats::rgamma(n.alleles, shape = 1.5); f / sum(f)
  })
  draw_geno <- function() {
    unlist(lapply(freqs, function(f) sample(seq_along(f), 2, replace = TRUE, prob = f)))
  }
  gamete <- function(g) {  # one allele per locus from a parent genotype
    idx <- seq(1, length(g), by = 2)
    g[idx + stats::rbinom(length(idx), 1, 0.5)]
  }

  n.m <- 30; n.f <- 30
  males   <- t(replicate(n.m, draw_geno())); rownames(males)   <- paste0("M", seq_len(n.m))
  females <- t(replicate(n.f, draw_geno())); rownames(females) <- paste0("F", seq_len(n.f))

  # 14 families; fathers drawn from the first 10 males (so some are reused),
  # mothers all distinct.
  fam.size <- rep(c(3, 4, 5, 6), length.out = 14)
  fam.father <- sample(rownames(males)[1:10], 14, replace = TRUE)
  fam.mother <- sample(rownames(females), 14)
  truth <- do.call(rbind, lapply(seq_len(14), function(i) {
    data.frame(family = i, father = fam.father[i], mother = fam.mother[i],
               n = fam.size[i], stringsAsFactors = FALSE)
  }))
  truth <- truth[rep(seq_len(14), fam.size), c("family", "father", "mother")]
  truth$offspring <- paste0("O", seq_len(nrow(truth)))
  rownames(truth) <- NULL

  off <- t(vapply(seq_len(nrow(truth)), function(i) {
    g <- c(rbind(gamete(males[truth$father[i], ]), gamete(females[truth$mother[i], ])))
    # genotyping error: replace an allele at random
    e <- stats::runif(length(g)) < error
    g[e] <- sample(seq_len(n.alleles), sum(e), replace = TRUE)
    g
  }, numeric(2 * n.loci)))
  rownames(off) <- truth$offspring

  # Candidate sets: each true parent is omitted with probability `miss.prob`.
  drop.m <- unique(truth$father)[stats::runif(length(unique(truth$father))) < miss.prob]
  drop.f <- unique(truth$mother)[stats::runif(length(unique(truth$mother))) < miss.prob]
  cand.m <- males[!rownames(males) %in% drop.m, , drop = FALSE]
  cand.f <- females[!rownames(females) %in% drop.f, , drop = FALSE]
  truth$father.in.candidates <- truth$father %in% rownames(cand.m)
  truth$mother.in.candidates <- truth$mother %in% rownames(cand.f)

  write_geno <- function(m, path) {
    utils::write.table(data.frame(rownames(m), m), path, quote = FALSE,
                       row.names = FALSE, col.names = FALSE)
  }
  p <- function(x) file.path(dir, x)

  write_geno(off,    p("offspring.txt"))
  write_geno(cand.m, p("fathers.txt"))
  write_geno(cand.f, p("mothers.txt"))

  # Markers: ID, type (0 = codominant), dropout rate, other error rate
  markers <- rbind(paste0("mk", seq_len(n.loci)), rep(0, n.loci),
                   rep(0, n.loci), rep(error, n.loci))
  utils::write.table(markers, p("markers.txt"), quote = FALSE,
                     row.names = FALSE, col.names = FALSE)

  # Known paternities: first column is the father, the rest his offspring.
  # A row starting with 0 is a known paternal sibship (unknown father).
  known <- truth[truth$father.in.candidates, ]
  kp.rows <- lapply(unique(known$father)[1:2], function(f) {
    c(f, known$offspring[known$father == f][1])
  })
  fam.unknown <- truth[!truth$father.in.candidates, ]
  ps.rows <- if (nrow(fam.unknown) >= 2) {
    list(c("0", fam.unknown$offspring[fam.unknown$family == fam.unknown$family[1]][1:2]))
  } else list()
  paternal <- c(kp.rows, ps.rows)
  write_ragged(paternal, p("known_paternal.txt"))

  # Known maternal sibship (unknown mother): two offspring of one family
  fam.mu <- truth[!truth$mother.in.candidates, ]
  maternal <- if (nrow(fam.mu) >= 2) {
    list(c("0", fam.mu$offspring[fam.mu$family == fam.mu$family[1]][1:2]))
  } else list()
  write_ragged(maternal, p("known_maternal.txt"))

  # Excluded paternities: for 4 offspring, two males that are not the father
  ex <- lapply(truth$offspring[c(1, 8, 15, 22)], function(o) {
    f <- truth$father[truth$offspring == o]
    c(o, sample(setdiff(rownames(cand.m), f), 2))
  })
  write_ragged(ex, p("excluded_paternity.txt"))

  # --- Extra inputs, drawn after everything above so that the data and files
  # above are unchanged. Used to exercise the less common sections.
  # Known allele frequencies: two rows per locus (allele IDs, then frequencies)
  af <- unlist(lapply(freqs, function(f) {
    list(paste(seq_along(f), collapse = " "), paste(sprintf("%.4f", f), collapse = " "))
  }))
  writeLines(af, p("allele_freq.txt"))

  # Excluded maternities: for 3 offspring, two candidate females that are not the mother
  exm <- lapply(truth$offspring[c(3, 10, 17)], function(o) {
    m <- truth$mother[truth$offspring == o]
    c(o, sample(setdiff(rownames(cand.f), m), 2))
  })
  write_ragged(exm, p("excluded_maternity.txt"))

  # Excluded sibships: offspring that cannot share a father (or mother) with the
  # first one listed, taken from families with a different father (or mother).
  other <- function(o, col) {
    sample(truth$offspring[truth[[col]] != truth[[col]][truth$offspring == o]], 2)
  }
  write_ragged(lapply(c("O5", "O20"), function(o) c(o, other(o, "father"))),
               p("excluded_paternal_sibships.txt"))
  write_ragged(lapply(c("O2", "O30", "O45"), function(o) c(o, other(o, "mother"))),
               p("excluded_maternal_sibships.txt"))

  list(dir = dir, truth = truth, n.loci = n.loci, n.offspring = nrow(off),
       n.males = nrow(cand.m), n.females = nrow(cand.f),
       n.known.paternal = length(paternal), n.known.maternal = length(maternal),
       n.excluded.paternity = length(ex), n.excluded.maternity = length(exm),
       n.excluded.paternal.sibships = 2, n.excluded.maternal.sibships = 3,
       files = list(offspring = p("offspring.txt"), fathers = p("fathers.txt"),
                    mothers = p("mothers.txt"), markers = p("markers.txt"),
                    known.paternal = p("known_paternal.txt"),
                    known.maternal = p("known_maternal.txt"),
                    excluded.paternity = p("excluded_paternity.txt"),
                    excluded.maternity = p("excluded_maternity.txt"),
                    excluded.paternal.sibships = p("excluded_paternal_sibships.txt"),
                    excluded.maternal.sibships = p("excluded_maternal_sibships.txt"),
                    allele.freq = p("allele_freq.txt")))
}

write_ragged <- function(rows, path) {
  writeLines(vapply(rows, paste, character(1), collapse = " "), path)
}
