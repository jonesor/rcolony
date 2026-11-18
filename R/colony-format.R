# The layout of a Colony input (DAT) file, described once and used by both the
# writer (write.colony.input) and the reader (get.colony.data, via
# read.colony.dat), so that the two cannot drift apart.
#
# A DAT file is a sequence of lines (blank lines are ignored, and anything after
# a "!" on a line is a comment):
#   1. a header of one line per setting, in the order of `colony.header.spec`
#   2. if the population allele frequencies are known: a line with the number of
#      alleles at each locus, then two lines per locus (allele IDs, frequencies)
#   3. the run settings (`colony.run.spec`), one line each
#   4. the marker rows (`colony.marker.spec`), one column per locus
#   5. the offspring genotypes, then the probabilities that the father and
#      mother are among the candidates, the numbers of candidates, and the
#      candidate fathers and mothers
#   6. the known and excluded relationships (`colony.section.spec`)
# Later versions of Colony added lines to the header (`since`) and a marker row.
# `colony.layouts` lists the layouts that are understood.

# Header lines, in file order. `since` is the first Colony version that has the
# line. `min.tokens`/`max.tokens` are the number of entries allowed on the line.
colony.header.spec <- data.frame(
  key = c("dataset", "output", "n.offspring", "n.loci", "seed", "update.allele.freq",
          "species", "inbreeding", "ploidy", "mating", "clone.inference",
          "scale.sibship.size", "sibship.prior", "known.allele.freq"),
  since = c("2.0", "2.0", "2.0", "2.0", "2.0", "2.0",
            "2.0", "2.0.6", "2.0", "2.0", "2.0.6",
            "2.0.6", "2.0", "2.0"),
  min.tokens = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1),
  max.tokens = c(Inf, Inf, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 3, 1),
  comment = c("! C, Dataset name, Length<51",
              "! C, Main output file name, Length<21",
              "! I, Number of offspring in the sample",
              "! I, Number of loci",
              "! I, Seed for random number generator",
              "! B, 0/1=Not updating/updating allele frequency",
              "! 2/1=Dioecious/Monoecious",
              "! B, 0/1=No inbreeding/inbreeding",
              "! B, 0/1=Diploid species/HaploDiploid species",
              "! B, 0/1=Polygamy/Monogamy for males & females",
              "! B, 0/1=Clone inference =No/Yes",
              "! B, 0/1=Full sibship size scaling =No/Yes",
              "! 0/1/2/3/4=No/Weak/Medium/Strong/Optimal sibship prior; mean paternal & maternal sibship size",
              "! B, 0/1=Unknown/Known population allele frequency"),
  stringsAsFactors = FALSE)

colony.allele.freq.comment <- "!Number of alleles per locus"

colony.run.spec <- data.frame(
  key = c("n.runs", "run.length", "monitor.by", "monitor.interval", "platform", "likelihood", "precision"),
  comment = c("! I, Number of runs",
              "! I, Length of Run (1, 2, 3) = (Short, Medium, Long)",
              "! B, 0/1=Monitor method by Iterate#/Time in second",
              "! I, Monitor interval in Iterate#/Seconds",
              "! B, 0/1=Other platform/Windows execution",
              "! 1/0=Full-likelihood/pairwise-likelihood score method",
              "! 1/2/3=low/medium/high precision"),
  stringsAsFactors = FALSE)

colony.marker.spec <- data.frame(
  key = c("id", "type", "dropout", "error"),
  since = c("2.0", "2.0", "2.0.3", "2.0"),
  comment = c("!Marker IDs",
              "!Marker types, 0/1=Codominant/Dominant",
              "!Marker-specific allelic dropout rate",
              "!Other marker-specific typing-error rate"),
  stringsAsFactors = FALSE)

# Comments on the lines that follow the genotypes
colony.candidate.comments <- c(
  offspring = "!Offspring ID and genotypes",
  probabilities = "!Probabilities that the father and mother of an offspring included in candidates",
  numbers = "!Numbers of candidate males and females")

# The known and excluded relationships, in file order (Colony 2.0.6.3 or later).
# `count` is the comment on the line that gives the number of entries, and `row`
# the comment on the first entry.
colony.section.spec <- data.frame(
  key = c("known.paternities", "known.maternities", "known.paternal.sibships",
          "known.maternal.sibships", "excluded.paternities", "excluded.maternities",
          "excluded.paternal.sibships", "excluded.maternal.sibships"),
  count = c("!Number of known paternities, mismatch threshold",
            "!Number of known maternities, mismatch threshold",
            "!Number of known paternal sibships with unknown fathers",
            "!Number of known maternal sibships with unknown mothers",
            "!Number of offspring with known excluded paternity",
            "!Number of offspring with known excluded maternity",
            "!Number of offspring with known excluded paternal sibships",
            "!Number of offspring with known excluded maternal sibships"),
  row = c("!IDs of known offspring-father dyad",
          "!IDs of known offspring-mother dyad",
          "",
          "",
          "!Offspring ID, number of excluded males, the IDs of excluded males",
          "!Offspring ID, number of excluded females, the IDs of excluded females",
          "!Offspring ID, number of excluded offspring, the IDs of excluded offspring",
          "!Offspring ID, number of excluded offspring, the IDs of excluded offspring"),
  stringsAsFactors = FALSE)

# The layouts, newest first. `since` is the first Colony version that uses it.
colony.layouts <- list(
  "2.0.6" = list(since = "2.0.6", relationships = TRUE),
  "2.0.3" = list(since = "2.0.3", relationships = FALSE),
  "2.0"   = list(since = "2.0",   relationships = FALSE))

# The header and marker lines of a layout
colony.layout <- function(name){
  l <- colony.layouts[[name]]
  if(is.null(l)) stop("Unknown layout `", name, "`.", call. = FALSE)
  keep <- function(since) numeric_version(since) <= numeric_version(l$since)
  c(list(name = name),
    l,
    list(header = colony.header.spec[keep(colony.header.spec$since), ],
         markers = colony.marker.spec[keep(colony.marker.spec$since), ]))
}

# The layout that goes with a Colony version number
colony.layout.name <- function(version){
  v <- tryCatch(numeric_version(version), error = function(e) NULL)
  if(is.null(v) || length(v) != 1 || v < "2.0" || v >= "2.1"){
    stop("This function only works with Colony versions 2.0.x; `", paste(version, collapse = " "), "` is not one of them. Leave colonyVersion out to detect the layout automatically.", call. = FALSE)
  }
  if(v >= "2.0.6") "2.0.6" else if(v >= "2.0.3") "2.0.3" else "2.0"
}

# Build "value comment" lines for the keys of a table in the spec. `values`
# is a named list of the text to put on each line; a key with no value is an
# error, which catches a writer that has fallen out of step with the spec.
colony.spec.lines <- function(spec, values){
  missing <- setdiff(spec$key, names(values))
  if(length(missing) > 0) stop("No value for ", paste(missing, collapse = ", "), call. = FALSE)
  paste(unlist(values[spec$key]), spec$comment)
}

#######################################################
# Reading
#######################################################

# Entries on a line of a DAT file (the comment is dropped)
dat.tokens <- function(line){
  line <- trimws(sub("!.*$", "", line))
  if(line == "") character() else strsplit(line, "[[:space:]]+")[[1]]
}

# Read a Colony input file.
#
# The file is checked against each layout in turn (newest first), and the first
# one that fits the whole structure is used, so the Colony version does not
# have to be known. If `layout` is given only that layout is tried. With
# `relationships = TRUE` the known and excluded relationships are read too
# (Colony 2.0.6.3 or later); otherwise reading stops after the candidate
# parents. Returns a list describing the file; see parse.dat.layout().
read.colony.dat <- function(path, layout = NULL, relationships = FALSE){
  raw <- readLines(path, warn = FALSE)
  raw <- raw[!grepl("^[[:space:]]*$", raw)]
  tk <- lapply(raw, dat.tokens)
  tk <- Filter(function(t) length(t) > 0, tk)  # lines that are only a comment
  names.to.try <- if(is.null(layout)) names(colony.layouts) else layout
  problems <- character()
  for(nm in names.to.try){
    res <- tryCatch(parse.dat.layout(tk, colony.layout(nm), relationships),
                    error = function(e){problems[nm] <<- conditionMessage(e); NULL})
    if(!is.null(res)) return(res)
  }
  if(!is.null(layout)){
    stop("The file does not match the layout for this Colony version: ", problems[[1]], call. = FALSE)
  }
  stop("The file does not match any known Colony input layout.\n",
       paste0("  layout ", names(problems), ": ", problems, collapse = "\n"), call. = FALSE)
}

parse.dat.layout <- function(tk, layout, relationships = FALSE){
  pos <- 0
  fail <- function(...) stop(..., call. = FALSE)
  next.line <- function(what){
    pos <<- pos + 1
    if(pos > length(tk)) fail("the file ended while reading ", what)
    tk[[pos]]
  }
  numbers <- function(t, what){
    x <- suppressWarnings(as.numeric(t))
    if(anyNA(x)) fail(what, " (line ", pos, ") should be numeric but is `", paste(t, collapse = " "), "`")
    x
  }
  whole <- function(x, what, min = 0){
    if(any(x != floor(x)) || any(x < min)) fail(what, " (line ", pos, ") should be a whole number of at least ", min)
    x
  }
  tokens.in <- function(t, what, lo, hi = lo){
    if(length(t) < lo || length(t) > hi) fail(what, " (line ", pos, ") has ", length(t), " entries")
  }
  out <- list(layout = layout$name)

  # header
  header <- list()
  for(i in seq_len(nrow(layout$header))){
    h <- layout$header[i, ]
    t <- next.line(h$key)
    tokens.in(t, h$key, h$min.tokens, h$max.tokens)
    header[[h$key]] <- if(h$key %in% c("dataset", "output")) gsub("^['\"]|['\"]$", "", t[1]) else numbers(t, h$key)
  }
  n.off <- whole(header$n.offspring, "the number of offspring", 1)
  n.loci <- whole(header$n.loci, "the number of loci", 1)
  if(!header$known.allele.freq %in% c(0, 1)) fail("the known allele frequency flag (line ", pos, ") should be 0 or 1")
  out$header <- header

  # known allele frequencies
  if(header$known.allele.freq == 1){
    counts <- whole(numbers(next.line("the number of alleles"), "the number of alleles"), "the number of alleles", 1)
    if(length(counts) != n.loci) fail("the number of alleles (line ", pos, ") has ", length(counts), " entries but there are ", n.loci, " loci")
    rows <- vector("list", 2 * n.loci)
    for(j in seq_len(2 * n.loci)){
      t <- next.line("the allele frequencies")
      if(length(t) != counts[(j + 1) %/% 2]) fail("allele frequency line ", pos, " has ", length(t), " entries but ", counts[(j + 1) %/% 2], " were expected")
      rows[[j]] <- t
    }
    out$allele.freq <- list(table = dat.table(rows), n.alleles = counts)
  }

  # run settings
  run <- list()
  for(k in colony.run.spec$key){
    run[[k]] <- numbers(next.line(k), k)
    tokens.in(run[[k]], k, 1)
  }
  out$run <- run

  # marker rows
  markers <- list()
  for(k in layout$markers$key){
    t <- next.line(paste("the marker", k, "row"))
    tokens.in(t, paste("the marker", k, "row"), n.loci)
    markers[[k]] <- t
  }
  out$markers <- dat.table(markers)
  rownames(out$markers) <- layout$markers$key

  # genotypes
  geno <- function(n, what, strict){
    rows <- vector("list", n)
    for(j in seq_len(n)){
      t <- next.line(what)
      if(strict) tokens.in(t, what, 1 + 2 * n.loci) else tokens.in(t, what, 2, Inf)
      rows[[j]] <- t
    }
    dat.table(rows)
  }
  diploid <- header$ploidy == 0
  out$offspring <- geno(n.off, "an offspring genotype", TRUE)

  probs <- numbers(next.line("the candidate probabilities"), "the candidate probabilities")
  tokens.in(probs, "the candidate probabilities", 2)
  if(any(probs < 0 | probs > 1)) fail("the candidate probabilities (line ", pos, ") should be between 0 and 1")
  out$candidate.probabilities <- probs
  nc <- whole(numbers(next.line("the numbers of candidates"), "the numbers of candidates"), "the numbers of candidates")
  tokens.in(nc, "the numbers of candidates", 2)
  out$n.candidates <- nc
  out$fathers <- if(nc[1] > 0) geno(nc[1], "a candidate father", diploid) else NULL
  out$mothers <- if(nc[2] > 0) geno(nc[2], "a candidate mother", diploid) else NULL

  if(relationships){
    if(!isTRUE(layout$relationships)) fail("the known and excluded relationships can only be read from the 2.0.6 layout")
    out <- c(out, parse.dat.relationships(tk, pos, fail))
  }
  out
}

# A list of rows of tokens -> data frame of character columns (NA padded),
# in the same form as colony.table()
dat.table <- function(rows){
  w <- max(1, vapply(rows, length, 1L))
  m <- t(vapply(rows, function(r) c(r, rep(NA_character_, w - length(r))), character(w)))
  if(w == 1) m <- matrix(m, ncol = 1)
  x <- as.data.frame(m, stringsAsFactors = FALSE)
  names(x) <- seq_len(w)
  rownames(x) <- NULL
  x
}

# The known and excluded relationships, starting after line `pos`
parse.dat.relationships <- function(tk, pos, fail){
  next.line <- function(what){
    pos <<- pos + 1
    if(pos > length(tk)) fail("the file ended while reading ", what)
    tk[[pos]]
  }
  num <- function(t, what){
    x <- suppressWarnings(as.numeric(t))
    if(anyNA(x) || any(x < 0) || any(x != floor(x))) fail(what, " (line ", pos, ") should be whole numbers")
    x
  }
  out <- list()

  dyads <- function(key){
    h <- num(next.line(key), paste("the number of", key))
    if(length(h) != 2) fail("the number of ", key, " (line ", pos, ") should be followed by the mismatch threshold")
    d <- NULL
    for(j in seq_len(h[1])){
      t <- next.line(key)
      if(length(t) != 2) fail("a line of ", key, " (line ", pos, ") should have 2 entries")
      d <- rbind(d, data.frame(offspring = t[1], parent = t[2], stringsAsFactors = FALSE))
    }
    list(dyads = d, threshold = h[2])
  }
  sibships <- function(key){
    k <- num(next.line(key), paste("the number of", key))
    lapply(seq_len(k), function(j){
      t <- next.line(key)
      n <- num(t[1], "the size of a sibship")
      if(length(t) != n + 1) fail("a sibship (line ", pos, ") has ", length(t) - 1, " members but its size is ", n)
      t[-1]
    })
  }
  exclusions <- function(key){
    k <- num(next.line(key), paste("the number of", key))
    lapply(seq_len(k), function(j){
      t <- next.line(key)
      n <- num(t[2], "the number of exclusions")
      if(length(t) != n + 2) fail("an exclusion line (line ", pos, ") lists ", length(t) - 2, " IDs but says ", n)
      list(t[1], t[-(1:2)])
    })
  }
  out$known.paternities <- dyads("known paternities")
  out$known.maternities <- dyads("known maternities")
  out$known.paternal.sibships <- sibships("known paternal sibships")
  out$known.maternal.sibships <- sibships("known maternal sibships")
  out$excluded.paternities <- exclusions("excluded paternities")
  out$excluded.maternities <- exclusions("excluded maternities")
  out$excluded.paternal.sibships <- exclusions("excluded paternal sibships")
  out$excluded.maternal.sibships <- exclusions("excluded maternal sibships")
  out
}
