#' Write a Colony2 input (DAT) file from R objects
#'
#' A non-interactive way to create a Colony2 input file. All of the information
#' that \code{\link{build.colony.input}} asks for is given as arguments, so the
#' call can be kept in a script and re-run. The data can be supplied either as
#' data frames/matrices or as paths to text files. Everything is checked before
#' anything is written, and the file is written in one go, so a failed call does
#' not leave a half-written file behind.
#'
#' Each of the settings is described in more detail in
#' \code{\link{build.colony.input}}, which asks for the same information
#' interactively.
#'
#' \emph{Data tables.} A table can be a data frame or matrix, or the path to a
#' text file with columns separated by \code{delim} (white space by default),
#' with no column headings. Everything is treated as text, so IDs and alleles
#' may be numbers or strings. Rows may be ragged (shorter rows are padded).
#' \itemize{
#'   \item \code{offspring}: one row per offspring. Column 1 is the ID, followed
#'   by two columns (alleles) per locus.
#'   \item \code{markers}: four rows and one column per locus: marker ID, marker
#'   type (0 = codominant, 1 = dominant), allelic dropout rate and other
#'   typing-error rate.
#'   \item \code{fathers}, \code{mothers}: candidate parents, in the same format
#'   as \code{offspring}.
#'   \item \code{known.paternal}, \code{known.maternal}: one row per known
#'   paternal (maternal) sibship or parent-offspring relationship. Column 1 is
#'   the father (mother) ID, or 0 if the parent is unknown or not among the
#'   candidates; the remaining columns are the IDs of the offspring. Rows with a
#'   parent ID are written to the file as parent-offspring dyads, and rows with 0
#'   as sibships with an unknown parent.
#'   \item \code{excluded.paternities}, \code{excluded.maternities}: one row per
#'   offspring. Column 1 is the offspring ID, followed by the IDs of the
#'   candidate fathers (mothers) that are excluded as its parent.
#'   \item \code{excluded.paternal.sibships}, \code{excluded.maternal.sibships}:
#'   one row per offspring. Column 1 is the offspring ID, followed by the IDs of
#'   the offspring that are excluded from sharing its father (mother).
#'   \item \code{allele.freq}: the known population allele frequencies, either
#'   as a table with two rows per locus (the allele IDs, then their
#'   frequencies), or as a list with one named numeric vector per locus (names
#'   are the allele IDs; if there are no names the alleles are called 1, 2, ...).
#' }
#'
#' @param file The path of the DAT file to write.
#' @param offspring Offspring IDs and genotypes (see Details).
#' @param markers Marker IDs, types and error rates (see Details).
#' @param fathers,mothers Candidate fathers and mothers and their genotypes
#'   (see Details), or \code{NULL} if there are none.
#' @param father.prob,mother.prob The probability that the father (mother) of
#'   an offspring is included among the candidates. Required if there are
#'   candidate fathers (mothers).
#' @param known.paternal,known.maternal Known paternal and maternal sibships and
#'   parent-offspring relationships (see Details), or \code{NULL}.
#' @param paternity.threshold,maternity.threshold The mismatch threshold for
#'   known parent-offspring relationships: the largest number of loci (between
#'   0 and the number of loci) at which a known parent and offspring may be
#'   Mendelian-incompatible before the relationship is rejected. Required by
#'   Colony 2.0.6.3 and later.
#' @param excluded.paternities,excluded.maternities,excluded.paternal.sibships,excluded.maternal.sibships
#'   Known exclusions (see Details), or \code{NULL}.
#' @param allele.freq Known population allele frequencies (see Details), or
#'   \code{NULL} if the frequencies are unknown and are to be estimated.
#' @param dataset.name A name for the dataset (at most 50 characters, no
#'   spaces). Defaults to the DAT file name without its extension.
#' @param output.name The name Colony uses for its output files (at most 20
#'   characters, no spaces). Defaults to \code{dataset.name}.
#' @param note A sentence describing the dataset. It is not part of the DAT file
#'   but is kept in the project information file.
#' @param seed Seed for the random number generator (a whole number).
#' @param update.allele.freq Should allele frequencies be updated during the
#'   search, taking the inferred relationships into account?
#' @param species \code{"dioecious"} or \code{"monoecious"}.
#' @param inbreeding Should inbreeding be allowed for?
#' @param ploidy \code{"diploid"} or \code{"haplodiploid"}.
#' @param male.monogamy,female.monogamy Are males (females) monogamous? The
#'   default, \code{FALSE}, means polygamous.
#' @param clone.inference Should clones be inferred?
#' @param scale.sibship.size Should full-sibship sizes be scaled?
#' @param sibship.prior The sibship-size prior: \code{"none"}, \code{"weak"},
#'   \code{"medium"}, \code{"strong"} or \code{"optimal"}.
#' @param mean.sibship.size The mean paternal and maternal sibship sizes, as a
#'   vector of two numbers. Required unless \code{sibship.prior = "none"}.
#' @param n.runs The number of runs (a whole number).
#' @param run.length \code{"short"}, \code{"medium"} or \code{"long"}.
#' @param monitor.by Is progress reported every \code{monitor.interval}
#'   \code{"iterations"} or \code{"seconds"}?
#' @param monitor.interval The monitoring interval.
#' @param platform \code{"unix"} (Mac/Linux) or \code{"windows"}. By default
#'   the platform R is running on.
#' @param likelihood \code{"full"} or \code{"pairwise"} likelihood method.
#' @param precision The precision of the full-likelihood method: \code{"low"},
#'   \code{"medium"} or \code{"high"}.
#' @param version The Colony version the file is for. Only version 2.0.6.3 or
#'   later is accepted. The same file layout is used for all of them, and only
#'   version 2.0.7.2 has been tested.
#' @param delim The column separator in any text files given as paths
#'   (default is white space).
#' @param project.info Should a short, human-readable summary of the settings
#'   (including \code{note}) be written next to the DAT file, as
#'   \code{<name>_ProjectInformation.txt}?
#' @param overwrite Should an existing file be overwritten?
#' @return The path of the DAT file, invisibly.
#' @author Owen R. Jones
#' @seealso \code{\link{build.colony.input}}, \code{\link{run.colony}}
#' @examples
#' \dontrun{
#' write.colony.input("Colony2.DAT",
#'                    offspring = "offspring.txt", markers = "markers.txt",
#'                    fathers = "males.txt", father.prob = 0.8,
#'                    mothers = "females.txt", mother.prob = 0.8,
#'                    known.paternal = "known_fathers.txt",
#'                    paternity.threshold = 2)
#' }
#' @export
write.colony.input <- function(file = "Colony2.DAT",
                               offspring, markers,
                               fathers = NULL, mothers = NULL,
                               father.prob = NULL, mother.prob = NULL,
                               known.paternal = NULL, known.maternal = NULL,
                               paternity.threshold = 0, maternity.threshold = 0,
                               excluded.paternities = NULL,
                               excluded.maternities = NULL,
                               excluded.paternal.sibships = NULL,
                               excluded.maternal.sibships = NULL,
                               allele.freq = NULL,
                               dataset.name = tools::file_path_sans_ext(basename(file)),
                               output.name = dataset.name, note = "",
                               seed = 1234, update.allele.freq = FALSE,
                               species = c("dioecious", "monoecious"),
                               inbreeding = FALSE,
                               ploidy = c("diploid", "haplodiploid"),
                               male.monogamy = FALSE, female.monogamy = FALSE,
                               clone.inference = FALSE, scale.sibship.size = TRUE,
                               sibship.prior = c("none", "weak", "medium", "strong", "optimal"),
                               mean.sibship.size = NULL,
                               n.runs = 1,
                               run.length = c("medium", "short", "long"),
                               monitor.by = c("iterations", "seconds"),
                               monitor.interval = 10000,
                               platform = if (.Platform$OS.type == "windows") "windows" else "unix",
                               likelihood = c("full", "pairwise"),
                               precision = c("high", "medium", "low"),
                               version = "2.0.7.2", delim = "",
                               project.info = TRUE, overwrite = FALSE) {

  species <- match.arg(species)
  ploidy <- match.arg(ploidy)
  sibship.prior <- match.arg(sibship.prior)
  run.length <- match.arg(run.length)
  monitor.by <- match.arg(monitor.by)
  likelihood <- match.arg(likelihood)
  precision <- match.arg(precision)
  platform <- match.arg(platform, c("unix", "windows"))

  if(missing(offspring)) stop("`offspring` is required.", call. = FALSE)
  if(missing(markers)) stop("`markers` is required.", call. = FALSE)
  if(numeric_version(version) < "2.0.6.3"){
    stop("Only Colony version 2.0.6.3 or later is supported (earlier versions use a different file layout).", call. = FALSE)
  }
  if(file.exists(file) && !overwrite){
    stop("`", file, "` already exists. Use overwrite = TRUE to replace it.", call. = FALSE)
  }
  if(!dir.exists(dirname(file))) stop("The directory `", dirname(file), "` does not exist.", call. = FALSE)

  #######################################################
  #Check single settings
  #######################################################
  is.num1 <- function(x) is.numeric(x) && length(x) == 1 && !is.na(x)
  is.whole1 <- function(x) is.num1(x) && x == floor(x)
  is.flag <- function(x) is.logical(x) && length(x) == 1 && !is.na(x)
  for(nm in c("update.allele.freq", "inbreeding", "male.monogamy", "female.monogamy", "clone.inference", "scale.sibship.size", "project.info", "overwrite")){
    if(!is.flag(get(nm))) stop("`", nm, "` must be TRUE or FALSE.", call. = FALSE)
  }
  if(!is.whole1(seed)) stop("`seed` must be a whole number.", call. = FALSE)
  if(!is.whole1(n.runs) || n.runs < 1) stop("`n.runs` must be a whole number of at least 1.", call. = FALSE)
  if(!is.num1(monitor.interval) || monitor.interval <= 0) stop("`monitor.interval` must be a positive number.", call. = FALSE)
  for(nm in c("dataset.name", "output.name")){
    x <- get(nm)
    if(!is.character(x) || length(x) != 1 || is.na(x) || x == "" || grepl("[[:space:]]", x)){
      stop("`", nm, "` must be a single non-empty string without spaces.", call. = FALSE)
    }
  }
  if(nchar(dataset.name) > 50) stop("`dataset.name` must be at most 50 characters.", call. = FALSE)
  if(nchar(output.name) > 20) stop("`output.name` must be at most 20 characters.", call. = FALSE)
  if(!is.character(note) || length(note) != 1 || is.na(note)) stop("`note` must be a single string.", call. = FALSE)

  if(sibship.prior == "none"){
    mean.sibship.size <- c(0, 0)
  }else{
    if(!is.numeric(mean.sibship.size) || length(mean.sibship.size) != 2 || any(is.na(mean.sibship.size)) || any(mean.sibship.size <= 0)){
      stop("`mean.sibship.size` must be two positive numbers (paternal, maternal) when a sibship prior is used.", call. = FALSE)
    }
  }

  #######################################################
  #Read and check the data tables
  #######################################################
  offspring <- colony.table(offspring, "offspring", delim)
  markers <- colony.table(markers, "markers", delim)

  if(nrow(markers) != 4) stop("`markers` must have 4 rows (ID, type, dropout rate, error rate) but has ", nrow(markers), ".", call. = FALSE)
  n.loci <- ncol(markers)
  if(anyNA(markers)) stop("`markers` contains missing values.", call. = FALSE)
  if(!all(markers[2, ] %in% c("0", "1"))) stop("Marker types (row 2 of `markers`) must be 0 (codominant) or 1 (dominant).", call. = FALSE)
  rates <- suppressWarnings(as.numeric(unlist(markers[3:4, ])))
  if(anyNA(rates) || any(rates < 0 | rates > 1)) stop("Marker error rates (rows 3 and 4 of `markers`) must be numbers between 0 and 1.", call. = FALSE)

  geno.table <- function(x, name){
    x <- colony.table(x, name, delim)
    if(is.null(x)) return(NULL)
    if(ploidy == "diploid" && ncol(x) != 1 + 2 * n.loci){
      stop("`", name, "` has ", ncol(x), " columns but ", 1 + 2 * n.loci, " were expected (an ID and two alleles for each of the ", n.loci, " loci).", call. = FALSE)
    }
    if(anyDuplicated(x[[1]])) stop("`", name, "` has duplicated IDs: ", paste(unique(x[[1]][duplicated(x[[1]])]), collapse = ", "), call. = FALSE)
    x
  }
  offspring <- geno.table(offspring, "offspring")
  fathers <- geno.table(fathers, "fathers")
  mothers <- geno.table(mothers, "mothers")
  n.offspring <- nrow(offspring)
  n.fathers <- if(is.null(fathers)) 0 else nrow(fathers)
  n.mothers <- if(is.null(mothers)) 0 else nrow(mothers)

  check.prob <- function(p, n, name){
    if(n == 0){
      if(is.null(p)) p <- 0
    }
    if(is.null(p)) stop("`", name, "` is required when there are candidate parents.", call. = FALSE)
    if(!is.num1(p) || p < 0 || p > 1) stop("`", name, "` must be a number between 0 and 1.", call. = FALSE)
    p
  }
  father.prob <- check.prob(father.prob, n.fathers, "father.prob")
  mother.prob <- check.prob(mother.prob, n.mothers, "mother.prob")

  # Known parentage / sibships
  known <- function(x, name, parents, parent.label, threshold, threshold.name){
    x <- colony.table(x, name, delim)
    if(!is.whole1(threshold) || threshold < 0 || threshold > n.loci){
      stop("`", threshold.name, "` must be a whole number between 0 and the number of loci (", n.loci, ").", call. = FALSE)
    }
    out <- list(dyads = NULL, sibships = NULL, threshold = threshold)
    if(is.null(x) || nrow(x) == 0) return(out)
    ids <- x[[1]]
    if(anyNA(ids)) stop("`", name, "` has rows with no ID in the first column (use 0 for an unknown ", parent.label, ").", call. = FALSE)
    real <- ids[ids != "0"]
    if(anyDuplicated(real)) stop("`", name, "` lists a ", parent.label, " more than once: ", paste(unique(real[duplicated(real)]), collapse = ", "), call. = FALSE)
    if(!all(real %in% parents)){
      stop("`", name, "` contains ", parent.label, "s that are not among the candidates: ", paste(setdiff(real, parents), collapse = ", "), call. = FALSE)
    }
    kids <- unlist(x[, -1, drop = FALSE], use.names = FALSE)
    kids <- kids[!is.na(kids)]
    if(!all(kids %in% offspring[[1]])){
      stop("`", name, "` contains offspring that are not in `offspring`: ", paste(setdiff(kids, offspring[[1]]), collapse = ", "), call. = FALSE)
    }
    d <- NULL
    for(i in which(ids != "0")){
      o <- unlist(x[i, -1], use.names = FALSE); o <- o[!is.na(o)]
      if(length(o) > 0) d <- rbind(d, data.frame(offspring = o, parent = ids[i], stringsAsFactors = FALSE))
    }
    s <- lapply(which(ids == "0"), function(i){o <- unlist(x[i, -1], use.names = FALSE); o[!is.na(o)]})
    s <- Filter(function(o) length(o) > 0, s)
    out$dyads <- d
    out$sibships <- s
    out
  }
  kp <- known(known.paternal, "known.paternal", fathers[[1]], "father", paternity.threshold, "paternity.threshold")
  km <- known(known.maternal, "known.maternal", mothers[[1]], "mother", maternity.threshold, "maternity.threshold")

  # Exclusions
  excl <- function(x, name, allowed, what){
    x <- colony.table(x, name, delim)
    if(is.null(x) || nrow(x) == 0) return(list())
    if(anyNA(x[[1]])) stop("`", name, "` has rows with no offspring ID in the first column.", call. = FALSE)
    if(!all(x[[1]] %in% offspring[[1]])){
      stop("`", name, "` contains offspring that are not in `offspring`: ", paste(setdiff(x[[1]], offspring[[1]]), collapse = ", "), call. = FALSE)
    }
    if(anyDuplicated(x[[1]])) stop("`", name, "` lists an offspring more than once: ", paste(unique(x[[1]][duplicated(x[[1]])]), collapse = ", "), call. = FALSE)
    rows <- lapply(seq_len(nrow(x)), function(i){o <- unlist(x[i, -1], use.names = FALSE); c(x[[1]][i], list(o[!is.na(o)]))})
    bad <- unique(unlist(lapply(rows, function(r) setdiff(r[[2]], allowed))))
    if(length(bad) > 0) stop("`", name, "` contains ", what, " that are not in the data: ", paste(bad, collapse = ", "), call. = FALSE)
    rows
  }
  ex.pat <- excl(excluded.paternities, "excluded.paternities", fathers[[1]], "fathers")
  ex.mat <- excl(excluded.maternities, "excluded.maternities", mothers[[1]], "mothers")
  ex.psib <- excl(excluded.paternal.sibships, "excluded.paternal.sibships", offspring[[1]], "offspring")
  ex.msib <- excl(excluded.maternal.sibships, "excluded.maternal.sibships", offspring[[1]], "offspring")

  # Allele frequencies
  if(!is.null(allele.freq)){
    if(is.list(allele.freq) && !is.data.frame(allele.freq)){
      af <- lapply(allele.freq, function(f){
        ids <- names(f); if(is.null(ids)) ids <- seq_along(f)
        rbind(as.character(ids), as.character(f))
      })
      width <- max(vapply(af, ncol, 1L))
      af <- do.call(rbind, lapply(af, function(m) cbind(m, matrix(NA_character_, 2, width - ncol(m)))))
      allele.freq <- as.data.frame(af, stringsAsFactors = FALSE)
    }else{
      allele.freq <- colony.table(allele.freq, "allele.freq", delim)
    }
    if(nrow(allele.freq) != 2 * n.loci){
      stop("`allele.freq` must have two rows for each of the ", n.loci, " loci but has ", nrow(allele.freq), " rows.", call. = FALSE)
    }
    id.rows <- allele.freq[seq(1, 2 * n.loci, 2), , drop = FALSE]
    fr.rows <- allele.freq[seq(2, 2 * n.loci, 2), , drop = FALSE]
    n.alleles <- rowSums(!is.na(fr.rows))
    if(any(n.alleles != rowSums(!is.na(id.rows)))) stop("`allele.freq` has loci where the number of allele IDs and frequencies differ.", call. = FALSE)
    fr <- suppressWarnings(matrix(as.numeric(as.matrix(fr.rows)), nrow = n.loci))
    if(any(is.na(fr[!is.na(as.matrix(fr.rows))])) || any(fr < 0, na.rm = TRUE)) stop("`allele.freq` has frequencies that are not non-negative numbers.", call. = FALSE)
    bad <- which(abs(rowSums(fr, na.rm = TRUE) - 1) > 0.01)
    if(length(bad) > 0) warning("Allele frequencies do not sum to 1 (within 0.01) at loci: ", paste(bad, collapse = ", "), call. = FALSE)
  }

  #######################################################
  #Build the file
  #######################################################
  row.text <- function(x) paste(x[!is.na(x)], collapse = " ")
  table.text <- function(x, comment = NULL){
    out <- vapply(seq_len(nrow(x)), function(i) row.text(unlist(x[i, ], use.names = FALSE)), "")
    if(!is.null(comment) && length(out) > 0) out[1] <- paste(out[1], comment)
    out
  }
  flag <- function(x) as.integer(x)
  num <- function(x) format(x, scientific = FALSE, trim = TRUE)

  L <- c(
    paste(dataset.name, "! C, Dataset name, Length<51"),
    paste(output.name, "! C, Main output file name, Length<21"),
    paste(n.offspring, "! I, Number of offspring in the sample"),
    paste(n.loci, "! I, Number of loci"),
    paste(num(seed), "! I, Seed for random number generator"),
    paste(flag(update.allele.freq), "! B, 0/1=Not updating/updating allele frequency"),
    paste(if(species == "dioecious") 2 else 1, "! 2/1=Dioecious/Monoecious"),
    paste(flag(inbreeding), "! B, 0/1=No inbreeding/inbreeding"),
    paste(if(ploidy == "diploid") 0 else 1, "! B, 0/1=Diploid species/HaploDiploid species"),
    paste(flag(male.monogamy), flag(female.monogamy), "! B, 0/1=Polygamy/Monogamy for males & females"),
    paste(flag(clone.inference), "! B, 0/1=Clone inference =No/Yes"),
    paste(flag(scale.sibship.size), "! B, 0/1=Full sibship size scaling =No/Yes"),
    paste(match(sibship.prior, c("none", "weak", "medium", "strong", "optimal")) - 1, num(mean.sibship.size[1]), num(mean.sibship.size[2]),
          "! 0/1/2/3/4=No/Weak/Medium/Strong/Optimal sibship prior; mean paternal & maternal sibship size"),
    paste(flag(!is.null(allele.freq)), "! B, 0/1=Unknown/Known population allele frequency")
  )
  if(!is.null(allele.freq)){
    L <- c(L, "", paste(paste(n.alleles, collapse = " "), "!Number of alleles per locus"), table.text(allele.freq))
  }
  L <- c(L, "",
    paste(n.runs, "! I, Number of runs"),
    paste(match(run.length, c("short", "medium", "long")), "! I, Length of Run (1, 2, 3) = (Short, Medium, Long)"),
    paste(if(monitor.by == "iterations") 0 else 1, "! B, 0/1=Monitor method by Iterate#/Time in second"),
    paste(num(monitor.interval), "! I, Monitor interval in Iterate#/Seconds"),
    paste(if(platform == "windows") 1 else 0, "! B, 0/1=Other platform/Windows execution"),
    paste(if(likelihood == "full") 1 else 0, "! 1/0=Full-likelihood/pairwise-likelihood score method"),
    paste(match(precision, c("low", "medium", "high")), "! 1/2/3=low/medium/high precision"),
    "",
    paste(row.text(markers[1, ]), "!Marker IDs"),
    paste(row.text(markers[2, ]), "!Marker types, 0/1=Codominant/Dominant"),
    paste(row.text(markers[3, ]), "!Marker-specific allelic dropout rate"),
    paste(row.text(markers[4, ]), "!Other marker-specific typing-error rate"),
    "",
    table.text(offspring, "!Offspring ID and genotypes"),
    "",
    paste(father.prob, mother.prob, "!Probabilities that the father and mother of an offspring included in candidates"),
    paste(n.fathers, n.mothers, "!Numbers of candidate males and females"),
    "",
    if(n.fathers > 0) table.text(fathers),
    "",
    if(n.mothers > 0) table.text(mothers),
    ""
  )

  dyad.block <- function(k, label, kind){
    n <- if(is.null(k$dyads)) 0 else nrow(k$dyads)
    # The threshold only applies to dyads, so it is written as 0 when there are none
    # (as in the example dataset distributed with COLONY).
    c(paste(n, if(n > 0) k$threshold else 0, paste0("!Number of known ", label, ", mismatch threshold")),
      if(n > 0) paste(k$dyads$offspring, k$dyads$parent, c(paste("!IDs of known offspring-", kind, " dyad"), rep("", n - 1))),
      "")
  }
  sib.block <- function(k, adjective, parent){
    n <- length(k$sibships)
    c(paste(n, paste0("!Number of known ", adjective, " sibships with unknown ", parent, "s")),
      vapply(k$sibships, function(o) paste(length(o), paste(o, collapse = " ")), ""),
      "")
  }
  excl.block <- function(rows, label, what){
    c(paste(length(rows), paste0("!Number of offspring with known excluded ", label)),
      vapply(seq_along(rows), function(i){
        r <- rows[[i]]
        paste(r[[1]], length(r[[2]]), paste(r[[2]], collapse = " "),
              if(i == 1) paste0("!Offspring ID, number of excluded ", what, ", the IDs of excluded ", what) else "")
      }, ""),
      "")
  }
  L <- c(L,
    dyad.block(kp, "paternities", "father"),
    dyad.block(km, "maternities", "mother"),
    sib.block(kp, "paternal", "father"),
    sib.block(km, "maternal", "mother"),
    excl.block(ex.pat, "paternity", "males"),
    excl.block(ex.mat, "maternity", "females"),
    excl.block(ex.psib, "paternal sibships", "offspring"),
    excl.block(ex.msib, "maternal sibships", "offspring"))

  writeLines(trimws(L, which = "right"), file)

  if(project.info){
    info <- c(
      paste("DAT file :", normalizePath(file)),
      paste("Colony version (file layout) :", version),
      paste("Number of loci :", n.loci),
      paste("Number of offspring in the sample :", n.offspring),
      paste("Number of male candidates :", n.fathers),
      paste("Number of female candidates :", n.mothers),
      paste("Probability the father / mother is a candidate :", father.prob, "/", mother.prob),
      paste("Number of known paternities / paternal sibships :", if(is.null(kp$dyads)) 0 else nrow(kp$dyads), "/", length(kp$sibships), "(mismatch threshold", paste0(kp$threshold, ")")),
      paste("Number of known maternities / maternal sibships :", if(is.null(km$dyads)) 0 else nrow(km$dyads), "/", length(km$sibships), "(mismatch threshold", paste0(km$threshold, ")")),
      paste("Offspring with excluded fathers / mothers :", length(ex.pat), "/", length(ex.mat)),
      paste("Offspring with excluded paternal / maternal sibships :", length(ex.psib), "/", length(ex.msib)),
      paste("Species :", species, "/", ploidy),
      paste("Male / female mating system :", if(male.monogamy) "monogamous" else "polygamous", "/", if(female.monogamy) "monogamous" else "polygamous"),
      paste("Inbreeding :", inbreeding),
      paste("Clone inference :", clone.inference),
      paste("Full sibship size scaling :", scale.sibship.size),
      paste("Sibship prior :", sibship.prior, if(sibship.prior != "none") paste0("(mean paternal ", mean.sibship.size[1], ", maternal ", mean.sibship.size[2], ")")),
      paste("Known population allele frequencies :", !is.null(allele.freq)),
      paste("Update allele frequencies :", update.allele.freq),
      paste("Seed :", num(seed)),
      paste("Number of runs :", n.runs),
      paste("Length of run :", run.length),
      paste("Monitoring :", paste("every", num(monitor.interval), monitor.by)),
      paste("Likelihood method :", likelihood, if(likelihood == "full") paste0("(", precision, " precision)")),
      paste("Platform :", platform),
      paste("Input file produced :", format(Sys.time())),
      paste("NOTE to the project :", note))
    writeLines(info, file.path(dirname(file), paste0(tools::file_path_sans_ext(basename(file)), "_ProjectInformation.txt")))
  }

  invisible(normalizePath(file))
}

# Read a data table that was supplied as a path, data frame or matrix, and
# return a data frame of character columns with NA for empty cells. NULL stays
# NULL.
colony.table <- function(x, name, delim = ""){
  if(is.null(x)) return(NULL)
  if(is.character(x) && length(x) == 1 && is.null(dim(x))){
    if(!file.exists(x)) stop("`", name, "`: the file `", x, "` does not exist.", call. = FALSE)
    n <- max(utils::count.fields(x, sep = delim), 1)
    x <- utils::read.table(x, header = FALSE, colClasses = "character", sep = delim, fill = TRUE, flush = TRUE, na.strings = "", col.names = seq_len(n))
  }else if(is.matrix(x) || is.data.frame(x)){
    x <- as.data.frame(x, stringsAsFactors = FALSE)
  }else{
    stop("`", name, "` must be a data frame, a matrix or the path to a file.", call. = FALSE)
  }
  x[] <- lapply(x, function(col){col <- trimws(as.character(col)); col[col == ""] <- NA; col})
  names(x) <- seq_len(ncol(x))
  x
}
