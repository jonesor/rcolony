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
  #Read and check the data tables (see colony-checks.R)
  #######################################################
  markers <- check.markers(markers, delim)
  n.loci <- ncol(markers)
  offspring <- check.genotypes(offspring, "offspring", n.loci, ploidy, delim)
  fathers <- check.genotypes(fathers, "fathers", n.loci, ploidy, delim)
  mothers <- check.genotypes(mothers, "mothers", n.loci, ploidy, delim)
  n.offspring <- nrow(offspring)
  n.fathers <- if(is.null(fathers)) 0 else nrow(fathers)
  n.mothers <- if(is.null(mothers)) 0 else nrow(mothers)
  father.prob <- check.probability(father.prob, n.fathers, "father.prob")
  mother.prob <- check.probability(mother.prob, n.mothers, "mother.prob")

  paternity.threshold <- check.threshold(paternity.threshold, "paternity.threshold", n.loci)
  maternity.threshold <- check.threshold(maternity.threshold, "maternity.threshold", n.loci)
  kp <- check.known(known.paternal, "known.paternal", fathers[[1]], offspring[[1]], "father", delim)
  km <- check.known(known.maternal, "known.maternal", mothers[[1]], offspring[[1]], "mother", delim)
  kp$threshold <- paternity.threshold
  km$threshold <- maternity.threshold

  ex.pat <- check.exclusions(excluded.paternities, "excluded.paternities", offspring[[1]], fathers[[1]], "fathers", delim)
  ex.mat <- check.exclusions(excluded.maternities, "excluded.maternities", offspring[[1]], mothers[[1]], "mothers", delim)
  ex.psib <- check.exclusions(excluded.paternal.sibships, "excluded.paternal.sibships", offspring[[1]], offspring[[1]], "offspring", delim)
  ex.msib <- check.exclusions(excluded.maternal.sibships, "excluded.maternal.sibships", offspring[[1]], offspring[[1]], "offspring", delim)

  if(!is.null(allele.freq)){
    af <- check.allele.freq(allele.freq, n.loci, delim)
    allele.freq <- af$table
    n.alleles <- af$n.alleles
  }

  #######################################################
  #Build the file. The order of the lines and their comments come from the
  #layout description in colony-format.R, which the reader uses as well.
  #######################################################
  layout <- colony.layout(colony.layout.name(version))
  row.text <- function(x) paste(x[!is.na(x)], collapse = " ")
  table.text <- function(x, comment = NULL){
    out <- vapply(seq_len(nrow(x)), function(i) row.text(unlist(x[i, ], use.names = FALSE)), "")
    if(!is.null(comment) && length(out) > 0) out[1] <- paste(out[1], comment)
    out
  }
  flag <- function(x) as.integer(x)
  num <- function(x) format(x, scientific = FALSE, trim = TRUE)

  L <- colony.spec.lines(layout$header, list(
    dataset = dataset.name,
    output = output.name,
    n.offspring = n.offspring,
    n.loci = n.loci,
    seed = num(seed),
    update.allele.freq = flag(update.allele.freq),
    species = if(species == "dioecious") 2 else 1,
    inbreeding = flag(inbreeding),
    ploidy = if(ploidy == "diploid") 0 else 1,
    mating = paste(flag(male.monogamy), flag(female.monogamy)),
    clone.inference = flag(clone.inference),
    scale.sibship.size = flag(scale.sibship.size),
    sibship.prior = paste(match(sibship.prior, c("none", "weak", "medium", "strong", "optimal")) - 1,
                          num(mean.sibship.size[1]), num(mean.sibship.size[2])),
    known.allele.freq = flag(!is.null(allele.freq))))
  if(!is.null(allele.freq)){
    L <- c(L, "", paste(paste(n.alleles, collapse = " "), colony.allele.freq.comment), table.text(allele.freq))
  }
  L <- c(L, "",
    colony.spec.lines(colony.run.spec, list(
      n.runs = n.runs,
      run.length = match(run.length, c("short", "medium", "long")),
      monitor.by = if(monitor.by == "iterations") 0 else 1,
      monitor.interval = num(monitor.interval),
      platform = if(platform == "windows") 1 else 0,
      likelihood = if(likelihood == "full") 1 else 0,
      precision = match(precision, c("low", "medium", "high")))),
    "",
    colony.spec.lines(layout$markers, stats::setNames(
      lapply(seq_len(nrow(layout$markers)), function(i) row.text(markers[i, ])), layout$markers$key)),
    "",
    table.text(offspring, colony.candidate.comments[["offspring"]]),
    "",
    paste(father.prob, mother.prob, colony.candidate.comments[["probabilities"]]),
    paste(n.fathers, n.mothers, colony.candidate.comments[["numbers"]]),
    "",
    if(n.fathers > 0) table.text(fathers),
    "",
    if(n.mothers > 0) table.text(mothers),
    "")

  section <- colony.section.spec
  rownames(section) <- section$key
  dyad.block <- function(key, k){
    n <- if(is.null(k$dyads)) 0 else nrow(k$dyads)
    # The threshold only applies to dyads, so it is written as 0 when there are none
    # (as in the example dataset distributed with COLONY).
    c(paste(n, if(n > 0) k$threshold else 0, section[key, "count"]),
      if(n > 0) paste(k$dyads$offspring, k$dyads$parent, c(section[key, "row"], rep("", n - 1))),
      "")
  }
  sib.block <- function(key, k){
    c(paste(length(k$sibships), section[key, "count"]),
      vapply(k$sibships, function(o) paste(length(o), paste(o, collapse = " ")), ""),
      "")
  }
  excl.block <- function(key, rows){
    c(paste(length(rows), section[key, "count"]),
      vapply(seq_along(rows), function(i){
        r <- rows[[i]]
        paste(r[[1]], length(r[[2]]), paste(r[[2]], collapse = " "), if(i == 1) section[key, "row"] else "")
      }, ""),
      "")
  }
  L <- c(L,
    dyad.block("known.paternities", kp),
    dyad.block("known.maternities", km),
    sib.block("known.paternal.sibships", kp),
    sib.block("known.maternal.sibships", km),
    excl.block("excluded.paternities", ex.pat),
    excl.block("excluded.maternities", ex.mat),
    excl.block("excluded.paternal.sibships", ex.psib),
    excl.block("excluded.maternal.sibships", ex.msib))

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
