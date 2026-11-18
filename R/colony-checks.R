# Internal helpers that read and validate the inputs to a Colony input file.
# They are shared by write.colony.input(), which runs them all before writing
# anything, and by build.colony.input(), which runs each one as soon as the user
# has chosen a file so that a mistake can be corrected immediately. Each stops
# with an informative message (without a call) if the input is not valid.

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

is.num1 <- function(x) is.numeric(x) && length(x) == 1 && !is.na(x)
is.whole1 <- function(x) is.num1(x) && x == floor(x)

# Marker IDs, types and error rates: 4 rows, 1 column per locus.
check.markers <- function(markers, delim = ""){
  markers <- colony.table(markers, "markers", delim)
  if(nrow(markers) != 4) stop("`markers` must have 4 rows (ID, type, dropout rate, error rate) but has ", nrow(markers), ".", call. = FALSE)
  if(anyNA(markers)) stop("`markers` contains missing values.", call. = FALSE)
  if(!all(markers[2, ] %in% c("0", "1"))) stop("Marker types (row 2 of `markers`) must be 0 (codominant) or 1 (dominant).", call. = FALSE)
  rates <- suppressWarnings(as.numeric(unlist(markers[3:4, ])))
  if(anyNA(rates) || any(rates < 0 | rates > 1)) stop("Marker error rates (rows 3 and 4 of `markers`) must be numbers between 0 and 1.", call. = FALSE)
  markers
}

# Genotype tables (offspring, candidate fathers, candidate mothers).
check.genotypes <- function(x, name, n.loci, ploidy = "diploid", delim = ""){
  x <- colony.table(x, name, delim)
  if(is.null(x)) return(NULL)
  if(ploidy == "diploid" && ncol(x) != 1 + 2 * n.loci){
    stop("`", name, "` has ", ncol(x), " columns but ", 1 + 2 * n.loci, " were expected (an ID and two alleles for each of the ", n.loci, " loci).", call. = FALSE)
  }
  if(anyNA(x[[1]])) stop("`", name, "` has rows with no ID in the first column.", call. = FALSE)
  if(anyDuplicated(x[[1]])) stop("`", name, "` has duplicated IDs: ", paste(unique(x[[1]][duplicated(x[[1]])]), collapse = ", "), call. = FALSE)
  x
}

check.probability <- function(p, n.candidates, name){
  if(is.null(p) && n.candidates == 0) p <- 0
  if(is.null(p)) stop("`", name, "` is required when there are candidate parents.", call. = FALSE)
  if(!is.num1(p) || p < 0 || p > 1) stop("`", name, "` must be a number between 0 and 1.", call. = FALSE)
  p
}

check.threshold <- function(threshold, name, n.loci){
  if(!is.whole1(threshold) || threshold < 0 || threshold > n.loci){
    stop("`", name, "` must be a whole number between 0 and the number of loci (", n.loci, ").", call. = FALSE)
  }
  threshold
}

# Known paternal/maternal sibships and parent-offspring dyads. Returns the
# dyads (offspring, parent) and the sibships with an unknown parent (a list of
# offspring IDs).
check.known <- function(x, name, parents, offspring.ids, parent.label, delim = ""){
  out <- list(dyads = NULL, sibships = list(), n.rows = 0)
  x <- colony.table(x, name, delim)
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
  if(!all(kids %in% offspring.ids)){
    stop("`", name, "` contains offspring that are not in `offspring`: ", paste(setdiff(kids, offspring.ids), collapse = ", "), call. = FALSE)
  }
  d <- NULL
  for(i in which(ids != "0")){
    o <- unlist(x[i, -1], use.names = FALSE); o <- o[!is.na(o)]
    if(length(o) > 0) d <- rbind(d, data.frame(offspring = o, parent = ids[i], stringsAsFactors = FALSE))
  }
  s <- lapply(which(ids == "0"), function(i){o <- unlist(x[i, -1], use.names = FALSE); o[!is.na(o)]})
  out$dyads <- d
  out$sibships <- Filter(function(o) length(o) > 0, s)
  out$n.rows <- nrow(x)
  out
}

# Exclusions: one row per offspring, then the IDs that are excluded. Returns a
# list with one element per row: list(offspring ID, excluded IDs).
check.exclusions <- function(x, name, offspring.ids, allowed, what, delim = ""){
  x <- colony.table(x, name, delim)
  if(is.null(x) || nrow(x) == 0) return(list())
  if(anyNA(x[[1]])) stop("`", name, "` has rows with no offspring ID in the first column.", call. = FALSE)
  if(!all(x[[1]] %in% offspring.ids)){
    stop("`", name, "` contains offspring that are not in `offspring`: ", paste(setdiff(x[[1]], offspring.ids), collapse = ", "), call. = FALSE)
  }
  if(anyDuplicated(x[[1]])) stop("`", name, "` lists an offspring more than once: ", paste(unique(x[[1]][duplicated(x[[1]])]), collapse = ", "), call. = FALSE)
  rows <- lapply(seq_len(nrow(x)), function(i){o <- unlist(x[i, -1], use.names = FALSE); list(x[[1]][i], o[!is.na(o)])})
  bad <- unique(unlist(lapply(rows, function(r) setdiff(r[[2]], allowed))))
  if(length(bad) > 0) stop("`", name, "` contains ", what, " that are not in the data: ", paste(bad, collapse = ", "), call. = FALSE)
  rows
}

# Known population allele frequencies, as a two-rows-per-locus table or a list
# of named numeric vectors. Returns the table and the number of alleles per
# locus.
check.allele.freq <- function(x, n.loci, delim = ""){
  if(is.list(x) && !is.data.frame(x)){
    af <- lapply(x, function(f){
      ids <- names(f); if(is.null(ids)) ids <- seq_along(f)
      rbind(as.character(ids), as.character(f))
    })
    width <- max(vapply(af, ncol, 1L))
    af <- do.call(rbind, lapply(af, function(m) cbind(m, matrix(NA_character_, 2, width - ncol(m)))))
    x <- as.data.frame(af, stringsAsFactors = FALSE)
  }
  x <- colony.table(x, "allele.freq", delim)
  if(nrow(x) != 2 * n.loci){
    stop("`allele.freq` must have two rows for each of the ", n.loci, " loci but has ", nrow(x), " rows.", call. = FALSE)
  }
  id.rows <- x[seq(1, 2 * n.loci, 2), , drop = FALSE]
  fr.rows <- x[seq(2, 2 * n.loci, 2), , drop = FALSE]
  n.alleles <- rowSums(!is.na(fr.rows))
  if(any(n.alleles != rowSums(!is.na(id.rows)))) stop("`allele.freq` has loci where the number of allele IDs and frequencies differ.", call. = FALSE)
  fr <- suppressWarnings(matrix(as.numeric(as.matrix(fr.rows)), nrow = n.loci))
  if(any(is.na(fr[!is.na(as.matrix(fr.rows))])) || any(fr < 0, na.rm = TRUE)) stop("`allele.freq` has frequencies that are not non-negative numbers.", call. = FALSE)
  bad <- which(abs(rowSums(fr, na.rm = TRUE) - 1) > 0.01)
  if(length(bad) > 0) warning("Allele frequencies do not sum to 1 (within 0.01) at loci: ", paste(bad, collapse = ", "), call. = FALSE)
  list(table = x, n.alleles = n.alleles)
}
