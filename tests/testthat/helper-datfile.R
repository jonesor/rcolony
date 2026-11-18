# Helpers for comparing Colony input files.

# Strip comments, blank lines and extra white space, so that only the content
# that COLONY reads is compared.
dat_content <- function(path) {
  x <- readLines(path)
  x <- sub("!.*$", "", x)
  x <- gsub("[[:space:]]+", " ", trimws(x))
  x <- x[x != ""]
  # compare numbers as numbers, so 0.0590 and 0.059 are the same
  vapply(strsplit(x, " "), function(tok) {
    num <- suppressWarnings(as.numeric(tok))
    paste(ifelse(is.na(num), tok, format(num, digits = 10)), collapse = " ")
  }, "")
}

