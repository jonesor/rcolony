# rcolony 1.5.0

## COLONY 2.0.7.2 compatibility

Tested end to end against COLONY 2.0.7.2 (Mac build, 15 July 2025).

* `get.colony.data()` accepts `colonyVersion = "2.0.7.1"` and `"2.0.7.2"`
  (same input-file layout as 2.0.7), and now reads COLONY 2.0.7.x output
  correctly: comma-delimited `*.Maternity`/`*.Paternity` files, and pairwise
  parentage files whose headers contain `#` (the new `#OffGtype`, `#PrntGtype`,
  `#PairGtype`, `#ExcLoci` columns are returned as `X.OffGtype` etc.).

* `build.colony.input()` now asks for, and writes, the mismatch threshold that
  COLONY (>= 2.0.6.3) requires on the known-paternity and known-maternity
  lines. Previously COLONY rejected any input file that had known dyads.

## Reader and writer share one description of the file layout

* The layout of a COLONY input file (which header lines each version has, the
  run settings, the marker rows, and the order of the known and excluded
  relationships) is now described once, in `R/colony-format.R`, and used by both
  `write.colony.input()` and `get.colony.data()`. Previously the writer built the
  lines by hand and the reader hard-coded line numbers (22, 23 and 26) for each
  version.

* `get.colony.data()` now reads the input file by its structure and detects the
  layout itself, so `colonyVersion` is optional (it now defaults to `NULL`;
  the default used to be `"2.0"`, which mis-read files from later versions).
  Giving a version still works and still stops with an error if the file does
  not match it. The result is unchanged for files that were read correctly
  before.

* Fixed in the process, found by checking against 13 real input files from
  COLONY 2.0.3 era datasets: files with no candidate fathers or mothers were
  read as having two phantom candidates called "0"; and a file containing
  whitespace-only lines (not just empty ones) was read one line out of step.

## New features

* New `write.colony.input()` creates a COLONY input file non-interactively,
  from data frames or file paths, so an analysis can be scripted and re-run.
  All settings are checked before anything is written, and the file is written
  in one go. It writes a short `<name>_ProjectInformation.txt` summary beside
  the DAT file (`project.info = FALSE` to skip).

* `build.colony.input()` is now a thin layer over `write.colony.input()`. Its
  prompts and their order are unchanged, but each answer is checked as soon as
  it is given (and asked again if it is invalid), the equivalent
  `write.colony.input()` call is printed so that the analysis can be repeated
  from a script, and the file is written only after a final confirmation. It
  no longer writes the seven copies of the data tables (`OffspringGenotype.txt`,
  `MarkerTypeErrorRate.txt`, `AlleleFrequency.txt` and so on) that COLONY and
  rcolony never used, and the `wd` argument now really sets where the file is
  written. The project note is now read as a whole sentence rather than just
  its first word.

## Bug fixes

* `build.colony.input()` wrote the wrong count for each offspring on an
  excluded paternal or maternal sibship line (one too many), which COLONY
  rejects ("Error in reading known excluded paternal sibship").


* `build.colony.input()` no longer fails with "the condition has length > 1"
  when there are known paternities, maternities or sibships.

* `get.colony.data()`: the default `filename` now matches only files ending in
  `.DAT`, rather than any file whose name contains "dat" (such as
  `*.data` and `*.GtypeData`).

* `get.colony.data()` no longer appends a spurious `"Unknown"` row to the
  offspring table, which inflated the Unknown/Unknown count in
  `get.parentage()` by one. `get.parentage()` and `plotsibs()` ignore the row
  in objects created by earlier versions, including `testdata`.

* `get.parentage(pairwise = TRUE)` no longer fails (it merged on column names
  that do not exist).

* `run.colony()` runs COLONY in the directory containing the DAT file, so the
  output files are written there rather than in the caller's working
  directory.

## Testing

* Added a testthat suite. Tests of the input wizard and of the parsers need no
  COLONY binary; an end-to-end test (simulated pedigree with candidate parents,
  known and excluded relationships, genotyping error) runs when the environment
  variable `RCOLONY_EXEC` points at a COLONY executable.

# rcolony 1.4.0

## New features

* `build.colony.input()` now writes input files in the format used by current
  COLONY releases (2.0.6.x / 2.0.7.x). It prompts for, and writes, the
  parameter lines that later COLONY versions added: inbreeding, clone
  inference and full-sibship-size scaling. The sibship-prior prompt now uses
  the `0/1/2/3/4` (No / Weak / Medium / Strong / Optimal) encoding.

* `get.colony.data()` gains support for `colonyVersion = "2.0.6"` and
  `"2.0.7"`, accounting for the extra header lines those versions add. It now
  fails with a clear error, rather than silently mis-parsing, when the chosen
  `colonyVersion` does not match the input file.

## Bug fixes

* `get.colony.data()` no longer crashes when a project contains no
  sibship-dyad output files, and the pairwise sibships are now stored in
  `$pairwise.sibs` instead of overwriting the non-pairwise `$sibs`.

* `get.colony.data()` reads the unknown/known allele-frequency flag from the
  correct line for each COLONY version, and the known-allele-frequency offset
  is corrected (a previous off-by-one).

* `get.colony.data()`'s "too many matching output files" warning now fires
  when it should (it was previously unreachable).

* `build.colony.input()` correctly re-prompts after a non-whole-number entry
  for the number of candidate fathers/mothers and known paternities/
  maternities, writes allele frequencies to `AlleleFrequency.txt` (rather than
  overwriting `MarkerTypeErrorRate.txt`), and reports the correct known-sibship
  and excluded-maternal-sibship counts in `ProjectInformation.txt`.

* `monitor.colony()` correctly truncates `temp.txt` to its last lines, and no
  longer overwrites its `n` argument.
