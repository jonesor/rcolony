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
