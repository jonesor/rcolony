# rcolony

`rcolony` provides tools for the implementation of the
[COLONY](https://www.zsl.org/about-zsl/resources/software/colony) pedigree
analysis software (Jinliang Wang, Zoological Society of London) from within R.
In addition to helping you build and run COLONY analyses, it provides functions
for visualising and summarising the results.

The package helps with the whole COLONY workflow:

1. **Build** a COLONY input file, either from a script or with a guided wizard.
2. **Run** COLONY from within R.
3. **Import** the results into a single R object.
4. **Summarise and visualise** them.

## Installation

`rcolony` is not on CRAN. Install the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("jonesor/rcolony")
```

You will also need the COLONY program itself, which is available (free, for
academic use) from the
[ZSL COLONY page](https://www.zsl.org/about-zsl/resources/software/colony).
The package has been tested against COLONY 2.0.7.2.

## Functions

| Function                 | Purpose                                                              |
| ------------------------ | -------------------------------------------------------------------- |
| `write.colony.input()`   | Write a COLONY input (`.DAT`) file from R objects or files. Scriptable. |
| `build.colony.input()`   | Interactive wizard that asks the same questions, then calls the above. |
| `run.colony()`           | Run COLONY on a `.DAT` file from within R.                           |
| `get.colony.data()`      | Read COLONY's input and output files into a single R list object.    |
| `get.parentage()`        | Extract maternity/paternity assignments from that object.            |
| `plotsibs()`             | Plot inferred full- and half-sibships.                               |
| `categoricalxy()`        | Plot two-dimensional categorical (e.g. parentage) data.              |
| `monitor.colony()`, `get.interm.data()` | Follow the intermediate results of a running analysis (see below). |

## Workflow

### 1. Prepare your data

COLONY needs tables of genotypes and, optionally, information about known and
excluded relationships. Each can be a data frame, a matrix, or the path to a
text file with no column headings (white-space separated by default; see the
`delim` argument).

| Table                       | Contents                                                                     |
| --------------------------- | ---------------------------------------------------------------------------- |
| `offspring`                 | One row per offspring: ID, then two columns (alleles) per locus.             |
| `markers`                   | Four rows, one column per locus: marker ID, type (0 = codominant, 1 = dominant), allelic dropout rate, other error rate. |
| `fathers`, `mothers`        | Candidate parents, in the same format as `offspring` (optional).             |
| `known.paternal`, `known.maternal` | One row per known sibship or parent-offspring relationship: the parent's ID (or `0` if unknown or not a candidate), then the offspring IDs. |
| `excluded.paternities`, `excluded.maternities` | One row per offspring: its ID, then the candidate parents excluded as its father (mother). |
| `excluded.paternal.sibships`, `excluded.maternal.sibships` | One row per offspring: its ID, then the offspring excluded from sharing its father (mother). |
| `allele.freq`               | Known population allele frequencies (optional), as two rows per locus (allele IDs, then frequencies) or as a list of named numeric vectors. |

`?write.colony.input` describes every table and setting.

### 2. Build the input file

**From a script (recommended).** `write.colony.input()` takes the data and the
settings as arguments, checks everything, and writes the file in one go, so the
whole analysis can be kept in a script and repeated:

```r
library(rcolony)

# A small made-up dataset: 10 offspring, 5 candidate fathers and mothers, 4 loci
set.seed(1)
n.loci <- 4
genotypes <- function(ids) {
  alleles <- matrix(sample(1:5, length(ids) * 2 * n.loci, replace = TRUE), nrow = length(ids))
  data.frame(id = ids, alleles)
}
offspring <- genotypes(paste0("O", 1:10))
fathers   <- genotypes(paste0("M", 1:5))
mothers   <- genotypes(paste0("F", 1:5))

# Marker ID, type (0 = codominant), allelic dropout rate, other error rate
markers <- rbind(paste0("mk", 1:n.loci), 0, 0, 0.01)

write.colony.input("Colony2.DAT",
                   offspring = offspring, markers = markers,
                   fathers = fathers, father.prob = 0.8,   # probability the true father is a candidate
                   mothers = mothers, mother.prob = 0.8,
                   dataset.name = "example", output.name = "example",
                   sibship.prior = "weak", mean.sibship.size = c(2, 2),
                   run.length = "short", precision = "low")
```

With real data you would normally give file paths instead, for example
`offspring = "offspring.txt"`. Known and excluded relationships and known
allele frequencies are added with the other arguments listed above; a mismatch
threshold is needed for known parent-offspring relationships
(`paternity.threshold`, `maternity.threshold`). A short summary of the settings
is written alongside the file as `<name>_ProjectInformation.txt` (use
`project.info = FALSE` to skip it). An existing file is not overwritten unless
you say `overwrite = TRUE`.

**Interactively.** `build.colony.input()` asks the same questions one at a time
(see `?build.colony.input` for what each means), checks each answer as you give
it, and asks again if there is a problem:

```r
build.colony.input(name = "Colony2.DAT")
```

At the end it prints the equivalent `write.colony.input()` call and asks you to
confirm before it writes the file. Copy that call into your script and you can
repeat the analysis without answering the questions again.

### 3. Run COLONY

```r
run.colony("path/to/colony2s.out", "Colony2.DAT")
```

`colony2s.out` is the executable in the 2.0.7.2 Mac download; use whichever
executable you have for your platform. COLONY is run in the directory that holds
the `.DAT` file, and writes its output files there. With no arguments,
`run.colony()` asks you to select the executable and the `.DAT` file. Long
analyses can take a long time, so you may want to run them outside R, for
example in a terminal with `colony2s.out IFN:Colony2.DAT`.

To follow the intermediate results of a running analysis, use
`run.colony.deprecated(monitor = TRUE, wait = FALSE)` together with
`monitor.colony()` and `get.interm.data()`. `run.colony()` does not produce the
file they read. This route is for Mac and Linux only, and has not been
re-tested with recent versions of COLONY.

### 4. Read, summarise and visualise the results

Point `get.colony.data()` at the directory holding the `.DAT` file and COLONY's
output (include the trailing slash). It detects the layout of the input file
itself, so you do not need to say which COLONY version produced it:

```r
mydata <- get.colony.data("path/to/output/")

# Summarise parentage assignments
parents <- get.parentage(mydata)

# Visualise parentage frequencies and sibships
categoricalxy(parents, colscheme = "heat", axes.cex = 0.5)
plotsibs(mydata, maintitle = "Sibships (full)", pairwise = FALSE)
```

Summaries and plots are fully reproducible. The package ships with a parsed
example object, `testdata`, so you can try them without running COLONY:

```r
data(testdata)
head(get.parentage(testdata))
categoricalxy(get.parentage(testdata), colscheme = "heat", axes.cex = 0.5)
plotsibs(testdata, maintitle = "Sibships (full)", pairwise = FALSE)
```

## COLONY version support

The layout of the COLONY input file changed between releases (extra header
lines were added in 2.0.6, and an extra marker row in 2.0.3). `get.colony.data()`
detects the layout from the structure of the file, so you do not need to say
which COLONY version produced it; `colonyVersion` is only needed to insist on a
particular layout. `write.colony.input()` and `build.colony.input()` write the
current layout (COLONY 2.0.6.3 or later), and have been tested against COLONY
2.0.7.2. The layout is described once, in `R/colony-format.R`, and used by both
the writer and the reader.

## Testing

```r
devtools::test()   # or testthat::test_local()
```

Tests of the input writer, the wizard and the output parsers need no COLONY
binary. To also run the end-to-end tests against a real COLONY (about a minute),
set the path to the executable first:

```r
Sys.setenv(RCOLONY_EXEC = "~/Software/colony_2.0.7.2/colony2s.out")
```

## Citation

Jones, O.R. & Wang, J. (2010) COLONY: a program for parentage and sibship
inference from multilocus genotype data. *Molecular Ecology Resources* 10:
551–555. <https://doi.org/10.1111/j.1755-0998.2009.02787.x>

## Links

- COLONY program: <https://www.zsl.org/about-zsl/resources/software/colony>
- Report a bug: <https://github.com/jonesor/rcolony/issues>

## License

GPL-3
