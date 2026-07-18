# rcolony

`rcolony` provides tools for the implementation of the
[COLONY](https://www.zsl.org/about-zsl/resources/software/colony) pedigree
analysis software (Jinliang Wang, Zoological Society of London) from within R.
In addition to helping you build and run COLONY analyses, it provides functions
for visualising and summarising the results.

The package helps with the whole COLONY workflow:

1. **Build** a COLONY input file with a guided, wizard-like function.
2. **Run** COLONY from within R.
3. **Monitor** a running analysis.
4. **Import, summarise and visualise** the results.

## Installation

`rcolony` is not on CRAN. Install the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("jonesor/rcolony")
```

You will also need the COLONY program itself, which is available (free, for
academic use) from the
[ZSL COLONY page](https://www.zsl.org/about-zsl/resources/software/colony).

## Functions

| Function                 | Purpose                                                        |
| ------------------------ | -------------------------------------------------------------- |
| `build.colony.input()`   | Interactive wizard to build a COLONY input (`.DAT`) file.      |
| `run.colony()`           | Run COLONY on a `.DAT` file from within R.                     |
| `monitor.colony()`       | Plot intermediate results while COLONY is running.             |
| `get.interm.data()`      | Collect the intermediate data produced by a running analysis. |
| `get.colony.data()`      | Read COLONY output files into a single R list object.          |
| `get.parentage()`        | Extract maternity/paternity assignments from that object.      |
| `plotsibs()`             | Plot inferred full- and half-sibships.                         |
| `categoricalxy()`        | Plot two-dimensional categorical (e.g. parentage) data.        |

## Example

Building and running an analysis is interactive (you are prompted for settings
and file locations), so those steps are shown here in outline:

```r
library(rcolony)

# 1. Build an input file (you will be prompted for settings and data files).
#    See ?build.colony.input for a full description of each prompt.
build.colony.input(name = "Colony2.DAT")

# 2. Run COLONY (you will be prompted to locate the executable and the .DAT).
run.colony()
```

Reading and visualising results is fully reproducible. The package ships with a
parsed example object, `testdata`, so you can try the summary and plotting
functions without running COLONY:

```r
library(rcolony)
data(testdata)

# Summarise parentage assignments.
parents <- get.parentage(testdata)
head(parents)

# Visualise parentage frequencies and sibships.
categoricalxy(parents, colscheme = "heat", axes.cex = 0.5)
plotsibs(testdata, maintitle = "Sibships (full)", pairwise = FALSE)
```

To read your own results, point `get.colony.data()` at the directory holding the
COLONY output and `.DAT` file:

```r
mydata <- get.colony.data("/path/to/output/", colonyVersion = "2.0.7")
```

## COLONY version support

`get.colony.data()` understands the input-file layouts of COLONY versions
`2.0`, `2.0.3`, `2.0.6` and `2.0.7` (the last covering the current
2.0.6.x/2.0.7.x releases). Pass the matching version via the `colonyVersion`
argument; `build.colony.input()` writes the current-format input file.

## Citation

Jones, O.R. & Wang, J. (2010) COLONY: a program for parentage and sibship
inference from multilocus genotype data. *Molecular Ecology Resources* 10:
551–555. <https://doi.org/10.1111/j.1755-0998.2009.02787.x>

## Links

- COLONY program: <https://www.zsl.org/about-zsl/resources/software/colony>
- Report a bug: <https://github.com/jonesor/rcolony/issues>

## License

GPL-3
