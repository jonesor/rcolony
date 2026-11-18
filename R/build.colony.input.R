#' A wizard-like function to help build Colony2 input files.
#'
#' This function prompts the user to provide information about their system in
#' order to correctly build and format a Colony2 input file.
#'
#' This wizard will guide the user through the process of creating an input
#' file to be executed by Colony2. Each answer is checked as soon as it is
#' given (for example, that a data file has the expected number of rows and
#' columns, and that the IDs in known and excluded relationships exist), and the
#' question is asked again if there is a problem.
#'
#' At the end the wizard shows the equivalent call to
#' \code{\link{write.colony.input}}, which does the actual work, and asks for
#' confirmation before writing the file. Keep that call in a script if you want
#' to repeat the analysis without answering the questions again.
#'
#' \emph{Dataset name.} Provide a name for the dataset.
#'
#' \emph{Output file name.} Provide a name for the output file.
#'
#' \emph{Note for the project.} You can enter any text here, such as when you
#' set up the project, notes about the dataset, etc.
#'
#' \emph{Number of offspring.} Provide the number of offspring present in the
#' sample.
#'
#' \emph{Number of loci.} Provide the maximum number of marker loci genotyped
#' for the individuals in your sample.
#'
#' \emph{Random number seed.} Provide a seed for the random number generator.
#' Colony takes a simulated annealing algorithm to search for the ML
#' configuration. It is a Monte Carlo method similar to MCMC, with a fine
#' control of re-configuration acceptance rate though "temperature". Starting
#' from the initial configuration in which all individuals are unrelated except
#' for those individuals with known relationships, a random change is made to
#' part of the configuration to generate a new configuration. The likelihoods
#' of the new and old configurations are then calculated and compared to
#' determine whether the new one is accepted or rejected. If the new likelihood
#' is larger than the old one, then the new configuration is accepted.
#' Otherwise, an acceptance rate is calculated using the current temperature,
#' the new and old likelihood values, and is compared with a random number
#' drawn from a uniform distribution in the range of [0,1]. If the random
#' number value is smaller than the acceptance rate, the new configuration is
#' still accepted although it is inferior to the old one. This is intended to
#' avoid the algorithm getting stuck on a local maximum in the likelihood
#' surface. Therefore, the random number seed partially determines the
#' searching path. With exactly the same data and parameter values, different
#' runs using different random number seeds may give slightly different final
#' best configuration and likelihood values. Such a case occurs occasionally
#' when there is not enough information in the maker data to infer the genetic
#' structure, the actual genetic structure of the sample is extremely weak, or
#' the sample size is very large (i.e. thousands of individuals). For example,
#' when the number of markers is small, and/or the markers are not informative
#' (few alleles with uneven frequency distribution), and/or most families are
#' extremely small (e.g. one offspring per sibship), it is difficult to have
#' replicate runs (using different random number seeds) converge to the same
#' best configuration. One can do multiple runs for the same dataset by using
#' different random number seeds to check/confirm the reliability of the
#' analysis results. In the case replicate runs yield different results, the
#' good news is that relationships reliably inferred are usually reconstructed
#' consistently among runs, while dubious relationships are inferred
#' inconsistently among the runs. One just needs to focus on those reliable,
#' consistent relationships and ignore (abandon) those unreliable, inconsistent
#' relationships in downstream analyses.
#'
#' \emph{Should allele frequency be updated?} Allele frequencies are required
#' in calculating the likelihood of a configuration. These frequencies can be
#' provided by the user (see below) or are calculated by Colony using the
#' genotypes in OFS, CMS (optional) and CFS (optional). In the latter case, you
#' can ask Colony to update allele frequency estimates by taking into account
#' of the inferred sibship and parentage relationships during the process of
#' searching for the maximum likelihood configuration. However, updating allele
#' frequencies could increase computational time substantially, and may not
#' improve relationship inference much if the genetic structure of your sample
#' is not strong (i.e. family sizes small and evenly distributed, most
#' candidates are not assigned parentage). I suggest not updating allele
#' frequencies except when family sizes (unknown) are large relative to sample
#' size.
#'
#' \emph{Species ploidy.} Select whether the species is diploid or
#' haplodiploid.  Colony can be used for both diploid species and haplodiploid
#' species. In both cases, the offspring are always assumed to be diploid (for
#' haploid offspring, please use a previous version of Colony). In the
#' haplodiploid case, males and females are assumed to be haploid and diploid
#' respectively (for species with diploid males and haploid females, you just
#' need to swap the two sexes).
#'
#' \emph{Male and female mating system.} You will be asked whether males, and
#' females are monogamous or polygamous. Here in our specific context, male
#' monogamous signifies that two offspring in the OFS sample must be fathered
#' by 2 different males if they have separate mothers. In other words, male
#' monogamous specifies that no paternal halfsibs exist in the OFS sample. Note
#' that the mating system herein is defined with regard to the samples being
#' analyzed, not to the population or species from where the samples are taken.
#' For example, consider a population in which males mate singly with females
#' in a breeding season but mate with different females in different breeding
#' seasons. An OFS sample with individuals taken from multiple breeding seasons
#' may contain offspring from different mothers but from a single male (i.e.
#' paternal halfsibs). Therefore, for the purpose of the Colony analysis, the
#' male mating system should still be set as polygamous. The female mating
#' system is similarly defined. Note also that when both males and females are
#' defined as polygamous and the markers have genotyping errors, the
#' computation can become very slow simply because all offspring in the OFS can
#' be related in the pedigree and must be considered together in computing the
#' likelihood of a configuration.
#'
#' \emph{Inbreeding.} Indicate whether inbreeding should be allowed for. If no
#' inbreeding is assumed, the population is taken to be in Hardy-Weinberg
#' equilibrium and inbreeding is not inferred.
#'
#' \emph{Clone inference.} Indicate whether clones (individuals sharing an
#' identical multilocus genotype) should be inferred. If clones are not
#' inferred, all offspring genotypes are assumed to come from distinct
#' individuals.
#'
#' \emph{Full sibship size scaling.} Indicate whether full-sibship sizes should
#' be scaled. Scaling is generally recommended and is selected by default.
#'
#' \emph{Sibship size prior.} You can choose to use, or not use, a prior
#' distribution for the paternal and maternal sibship sizes of the offspring.
#' Select \dQuote{No sibship prior} if you have no idea about the average
#' sibship size, or you simply do not want to use a prior. Otherwise select a
#' \dQuote{Weak}, \dQuote{Medium} or \dQuote{Strong} prior according to how much
#' weight you want the prior to carry, or \dQuote{Optimal} to let Colony
#' determine the prior itself. For any option other than \dQuote{No sibship
#' prior} you are required to provide the average paternal (\eqn{np}) and
#' maternal (\eqn{nm}) sibship sizes. Using paternal
#' sibship prior as an example, the prior probability is calculated using
#' Ewen's sampling formula as follows. Suppose paternal sibship size
#' distribution is \eqn{m = {m1, m2,\ldots , mn}}, where \eqn{mi (i=1, \ldots,
#' n)} is the number of paternal sibships each consisting of exactly \eqn{i}
#' offspring. The total number of offspring is \deqn{k= \sum_{i=1}^1 im_i}, and
#' the average number of non-empty paternal sibships (= the number of
#' contributing fathers) is \deqn{k= \sum_{i=0}^{n-1} \alpha/(i+\alpha)}, where
#' \eqn{\alpha} is a concentration parameter that determines the degree to
#' which individuals are allocated to the same father. We can substitute
#' \eqn{k} by \deqn{n/np} and solve numerically for \eqn{\alpha}. Given
#' \eqn{\alpha}, the prior probability of \eqn{m={m1, m2, \ldots, mn}} is
#' \deqn{\frac{n!}{\prod_{i=0}^{n-1} (\alpha+i)} \prod_{i=1}^{n}
#' \biggl(\frac{\alpha}{i}\biggr)^{m_i} \frac{1}{m_i!}}.
#'
#' Note that whenever the male or female mating system parameters have changed,
#' the sibship prior is reset automatically to the default value. Therefore, if
#' you decide to use the sibship prior, you should input the prior parameters
#' \emph{after} setting the mating system parameters.
#'
#' \emph{Allele frequency (known/unknown).} You should indicate whether
#' population allele frequencies are known or unknown. If you select unknown
#' the allele frequencies will be estimated from the current dataset. If you
#' select known, because, for examplepopulation allele frequencies have been
#' estimated from another larger, more appropriate sample, you will be prompted
#' to select the allele frequency file. The file should be prepared, before
#' selection, in the following format.
#'
#' Each locus takes 2 consecutive rows. The first row lists the allele
#' names/identifications (using an unique integer number, 1-999999999), and the
#' second row lists the corresponding frequencies of the alleles. Alleles (or
#' allele frequencies) on the same row should be separated by a comma or white
#' space. The first two rows are for locus 1, the 3rd and 4th rows are for
#' locus 2, etc. Within a locus, allele names/identifications must unique but
#' are not necessarily ordered or sequential. Alleles at different loci are
#' allowed to have the same identification number.
#'
#' An example file of the allele frequency data is shown below.
#'
#' Note that when allele frequencies are specified as known, the allele
#' frequency file loaded should contain all alleles found in the offspring and
#' candidate genotypes. Otherwise, an error occurs in running Colony. Note also
#' that for a dominant locus, only two alleles are allowed and they are always
#' indexed as 1 to indicate the dominant allele (presence of a band) and 2 to
#' indicate the recessive allele (absence of a band when homozygous).
#'
#' The function will check the loaded file for number of loci, and that the
#' frequencies are numeric rather than characters.
#'
#' \emph{Number of runs:} For the same dataset and parameters of a project,
#' multiple runs can be conducted so that the best configuration with the
#' maximum likelihood is more likely to be found and the uncertainties of the
#' estimates (see below) are more reliable. However, it is very time costly to
#' do multiple runs. Furthermore, in typical situations a single run suffices.
#'
#' \emph{Length of run:} Longer runs consider more configurations in the
#' searching process and thus are more likely to find the maximum likelihood
#' configuration, but take more time to do so. In most cases, a medium run is a
#' good compromise.
#'
#' Note that marker loci have an implicit order which should be followed
#' consistently in the entire input. For example, in offspring, candidate male
#' and female genotype data, in allele frequency data and in marker type and
#' genotyping error data, the same order of marker loci must be followed. In
#' all these files, the first locus or locus 1, for example, must refer to the
#' same marker.
#'
#' \emph{Monitor by iteration, or by time in seconds?} Indicate whether you
#' wish to monitor by iteration, or by time in seconds.
#'
#' \emph{Monitor interval.} Give a number to specify the interval by which
#' intermediate results are directed to the monitor. The unit of the interval
#' is number of iterates or seconds, depending on how you choose to monitor
#' Colony.
#'
#' \emph{Platform.} Indicate what computer platform the file is intended to run
#' on (Microsoft Windows/Other).
#'
#' \emph{Likelihood method.} Indicate whether full or pairwise likelihood
#' methods should be used.
#'
#' \emph{Precision.} Indicate whether low, medium, or high precision should be
#' used in the analysis.
#'
#' \emph{Select the marker type and error rate file.} You will be prompted to
#' select the file for the markers. In the file, 4 values are provided for each
#' marker (in columns). The first value (on row 1) specifies the marker name or
#' ID (consisting of a maximum of 20 letters/numbers, others such as space,
#' comma, full stop, forward and backward slashes are not allowed in the
#' name/ID). The second (on row 2) indicates the marker type, whether it is
#' codominant (0) or dominant (1). The third and fourth values (on rows 3 and 4
#' respectively) give the allelic dropout rate and the rate of other kinds of
#' genotyping errors (including mutations) of the marker. For more information
#' about the models of genotyping errors, see \cite{Wang (2004)}.
#'
#' Note that this file sets the order of marker loci that must be followed in
#' all the following input. The first column is for locus 1, the second for
#' locus 2, etc.
#'
#' Note also that computation becomes slow when markers suffer from genotyping
#' errors. This is especially obvious when both males and females are specified
#' as polygamous.
#'
#' An example file is shown below. Note the column headers should not be
#' included in the file. The column headers are added by rcolony automatically
#' when loading the file. This is true for all of the following files loaded
#' into rcolony.
#'
#' \tabular{rrrrr}{ mk1 \tab mk2 \tab mk3 \tab mk4 \tab mk5 \cr 0 \tab 0 \tab 0
#' \tab 0 \tab 0 \cr 0.0000 \tab 0.0000 \tab 0.0000 \tab 0.0000 \tab 0.0000 \cr
#' 0.0001 \tab 0.0001 \tab 0.0001 \tab 0.0001 \tab 0.0001 }
#'
#' \emph{Offspring genotypes.}
#'
#' You will be prompted to select the offspring genotype file. The file
#' contains the individual IDs and the genotypes at each locus. Each individual
#' takes a single row. The first column gives the individual ID (a string
#' containing a maximum of 20 letters and/or numbers, no other characters are
#' allowed), the 2nd and 3rd columns give the alleles observed for the
#' individual at the first locus, the 4th and 5th give the alleles observed for
#' the individual at the 2nd locus, etc. An allele is identified by an integer,
#' in the range of 1-999999999. If the locus is a dominant marker, then only
#' one (instead of 2) column is required for the marker, and the value for the
#' genotype should be either 1 (dominant phenotype, presence of a band) or 2
#' (recessive phenotype, absence of a band). Missing genotypes are indicated by
#' 0 0 for a codominant marker and 0 for a dominant marker. Note that offspring
#' IDs should be unique. They are case sensitive, which means that, for
#' example, "offspring2" and "Offspring2" are treated as different. An
#' offspring with missing genotypes at all loci (no marker information at all)
#' should not be included in the offspring genotype file.
#'
#' Part of an example offspring genotype file is shown below. Note the column
#' headers (e.g. Offspring) should not be included in the offspring genotype
#' file. They are added by rcolony automatically when loaded.
#'
#' [table removed]
#'
#' rcolony checks the number of individuals and the number of loci present in
#' the file.
#'
#' \emph{Probability an actual father included in candidates}.  Provide a guess
#' (estimate) of the probability that the actual father of an offspring in the
#' OFS is included in the CMS sample.
#'
#' \emph{Number of candidate fathers.} Provide the number of candidate males
#' included in the CMS sample. Note that known fathers are also included in the
#' CMS sample. The minimum value is 0, in which case paternity is not inferred.
#'
#' \emph{Select the candidate father data file.} The format of male genotype
#' file is the same as the offspring genotype file, except for 2 cases. One is
#' that, for haplodiploid species (in which males are assumed to be haploid),
#' each locus takes just one column rather than 2 columns, no matter the marker
#' is codominant or dominant. The other is that if an individual already
#' included in the offspring genotype file is present in the candidate male
#' file, it is not necessary to provide its genotypes, just the first column
#' for its individual ID will do.
#'
#' rcolony will check the number of individuals and loci in the file
#'
#' \emph{Probability an actual mother included in candidates}.  Provide a guess
#' (estimate) of the probability that the actual mother of an offspring in the
#' OFS is included in the CMS sample.
#'
#' \emph{Number of candidate mothers.} Provide the number of candidate females
#' included in the CMS sample. Note that known mothers are also included in the
#' CMS sample. The minimum value is 0, in which case maternity is not inferred.
#'
#' \emph{Select the candidate mother data file.} When the number of candidate
#' females is larger than 0, the user is asked to load a file containing the
#' candidate female genotypes. The format of female genotype file is the same
#' as the offspring genotype file, except that if an individual already
#' included in the offspring genotype file is present in the candidate female
#' file, it is not necessary to provide its genotypes, just the first column
#' for its individual ID will do.
#'
#' rcolony will check the number of individuals and loci in the file
#'
#' \emph{Number of known paternal diads.} Provide the number of known
#' paternal-offspring diads included in the samples. The minimum value is 0.
#'
#' \emph{Select the paternal diads file.} If the number of known paternal diads
#' is larger than zero, then you will be prompted to select a file for the
#' known paternal diads. This file should have 2 columns, the first gives
#' father ID, while the second gives the corresponding offspring ID.
#'
#' \emph{Number of known maternal diads.} Provide the number of known
#' maternal-offspring diads included in the samples. The minimum value is 0.
#'
#' \emph{Select the maternal diads file.} If the number of known maternal diads
#' is larger than zero, then you will be prompted to select a file for the
#' known maternal diads. This file should have 2 columns, the first gives
#' mother ID, while the second gives the corresponding offspring ID.
#'
#' \emph{Number of known paternal sibships.} Provide the number of known
#' paternal sibship or paternity included in the samples. The minimum value is
#' 0. A known paternal sibship contains all of the offspring in the OFS sample
#' who are known to share the same father no matter whether the father is known
#' or not.
#'
#' \emph{Select the paternal sibships file.} If the number of known paternal
#' sibship/paternity is larger than zero, then you will be prompted to select a
#' file for the known paternal sibship/paternity. In the file, each known
#' paternal sibship/paternity takes a row, with the first column containing the
#' father ID/name if the male is known and included in the candidate males or a
#' value of 0 to indicate that the father is unknown or not in the candidate
#' males. From the 2nd column on, the ID/name of each member of the paternal
#' sibship is listed.
#'
#' \emph{Mismatch threshold (paternal).} If there are known paternal diads or
#' sibships, you will be asked for a mismatch threshold: an integer from 0 to
#' the number of loci. A known paternal-offspring diad is rejected by Colony if
#' the two genotypes are Mendelian-incompatible at more loci than this. It is
#' written on the same line as the number of known paternal diads, as required
#' by Colony 2.0.6.3 and later.
#'
#' \emph{Number of known maternal sibships.} Provide the number of known
#' maternal sibship or maternity included in the samples. The minimum value is
#' 0. A known maternal sibship contains all of the offspring in the OFS sample
#' who are known to share the same mother no matter whether the mother is known
#' or not.
#'
#' \emph{Select the maternal sibships file.} If the number of known maternal
#' sibship/maternity is larger than zero, then you will be prompted to select a
#' file for the known maternal sibship/maternity. In the file, each known
#' maternal sibship/maternity takes a row, with the first column containing the
#' mother ID/name if the male is known and included in the candidate females or
#' a value of 0 to indicate that the mother is unknown or not in the candidate
#' females. From the 2nd column on, the ID/name of each member of the maternal
#' sibship is listed.
#'
#' \emph{Mismatch threshold (maternal).} If there are known maternal diads or
#' sibships, you will be asked for a mismatch threshold: an integer from 0 to
#' the number of loci. A known maternal-offspring diad is rejected by Colony if
#' the two genotypes are Mendelian-incompatible at more loci than this. It is
#' written on the same line as the number of known maternal diads, as required
#' by Colony 2.0.6.3 and later.
#'
#' \emph{Define the number of excluded paternities.} Provide the number of
#' offspring that each has at least one known excluded candidate male as its
#' father. The minimum value is 0.
#'
#' \emph{Provide a path to the excluded paternities file.} If the number of
#' offspring with known excluded paternity is larger than zero, then you will
#' be prompted to select a file that specifies the excluded candidate males for
#' an offspring. Each offspring with excluded paternity takes one row. The
#' first entry of the row is the offspring ID/name, followed by the IDs of the
#' candidate males that are excluded parentage.
#'
#' \emph{Define the number of excluded maternities.} Provide the number of
#' offspring that each has at least one known excluded candidate female as its
#' mother. The minimum value is 0.
#'
#' \emph{Provide a path to the excluded maternities file.} If the number of
#' offspring with known excluded maternity is larger than zero, then you will
#' be prompted to select a file that specifies the excluded candidate females
#' for an offspring. Each offspring with excluded maternity takes one row. The
#' first entry of the row is the offspring ID/name, followed by the IDs of the
#' candidate females that are excluded parentage.
#'
#' \emph{Number of offspring with known excluded paternal sibships.} Provide
#' the number of offspring that are known to each have at least one excluded
#' offspring as its paternal sibling. The minimum value is 0.
#'
#' \emph{Excluded paternal shibships file.} If the number of offspring with
#' known excluded paternal sibships is larger than zero, then you will be
#' prompted to select a file that specifies the excluded offspring as paternal
#' siblings for an offspring. Each offspring with one or more excluded paternal
#' siblings takes one row. The first entry of the row is the offspring ID/name,
#' followed by the IDs of the offsprings that are excluded paternal siblings.
#'
#' \emph{Number of offspring with known excluded maternal sibships.} Provide
#' the number of offspring that are known to each have at least one excluded
#' offspring as its maternal sibling. The minimum value is 0.
#'
#' \emph{Excluded maternal shibships file.} If the number of offspring with
#' known excluded maternal sibships is larger than zero, then you will be
#' prompted to select a file that specifies the excluded offspring as maternal
#' siblings for an offspring. Each offspring with one or more excluded maternal
#' siblings takes one row. The first entry of the row is the offspring ID/name,
#' followed by the IDs of the offsprings that are excluded maternal siblings.
#'
#' @param wd The directory where the generated file will be placed. The default
#' is the current working directory.
#' @param name The desired filename for the Colony input file (the default is
#' Colony2.DAT).
#' @param delim What is the delimiter for the input files (default is that they
#' are whitespace delimited).
#' @return A text file is produced. This file can be used by Colony2 as an
#' input file. The path of the file is returned invisibly (or \code{NULL} if
#' the wizard was cancelled before the file was written).
#' @author Owen R. Jones
#' @seealso \code{\link{write.colony.input}} for a non-interactive version that
#' can be scripted, \code{\link{run.colony}}
#' @references Wang, J. (2004) Sibship reconstruction from genetic data with
#' typing errors.  Genetics 166: 1963-1979.
#' @keywords manip
#' @export
build.colony.input <- function(wd=getwd(), name = "Colony2.DAT", delim = ""){

  # This function only asks the questions. It checks each answer as soon as it
  # is given (so that a mistake can be corrected straight away) and passes the
  # answers to write.colony.input(), which builds the file. The equivalent call
  # is printed so that the analysis can be repeated from a script.

  cat("This function will construct a Colony input file.\nPLEASE REFER TO THE HELP FILE ?build.colony.input.\n\n")
  cat(paste("It will be called", name, "and be placed in", wd, "...\n\n\n"))

  #######################################################
  #Helpers that repeat a question until the answer is valid.
  #`check` returns NULL if the answer is acceptable, otherwise a message.
  #######################################################
  complain <- function(msg) warning(msg, immediate. = TRUE, call. = FALSE)

  ask.word <- function(prompt, check = function(x) NULL){
    repeat{
      cat(prompt)
      x <- scan(n = 1, what = "character")
      if(length(x) == 1){
        msg <- check(x)
        if(is.null(msg)) return(x)
        complain(msg)
      }
    }
  }

  # A whole line of text (the first word only would be read by ask.word)
  ask.line <- function(prompt){
    repeat{
      cat(prompt)
      x <- scan(n = 1, what = "character", sep = "\n")
      if(length(x) == 1 && !is.na(x)) return(trimws(x))
    }
  }

  ask.number <- function(prompt, check = function(x) NULL){
    repeat{
      cat(prompt)
      x <- suppressWarnings(as.numeric(scan(n = 1, what = "integer")))
      if(length(x) == 1 && !is.na(x)){
        msg <- check(x)
        if(is.null(msg)) return(x)
        complain(msg)
      }else{
        complain("Please enter a number.")
      }
    }
  }

  ask.menu <- function(prompt, choices){
    repeat{
      cat(prompt)
      i <- utils::menu(choices)
      if(i > 0) return(i)
    }
  }

  # Ask for a file. `read` reads and checks it (stopping with a message if there
  # is a problem). Returns the path and what `read` returned.
  ask.file <- function(prompt, read){
    repeat{
      cat(prompt)
      Sys.sleep(.2)
      utils::flush.console()
      path <- file.choose()
      value <- tryCatch(read(path), error = function(e){complain(conditionMessage(e)); NULL})
      if(!is.null(value)) return(list(path = path, value = value))
    }
  }

  whole <- function(what, min = 0) function(x){
    if(x != floor(x) || x < min) paste0("The ", what, " must be a whole number", if(min > 0) paste(" of at least", min), "!") else NULL
  }
  probability <- function(x) if(x < 0 || x > 1) "Probabilities must be between 0 and 1." else NULL
  count.matches <- function(n, got, what){
    if(n != got) stop("The number of ", what, " you defined (", n, ") does not equal the number provided in the file selected (", got, ").", call. = FALSE)
  }

  #######################################################
  #Project
  #######################################################
  dataset.name <- ask.word("Enter dataset name (must be <51 characters).\n\n\n",
                           function(x) if(nchar(x) > 50) "The dataset name must be at most 50 characters." else NULL)
  output.name <- ask.word("Enter main output file name (must be <21 characters).\n\n\n",
                          function(x) if(nchar(x) > 20) "The output file name must be at most 20 characters." else NULL)
  note <- ask.line("Enter one sentence to describe your dataset (no carriage returns!).\n\n\n")
  n.offspring <- ask.number("Enter number of offspring in the sample.\n\n\n", whole("number of offspring", 1))
  n.loci <- ask.number("Enter number of loci.\n\n\n", whole("number of loci", 1))
  seed <- ask.number("Enter seed for random number generator.\n\n\n", whole("seed"))

  #######################################################
  #Biology and model
  #######################################################
  update.allele.freq <- ask.menu("Should allele frequency be updated?\n\n\n",
                                 c("Not updating allele frequency", "Updating allele frequency")) == 2
  species <- c("dioecious", "monoecious")[ask.menu("What kind of species is it?\nSee help for definitions.\n\n",
                                                    c("Dioecious species", "Monoecious species"))]
  inbreeding <- ask.menu("Should inbreeding be allowed for?\nSee help for definitions.\n\n",
                         c("No inbreeding", "Inbreeding")) == 2
  ploidy <- c("diploid", "haplodiploid")[ask.menu("What kind of species is it?\nSee help for definitions.\n\n",
                                                  c("Diploid species", "HaploDiploid species"))]
  male.monogamy <- ask.menu("Are males monogamous or polygamous?\nSee help for definitions.\n\n",
                            c("Males monogamous", "Males polygamous")) == 1
  female.monogamy <- ask.menu("Are females monogamous or polygamous?\nSee help for definitions.\n\n",
                              c("Females monogamous", "Females polygamous")) == 1
  clone.inference <- ask.menu("Should clones (identical multilocus genotypes) be inferred?\nSee help for definitions.\n\n",
                              c("Do not infer clones", "Infer clones")) == 2
  scale.sibship.size <- ask.menu("Should full sibship size be scaled?\nSee help for definitions.\n\n",
                                 c("Scale full sibship size", "Do not scale full sibship size")) == 1
  sibship.prior <- c("none", "weak", "medium", "strong", "optimal")[
    ask.menu("Use a sibship size prior?\nSee help for definitions.\n\n",
             c("No sibship prior", "Weak sibship prior", "Medium sibship prior", "Strong sibship prior", "Optimal sibship prior"))]
  mean.sibship.size <- NULL
  if(sibship.prior != "none"){
    positive <- function(x) if(x <= 0) "The mean sibship size must be greater than zero." else NULL
    mean.sibship.size <- c(ask.number("Enter the mean paternal sibship size.\n\n\n", positive),
                           ask.number("Enter the mean maternal sibship size.\n\n\n", positive))
  }

  allele.freq <- NULL
  known.freq <- ask.menu("Unknown/Known population allele frequency?\n\n\n", c("Unknown", "Known")) == 2
  if(known.freq){
    allele.freq <- ask.file("Select the ALLELE FREQUENCY file.\n\n\n", function(path){
      check.allele.freq(path, n.loci, delim)
      TRUE
    })$path
  }

  #######################################################
  #Run settings
  #######################################################
  n.runs <- ask.number("Number of runs.\n\n\n", whole("number of runs", 1))
  run.length <- c("short", "medium", "long")[ask.menu("Length of run?\n\n\n", c("Short", "Medium", "Long"))]
  monitor.by <- c("iterations", "seconds")[ask.menu("Monitor method by Iterate/Time in second?\n\n\n",
                                                    c("Monitor by iterate", "Monitor by time in seconds"))]
  monitor.interval <- ask.number("Monitor interval (in iterate number or seconds) depending on how you have chosen to monitor progress.\n\n\n",
                                 function(x) if(x <= 0) "The monitor interval must be greater than zero." else NULL)
  platform <- c("windows", "unix")[ask.menu("What platform is this to be executed on?\n\n\n",
                                            c("Microsoft Windows system", "Other system (e.g. Mac/Unix)"))]
  likelihood <- c("full", "pairwise")[ask.menu("Which likelihood method should be used?\n\n\n",
                                               c("Full likelihood", "Pairwise likelihood"))]
  precision <- c("low", "medium", "high")[ask.menu("What level of precision should be used?\n\n\n", c("Low", "Medium", "High"))]

  #######################################################
  #Data
  #######################################################
  markers <- ask.file("Provide the path to the Marker Types and Error Rate file.\n\n\n", function(path){
    m <- check.markers(path, delim)
    if(ncol(m) != n.loci){
      stop("The number of defined loci (", n.loci, ") does not equal the number of markers provided in the file selected (", ncol(m), ").", call. = FALSE)
    }
    m
  })$path

  offspring <- ask.file("\nProvide the path to the offspringID and genotype file.\n\n\n", function(path){
    g <- check.genotypes(path, "offspring", n.loci, ploidy, delim)
    count.matches(n.offspring, nrow(g), "offspring")
    g
  })
  offspring.ids <- offspring$value[[1]]
  offspring <- offspring$path

  # Candidate parents
  candidates <- function(label, FATHER){
    prob <- ask.number(paste0("What is the probability that the ", FATHER, " of an offpring is included in the candidate set?\n\n\n E.g. 0.5\n\n\n"), probability)
    n <- ask.number(paste0("How many candidate ", FATHER, "S are there?\n\n\n"), whole(paste("number of candidate", label)))
    file <- NULL
    ids <- NULL
    if(n > 0){
      res <- ask.file(paste0("Provide the path to the candidate ", FATHER, "S file.\n\n\n"), function(path){
        g <- check.genotypes(path, label, n.loci, ploidy, delim)
        count.matches(n, nrow(g), paste("candidate", label))
        g
      })
      file <- res$path
      ids <- res$value[[1]]
    }
    list(prob = prob, file = file, ids = ids)
  }
  fathers <- candidates("fathers", "FATHER")
  mothers <- candidates("mothers", "MOTHER")

  # Known relationships
  known <- function(label, PARENT, parent.label, parent.ids){
    n <- ask.number(paste0("Enter the number of known ", PARENT, " sibships or ", if(PARENT == "PATERNAL") "paternities" else "maternities", ".\n\n\n"),
                    whole("number of known sibships and parentage"))
    file <- NULL
    threshold <- 0
    if(n > 0){
      file <- ask.file(paste0("Provide the path to the ", PARENT, " sibships file.\n\n\n"), function(path){
        k <- check.known(path, label, parent.ids, offspring.ids, parent.label, delim)
        count.matches(n, k$n.rows, paste("known", parent.label, "dyads and sibships"))
        TRUE
      })$path
      threshold <- ask.number(paste0("Enter the mismatch threshold for known ", if(PARENT == "PATERNAL") "paternities" else "maternities", " (an integer from 0 to the number of loci).\n\n\n"),
                              function(x) if(x != floor(x) || x < 0 || x > n.loci) "The mismatch threshold must be a whole number between 0 and the number of loci!" else NULL)
    }
    list(file = file, threshold = threshold)
  }
  known.paternal <- known("known.paternal", "PATERNAL", "father", fathers$ids)
  known.maternal <- known("known.maternal", "MATERNAL", "mother", mothers$ids)

  # Excluded relationships
  excluded <- function(label, count.prompt, file.prompt, allowed, what){
    n <- ask.number(count.prompt, whole(paste("number of", gsub("[.]", " ", label))))
    file <- NULL
    if(n > 0){
      file <- ask.file(file.prompt, function(path){
        e <- check.exclusions(path, label, offspring.ids, allowed, what, delim)
        count.matches(n, length(e), paste("rows in", label))
        TRUE
      })$path
    }
    file
  }
  excluded.paternities <- excluded("excluded.paternities", "Enter the number of offspring with known excluded PATERNITY.\n\n\n",
                                   "Provide the path to the excluded PATERNITY file.\n\n\n", fathers$ids, "fathers")
  excluded.maternities <- excluded("excluded.maternities", "Enter the number of offspring with known excluded MATERNITY.\n\n\n",
                                   "Provide the path to the excluded MATERNITY file.\n\n\n", mothers$ids, "mothers")
  excluded.paternal.sibships <- excluded("excluded.paternal.sibships", "Enter the number of offspring with known excluded PATERNAL sibships.\n\n\n",
                                         "Provide the path to the excluded PATERNAL sibships file.\n\n\n", offspring.ids, "offspring")
  excluded.maternal.sibships <- excluded("excluded.maternal.sibships", "Enter the number of offspring with known excluded MATERNAL sibships.\n\n\n",
                                         "Provide the path to the excluded MATERNAL sibships file.\n\n\n", offspring.ids, "offspring")

  #######################################################
  #Build the file
  #######################################################
  args <- list(file = file.path(wd, name),
               offspring = offspring, markers = markers,
               fathers = fathers$file, mothers = mothers$file,
               father.prob = fathers$prob, mother.prob = mothers$prob,
               known.paternal = known.paternal$file, paternity.threshold = known.paternal$threshold,
               known.maternal = known.maternal$file, maternity.threshold = known.maternal$threshold,
               excluded.paternities = excluded.paternities,
               excluded.maternities = excluded.maternities,
               excluded.paternal.sibships = excluded.paternal.sibships,
               excluded.maternal.sibships = excluded.maternal.sibships,
               allele.freq = allele.freq,
               dataset.name = dataset.name, output.name = output.name, note = note,
               seed = seed, update.allele.freq = update.allele.freq,
               species = species, inbreeding = inbreeding, ploidy = ploidy,
               male.monogamy = male.monogamy, female.monogamy = female.monogamy,
               clone.inference = clone.inference, scale.sibship.size = scale.sibship.size,
               sibship.prior = sibship.prior, mean.sibship.size = mean.sibship.size,
               n.runs = n.runs, run.length = run.length,
               monitor.by = monitor.by, monitor.interval = monitor.interval,
               platform = platform, likelihood = likelihood, precision = precision,
               delim = delim, overwrite = TRUE)
  args <- Filter(Negate(is.null), args)

  # Show the equivalent call, so that it can be saved in a script
  shown <- vapply(args, function(v) paste(deparse(v, width.cutoff = 500), collapse = " "), "")
  cat("\nThis will create the file with the following call, which you can keep in a script to repeat the analysis:\n\n")
  cat("write.colony.input(", paste0("  ", names(shown), " = ", shown, collapse = ",\n"), ")\n\n", sep = "\n")

  if(file.exists(args$file)) cat("Note: ", args$file, " already exists and will be replaced.\n\n", sep = "")
  if(ask.menu("Write the input file?\n\n", c("Write the input file", "Cancel")) == 2){
    cat("No file was written.\n")
    return(invisible(NULL))
  }

  path <- do.call(write.colony.input, args)
  cat("Finished! The input file is", path, "\n")
  invisible(path)
}
