#' A wizard-like function to help build Colony2 input files.
#'
#' This function prompts the user to provide information about their system in
#' order to correctly build and format a Colony2 input file.
#'
#' This wizard will guide the user through the process of creating an input
#' file to be executed by Colony2.
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
#' \emph{Sibship size prior.} You can choose to use or not use a prior
#' distribution for the paternal and maternal sibship sizes of the offspring.
#' Select NO if you have no idea about the average sibship size, or you simply
#' do not want to use a prior. Select Yes if you have a rough estimate of the
#' average paternal and maternal sibship sizes and want to use them in the
#' inference. If you select YES, you are required to provide the average
#' paternal (\eqn{np}) and maternal (\eqn{nm}) sibship sizes. Using paternal
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
#' input file.
#' @author Owen R. Jones
#' @seealso \code{\link{run.colony}}
#' @references Wang, J. (2004) Sibship reconstruction from genetic data with
#' typing errors.  Genetics 166: 1963-1979.
#' @keywords manip
#' @export
build.colony.input <- function(wd=getwd(), name = "Colony2.DAT", delim = ""){

#   wd = getwd()
#  name = "Colony2.DAT"
# delim = ""

  colonyfile = NULL

  cat("This function will construct a Colony input file.\nPLEASE REFER TO THE HELP FILE ?build.colony.input.\n\n")
  cat(paste("It will be called", name, "and be placed in", wd, "...\n\n\n"))

  #Functions used here
  is.whole <- function(a) {floor(a) == a}

  #######################################################
  #  ! C, Dataset name, Length<51
  #######################################################

  while(length(colonyfile$datasetname) == 0){
    cat("Enter dataset name (must be <51 characters).\n\n\n")
    colonyfile$datasetname = scan(n = 1, what = "character")
    write(paste(colonyfile$datasetname, "! C, Dataset name, Length<51"), name, append = FALSE)
  }

  #######################################################
  #  ! C, Main output file name, Length<21
  #######################################################
  while(length(colonyfile$outfile) == 0){
    cat("Enter main output file name (must be <21 characters).\n\n\n")
    colonyfile$outfile = scan(n = 1, what = "character")
    write(paste(colonyfile$outfile, "! C, Main output file name, Length<21"), name, append = TRUE)
  }

  #######################################################
  #  ! C, Note to the project
  #######################################################

  while(length(colonyfile$note) == 0){
    cat("Enter one sentence to describe your dataset (no carriage returns!).\n\n\n")
    colonyfile$note = scan(n = 1, what = "character")
  }

  #######################################################
  #  ! I, Number of offspring in the sample
  #######################################################
  while(length(colonyfile$n.offspring) == 0){
    cat("Enter number of offspring in the sample.\n\n\n")
    colonyfile$n.offspring = as.numeric(scan(n = 1, what = "integer"))
    write(paste(colonyfile$n.offspring, "! I, Number of offspring in the sample"), name, append = TRUE)

    if(length(colonyfile$n.offspring) != 0){
      #Whole number warning
      if(is.whole(colonyfile$n.offspring) == FALSE){
        utils::flush.console()
        colonyfile = colonyfile[which(names(colonyfile) != "n.offspring")]
        warning("The number of offspring must be a whole number!\n", immediate. = TRUE)
      }
    }
  }


  #######################################################
  #  ! I, Number of loci
  #######################################################
  while(length(colonyfile$n.loci) == 0){
    cat("Enter number of loci.\n\n\n")
    colonyfile$n.loci = as.numeric(scan(n = 1, what = "integer"))
    write(paste(colonyfile$n.loci, "! I, Number of loci"), name, append = TRUE)

    if(length(colonyfile$n.loci) != 0){
      #Whole number warning
      if(is.whole(colonyfile$n.loci) == FALSE){
        utils::flush.console()
        colonyfile = colonyfile[which(names(colonyfile) != "n.loci")]
        warning("The number of loci must be a whole number!\n", immediate. = TRUE)
      }
    }
  }

  #######################################################
  #  ! I, Seed for random number generator
  #######################################################
  while(length(colonyfile$rseed) == 0){
    cat("Enter seed for random number generator.\n\n\n")
    colonyfile$rseed = as.numeric(scan(n = 1, what = "integer"))
    write(paste(colonyfile$rseed, "! I, Seed for random number generator"), name, append = TRUE)
  }

  #######################################################
  #  ! B, 0/1=Not updating/updating allele frequency
  #######################################################
  cat("Should allele frequency be updated?\n\n\n")
  switch(utils::menu(c("Not updating allele frequency", "Updating allele frequency")) + 1,
         cat("Nothing done\n\n\n"), colonyfile$updateallelefreq <- 0, colonyfile$updateallelefreq <- 1)
  write(paste(colonyfile$updateallelefreq, "! B, 0/1=Not updating/updating allele frequency"), name, append = TRUE)


  #######################################################
  #  ! 2/1=Dioecious/Monoecious
  #######################################################
  cat("What kind of species is it?\nSee help for definitions.\n\n")
  switch(utils::menu(c("Dioecious species", "Monoecious species")) + 1,
         cat("Nothing done\n\n\n"), colonyfile$diomonoecy <- 2, colonyfile$diomonoecy <- 1)
  write(paste(colonyfile$diomonoecy, "! 2/1=Dioecious/Monoecious"), name, append = TRUE)

  #######################################################
  #  ! B, 0/1=Diploid species/HaploDiploid species
  #######################################################
  cat("What kind of species is it?\nSee help for definitions.\n\n")
  switch(utils::menu(c("Diploid species", "HaploDiploid species")) + 1,
         cat("Nothing done\n\n\n"), colonyfile$ploidy <- 0, colonyfile$ploidy <- 1)
  write(paste(colonyfile$ploidy, "! B, 0/1=Diploid species/HaploDiploid species"), name, append = TRUE)

  #######################################################
  #  ! B, 0/1=Polygamy/Monogamy for males & females
  #######################################################
  cat("Are males monogamous or polygamous?\nSee help for definitions.\n\n")
  switch(utils::menu(c("Males monogamous", "Males polygamous")) + 1,
         cat("Nothing done\n\n\n"), colonyfile$malepolygamy <- 1, colonyfile$malepolygamy <- 0)

  cat("Are females monogamous or polygamous?\nSee help for definitions.\n\n")
  switch(utils::menu(c("Females monogamous", "Females polygamous")) + 1,
         cat("Nothing done\n\n\n"), colonyfile$femalepolygamy <- 1, colonyfile$femalepolygamy <- 0)
  write(paste(colonyfile$malepolygamy, colonyfile$femalepolygamy, "! B, 0/1=Polygamy/Monogamy for males & females"), name, append = TRUE)

  #######################################################
  #  ! B, R, R : Use sibship prior, Y/N=1/0. If Yes, give mean paternal, maternal sibship size
  #######################################################
  cat("Use sibship prior?\n\n\n")
  switch(utils::menu(c("Yes", "No")) + 1,
         cat("Nothing done\n\n\n"), colonyfile$sibship.prior <- 1, colonyfile$sibship.prior <- 0)

  if(colonyfile$sibship.prior==0){
    colonyfile$sibship.prior.paternal = 0
    colonyfile$sibship.prior.maternal = 0
    write(paste(colonyfile$sibship.prior, colonyfile$sibship.prior.paternal, colonyfile$sibship.prior.maternal, "! B, R, R : Use sibship prior, Y/N=1/0. If Yes, give mean paternal, maternal sibship size"), name, append = TRUE)
    }else{
      while(length(colonyfile$sibship.prior.paternal) == 0){
        cat("Enter the paternal sibship size (number of sibships).\n\n\n")
        colonyfile$sibship.prior.paternal = as.numeric(scan(n = 1, what = "integer"))
      }

    while(length(colonyfile$sibship.prior.maternal) == 0){
      cat("Enter the maternal sibship size (number of sibships).\n\n\n")
      colonyfile$sibship.prior.maternal = as.numeric(scan(n = 1, what = "integer"))
    }
      write(paste(colonyfile$sibship.prior, colonyfile$sibship.prior.paternal, colonyfile$sibship.prior.maternal, "! B, R, R : Use sibship prior, Y/N=1/0. If Yes, give mean paternal, maternal sibship size"), name, append = TRUE)
    }

  #######################################################
  #  ! B, 0/1=Unknown/Known population allele frequency
  #######################################################
  cat("Unknown/Known population allele frequency?\n\n\n")
  switch(utils::menu(c("Unknown", "Known")) + 1,
         cat("Nothing done\n\n\n"), colonyfile$knownAFreq <- 0, colonyfile$knownAFreq <- 1)

  write(paste(colonyfile$knownAFreq, "! B, 0/1=Unknown/Known population allele frequency\n"), name, append = TRUE)

  if(colonyfile$knownAFreq == 1){
    while(length(colonyfile$AlleleFreqPATH) == 0){
      cat("Select the ALLELE FREQUENCY file.\n\n\n")
      Sys.sleep(.2)
      utils::flush.console()
      colonyfile$AlleleFreqPATH = file.choose()
      colonyfile$allele.frequency = utils::read.table(colonyfile$AlleleFreqPATH, header = FALSE, colClasses = c("character"), sep = delim, fill = TRUE, flush = TRUE, na.strings = "", col.names = 1:max(utils::count.fields(colonyfile$AlleleFreqPATH)))
      utils::flush.console()
    }

    x = utils::count.fields(colonyfile$AlleleFreqPATH)[seq(2, length(utils::count.fields(colonyfile$AlleleFreqPATH)), 2)]
    x = paste(paste(x, collapse = " "), "!Number of alleles per locus", collapse=" ")

    utils::write.table(x, name, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE, na="")
    utils::write.table(colonyfile$allele.frequency, name, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")
    write("\n", name, append = TRUE)
  }

  #######################################################
  #  ! I, Number of runs
  #######################################################
  while(length(colonyfile$n.runs) == 0){
    cat("Number of runs.\n\n\n")
    colonyfile$n.runs = as.numeric(scan(n = 1, what = "integer"))
    write(paste(colonyfile$n.runs, "! I, Number of runs"), name, append = TRUE)

    if(length(colonyfile$n.runs) != 0){
      #Whole number warning
      if(is.whole(colonyfile$n.runs) == FALSE){
        utils::flush.console()
        colonyfile = colonyfile[which(names(colonyfile) != "n.runs")]
        warning("The number of runs must be a whole number!\n", immediate. = TRUE)
      }
    }
  }

  #######################################################
  #  ! I, Length of Run (1, 2, 3) = (Short, Medium, Long)
  #######################################################
  while(length(colonyfile$runlength) == 0){
    cat("Length of run?\n\n\n")
    switch(utils::menu(c("Short", "Medium", "Long")) + 1,
           cat("Nothing done\n\n\n"), colonyfile$runlength <- 1, colonyfile$runlength <- 2, colonyfile$runlength <- 3)
    write(paste(colonyfile$runlength, "! I, Length of Run (1, 2, 3) = (Short, Medium, Long)"), name, append = TRUE)
  }

  #######################################################
  #  ! B, 0/1=Monitor method by Iterate#/Time in second
  #######################################################
  cat("Monitor method by Iterate/Time in second?\n\n\n")
  switch(utils::menu(c("Monitor by iterate", "Monitor by time in seconds")) + 1,
         cat("Nothing done\n\n\n"), colonyfile$monitortype <- 0, colonyfile$monitortype <- 1)
  write(paste(colonyfile$monitortype, "! B, 0/1=Monitor method by Iterate#/Time in second"), name, append = TRUE)

  #######################################################
  #  ! I, Monitor interval in Iterate#/Seconds
  #######################################################
  while(length(colonyfile$interval) == 0){
    cat("Monitor interval (in iterate number or seconds) depending on how you have chosen to monitor progress.\n\n\n")
    colonyfile$interval = as.numeric(scan(n = 1, what = "integer"))
    write(paste(format(colonyfile$interval, scientific = FALSE), "! I, Monitor interval in Iterate#/Seconds"), name, append = TRUE)
  }

  #######################################################
  #  ! B, 0/1=Other platform/Windows execution
  #######################################################
  cat("What platform is this to be executed on?\n\n\n")
  switch(utils::menu(c("Microsoft Windows system", "Other system (e.g. Mac/Unix)")) + 1,
         cat("Nothing done\n\n\n"), colonyfile$sys <- 1, colonyfile$sys <- 0)
  write(paste(colonyfile$sys, "! B, 0/1=Other platform/Windows execution"), name, append = TRUE)

  #######################################################
  #  ! 1/0=Full-likelihood/pair-likelihood score method
  #######################################################
  cat("Which likelihood method should be used?\n\n\n")
  switch(utils::menu(c("Full likelihood", "Pairwise likelihood")) + 1,
         cat("Nothing done\n\n\n"), colonyfile$likelihood.method <- 1, colonyfile$likelihood.method <- 0)
  write(paste(colonyfile$likelihood.method, "! 1/0=Full-likelihood/pairwise-likelihood score method"), name, append = TRUE)

  #######################################################
  #  ! 1/2/3=low/medium/high precision
  #######################################################
  cat("What level of precision should be used?\n\n\n")
  switch(utils::menu(c("Low", "Medium", "High")) + 1,
         cat("Nothing done\n\n\n"), colonyfile$precision <- 1, colonyfile$precision <- 2, colonyfile$precision <- 3)
  write(paste(colonyfile$precision, "! 1/2/3=low/medium/high precision"), name, append = TRUE)
  write("\n", name, append = TRUE)

  #######################################################
  #Marker file import
  #######################################################

  #Give the path to the marker types and error rate file. This should be a file with a number of columns equal to the number of markers used.
  #There should be 4 rows, 1) marker ID, 2) marker type, 3) marker specific allelic dropout rate, 4) marker specific other typing error rate.

  while(length(colonyfile$MarkerPATH) == 0){
    cat("Provide the path to the Marker Types and Error Rate file.\n\n\n")
    Sys.sleep(.5)
    utils::flush.console()
    colonyfile$MarkerPATH = file.choose()
    colonyfile$Markers = utils::read.table(colonyfile$MarkerPATH, header = FALSE, colClasses = c("character"), sep = delim)
    utils::flush.console()

    if(colonyfile$n.loci != dim(colonyfile$Markers)[2]){
      colonyfile = colonyfile[which(names(colonyfile) != "MarkerPATH")]
      warning(paste("The number of defined loci ", "(",  colonyfile$n.loci, ") does not equal the number of markers provided in the file selected (", dim(colonyfile$Markers)[2], ").\n\n", sep = ""), immediate. = TRUE)
    }
  }

  colonyfile$Markers[, 1 + dim(colonyfile$Markers)[2]] = c("!Marker IDs", "!Marker types, 0/1=Codominant/Dominant", "!Marker-specific allelic dropout rate", "!Other marker-specific typing-error rate")
  utils::write.table(colonyfile$Markers, name, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
  write("\n", name, append = TRUE)

  #######################################################
  #Offspring genotype import
  #######################################################

  #Give the path to the offpring ID and genotype file
  #This should have a first column giving the ID, then 2 columns for each locus (1 for each allele at that locus), at least for diploid species.
  #Therefore, with 4 loci, there should be 9 columns.

  cat("\nProvide the path to the offspringID and genotype file.\n\n\n")
  utils::flush.console()

  while(length(colonyfile$OSGenotypePATH) == 0){
    colonyfile$OSGenotypePATH = file.choose()
    colonyfile$Offspring = utils::read.table(colonyfile$OSGenotypePATH, header = FALSE, colClasses = c("character"), sep = delim)

    if(colonyfile$n.offspring != dim(colonyfile$Offspring)[1]){
      colonyfile = colonyfile[which(names(colonyfile) != "OSGenotypePATH")]
      utils::flush.console()
      warning(paste("The number of defined offspring ", "(", colonyfile$n.offspring, ") does not equal the number of offspring provided in the file selected (", dim(colonyfile$Offspring)[1], ").\n\n", sep = ""), immediate. = TRUE)
    }

    fileloci = (dim(colonyfile$Offspring)[2] - 1) / 2

    if(colonyfile$ploidy == 0){
      if((colonyfile$n.loci) != fileloci){
        colonyfile = colonyfile[which(names(colonyfile) != "OSGenotypePATH")]
        utils::flush.console()
        warning(paste("The number of defined loci ", "(", colonyfile$n.loci, ") does not appear to equal the number of loci provided in the file selected (", fileloci, ").\n\n", sep = ""), immediate. = TRUE)
      }
    }
  }

  colonyfile$Offspring[, 1 + dim(colonyfile$Offspring)[2]] = c("!Offspring ID and genotypes", rep("", dim(colonyfile$Offspring)[1] - 1))
  utils::write.table(colonyfile$Offspring, name, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
  write("", name, append = TRUE)

  ######################################################
  #Sampling of candidate parents
  #######################################################
  #FATHERS - probability of inclusion in candidate set
  #######################################################
  while(length(colonyfile$fatherprob) == 0){
    cat("What is the probability that the FATHER of an offpring is included in the candidate set?\n\n\n E.g. 0.5\n\n\n")
    colonyfile$fatherprob = as.numeric(scan(n = 1, what = "integer"))

    if(colonyfile$fatherprob > 1){
      utils::flush.console()
      cat("Probabilities must be less than or equal to 1.\n")
      colonyfile = colonyfile[which(names(colonyfile) != "fatherprob")]
    }
  }

  #######################################################
  #FATHERS - number of candidate fathers
  #######################################################
  while(length(colonyfile$n.father) == 0){
    cat("How many candidate FATHERS are there?\n\n\n")
    colonyfile$n.father = as.numeric(scan(n = 1, what = "integer"))

    if(length(colonyfile$n.father) != 0){

      #Whole number warning
      if(is.whole(colonyfile$n.father) == FALSE){
        utils::flush.console()
        colonyfile = colonyfile[which(names(colonyfile) != "n.fathers")]
        warning("The number of fathers must be a whole number!\n", immediate. = TRUE)
      }
    }
  }

  #######################################################
  #FATHERS - Import candidate FATHERS file
  #######################################################
  if(colonyfile$n.father != 0){
    while(length(colonyfile$fathersPATH) == 0){
      cat("Provide the path to the candidate FATHERS file.\n\n\n")
      utils::flush.console()
      colonyfile$fathersPATH = file.choose()


      colonyfile$fathers = utils::read.table(colonyfile$fathersPATH, header = FALSE, sep = delim, colClasses = c("character"))
      if(colonyfile$n.father != dim(colonyfile$fathers)[1]){
        colonyfile = colonyfile[which(names(colonyfile) != "fathersPATH")]
        utils::flush.console()
        warning(paste("The number of defined FATHERS ", "(", colonyfile$n.father, ") does not equal the number of FATHERS provided in the file selected (", dim(colonyfile$fathers)[1], ").\n\n", sep=""), immediate. = TRUE)
      }
    }
  }else{
    colonyfile$fathersPATH = NA
    colonyfile$fathers = matrix(nrow = 1, ncol = 1)
  }

  #######################################################
  #MOTHERS - probability of inclusion in candidate set
  #######################################################
  while(length(colonyfile$motherprob) == 0){
    cat("What is the probability that the MOTHER of an offpring is included in the candidate set?\n\n\n E.g. 0.5\n\n\n")
    colonyfile$motherprob = as.numeric(scan(n = 1, what = "integer"))
    if(colonyfile$motherprob > 1){
      utils::flush.console()
      cat("Probabilities must be less than or equal to 1.\n")
      colonyfile = colonyfile[which(names(colonyfile) != "motherprob")]
    }
  }

  #######################################################
  #MOTHERS - Number of candidate mothers
  #######################################################
  while(length(colonyfile$n.mother) == 0){
    cat("How many candidate MOTHERS are there?\n\n\n")
    colonyfile$n.mother = as.numeric(scan(n = 1, what = "integer"))

    if(length(colonyfile$n.mother) != 0){

      #Whole number warning
      if(is.whole(colonyfile$n.mother) == FALSE){
        utils::flush.console()
        colonyfile = colonyfile[which(names(colonyfile) != "n.mothers")]
        warning("The number of mothers must be a whole number!\n", immediate. = TRUE)
      }
    }
  }

  #######################################################
  #MOTHERS - Import candidate MOTHERS
  #######################################################
  if(colonyfile$n.mother != 0){
    while(length(colonyfile$mothersPATH) == 0){
      cat("Provide the path to the candidate MOTHERS file.\n\n\n")

      utils::flush.console()
      colonyfile$mothersPATH = file.choose()
      utils::flush.console()

      colonyfile$mothers = utils::read.table(colonyfile$mothersPATH, header = FALSE, sep = delim, colClasses = c("character"))

      if(colonyfile$n.mother != dim(colonyfile$mothers)[1]){
        colonyfile = colonyfile[which(names(colonyfile) != "mothersPATH")]

        utils::flush.console()
        warning(paste("The number of defined MOTHERS ", "(", colonyfile$n.mother, ") does not equal the number of MOTHERS provided in the file selected (", dim(colonyfile$mothers)[1], ").\n\n", sep = ""), immediate. = TRUE)
      }
    }
  }else{
    colonyfile$mothersPATH = NA
    colonyfile$mothers = matrix(nrow=1, ncol = 1)
  }

  write(paste(colonyfile$fatherprob, colonyfile$motherprob, "!Probabilities that the father and mother of an offspring included in candidates"), name, append = TRUE)

  write(paste(colonyfile$n.father, colonyfile$n.mother, "!Numbers of candidate males and females"), name, append = TRUE)

  write("", name, append = TRUE)

  if(colonyfile$n.father != 0){
    colonyfile$fathers[, 1 + dim(colonyfile$fathers)[2]] = c("!Candidate M ID and genotypes", rep("", dim(colonyfile$fathers)[1] - 1))
    utils::write.table(colonyfile$fathers, name, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
  }

  write("", name, append = TRUE)

  if(colonyfile$n.mother != 0){
    colonyfile$mothers[, 1 + dim(colonyfile$mothers)[2]] = c("!Candidate F ID and genotypes", rep("", dim(colonyfile$mothers)[1] - 1))
    utils::write.table(colonyfile$mothers, name, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
  }

  write("", name, append = TRUE)

  #######################################################
  #Define known PATERNAL dyads and sibships
  #######################################################

  while(length(colonyfile$n.known.paternities.and.sibships) == 0){
    cat("Enter the number of known PATERNAL sibships or paternities.\n\n\n")
    colonyfile$n.known.paternities.and.sibships = as.numeric(scan(n = 1, what = "integer"))

    if(length(colonyfile$n.known.paternities.and.sibships) != 0){
      #Whole number warning
      if(is.whole(colonyfile$n.known.paternities.and.sibships) == FALSE){
        utils::flush.console()
        colonyfile = colonyfile[which(names(colonyfile) != "n.known.paternal.dyads")]
        warning("The number of known paternities and sibships must be a whole number!\n", immediate. = TRUE)
      }
    }
  }

  if(colonyfile$n.known.paternities.and.sibships > 0){
    #If there are some known paternal dyads...
    #Get the path, and delimiter, to the file...

    while(length(colonyfile$paternities.and.sibships.PATH) == 0){
      cat("Provide the path to the PATERNAL sibships file.\n\n\n")
      utils::flush.console()
      colonyfile$paternities.and.sibships.PATH = file.choose()
      colonyfile$paternities.and.sibships = utils::read.table(colonyfile$paternities.and.sibships.PATH, header = FALSE, colClasses = c("character"), sep = delim, fill = TRUE, flush = TRUE, na.strings = "", col.names = 1:max(utils::count.fields(colonyfile$paternities.and.sibships.PATH)))
      utils::flush.console()

      #split the file up
      colonyfile$paternal.dyads = subset(colonyfile$paternities.and.sibships, colonyfile$paternities.and.sibships$X1 != 0)
      dyad = NULL

      if(dim(colonyfile$paternal.dyads)[1] != 0){
        for(i in 1:dim(colonyfile$paternal.dyads)[1]){
          dyad1 = stats::na.omit(expand.grid(colonyfile$paternal.dyads[i, 1], unlist(colonyfile$paternal.dyads[i, 2:dim(colonyfile$paternal.dyads)[2]])))
          dyad = rbind(dyad, dyad1)
        }
        colonyfile$paternal.dyads = dyad
      }
      rm(dyad)

      #Sibships
      colonyfile$paternal.sibships = subset(colonyfile$paternities.and.sibships, colonyfile$X1 == 0)
      colonyfile$paternal.sibships[, 2:dim(colonyfile$paternal.sibships)[2]]

      if(dim(colonyfile$paternal.sibships)[1] != 0){
        for(i in 1:dim(colonyfile$paternal.sibships)[1]){
          colonyfile$paternal.sibships[i, 1] = sum(as.vector(is.na(colonyfile$paternal.sibships[i, ]) == FALSE)) - 1
        }
      }

      #Check the data
      #1) Do the number of sibships and paternities match up with the data provided?
      if(colonyfile$n.known.paternities.and.sibships != dim(colonyfile$paternities.and.sibships)[1]){
        colonyfile = colonyfile[which(names(colonyfile) != "paternities.and.sibships.PATH")]
        utils::flush.console()
        warning(paste("The number of defined paternities and sibships ", "(", colonyfile$n.known.paternities.and.sibships, ") does not equal the number of paternal dyads provided in the file selected (", dim(colonyfile$paternities.and.sibships)[1], ").\n\n", sep = ""), immediate. = TRUE)
      }

      #2) Are the fathers in the paternities and sibship file file present in the dataset?
      fathersinfile = colonyfile$paternities.and.sibships[, 1][colonyfile$paternities.and.sibships[, 1] != "0"] #fathers in file
      offspringinfile = as.vector(stats::na.omit(as.vector(unlist(colonyfile$paternities.and.sibships[, 2:ncol(colonyfile$paternities.and.sibships)]))))

      #must check these against the paternal and offpring genotype files
        if(sum(fathersinfile %in% colonyfile$fathers[, 1]) != length(fathersinfile)){
        colonyfile = colonyfile[which(names(colonyfile) != "paternities.and.sibships.PATH")]
        utils::flush.console()
        warning(paste("Fathers in the file you provided are not present in the paternal genotype data:", paste(fathersinfile[fathersinfile %in% colonyfile$fathers[, 1] == FALSE], collapse = ", ")), immediate. = TRUE)
      }

      if(sum(offspringinfile %in% colonyfile$Offspring[, 1]) != length(offspringinfile)){
        colonyfile = colonyfile[which(names(colonyfile) != "paternities.and.sibships.PATH")]
        utils::flush.console()
        warning(paste("Offspring in the file you provided are not present in the offspring genotype data:", paste(offspringinfile[offspringinfile %in% colonyfile$Offspring[, 1] == FALSE], collapse=", ")), immediate. = TRUE)
      }
    }
  }else{
    colonyfile$paternal.dyads = NA
    colonyfile$paternal.sibships = NA
  }


  #######################################################
  #Define known MATERNAL dyads and sibships
  #######################################################

  while(length(colonyfile$n.known.maternities.and.sibships) == 0){
    cat("Enter the number of known MATERNAL sibships or maternities.\n\n\n")
    colonyfile$n.known.maternities.and.sibships = as.numeric(scan(n = 1, what = "integer"))

    if(length(colonyfile$n.known.maternities.and.sibships) != 0){
      #Whole number warning
      if(is.whole(colonyfile$n.known.maternities.and.sibships) == FALSE){
        utils::flush.console()
        colonyfile = colonyfile[which(names(colonyfile) != "n.known.maternal.dyads")]
        warning("The number of known maternities and sibships must be a whole number!\n", immediate. = TRUE)
      }
    }
  }

  if(colonyfile$n.known.maternities.and.sibships > 0){

    #If there are some known maternal dyads...
    #Get the path, and delimiter, to the file...
    while(length(colonyfile$maternities.and.sibships.PATH) == 0){
      cat("Provide the path to the MATERNAL sibships file.\n\n\n")
      utils::flush.console()
      colonyfile$maternities.and.sibships.PATH = file.choose()

      #Read in the data...
      colonyfile$maternities.and.sibships = utils::read.table(colonyfile$maternities.and.sibships.PATH, header = FALSE, colClasses = c("character"), sep = delim, fill = TRUE, flush = TRUE, na.strings = "", col.names = 1:max(utils::count.fields(colonyfile$maternities.and.sibships.PATH)))
      utils::flush.console()

      #split the file up
      colonyfile$maternal.dyads = subset(colonyfile$maternities.and.sibships, colonyfile$maternities.and.sibships$X1 != 0)
      dyad = NULL

      if(!nrow(colonyfile$maternal.dyads) == 0){
        for(i in 1:nrow(colonyfile$maternal.dyads)){
          dyad1  = stats::na.omit(expand.grid(colonyfile$maternal.dyads[i, 1], unlist(colonyfile$maternal.dyads[i, 2:dim(colonyfile$maternal.dyads)[2]])))
          dyad = rbind(dyad, dyad1)
        }
        colonyfile$maternal.dyads = dyad
      }
      rm(dyad)

      #Sibships
      colonyfile$maternal.sibships = subset(colonyfile$maternities.and.sibships, colonyfile$maternities.and.sibships$X1 == 0)

      if(!nrow(colonyfile$maternal.sibships) == 0){
        for(i in 1:nrow(colonyfile$maternal.sibships)){
          colonyfile$maternal.sibships[i, 1] = sum(as.vector(!is.na(colonyfile$maternal.sibships[i, ]))) - 1
        }
      }

      #1) DO THE NUMBER OF SIBSHIPS/MATERNITIES MATCH UP WITH THE DATA PROVIDED?
      if(colonyfile$n.known.maternities.and.sibships != nrow(colonyfile$maternities.and.sibships)){
        colonyfile = colonyfile[which(names(colonyfile) != "maternities.and.sibships.PATH")]
        utils::flush.console()
        warning(paste("The number of defined maternities and sibships ", "(", colonyfile$n.known.maternities.and.sibships, ") does not equal the number of maternal dyads provided in the file selected (", dim(colonyfile$maternities.and.sibships)[1], ").\n\n", sep=""), immediate. = TRUE)
      }

      #2) ARE THE MOTHERS IN THE FILE ACTUALLY IN THE DATASET?
      mothersinfile = colonyfile$maternities.and.sibships[, 1][colonyfile$maternities.and.sibships[, 1] != "0"]
      offspringinfile = as.vector(stats::na.omit(as.vector(unlist(colonyfile$maternities.and.sibships[, 2:ncol(colonyfile$maternities.and.sibships)]))))

      #must check these against the maternal and offpring genotype files
      if(sum(mothersinfile %in% colonyfile$mothers[, 1]) != length(mothersinfile)){
        colonyfile = colonyfile[which(names(colonyfile) != "maternities.and.sibships.PATH")]
        utils::flush.console()
        warning(paste("Mothers in the file you provided are not present in the maternal genotype data:", paste(mothersinfile[mothersinfile %in% colonyfile$mothers[, 1] == FALSE], collapse = ", ")), immediate. = TRUE)
      }

      if(sum(offspringinfile %in% colonyfile$Offspring[, 1]) != length(offspringinfile)){
        colonyfile = colonyfile[which(names(colonyfile) != "maternities.and.sibships.PATH")]
        utils::flush.console()
        warning(paste("Offspring in the file you provided are not present in the offspring genotype data:", paste(offspringinfile[offspringinfile %in% colonyfile$Offspring[, 1] == FALSE], collapse = ", ")), immediate. = TRUE)
      }
    }
  }else{
    colonyfile$maternal.dyads = NA
    colonyfile$maternal.sibships = NA
  }

  #Paternal Dyads
  if(is.na(colonyfile$paternal.dyads)){
    utils::write.table("0 !Number of known paternities", name, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
    write("", name, append = TRUE)
    }else{
      utils::write.table(paste(dim(colonyfile$paternal.dyads)[1], "!Number of known paternities"), name, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
      colonyfile$paternal.dyads = colonyfile$paternal.dyads[2:1]
      colonyfile$paternal.dyads[, 1+dim(colonyfile$paternal.dyads)[2]] = c("!IDs of known offspring-father dyad", rep("", dim(colonyfile$paternal.dyads)[1] - 1))
      utils::write.table(colonyfile$paternal.dyads, name, append = TRUE, quote = FALSE, na = " ", row.names = FALSE, col.names = FALSE)
      write("", name, append = TRUE)
    }

  #Maternal Dyads
  if(is.na(colonyfile$maternal.dyads)){
    utils::write.table("0 !Number of known maternities", name, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
    write("", name, append = TRUE)
    }else{
      utils::write.table(paste(dim(colonyfile$maternal.dyads)[1], "!Number of known maternities"), name, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
      colonyfile$maternal.dyads = colonyfile$maternal.dyads[2:1]
      colonyfile$maternal.dyads[, 1 + dim(colonyfile$maternal.dyads)[2]] = c("!IDs of known offspring-mother dyad", rep("", dim(colonyfile$maternal.dyads)[1] - 1))
      utils::write.table(colonyfile$maternal.dyads, name, append = TRUE, quote = FALSE, na = " ", row.names = FALSE, col.names = FALSE)
      write("", name, append = TRUE)
    }

  #Paternal sibships
  if(is.na(colonyfile$paternal.sibships)){
    utils::write.table("0 !Number of known paternal sibships with unknown fathers", name, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
    write("", name, append = TRUE)
    }else{
      utils::write.table(paste(dim(colonyfile$paternal.sibships)[1], "!Number of known paternal sibships with unknown fathers "), name, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
      utils::write.table(colonyfile$paternal.sibships, name, append = TRUE, quote = FALSE, na = " ", row.names = FALSE, col.names = FALSE)
      write("", name, append = TRUE)
    }

  #Maternal sibships
  if(is.na(colonyfile$maternal.sibships)){
    utils::write.table("0  !Number of known maternal sibships with unknown mothers", name, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
    write("", name, append = TRUE)
    }else{
      utils::write.table(paste(dim(colonyfile$maternal.sibships)[1], "!Number of known maternal sibships with unknown mothers "), name, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
      utils::write.table(colonyfile$maternal.sibships, name, append = TRUE, quote = FALSE, na=" ", row.names = FALSE, col.names = FALSE)
      write("", name, append = TRUE)
      }



  #######################################################
  #Define excluded PATERNITIES
  #######################################################

  while(length(colonyfile$n.excluded.paternities) == 0){
    cat("Enter the number of offspring with known excluded PATERNITY.\n\n\n")
    colonyfile$n.excluded.paternities = as.numeric(scan(n=1, what="integer"))

    if(length(colonyfile$n.excluded.paternities) != 0){
      #Whole number warning
      if(is.whole(colonyfile$n.excluded.paternities) == FALSE){
        utils::flush.console()
        colonyfile = colonyfile[which(names(colonyfile) != "n.excluded.paternities")]
        warning("The number of excluded paternities must be a whole number!\n", immediate. = TRUE)
      }
    }
  }

  if(colonyfile$n.excluded.paternities > 0){


    #Get the path, and delimiter, to the file...
    while(length(colonyfile$excluded.paternities.PATH) == 0){
      cat("Provide the path to the excluded PATERNITY file.\n\n\n")
      utils::flush.console()
      colonyfile$excluded.paternities.PATH = file.choose()


      #Read in the data...
      colonyfile$excluded.paternities = utils::read.table(colonyfile$excluded.paternities.PATH, header = FALSE,
                                                  sep=delim, colClasses=c("character"), fill = TRUE, flush = TRUE, na.strings="")


      if(colonyfile$n.excluded.paternities > 0){#Write ExcludedPaternity.txt file
        temp1 = as.data.frame(colonyfile$excluded.paternities)
        names(temp1) = c("OffspringID", paste("ExcludedFatherID", 1:(dim(temp1)[2] - 1), sep=""))
        utils::write.table(temp1, "ExcludedPaternity.txt", row.names = FALSE, quote = FALSE, col.names = TRUE)
        }

      #Check the data
      if(colonyfile$n.excluded.paternities != dim(colonyfile$excluded.paternities)[1]){
        colonyfile = colonyfile[which(names(colonyfile) != "excluded.paternities.PATH")]
        utils::flush.console()
        warning(paste("The number of defined excluded paternities ", "(", colonyfile$n.excluded.paternities, ") does not equal the number provided in the file selected (", dim(colonyfile$excluded.paternities)[1], ").\n\n", sep=""), immediate. = TRUE)
      }


      #Further checks
      #if this is true, then all offspring in the dyad file are present in the offspring genotype file
      if(sum(colonyfile$excluded.paternities$V1 %in% colonyfile$Offspring[, 1]) == length(colonyfile$excluded.paternities$V1)){
      #Do nothing.
      }else{
        colonyfile = colonyfile[which(names(colonyfile) != "excluded.paternities.PATH")]
        utils::flush.console()
        warning(paste("Offspring in excluded paternities file are not present in the offspring genotype data:", paste(colonyfile$excluded.paternities$V1[which(colonyfile$excluded.paternities$V1%in%colonyfile$fathers[, 1] == FALSE)], collapse=", ")), immediate. = TRUE)
      }

      os = stats::na.omit(as.vector(as.matrix(colonyfile$excluded.paternities[, 2:dim(colonyfile$excluded.paternities)[2]])))

      if(sum(os%in%colonyfile$fathers[, 1]) == length(os)){
      #Do nothing
      }else{
        colonyfile = colonyfile[which(names(colonyfile) != "excluded.paternities.PATH")]
        utils::flush.console()
        warning(paste("Fathers in excluded paternities file are not present in the fathers genotype data:", paste(os[which(os%in%colonyfile$fathers[, 1] == FALSE)], collapse=", ")), immediate. = TRUE)
      }
    }

    csum = NULL
    for (i in 1:dim(colonyfile$excluded.paternities)[1]){
      csum[i] = length(colonyfile$excluded.paternities[i, ][!is.na(colonyfile$excluded.paternities[i, ])])
      }

    csum = csum - 1

    utils::write.table(paste(colonyfile$n.excluded.paternities, "!Number of offspring with known excluded paternity"), name, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)

    colonyfile$excluded.paternities = cbind(as.character(colonyfile$excluded.paternities[, 1]), csum, colonyfile$excluded.paternities[, 2:dim(colonyfile$excluded.paternities)[2]])

    colonyfile$excluded.paternities[, 1 + dim(colonyfile$excluded.paternities)[2]] = c("!Offspring ID, number of excluded males, the IDs of excluded males", rep("", dim(colonyfile$excluded.paternities)[1] - 1))

    utils::write.table(colonyfile$excluded.paternities, name, append = TRUE, quote = FALSE, na=" ", row.names = FALSE, col.names = FALSE)
    write("", name, append = TRUE)
    }else{
    #If there are no excluded paternities
    utils::write.table(paste(colonyfile$n.excluded.paternities, " !Number of offspring with known excluded paternity"), name, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
    write("", name, append = TRUE)
  }

  #######################################################
  #Define excluded MATERNITIES
  #######################################################

  while(length(colonyfile$n.excluded.maternities) == 0){
    cat("Enter the number of offspring with known excluded MATERNITY.\n\n\n")
    colonyfile$n.excluded.maternities = as.numeric(scan(n=1, what="integer"))

    if(length(colonyfile$n.excluded.maternities) != 0){
      #Whole number warning
      if(is.whole(colonyfile$n.excluded.maternities) == FALSE){
        utils::flush.console()
        colonyfile = colonyfile[which(names(colonyfile) != "n.excluded.maternities")]
        warning("The number of excluded maternities must be a whole number!\n", immediate. = TRUE)
      }
    }
  }

  if(colonyfile$n.excluded.maternities > 0){
    #Get the path, and delimiter, to the file...
    while(length(colonyfile$excluded.maternities.PATH) == 0){
      cat("Provide the path to the excluded MATERNITY file.\n\n\n")
      utils::flush.console()
      colonyfile$excluded.maternities.PATH = file.choose()

      #Read in the data...
      colonyfile$excluded.maternities = utils::read.table(colonyfile$excluded.maternities.PATH, header = FALSE,
                                                  sep=delim, colClasses=c("character"), fill = TRUE, flush = TRUE, na.strings="")

      #Check the data
      if(colonyfile$n.excluded.maternities != dim(colonyfile$excluded.maternities)[1]){
        colonyfile = colonyfile[which(names(colonyfile) != "excluded.maternities.PATH")]
        utils::flush.console()
        warning(paste("The number of defined excluded maternities ", "(", colonyfile$n.excluded.maternities, ") does not equal the number provided in the file selected (", dim(colonyfile$excluded.maternities)[1], ").\n\n", sep=""), immediate. = TRUE)
      }

      if(colonyfile$n.excluded.maternities > 0){#Write ExcludedMaternity.txt file
        temp1 = as.data.frame(colonyfile$excluded.maternities)
        names(temp1) = c("OffspringID", paste("ExcludedMotherID", 1:(dim(temp1)[2] - 1), sep = ""))

        utils::write.table(temp1, "ExcludedMaternity.txt", row.names = FALSE, quote = FALSE, col.names = TRUE)
        }

      #Futher checks
      #if this is true, then all offspring in the dyad file are present in the offspring genotype file
      if(sum(colonyfile$excluded.maternities$V1%in%colonyfile$Offspring[, 1]) == length(colonyfile$excluded.maternities$V1)){
      #Do nothing.
      }else{
        colonyfile = colonyfile[which(names(colonyfile) != "excluded.maternities.PATH")]
        utils::flush.console()
        warning(paste("Offspring in excluded maternities file are not present in the offspring genotype data:", paste(colonyfile$excluded.maternities$V1[which(colonyfile$excluded.maternities$V1%in%colonyfile$mothers[, 1] == FALSE)], collapse=", ")), immediate. = TRUE)
      }

      os = stats::na.omit(as.vector(as.matrix(colonyfile$excluded.maternities[, 2:dim(colonyfile$excluded.maternities)[2]])))

      if(sum(os%in%colonyfile$mothers[, 1]) == length(os)){
      #Do nothing
      }else{
        colonyfile = colonyfile[which(names(colonyfile) != "excluded.maternities.PATH")]
        utils::flush.console()
        warning(paste("Mothers in excluded maternities file are not present in the mothers genotype data:", paste(os[which(os%in%colonyfile$mothers[, 1] == FALSE)], collapse=", ")), immediate. = TRUE)
        }
    }

    csum = NULL
    for (i in 1:dim(colonyfile$excluded.maternities)[1]){
      csum[i] = length(colonyfile$excluded.maternities[i, ][!is.na(colonyfile$excluded.maternities[i, ])])
      }

    csum = csum - 1

    utils::write.table(paste(colonyfile$n.excluded.maternities, "!Number of offspring with known excluded maternity"), name, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)

    colonyfile$excluded.maternities = cbind(as.character(colonyfile$excluded.maternities[, 1]), csum, colonyfile$excluded.maternities[, 2:dim(colonyfile$excluded.maternities)[2]])

    colonyfile$excluded.maternities[, 1 + dim(colonyfile$excluded.maternities)[2]] = c("!Offspring ID, number of excluded females, the IDs of excluded females", rep("", dim(colonyfile$excluded.maternities)[1] - 1))

    utils::write.table(colonyfile$excluded.maternities, name, append = TRUE, quote = FALSE, na=" ", row.names = FALSE, col.names = FALSE)
    write("", name, append = TRUE)
    }else{
    #If there are no excluded maternities
    utils::write.table(paste(colonyfile$n.excluded.maternities, " !Number of offspring with known excluded maternity"), name, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
    write("", name, append = TRUE)
  }


  #######################################################
  #Define EXCLUDED PATERNAL sibships
  #######################################################
  while(length(colonyfile$n.excluded.paternal.sibships) == 0){
    cat("Enter the number of offspring with known excluded PATERNAL sibships.\n\n\n")
    colonyfile$n.excluded.paternal.sibships = as.numeric(scan(n=1, what="integer"))

    if(length(colonyfile$n.excluded.paternal.sibships) != 0){
      #Whole number warning
      if(is.whole(colonyfile$n.excluded.paternal.sibships) == FALSE){
        utils::flush.console()
        colonyfile = colonyfile[which(names(colonyfile) != "n.excluded.paternal.sibships")]
        warning("The number of excluded paternal sibships must be a whole number!\n", immediate. = TRUE)
      }
    }
  }

  if(colonyfile$n.excluded.paternal.sibships > 0){

    #Get the path, and delimiter, to the file...
    while(length(colonyfile$excluded.paternal.sibships.PATH) == 0){
      cat("Provide the path to the excluded PATERNAL sibships file.\n\n\n")
      utils::flush.console()
      colonyfile$excluded.paternal.sibships.PATH = file.choose()

      #Read in the data...
      colonyfile$excluded.paternal.sibships = utils::read.table(colonyfile$excluded.paternal.sibships.PATH, header = FALSE,
                                                        sep=delim, colClasses=c("character"), fill = TRUE, flush = TRUE, na.strings="")

      #Check the data
      if(colonyfile$n.excluded.paternal.sibships != dim(colonyfile$excluded.paternal.sibships)[1]){
        colonyfile = colonyfile[which(names(colonyfile) != "excluded.paternal.sibships.PATH")]
        utils::flush.console()
        warning(paste("The number of defined excluded paternal sibships ", "(", colonyfile$n.excluded.paternal.sibships, ") does not equal the number provided in the file selected (", dim(colonyfile$excluded.paternal.sibships)[1], ").\n\n", sep=""), immediate. = TRUE)
      }

      #Further checks - do excluded sibs appear in offspring file
      os = stats::na.omit(as.vector(as.matrix(colonyfile$excluded.paternal.sibships[, 2:dim(colonyfile$excluded.paternal.sibships)[2]])))

      if(!sum(os%in%colonyfile$Offspring[, 1]) == length(os)){
        colonyfile = colonyfile[which(names(colonyfile) != "excluded.paternal.sibships.PATH")]
        utils::flush.console()
        warning(paste("Offspring in excluded paternal sibships file are not present in the offspring genotype data:", paste(os[which(os%in%colonyfile$Offspring[, 1] == FALSE)], collapse=", ")), immediate. = TRUE)
      }
    }

    colonyfile$excluded.paternal.sibships[, 1 + dim(colonyfile$excluded.paternal.sibships)[2]] = c("!Size of known excluded paternal sibship, and IDs of excluded offspring in the sibship", rep("", dim(colonyfile$excluded.paternal.sibships)[1] - 1))
    csum = NULL
    for (i in 1:dim(colonyfile$excluded.paternal.sibships)[1]){
      csum[i] = length(colonyfile$excluded.paternal.sibships[i, ][!is.na(colonyfile$excluded.paternal.sibships[i, ])])
      }
    csum = csum - 1

    colonyfile$excluded.paternal.sibships = cbind(colonyfile$excluded.paternal.sibships[, 1], csum, colonyfile$excluded.paternal.sibships[, 2:ncol(colonyfile$excluded.paternal.sibships)])

    utils::write.table(paste(colonyfile$n.excluded.paternal.sibships, "!Number of offspring with known excluded paternal sibships"), name, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)


    utils::write.table(colonyfile$excluded.paternal.sibships, name, append = TRUE, quote = FALSE, na=" ", row.names = FALSE, col.names = FALSE)
    write("", name, append = TRUE)

  }else{
    #If there are no excluded sibships
    utils::write.table(paste(colonyfile$n.excluded.paternal.sibships, " !Number of offspring with known excluded paternal sibships"), name, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
    write("", name, append = TRUE)
  }


  #######################################################
  #Define EXCLUDED MATERNAL sibships
  #######################################################
  while(length(colonyfile$n.excluded.maternal.sibships) == 0){
    cat("Enter the number of offspring with known excluded MATERNAL sibships.\n\n\n")
    colonyfile$n.excluded.maternal.sibships = as.numeric(scan(n=1, what="integer"))

    if(length(colonyfile$n.excluded.maternal.sibships) != 0){
      #Whole number warning
      if(is.whole(colonyfile$n.excluded.maternal.sibships) == FALSE){
        utils::flush.console()
        colonyfile = colonyfile[which(names(colonyfile) != "n.excluded.maternal.sibships")]
        warning("The number of excluded maternal sibships must be a whole number!\n", immediate. = TRUE)
      }
    }
  }

  if(colonyfile$n.excluded.maternal.sibships > 0){

    #Get the path, and delimiter, to the file...
    while(length(colonyfile$excluded.maternal.sibships.PATH) == 0){
      cat("Provide the path to the excluded MATERNAL sibships file.\n\n\n")
      utils::flush.console()
      colonyfile$excluded.maternal.sibships.PATH = file.choose()

      #Read in the data...
      colonyfile$excluded.maternal.sibships = utils::read.table(colonyfile$excluded.maternal.sibships.PATH, header = FALSE,
                                                        sep=delim, colClasses = c("character"), fill = TRUE, flush = TRUE, na.strings="")

      #Check the data
      if(colonyfile$n.excluded.maternal.sibships != dim(colonyfile$excluded.maternal.sibships)[1]){
        colonyfile = colonyfile[which(names(colonyfile) != "excluded.maternal.sibships.PATH")]
        utils::flush.console()
        warning(paste("The number of defined excluded maternal sibships ", "(", colonyfile$n.excluded.maternal.sibships, ") does not equal the number provided in the file selected (", dim(colonyfile$excluded.maternal.sibships)[1], ").\n\n", sep=""), immediate. = TRUE)
      }

      #Further checks - do excluded sibs appear in offspring file
      os = stats::na.omit(as.vector(as.matrix(colonyfile$excluded.maternal.sibships[, 2:dim(colonyfile$excluded.maternal.sibships)[2]])))

      if(!sum(os%in%colonyfile$Offspring[, 1]) == length(os)){
        colonyfile = colonyfile[which(names(colonyfile) != "excluded.maternal.sibships.PATH")]
        utils::flush.console()
        warning(paste("Offspring in excluded maternal sibships file are not present in the offspring genotype data:", paste(os[which(os%in%colonyfile$Offspring[, 1] ==  FALSE)], collapse=", ")), immediate. = TRUE)
      }
    }

    colonyfile$excluded.maternal.sibships[, 1 + dim(colonyfile$excluded.maternal.sibships)[2]] = c("!Size of known excluded maternal sibship, and IDs of excluded offspring in the sibship", rep("", dim(colonyfile$excluded.maternal.sibships)[1] - 1))
    csum = NULL
    for (i in 1:dim(colonyfile$excluded.maternal.sibships)[1]){
      csum[i] = length(colonyfile$excluded.maternal.sibships[i, ][!is.na(colonyfile$excluded.maternal.sibships[i, ])])
      }
    csum = csum - 1

    colonyfile$excluded.maternal.sibships = cbind(colonyfile$excluded.maternal.sibships[, 1], csum, colonyfile$excluded.maternal.sibships[, 2:ncol(colonyfile$excluded.maternal.sibships)])

    utils::write.table(paste(colonyfile$n.excluded.maternal.sibships, "!Number of offspring with known excluded maternal sibships"), name, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)

    utils::write.table(colonyfile$excluded.maternal.sibships, name, append = TRUE, quote = FALSE, na=" ", row.names = FALSE, col.names = FALSE)
    write("", name, append = TRUE)
    }else{
    #If there are no excluded sibships
    utils::write.table(paste(colonyfile$n.excluded.maternal.sibships, " !Number of offspring with known excluded maternal sibships"), name, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
    write("", name, append = TRUE)
  }


  #######################################################
  #Other outputs
  #######################################################

  #MarkerTypeErrorRate.txt
  temp1 = as.data.frame(colonyfile$Markers)
  temp1 = temp1[, 1:dim(temp1)[2] - 1]
  names(temp1) = paste("Locus-", 1:dim(temp1)[2], sep="")
  utils::write.table(temp1, "MarkerTypeErrorRate.txt", row.names = FALSE, quote = FALSE, col.names = TRUE)

  #AlleleFrequency.txt
  if(colonyfile$knownAFreq == 1){
    temp1 = as.data.frame(colonyfile$allele.frequency)
    temp1 = temp1[, 1:dim(temp1)[2] - 1]
    names(temp1) = paste(paste("Locus-", rep(1:(dim(temp1)[2] / 2), each = 2), sep = ""), rep(1:2, (dim(temp1)[2]/2)), sep=".")
    utils::write.table(temp1, "MarkerTypeErrorRate.txt", row.names = FALSE, quote = FALSE, col.names = TRUE)
    }

  #OffspringGenotype.txt
  temp1 = as.data.frame(colonyfile$Offspring)
  temp1 = temp1[, 1:dim(temp1)[2] - 1]
  n = (dim(temp1)[2])-1
  names(temp1) = c("Offspring", paste(paste("Marker", rep(1:(n / 2), each = 2), sep=""), rep(1:2, (n/2)), sep = "-"))
  utils::write.table(temp1, "OffspringGenotype.txt", row.names = FALSE, quote = FALSE, col.names = TRUE)

  #MaleGenotype.txt
  if(colonyfile$n.father != 0){
    temp1 = as.data.frame(colonyfile$fathers)
    temp1 = temp1[, 1:dim(temp1)[2]-1]
    n = (dim(temp1)[2])-1
    names(temp1) = c("Male", paste(paste("Marker", rep(1:(n / 2), each = 2), sep=""), rep(1:2, (n / 2)), sep = "-"))
    utils::write.table(temp1, "MaleGenotype.txt", row.names = FALSE, quote = FALSE, col.names = TRUE)
  }

  if(colonyfile$n.mother != 0){
    #FemaleGenotype.txt
    temp1 = as.data.frame(colonyfile$mothers)
    temp1 = temp1[, 1:dim(temp1)[2] - 1]
    n = (dim(temp1)[2]) - 1
    names(temp1) = c("Female", paste(paste("Marker", rep(1:(n / 2), each=2), sep = ""), rep(1:2, (n / 2)), sep = "-"))
    utils::write.table(temp1, "FemaleGenotype.txt", row.names = FALSE, quote = FALSE, col.names = TRUE)
  }

  #KnownPaternalDyads.txt
  if(!is.na(colonyfile$paternal.dyads)){
    temp1 = colonyfile$paternal.dyads[c(2, 1)]
    names(temp1) = c("OffspringID", "Father")
    utils::write.table(temp1, "KnownPaternalDyads.txt", row.names = FALSE, quote = FALSE, col.names = TRUE)}

  #KnownMaternalDyads.txt
  if(!is.na(colonyfile$maternal.dyads)){
    temp1 = colonyfile$maternal.dyads[c(2, 1)]
    names(temp1) = c("OffspringID", "Mother")
    utils::write.table(temp1, "KnownMaternalDyads.txt", row.names = FALSE, quote = FALSE, col.names = TRUE)}


  #KnownPaternity.txt - see earlier
  #KnownMaternity.txt - see earlier

  #ExcludedPaternity.txt - see earlier
  #ExcludedMaternity.txt - see earlier

  #Produce summary information text file.
  utils::write.table(paste("Output file path & name : ", wd, name, "\n",
                    "Number of loci : ", colonyfile$n.loci, "\n",
                    "Number of offspring in the sample : ", colonyfile$n.offspring, "\n",
                    "Number of male candidates : ", colonyfile$n.father, "\n",
                    "Number of female candidates : ", colonyfile$n.mother, "\n",
                    "Number of known paternal sibships : ", colonyfile$n.paternal.sibs.or.paternities, "\n",
                    "Number of known maternal sibships : ", colonyfile$n.maternal.sibs.or.maternities, "\n",
                    "Number of offspring with excluded fathers : ", colonyfile$n.excluded.paternities, "\n",
                    "Number of offspring with excluded mothers : ", colonyfile$n.excluded.maternities, "\n",
                    "Male mating system : ", if(colonyfile$malepolygamy == 0){"Polygamous"}else{"Monogamous"}, "\n",
                    "Female mating system : ", if(colonyfile$femalepolygamy == 0){"Polygamous"}else{"Monogamous"}, "\n",
                    "Dioecious/Monoecious : ", if(colonyfile$diomonoecy == 1){"Monoecious"}else{"Dioecious"}, "\n",
                    "Number of threads : ", "nA", "\n",
                    "Number of Excluded Paternal Sibships : ", colonyfile$n.excluded.paternal.sibships, "\n",
                    "Number of Excluded Maternal Sibships : ", colonyfile$n.excluded.paternal.sibships, "\n",
                    "Seed for random number generator : ", colonyfile$rseed, "\n",
                    "Allele frequency : ", if(colonyfile$updateallelefreq == 1){"Updating by accounting for the inferred relationship"}else{"No updating by accounting for the inferred relationship"}, "\n",
                    "Species : ", if(colonyfile$ploidy == 1){"HaploDiploid"}else{"Diploid"}, "\n",
                    "Known population allele frequency : ", if(colonyfile$knownAFreq == 1){"Yes"}else{"No"}, "\n",
                    "Number of run : ", colonyfile$n.runs, "\n",
                    "Length of run : ", if(colonyfile$runlength == 1){"Small"}else{if(colonyfile$runlength == 2){"Medium"}else{"Long"}}, "\n",
                    "Monitor intermiediate results by : ", if(colonyfile$monitortype == 1){paste("Every", colonyfile$interval, "seconds")}else{paste("Every", colonyfile$interval, "iterations")}, "\n",
                    "Project data input produced : ", date(), "\n",
                    "NOTE to the Project: ", colonyfile$note, "\n", sep=""), "ProjectInformation.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)



  cat("Finished!")
  cat(paste("Your file is called", name, "and is placed in", wd, "...\n\n\n"))

  #This could be useful at some point.
  #return(colonyfile)
}
