\name{build.colony.input}
\alias{build.colony.input}

\title{A wizard-like function to help build Colony2 input files.}

\description{
This function prompts the user to provide information about their system in order to correctly build and format a Colony2 input file.
}

\usage{build.colony.input(wd=getwd(), name="Colony2.DAT", delim="")}

\arguments{
  \item{wd}{The directory where the generated file will be placed. The default is the current working directory.}
  \item{name}{The desired filename for the Colony input file (the default is Colony2.DAT).}
  \item{delim}{What is the delimiter for the input files (default is that they are whitespace delimited).}
   }

\details{
This wizard will guide the user through the process of creating an input file to be executed by Colony2.

\emph{Dataset name.} Provide a name for the dataset.

\emph{Output file name.} Provide a name for the output file.

\emph{Note for the project.} You can enter any text here, such as when you set up the project, notes about the dataset, etc.

\emph{Number of offspring.} Provide the number of offspring present in the sample.


\emph{Number of loci.} Provide the maximum number of marker loci genotyped for the individuals in your sample. 

\emph{Random number seed.} Provide a seed for the random number generator. Colony takes a simulated annealing algorithm to search for the ML configuration. It is a Monte Carlo method similar to MCMC, with a fine control of re-configuration acceptance rate though "temperature". Starting from the initial configuration in which all individuals are unrelated except for those individuals with known relationships, a random change is made to part of the configuration to generate a new configuration. The likelihoods of the new and old configurations are then calculated and compared to determine whether the new one is accepted or rejected. If the new likelihood is larger than the old one, then the new configuration is accepted. Otherwise, an acceptance rate is calculated using the current temperature, the new and old likelihood values, and is compared with a random number drawn from a uniform distribution in the range of [0,1]. If the random number value is smaller than the acceptance rate, the new configuration is still accepted although it is inferior to the old one. This is intended to avoid the algorithm getting stuck on a local maximum in the likelihood surface. Therefore, the random number seed partially determines the searching path. With exactly the same data and parameter values, different runs using different random number seeds may give slightly different final best configuration and likelihood values. Such a case occurs occasionally when there is not enough information in the maker data to infer the genetic structure, the actual genetic structure of the sample is extremely weak, or the sample size is very large (i.e. thousands of individuals). For example, when the number of markers is small, and/or the markers are not informative (few alleles with uneven frequency distribution), and/or most families are extremely small (e.g. one offspring per sibship), it is difficult to have replicate runs (using different random number seeds) converge to the same best configuration. One can do multiple runs for the same dataset by using different random number seeds to check/confirm the reliability of the analysis results. In the case replicate runs yield different results, the good news is that relationships reliably inferred are usually reconstructed consistently among runs, while dubious relationships are inferred inconsistently among the runs. One just needs to focus on those reliable, consistent relationships and ignore (abandon) those unreliable, inconsistent relationships in downstream analyses.

\emph{Should allele frequency be updated?} Allele frequencies are required in calculating the likelihood of a configuration. These frequencies can be provided by the user (see below) or are calculated by Colony using the genotypes in OFS, CMS (optional) and CFS (optional). In the latter case, you can ask Colony to update allele frequency estimates by taking into account of the inferred sibship and parentage relationships during the process of searching for the maximum likelihood configuration. However, updating allele frequencies could increase computational time substantially, and may not improve relationship inference much if the genetic structure of your sample is not strong (i.e. family sizes small and evenly distributed, most candidates are not assigned parentage). I suggest not updating allele frequencies except when family sizes (unknown) are large relative to sample size.

\emph{Species ploidy.} Select whether the species is diploid or haplodiploid.  Colony can be used for both diploid species and haplodiploid species. In both cases, the offspring are always assumed to be diploid (for haploid offspring, please use a previous version of Colony). In the haplodiploid case, males and females are assumed to be haploid and diploid respectively (for species with diploid males and haploid females, you just need to swap the two sexes).

\emph{Male and female mating system.} You will be asked whether males, and females are monogamous or polygamous. Here in our specific context, male monogamous signifies that two offspring in the OFS sample must be fathered by 2 different males if they have separate mothers. In other words, male monogamous specifies that no paternal halfsibs exist in the OFS sample. Note that the mating system herein is defined with regard to the samples being analyzed, not to the population or species from where the samples are taken. For example, consider a population in which males mate singly with females in a breeding season but mate with different females in different breeding seasons. An OFS sample with individuals taken from multiple breeding seasons may contain offspring from different mothers but from a single male (i.e. paternal halfsibs). Therefore, for the purpose of the Colony analysis, the male mating system should still be set as polygamous. The female mating system is similarly defined. Note also that when both males and females are defined as polygamous and the markers have genotyping errors, the computation can become very slow simply because all offspring in the OFS can be related in the pedigree and must be considered together in computing the likelihood of a configuration.

\emph{Sibship size prior.} You can choose to use or not use a prior distribution for the paternal and maternal sibship sizes of the offspring. Select NO if you have no idea about the average sibship size, or you simply do not want to use a prior. Select Yes if you have a rough estimate of the average paternal and maternal sibship sizes and want to use them in the inference. If you select YES, you are required to provide the average paternal (\eqn{np}) and maternal (\eqn{nm}) sibship sizes. Using paternal sibship prior as an example, the prior probability is calculated using Ewen's sampling formula as follows. Suppose paternal sibship size distribution is \eqn{m = {m1, m2,\ldots , mn}}, where \eqn{mi (i=1, \ldots, n)} is the number of paternal sibships each consisting of exactly \eqn{i} offspring. The total number of offspring is \deqn{k= \sum_{i=1}^1 im_i}, and the average number of non-empty paternal sibships (= the number of contributing fathers) is \deqn{k= \sum_{i=0}^{n-1} \alpha/(i+\alpha)}, where \eqn{\alpha} is a concentration parameter that determines the degree to which individuals are allocated to the same father. We can substitute \eqn{k} by \deqn{n/np} and solve numerically for \eqn{\alpha}. Given \eqn{\alpha}, the prior probability of \eqn{m={m1, m2, \ldots, mn}} is \deqn{\frac{n!}{\prod_{i=0}^{n-1} (\alpha+i)} \prod_{i=1}^{n} \biggl(\frac{\alpha}{i}\biggr)^{m_i} \frac{1}{m_i!}}.

Note that whenever the male or female mating system parameters have changed, the sibship prior is reset automatically to the default value. Therefore, if you decide to use the sibship prior, you should input the prior parameters \emph{after} setting the mating system parameters.

\emph{Allele frequency (known/unknown).} You should indicate whether population allele frequencies are known or unknown. If you select unknown the allele frequencies will be estimated from the current dataset. If you select known, because, for examplepopulation allele frequencies have been estimated from another larger, more appropriate sample, you will be prompted to select the allele frequency file. The file should be prepared, before selection, in the following format.

 Each locus takes 2 consecutive rows. The first row lists the allele names/identifications (using an unique integer number, 1-999999999), and the second row lists the corresponding frequencies of the alleles. Alleles (or allele frequencies) on the same row should be separated by a comma or white space. The first two rows are for locus 1, the 3rd and 4th rows are for locus 2, etc. Within a locus, allele names/identifications must unique but are not necessarily ordered or sequential. Alleles at different loci are allowed to have the same identification number.

 An example file of the allele frequency data is shown below.



Note that when allele frequencies are specified as known, the allele frequency file loaded should contain all alleles found in the offspring and candidate genotypes. Otherwise, an error occurs in running Colony. Note also that for a dominant locus, only two alleles are allowed and they are always indexed as 1 to indicate the dominant allele (presence of a band) and 2 to indicate the recessive allele (absence of a band when homozygous).

The function will check the loaded file for number of loci, and that the frequencies are numeric rather than characters.

\emph{Number of runs:} For the same dataset and parameters of a project, multiple runs can be conducted so that the best configuration with the maximum likelihood is more likely to be found and the uncertainties of the estimates (see below) are more reliable. However, it is very time costly to do multiple runs. Furthermore, in typical situations a single run suffices.

\emph{Length of run:} Longer runs consider more configurations in the searching process and thus are more likely to find the maximum likelihood configuration, but take more time to do so. In most cases, a medium run is a good compromise.

Note that marker loci have an implicit order which should be followed consistently in the entire input. For example, in offspring, candidate male and female genotype data, in allele frequency data and in marker type and genotyping error data, the same order of marker loci must be followed. In all these files, the first locus or locus 1, for example, must refer to the same marker.

\emph{Monitor by iteration, or by time in seconds?} Indicate whether you wish to monitor by iteration, or by time in seconds.

\emph{Monitor interval.} Give a number to specify the interval by which intermediate results are directed to the monitor. The unit of the interval is number of iterates or seconds, depending on how you choose to monitor Colony.

\emph{Platform.} Indicate what computer platform the file is intended to run on (Microsoft Windows/Other).

\emph{Likelihood method.} Indicate whether full or pairwise likelihood methods should be used.

\emph{Precision.} Indicate whether low, medium, or high precision should be used in the analysis.

\emph{Select the marker type and error rate file.}	You will be prompted to select the file for the markers. In the file, 4 values are provided for each marker (in columns). The first value (on row 1) specifies the marker name or ID (consisting of a maximum of  20 letters/numbers, others such as space, comma, full stop, forward and backward slashes are not allowed in the name/ID). The second (on row 2) indicates the marker type, whether it is codominant (0) or dominant (1). The third and fourth values (on rows 3 and 4 respectively) give the allelic dropout rate and the rate of other kinds of genotyping errors (including mutations) of the marker. For more information about the models of genotyping errors, see \cite{Wang (2004)}.

Note that this file sets the order of marker loci that must be followed in all the following input. The first column is for locus 1, the second for locus 2, etc.

Note also that computation becomes slow when markers suffer from genotyping errors. This is especially obvious when both males and females are specified as polygamous.

An example file is shown below. Note the column headers should not be included in the file. The column headers are added by rcolony automatically when loading the file. This is true for all of the following files loaded into rcolony.

 \tabular{rrrrr}{
 mk1 \tab     mk2 \tab     mk3  \tab   mk4 \tab     mk5 \cr
       0 \tab       0 \tab      0 \tab      0 \tab       0 \cr
  0.0000 \tab  0.0000 \tab 0.0000 \tab 0.0000 \tab  0.0000 \cr
  0.0001 \tab  0.0001 \tab  0.0001 \tab 0.0001 \tab  0.0001 
}

\emph{Offspring genotypes.}

You will be prompted to select the offspring genotype file. The file contains the individual IDs and the genotypes at each locus. Each individual takes a single row. The first column gives the individual ID (a string containing a maximum of 20 letters and/or numbers, no other characters are allowed), the 2nd and 3rd columns give the alleles observed for the individual at the first locus, the 4th and 5th give the alleles observed for the individual at the 2nd locus, etc. An allele is identified by an integer, in the range of 1-999999999. If the locus is a dominant marker, then only one (instead of 2) column is required for the marker, and the value for the genotype should be either 1 (dominant phenotype, presence of a band) or 2 (recessive phenotype, absence of a band). Missing genotypes are indicated by 0  0  for a codominant marker and 0 for a dominant marker. Note that offspring IDs should be unique. They are case sensitive, which means that, for example, "offspring2" and "Offspring2" are treated as different. An offspring with missing genotypes at all loci (no marker information at all) should not be included in the offspring genotype file.

 Part of an example offspring genotype file is shown below. Note the column headers (e.g. Offspring) should not be included in the offspring genotype file. They are added by rcolony automatically when loaded.

[table removed]
 


rcolony checks the number of individuals and the number of loci present in the file.

\emph{Probability an actual father included in candidates}.  Provide a guess (estimate) of the probability that the actual father of an offspring in the OFS is included in the CMS sample.

\emph{Number of candidate fathers.} Provide the number of candidate males included in the CMS sample. Note that known fathers are also included in the CMS sample. The minimum value is 0, in which case paternity is not inferred.	

\emph{Select the candidate father data file.} The format of male genotype file is the same as the offspring genotype file, except for 2 cases. One is that, for haplodiploid species (in which males are assumed to be haploid), each locus takes just one column rather than 2 columns, no matter the marker is codominant or dominant. The other is that if an individual already included in the offspring genotype file is present in the candidate male file, it is not necessary to provide its genotypes, just the first column for its individual ID will do. 

rcolony will check the number of individuals and loci in the file


\emph{Probability an actual mother included in candidates}.  Provide a guess (estimate) of the probability that the actual mother of an offspring in the OFS is included in the CMS sample.

\emph{Number of candidate mothers.} Provide the number of candidate females included in the CMS sample. Note that known mothers are also included in the CMS sample. The minimum value is 0, in which case maternity is not inferred.	

\emph{Select the candidate mother data file.} When the number of candidate females is larger than 0, the user is asked to load a file containing the candidate female genotypes. The format of female genotype file is the same as the offspring genotype file, except that if an individual already included in the offspring genotype file is present in the candidate female file, it is not necessary to provide its genotypes, just the first column for its individual ID will do. 

rcolony will check the number of individuals and loci in the file

\emph{Number of known paternal diads.} Provide the number of known paternal-offspring diads included in the samples. The minimum value is 0.

\emph{Select the paternal diads file.} If the number of known paternal diads is larger than zero, then you will be prompted to select a file for the known paternal diads. This file should have 2 columns, the first gives father ID, while the second gives the corresponding offspring ID.

\emph{Number of known maternal diads.} Provide the number of known maternal-offspring diads included in the samples. The minimum value is 0.

\emph{Select the maternal diads file.} If the number of known maternal diads is larger than zero, then you will be prompted to select a file for the known maternal diads. This file should have 2 columns, the first gives mother ID, while the second gives the corresponding offspring ID.

\emph{Number of known paternal sibships.} Provide the number of known paternal sibship or paternity included in the samples. The minimum value is 0. A known paternal sibship contains all of the offspring in the OFS sample who are known to share the same father no matter whether the father is known or not.

\emph{Select the paternal sibships file.} If the number of known paternal sibship/paternity is larger than zero, then you will be prompted to select a file for the known paternal sibship/paternity. In the file, each known paternal sibship/paternity takes a row, with the first column containing the father ID/name if the male is known and included in the candidate males or a value of 0 to indicate that the father is unknown or not in the candidate males. From the 2nd column on, the ID/name of each member of the paternal sibship is listed.

\emph{Number of known maternal sibships.} Provide the number of known maternal sibship or maternity included in the samples. The minimum value is 0. A known maternal sibship contains all of the offspring in the OFS sample who are known to share the same mother no matter whether the mother is known or not.

\emph{Select the maternal sibships file.} If the number of known maternal sibship/maternity is larger than zero, then you will be prompted to select a file for the known maternal sibship/maternity. In the file, each known maternal sibship/maternity takes a row, with the first column containing the mother ID/name if the male is known and included in the candidate females or a value of 0 to indicate that the mother is unknown or not in the candidate females. From the 2nd column on, the ID/name of each member of the maternal sibship is listed.

\emph{Define the number of excluded paternities.} Provide the number of offspring that each has at least one known excluded candidate male as its father. The minimum value is 0.

\emph{Provide a path to the excluded paternities file.} If the number of offspring with known excluded paternity is larger than zero, then you will be prompted to select a file that specifies the excluded candidate males for an offspring. Each offspring with excluded paternity takes one row. The first entry of the row is the offspring ID/name, followed by the IDs of the candidate males that are excluded parentage.

\emph{Define the number of excluded maternities.} Provide the number of offspring that each has at least one known excluded candidate female as its mother. The minimum value is 0.

\emph{Provide a path to the excluded maternities file.} If the number of offspring with known excluded maternity is larger than zero, then you will be prompted to select a file that specifies the excluded candidate females for an offspring. Each offspring with excluded maternity takes one row. The first entry of the row is the offspring ID/name, followed by the IDs of the candidate females that are excluded parentage.

\emph{Number of offspring with known excluded paternal sibships.} Provide the number of offspring that are known to each have at least one excluded offspring as its paternal sibling. The minimum value is 0.

\emph{Excluded paternal shibships file.} If the number of offspring with known excluded paternal sibships is larger than zero, then you will be prompted to select a file that specifies the excluded offspring as paternal siblings for an offspring. Each offspring with one or more excluded paternal siblings takes one row. The first entry of the row is the offspring ID/name, followed by the IDs of the offsprings that are excluded paternal siblings.

\emph{Number of offspring with known excluded maternal sibships.} Provide the number of offspring that are known to each have at least one excluded offspring as its maternal sibling. The minimum value is 0.

\emph{Excluded maternal shibships file.} If the number of offspring with known excluded maternal sibships is larger than zero, then you will be prompted to select a file that specifies the excluded offspring as maternal siblings for an offspring. Each offspring with one or more excluded maternal siblings takes one row. The first entry of the row is the offspring ID/name, followed by the IDs of the offsprings that are excluded maternal siblings.
}

\value{A text file is produced. This file can be used by Colony2 as an input file.}

\references{Wang, J. (2004) Sibship reconstruction from genetic data with typing errors.  Genetics 166: 1963-1979.}
\author{Owen R. Jones}
\note{}
\seealso{\code{\link{run.colony}}}
\examples{}
\keyword{manip}
