\name{get.parentage}
\alias{get.parentage}

\title{A function to extract the paternity and maternity information from the colony object created with get.colony.data. }
\description{This function extracts the paternity and maternity information from a colony object that was created using \code{\link{get.colony.data}}.
The output is a dataframe with 3 columns. (1) mums, (2) dads, (3) frequency.
The output can either be derived from pairwise-likelihood or the full-likelihood method (if available).}
\usage{get.parentage(colony.object, pairwise = FALSE)}

\arguments{
  \item{colony.object}{A list created by \code{\link{get.colony.data}}}.
  \item{pairwise}{Should the parentage information be derived from the pairwise likelihood estimates (TRUE), of from the full likelihood methods (FALSE). See Wang et al. for details.}
}
\details{}
\value{
  A data frame with 3 columns. (1) mums, (2) dads, (3) frequency. 
}
\references{ Wang, J. (2004) Sibship reconstruction from genetic data with typing errors.  Genetics 166: 1963-1979. }
\author{Owen R. Jones}
\note{}

\seealso{ \code{\link{get.colony.data}} }

\examples{

data(testdata)
parents<-get.parentage(testdata)
head(parents)
}

\keyword{manip}
