\name{get.colony.data}
\alias{get.colony.data}

\title{This function creates a data object from the Colony2 outputs.}

\description{When provided with the directory name, and the name of the *.DAT colony input file, this function creates a list object containing pertinent information from the Colony output files.}

\usage{get.colony.data(datadir, filename, colonyVersion)}

\arguments{
  \item{datadir}{The path to the directory that contains the Colony output files and the *.DAT Colony input file.}
  \item{filename}{Optional. The name of the Colony input file. If this argument is omitted, the command will search for a *.DAT file in the data directory (datadir) and use that as the input file.}
  \item{colonyVersion}{The version of Colony that was used (default is 2.0, other option is 2.0.3).}
  }
\details{}
\value{A list, containing data extracted from the Colony output files}

\references{\url{http://www.zoo.cam.ac.uk/ioz/software.htm#COLONY}}

\author{Owen R. Jones}
\note{}
\seealso{\code{\link{run.colony}}, \code{\link{build.colony.input}}}
\examples{
#Not run
#mynewdata<-get.colony.data("/Users/FredBlogs/Documents/Example/")
#mynewdata<-get.colony.data("/Users/FredBlogs/Documents/Example/","Example1.DAT")

}


\keyword{manip}


