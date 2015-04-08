\name{run.colony}
\alias{run.colony}
\title{A function to run Colony2 from within the R environment.}
\description{
This function used \code{system} to call Colony2 from within R.}
\usage{run.colony(colonyexecpath="prompt",datfilepath="prompt",wait=FALSE,monitor=TRUE)}

\arguments{
  \item{colonyexecpath}{The path to the Colony2 executable. If none is given, and the function defaults to \"prompt\", then you will be prompted to click on the file.}
  \item{datfilepath}{The path to the DAT file that you wish to run. If none is given, and the function defaults to \"prompt\", then you will be prompted to click on the file. }
  \item{wait}{Should R wait for Colony2 to be run by the system or not? TRUE/FALSE}
  \item{monitor}{Do you wish to monitor the output of Colony2 while it is running? }

}
\details{

If you wish to monitor the progress of the Colony2 analysis you must set monitor=TRUE, and wait=FALSE. The system will then redirect output from the Colony2 program to a temporary file (temp.txt).
You can then use \code{\link{monitor.colony}} to examine this file, and plot the outputs as Colony2 runs. There is a danger that, with very long runtimes, that the temporary file (temp.txt) will grow very large.
\code{\link{monitor.colony}} fixes this by periodically deleting all but the last few lines of the file. Therefore, if you intend to monitor a long running process it is recommended that you periodically, or continuously run \code{\link{monitor.colony}}.

Note that you will probably need to change the permissions of the Colony executable. On an Apple Macintosh, you can do this by navigating to the directory containing the executable and typing \code{chmod u+x Colony2}.

}
\value{
  
}
\references{ Wang, J. (2004) Sibship reconstruction from genetic data with typing errors.  Genetics 166: 1963-1979. }
\author{ Owen R. Jones }
\note{}
\seealso{\code{\link{monitor.colony}}}
\examples{

}
\keyword{IO}
