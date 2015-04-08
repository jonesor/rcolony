\name{categoricalxy}
\alias{categoricalxy}

\title{A function to plot 2 dimensional categorical data as presence/absence or with frequency.}

\description{
This function produces a plot for two-dimensional categorical data (e.g. individual's ID) to show presence/absence or frequency for each category.
The resulting plot is a chequer-board with each square representing a pairwise observation. 
The square can provide further information on (for example) frequency by colouring and/or by the inclusion of text.
}

\usage{
categoricalxy(my.data, freqlabs = FALSE, colscheme = "heat", poly.border = FALSE, axes.cex = 1)
}

\arguments{
  \item{my.data}{A dataframe with 3 columns. The first two columns should be categorical variables (coded as factors), while the third should indicate another dimension such as frequency.}
  \item{freqlabs}{Should the values of the z dimension (the 3rd column in my.data) be included as text: TRUE/FALSE}
  \item{colscheme}{Colour scheme: options are "heat" (heatmap colours) or "greyscale" (greyscale colours. Any other input will result in black squares with a grey background.}
  \item{poly.border}{Defines the borders around the individual squares in the plot. See \code{\link{polygon}} for further details}
  \item{axes.cex}{Controls the size of the axis labels.}
}

\details{}

\value{A categorical x-y plot.}

\references{}
\author{Owen R. Jones}
\note{}
\seealso{\code{\link{get.colony.data}},\code{\link{get.parentage}}}
\examples{

foo<-expand.grid(ID1=c("A","B","C","D"),ID2=c("W","X","Y","Z"))
foo$freq<-ceiling(runif(dim(foo)[1],0,10))
categoricalxy(foo,freqlabs=TRUE)


data(testdata)
x<-get.parentage(testdata)
categoricalxy(x,colscheme="heat",axes.cex=0.5)
}


\keyword{ aplot }
