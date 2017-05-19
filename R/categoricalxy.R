#' A function to plot 2 dimensional categorical data as presence/absence or
#' with frequency.
#'
#' This function produces a plot for two-dimensional categorical data (e.g.
#' individual's ID) to show presence/absence or frequency for each category.
#' The resulting plot is a chequer-board with each square representing a
#' pairwise observation.  The square can provide further information on (for
#' example) frequency by colouring and/or by the inclusion of text.
#'
#'
#' @param my.data A dataframe with 3 columns. The first two columns should be
#' categorical variables (coded as factors), while the third should indicate
#' another dimension such as frequency.
#' @param freqlabs Should the values of the z dimension (the 3rd column in
#' my.data) be included as text: TRUE/FALSE
#' @param colscheme Colour scheme: options are "heat" (heatmap colours) or
#' "greyscale" (greyscale colours. Any other input will result in black squares
#' with a grey background.
#' @param poly.border Defines the borders around the individual squares in the
#' plot. See \code{\link{polygon}} for further details
#' @param axes.cex Controls the size of the axis labels.
#' @return A categorical x-y plot.
#' @author Owen R. Jones
#' @seealso \code{\link{get.colony.data}},\code{\link{get.parentage}}
#' @keywords aplot
#' @examples
#'
#'
#' foo<-expand.grid(ID1=c("A","B","C","D"),ID2=c("W","X","Y","Z"))
#' foo$freq<-ceiling(runif(dim(foo)[1],0,10))
#' categoricalxy(foo,freqlabs=TRUE)
#'
#'
#' data(testdata)
#' x<-get.parentage(testdata)
#' categoricalxy(x,colscheme="heat",axes.cex=0.5)
#'
#' @export
`categoricalxy` <-
function(my.data,freqlabs=FALSE,colscheme="heat",poly.border=FALSE,axes.cex=1){

#my.data should be a dataframe with 3 columns 2 parents and 1 frequency.

		 x<-as.numeric(my.data[,1])
		 y<-as.numeric(my.data[,2])
		 z<-my.data[,3]

     graphics::plot(x,y,xlim=c(0,max(x)),ylim=c(0,max(y)),axes=FALSE,type="n",xlab="",ylab="")
		 graphics::axis(1,at=unique(x)-0.5,labels=levels(my.data[,1]),las=2,cex.axis=axes.cex)
		 graphics::axis(2,at=unique(y)-0.5,labels=levels(my.data[,2]),las=2,cex.axis=axes.cex)

		 if(colscheme=="heat"){hc<-c("grey",grDevices::heat.colors(max(z)))
		 }else{
		 if(colscheme=="greyscale"){hc<-c("white",rev(grDevices::grey(0:max(z)/max(z))))
		 }else{hc<-c("grey",rep("black",max(z)))}}

		 for(i in 1:length(x)){
		 xp<-c(x[i]-1,x[i],x[i],x[i]-1)
		 yp<-c(y[i]-1,y[i]-1,y[i],y[i])
		 graphics::polygon(x=xp,y=yp,col=hc[z[i]+1],border=poly.border)

		 if(freqlabs==TRUE){graphics::text(x[i]-0.5,y[i]-0.5,as.character(z[i]))}
		 }
		 graphics::polygon(c(0,max(x),max(x),0),y=c(0,0,max(y),max(y)))
		 }

