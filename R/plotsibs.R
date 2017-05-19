#' A function to plot sibships (full and half-sibs) in an x-y style plot.
#'
#' This function plots the estimated sibships from Colony data.  Both the x and
#' y axis represent individuals in the population and points indicate a sibship
#' assignment between individuals x and y.
#'
#' Full sibships are shown in the upper left part of the plot, and
#' half-sibships are shown in the lower right part. The data can be derived
#' from pairwise methods, or from the full likelihood method.
#'
#' @param colony.object A list object derived from the colony data
#' (\code{\link{get.colony.data}}).
#' @param maintitle The plot's title.
#' @param prob Assignment probability threshold. The probability of sibship
#' must be greater than this value to be included in the plot.
#' @param pairwise TRUE/FALSE: should pairwise sibship estimates be used.
#' (TRUE= pairwise, FALSE=full likelihood)
#' @return A plot showing full- or half-sibships.
#' @author Owen R. Jones
#' @seealso \code{\link{get.colony.data}},\code{\link{run.colony}}
#' @references Wang, J. (2004) Sibship reconstruction from genetic data with
#' typing errors. Genetics 166: 1963-1979.
#' @keywords aplot
#' @examples
#'
#' data(testdata)
#'
#' plotsibs(testdata,maintitle="Sibships (pairwise)",pairwise=TRUE)
#' plotsibs(testdata,maintitle="Sibships (full)",pairwise=FALSE)
#'
#' @export
`plotsibs` <-
function(colony.object,maintitle="sibships",prob=0.8,pairwise=FALSE){

	 #Get full and half sibship data.

if(pairwise==FALSE){
sibs<-colony.object$sibs}else{
sibs<-colony.object$pairwise.sibs
sibs$Probability<-rep(1,dim(sibs)[1])}


alloffspring<-colony.object$offspring
alloffspring$offspring.numID<-as.numeric(alloffspring$offspring.numID)

	 #Apply the numeric ID to the sibship data.
	 sibs<-merge(sibs,alloffspring,by.x="OffspringID1",by.y="offspringID",sort=FALSE)
	 names(sibs)[names(sibs)=="offspring.numID"]<-"OffspringID1.num"

	 sibs<-merge(sibs,alloffspring,by.x="OffspringID2",by.y="offspringID",sort=FALSE)
	 names(sibs)[names(sibs)=="offspring.numID"]<-"OffspringID2.num"
	 sibs$OffspringID1.num<-as.numeric(sibs$OffspringID1.num)
	 sibs$OffspringID2.num<-as.numeric(sibs$OffspringID2.num)


	 #Set up the plot area.
	 graphics::plot(sibs$OffspringID1.num,sibs$OffspringID2.num,type="n",axes=FALSE,xlab="Offspring X",ylab="Offspring Y")
	 graphics::axis(1,at=alloffspring$offspring.numID,labels=alloffspring$offspringID,las=2)
	 graphics::axis(2,at=alloffspring$offspring.numID,labels=alloffspring$offspringID,las=2)

	 graphics::polygon(c(0,1.05*max(alloffspring$offspring.numID),1.05*max(alloffspring$offspring.numID)),c(0,0,1.05*max(alloffspring$offspring.numID)),col="#FF669930",border=NA)
	 graphics::polygon(c(0,0,1.05*max(alloffspring$offspring.numID)),c(0,1.05*max(alloffspring$offspring.numID),1.05*max(alloffspring$offspring.numID)),col="#33CCFF30",border=NA)

	 graphics::legend("topleft","Full Sib",bty="n")
	 graphics::legend("bottomright","Half Sib",bty="n")

	 graphics::title(main=maintitle)

	 #Plot the data.
	 hs<-subset(sibs,sibs$type=="Half"&sibs$Probability>prob)
	 for (i in 1:dim(hs)[1]){
		 #If Halfsib, then put the greater number first
		 if(hs[i,5]>hs[i,6]){graphics::points(hs[i,5],hs[i,6],pch=16,col="#33CCFF98")}else{graphics::points(hs[i,6],hs[i,5],pch=16,col="#33CCFF98")}
		 }

	 fs<-subset(sibs,sibs$type=="Full"&sibs$Probability>prob)
	 for (i in 1:dim(fs)[1]){
		 #If Halfsib, then put the greater number first
		 if(fs[i,5]<fs[i,6]){graphics::points(fs[i,5],fs[i,6],pch=16,col="#FF669998")}else{graphics::points(fs[i,6],fs[i,5],pch=16,col="#FF669998")}
		 }

	 }

