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
	 plot(sibs$OffspringID1.num,sibs$OffspringID2.num,type="n",axes=FALSE,xlab="Offspring X",ylab="Offspring Y")
	 axis(1,at=alloffspring$offspring.numID,labels=alloffspring$offspringID,las=2)
	 axis(2,at=alloffspring$offspring.numID,labels=alloffspring$offspringID,las=2)
	 
	 polygon(c(0,1.05*max(alloffspring$offspring.numID),1.05*max(alloffspring$offspring.numID)),c(0,0,1.05*max(alloffspring$offspring.numID)),col="#FF669930",border=NA)
	 polygon(c(0,0,1.05*max(alloffspring$offspring.numID)),c(0,1.05*max(alloffspring$offspring.numID),1.05*max(alloffspring$offspring.numID)),col="#33CCFF30",border=NA)
	 
	 legend("topleft","Full Sib",bty="n")
	 legend("bottomright","Half Sib",bty="n")
	 
	 title(main=maintitle)
	 
	 #Plot the data.
	 hs<-subset(sibs,sibs$type=="Half"&sibs$Probability>prob)
	 for (i in 1:dim(hs)[1]){
		 #If Halfsib, then put the greater number first
		 if(hs[i,5]>hs[i,6]){points(hs[i,5],hs[i,6],pch=16,col="#33CCFF98")}else{points(hs[i,6],hs[i,5],pch=16,col="#33CCFF98")}
		 }
	 
	 fs<-subset(sibs,sibs$type=="Full"&sibs$Probability>prob)
	 for (i in 1:dim(fs)[1]){
		 #If Halfsib, then put the greater number first
		 if(fs[i,5]<fs[i,6]){points(fs[i,5],fs[i,6],pch=16,col="#FF669998")}else{points(fs[i,6],fs[i,5],pch=16,col="#FF669998")}
		 }
	 
	 }

