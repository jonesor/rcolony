`categoricalxy` <-
function(my.data,freqlabs=FALSE,colscheme="heat",poly.border=FALSE,axes.cex=1){

#my.data should be a dataframe with 3 columns 2 parents and 1 frequency.

		 x<-as.numeric(my.data[,1])
		 y<-as.numeric(my.data[,2])
		 z<-my.data[,3]
		 
		 plot(x,y,xlim=c(0,max(x)),ylim=c(0,max(y)),axes=FALSE,type="n",xlab="",ylab="")
		 axis(1,at=unique(x)-0.5,labels=levels(my.data[,1]),las=2,cex.axis=axes.cex)
		 axis(2,at=unique(y)-0.5,labels=levels(my.data[,2]),las=2,cex.axis=axes.cex)
		 
		 if(colscheme=="heat"){hc<-c("grey",heat.colors(max(z)))
		 }else{
		 if(colscheme=="greyscale"){hc<-c("white",rev(grey(0:max(z)/max(z))))
		 }else{hc<-c("grey",rep("black",max(z)))}}
		 
		 for(i in 1:length(x)){
		 xp<-c(x[i]-1,x[i],x[i],x[i]-1)
		 yp<-c(y[i]-1,y[i]-1,y[i],y[i])
		 polygon(x=xp,y=yp,col=hc[z[i]+1],border=poly.border)
		 
		 if(freqlabs==TRUE){text(x[i]-0.5,y[i]-0.5,as.character(z[i]))}
		 }
		 polygon(c(0,max(x),max(x),0),y=c(0,0,max(y),max(y)))
		 }

