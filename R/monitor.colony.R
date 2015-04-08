
monitor.colony<-function(datadir=getwd(),variable="CrLogL",interv.t=2,last.few=10,n=1,showres=FALSE){


MidResult<-NULL

  #Get start time
  t1<-Sys.time()
  
  elapsed.t<-0
  
  hell<-"warm"
  while(hell=="warm"){#run forever
  
  t2<-Sys.time()
  elapsed.t<-as.numeric(t2-t1)
  
  if(elapsed.t>interv.t){
  
  fileok<-file.exists(paste(datadir,"temp.txt",sep="/"))

if(fileok==TRUE){ 
if(!is.na(get.interm.data(datadir=datadir,variable=variable,n=n)[1,1])){
	MidResult2<-get.interm.data(datadir=datadir,variable=variable,n=n)
	MidResult<-rbind(MidResult,MidResult2)
	MidResult<-unique(MidResult)

x<-MidResult[,1]
y<-MidResult[,2]
  

if(last.few==0){
plot(x,y,xlab="",ylab="",type="n")
points(x,y,type="b")
}else{
fr<-length(x)-last.few
if(fr<1){fr<-1}else{}
plot(x[fr:length(x)],y[fr:length(x)],xlab="",ylab="",type="n")
points(x[fr:length(x)],y[fr:length(x)],type="b")}



#add points to show max and min reached till that point

title(xlab="NumIterate",ylab=variable)

#add routine to delete contents if file is too big.
#50lines?

tmp<-readLines(paste(datadir,"temp.txt",sep="/"))
n<-50
from<-if(length(tmp)-n>0){length(tmp)-n>0}else{1}
to<-length(tmp)

write.table(tmp[from:to],file=paste(datadir,"temp.txt",sep="/"),row.names=FALSE,col.names=FALSE,quote=FALSE)

if(showres==TRUE){print(MidResult)}

cat("Plotting. Hit Esc to stop\n")

	}}else{cat("Waiting...\n")}
  
  
 
  t1<-Sys.time()
  
  
  
  }
  }
  }



