get.interm.data<-function(variable="CrLogL",datadir=getwd(),n=1){

getval<-function(variable,xx){

ind<-grep(paste(variable,"=",sep=""),xx)


s1<-"[A-Za-z =0-9,.#+-]*"
s2<-"[ ]*([-.+E0-9]+)[%A-Z#a-z =0-9,.]*"
s3<-paste(s1,paste(variable,"=",sep=""),s2,sep="")
x.out<-NULL

for (i in 1:length(ind)){
	x.out[i]<-as.numeric(sub(s3,"\\1",xx[ind][i]))
	}

return(x.out)}

outfile<-readLines(paste(datadir,"temp.txt",sep="/"))

Itr<-getval("Itr",outfile)
length(Itr)

assign(variable,getval(variable,outfile))

if(length(get(variable))==2*length(Itr)){
assign(variable,get(variable)[rep(1:2,length.out=length(get(variable)))==n])

chk<-c(length(Itr),length(get(variable)))
df1<-data.frame(Itr=Itr[1:min(chk)],XX=get(variable)[1:min(chk)])

}else{
	chk<-c(length(Itr),length(get(variable)))
	df1<-data.frame(Itr=Itr[1:min(chk)],XX=get(variable)[1:min(chk)])}

names(df1)[2]<-variable

return(df1)

}


