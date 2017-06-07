#' A function to collect intermediate data produced by Colony2 while it is
#' running.
#'
#' While it is running, Colony2 produces output to indicate the computation
#' progress. This function allows R to import the outputs produced. It is
#' intended to be called from the monitor.colony function rather than being
#' used directly by the user.
#'
#' The intermediate results available are:
#'
#' \emph{Run} : The replicate run number. Variable \emph{Tmr} : The number of
#' temperature reductions so far within the run. Variable \emph{Itr} : The
#' number of iterates (reconfigurations considered) so far within the run.
#' Variable \emph{NSucc} : The number of successful (accepted) reconfigurations
#' so far within the temperature. Variable \emph{NSuccLmt} : Maximum (Limit)
#' number of successful reconfigurations allowed within the temperature.
#' Constant \emph{NFail1} : The number of reconfigurations since the last
#' update of the best likelihood within the temperature. Variable
#' \emph{NFail1Lmt} : Maximum (Limit) value of NFail1 within the temperature.
#' Constant \emph{NFail2} : The total number of reconfigurations since the last
#' update of the best likelihood within the run. Variable \emph{NFail2Lmt} :
#' Maximum (Limit) value of NFail2 within a run. Constant. The run terminates
#' when NFail2Lmt=NFail2 and the successful rate (see below) < 0.01
#' \emph{SucRate\%} : =NSucc / Itr. Variable \emph{SucLmt\%} : =NSucc /
#' NSuccLmt. Variable \emph{FailLmt\%} : =NFail1 / NFail1Lmt. Variable
#' \emph{IterLmt\%} : = (Number of iterates) / (Maximum number of iterates)
#' within a temperature. Variable \emph{CrLogL} : The log likelihood of the
#' current configuration. Variable \emph{BtLogL} : The best log likelihood
#' reached with the current configuration. Variable \emph{#F1} : Current number
#' of paternal sib families. Variable \emph{#F2} : Current number of maternal
#' sib families. Variable \emph{#F3} : Current number of sib family clusters.
#' Variable \emph{#FS} : Current number of full sib families. Variable
#' \emph{HSPair} : Current number of half-sib dyads. Variable \emph{FSPair} :
#' Current number of full-sib dyads. Variable \emph{#AssgnC1} : Current number
#' of candidate males that are assigned parentage. Variable \emph{#AssgnC2} :
#' Current number of candidate females that are assigned parentage. Variable
#' \emph{#AssgnP1} : Current number of offspring that have assigned paternity.
#' Variable \emph{#AssgnP2} : Current number of offspring that have assigned
#' maternity. Variable
#'
#' F1, F2, F3, FS, HSPair, FSPair, AssgnC1, AssgnC2, AssgnP1 and AssgnP2 appear
#' twice in the intermediate outputs of Colony2. Their first appearance is for
#' the current status. While the second appearance is for the best status so
#' far (i.e. the maximum value). Users wishing to plot any of these values
#' should use the \emph{n} argument to select the appropriate value (1 for the
#' current value, 2 for best value).
#'
#' @param variable The name of the variable that you wish to monitor. See
#' details.
#' @param datadir The path to the directory that contains the Colony output
#' files.
#' @param n If there are two variables with the same name, which one should be
#' used? See details.
#' @author Owen R. Jones
#' @seealso \code{\link{monitor.colony}}
#' @references Wang, J. (2004) Sibship reconstruction from genetic data with
#' typing errors.  Genetics 166: 1963-1979.
#' @keywords datagen
#' @export
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


