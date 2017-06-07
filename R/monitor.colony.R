#' A function to monitor the intermediate results of the analysis being
#' undertaken by Colony2.
#'
#' This function examines the intermediate outputs from the Colony2 program.
#' These should be stored in a text file called "temp.txt" in the working
#' directory.  It plots the selected variable as a function of the iteration
#' number thereby allowing the user to monitor progress. The function
#' repeatedly calls a plot at an interval that can be set by the user (the
#' default is 2 seconds).  It keeps running until the Colony analysis has
#' finished, or until the user interupts it (by pressing the Esc key).
#'
#' The intermediate results available are:
#'
#' \emph{Run} : The replicate run number. Variable
#'
#' \emph{Tmr} : The number of temperature reductions so far within the run.
#' Variable
#'
#' \emph{Itr} : The number of iterates (reconfigurations considered) so far
#' within the run. Variable
#'
#' \emph{NSucc} : The number of successful (accepted) reconfigurations so far
#' within the temperature. Variable
#'
#' \emph{NSuccLmt} : Maximum (Limit) number of successful reconfigurations
#' allowed within the temperature. Constant
#'
#' \emph{NFail1} : The number of reconfigurations since the last update of the
#' best likelihood within the temperature. Variable
#'
#' \emph{NFail1Lmt} : Maximum (Limit) value of NFail1 within the temperature.
#' Constant
#'
#' \emph{NFail2} : The total number of reconfigurations since the last update
#' of the best likelihood within the run. Variable
#'
#' \emph{NFail2Lmt} : Maximum (Limit) value of NFail2 within a run. Constant.
#' The run terminates when NFail2Lmt=NFail2 and the successful rate (see below)
#' < 0.01
#'
#' \emph{SucRate\%} : =NSucc / Itr. Variable
#'
#' \emph{SucLmt\%} : =NSucc / NSuccLmt. Variable
#'
#' \emph{FailLmt\%} : =NFail1 / NFail1Lmt. Variable
#'
#' \emph{IterLmt\%} : = (Number of iterates) / (Maximum number of iterates)
#' within a temperature. Variable
#'
#' \emph{CrLogL} : The log likelihood of the current configuration. Variable
#'
#' \emph{BtLogL} : The best log likelihood reached with the current
#' configuration. Variable
#'
#' \emph{#F1} : Current number of paternal sib families. Variable
#'
#' \emph{#F2} : Current number of maternal sib families. Variable
#'
#' \emph{#F3} : Current number of sib family clusters. Variable
#'
#' \emph{#FS} : Current number of full sib families. Variable
#'
#' \emph{HSPair} : Current number of half-sib dyads. Variable
#'
#' \emph{FSPair} : Current number of full-sib dyads. Variable
#'
#' \emph{#AssgnC1} : Current number of candidate males that are assigned
#' parentage. Variable
#'
#' \emph{#AssgnC2} : Current number of candidate females that are assigned
#' parentage. Variable
#'
#' \emph{#AssgnP1} : Current number of offspring that have assigned paternity.
#' Variable
#'
#' \emph{#AssgnP2} : Current number of offspring that have assigned maternity.
#' Variable
#'
#' F1, F2, F3, FS, HSPair, FSPair, AssgnC1, AssgnC2, AssgnP1 and AssgnP2 appear
#' twice in the intermediate outputs of Colony2. Their first appearance is for
#' the current status. While the second appearance is for the best status so
#' far (i.e. the maximum value). Users wishing to plot any of these values
#' should use the \emph{n} argument to select the appropriate value (1 for the
#' current value, 2 for best value).
#'
#' @param datadir The path to the directory that contains the Colony output
#' files.
#' @param variable The name of the variable that you wish to monitor. See
#' details.
#' @param interv.t The interval (in seconds) at which R should retrieve
#' intermediate data.
#' @param last.few (not yet implemented) How many datapoints should be
#' displayed. Set to -1 to show all data. If the number is a positive integer,
#' then the last x datapoints are shown.
#' @param n If there are two variables with the same name, which one should be
#' used? See details.
#' @param showres Should R show the results obtained in a table to be displayed
#' in the R console window? Boolean.
#' @author Owen R. Jones
#' @seealso \code{\link{get.interm.data}}
#' @keywords data
#' @examples
#'
#'
#' ##Not run
#' #monitor.colony(variable="CrLogL",interv.t=2)
#' ##End not run
#'
#' @export
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
graphics::plot(x,y,xlab="",ylab="",type="n")
graphics::points(x,y,type="b")
}else{
fr<-length(x)-last.few
if(fr<1){fr<-1}else{}
graphics::plot(x[fr:length(x)],y[fr:length(x)],xlab="",ylab="",type="n")
graphics::points(x[fr:length(x)],y[fr:length(x)],type="b")}



#add points to show max and min reached till that point

graphics::title(xlab="NumIterate",ylab=variable)

#add routine to delete contents if file is too big.
#50lines?

tmp<-readLines(paste(datadir,"temp.txt",sep="/"))
n<-50
from<-if(length(tmp)-n>0){length(tmp)-n>0}else{1}
to<-length(tmp)

utils::write.table(tmp[from:to],file=paste(datadir,"temp.txt",sep="/"),row.names=FALSE,col.names=FALSE,quote=FALSE)

if(showres==TRUE){print(MidResult)}

cat("Plotting. Hit Esc to stop\n")

	}}else{cat("Waiting...\n")}



  t1<-Sys.time()



  }
  }
  }



