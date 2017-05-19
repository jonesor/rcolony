##' A function to run Colony2 from within the R environment.
##'
##' This function used \code{system} to call Colony2 from within R.
##'
##' If you wish to monitor the progress of the Colony2 analysis you must set monitor=TRUE, and wait=FALSE. The system will then redirect output from the Colony2 program to a temporary file (temp.txt).
##' You can then use \code{\link{monitor.colony}} to examine this file, and plot the outputs as Colony2 runs. There is a danger that, with very long runtimes, that the temporary file (temp.txt) will grow very large.
##' \code{\link{monitor.colony}} fixes this by periodically deleting all but the last few lines of the file. Therefore, if you intend to monitor a long running process it is recommended that you periodically, or continuously run \code{\link{monitor.colony}}.
##' Note that you will probably need to change the permissions of the Colony executable. On an Apple Macintosh, you can do this by navigating to the directory containing the executable and typing \code{chmod u+x Colony2}.
##' @param colonyexecpath The path to the Colony2 executable. If none is given, and the function defaults to \"prompt\", then you will be prompted to click on the file.
##' @param datfilepath The path to the DAT file that you wish to run. If none is given, and the function defaults to \"prompt\", then you will be prompted to click on the file.
##' @param wait Should R wait for Colony2 to be run by the system or not? TRUE/FALSE
##' @param monitor Do you wish to monitor the output of Colony2 while it is running?
##' @references Wang, J. (2004) Sibship reconstruction from genetic data with typing errors.  Genetics 166: 1963-1979.
##' @author Owen R. Jones
##' @seealso \code{\link{monitor.colony}}
##' @keywords IO
##' @export
run.colony<-function(colonyexecpath="prompt",datfilepath="prompt",wait=FALSE,monitor=TRUE){
	 #don't forget the trailing slash!

if(colonyexecpath=="prompt"){
cat("Please click to select your Colony2 executable (probably called Colony2.exe or Colony2).\n\n")
utils::flush.console()
colonyexecpath<-file.choose()}

if(datfilepath=="prompt"){
cat("Please click to select your DAT file.\n\n")
utils::flush.console()
datfilepath<-file.choose()}

datadir<-sub("([A-Z a-z0-9:/\\]+[/\\]+)([A-Z.a-z0-9]+)","\\1",datfilepath)
filename<-sub("([A-Z a-z0-9:/\\]+[/\\]+)([A-Z.a-z0-9]+)","\\2",datfilepath)
colonyexec<-sub("([A-Z a-z0-9:/\\]+[/\\]+)([A-Z.a-z0-9]+)","\\2",colonyexecpath)


current.wd<-getwd()

	 #Extract the output file name defined in the colony file.
	 readLines(paste(datadir,filename,sep=""),n=2)->x
	 outputfilename<-substring (x[2], 1, 20)
	 outputfilename <- sub("^[\t\n\f\r ]*", "",outputfilename) #remove leading whitespace
	 outputfilename <- sub("[\t\n\f\r ]*$", "", outputfilename); #remove trailing whitespace
	 outputfilename

if(file.exists(paste(datadir,outputfilename,".MidResult",sep=""))){stop("\nThere are output files already in the directory. \nColony has already run. \nTry deleting (or moving) these files and starting again.\n")}
setwd(datadir)

if(monitor==TRUE&wait==TRUE){stop("If you want to monitor the output, you must set wait as FALSE. Otherwise you cannot run other functions in the same R console.")}

cat("Be aware: this may take several minutes, hours, or even weeks to run, depending on the settings used.\n")

platform<-.Platform
if(platform$OS.type=="unix"){

#Unix/MacOSX commands

#Copy Colony2 program to the working directory
	 if(file.exists("Colony2")==FALSE){
system(paste("cp",colonyexecpath,datadir,sep=" "))
	}
#Rename the DAT file as Colony2.DAT (unless it is already called "Colony2.DAT")
if(filename!="Colony2.DAT"){system(paste("mv",paste(datadir,filename,sep=""),paste(datadir,"Colony2.DAT",sep=""),sep=" "))}

#Make a copy of the DAT file in it's original name
if(filename!="Colony2.DAT"){system(paste("cp",paste(datadir,"Colony2.DAT",sep=""),paste(datadir,filename,sep=""),sep=" "))}

#Run Colony
#If monitor = TRUE, then a temp.txt file is produced (temp.txt). This can be monitored using monitor.colony.R so progress can be observed.
#wait should be set to FALSE if monitor =TRUE
#It is recommended that monitor = TRUE only be used if you will periodically monitor the system, otherwise the text file will grow very large and may burden the system.
#There is currently no way of monitoring the Windows system.
#if(monitor==TRUE){system("./Colony2.exe 2>&1 | tee temp.txt",wait=wait)}else{system("./Colony2.exe",wait=wait)}

cat("#! /bin/sh\necho Running Colony2\nexport G95_MEM_SEGMENTS=0\n./Colony2" , file = paste(datadir,"Colony2.sh",sep=""),append = FALSE)

if(monitor==TRUE){system("sh Colony2.sh | tee temp.txt",wait=wait)}else{system("sh Colony2.sh",wait=wait)}
#if(monitor==TRUE){system("./Colony2 | tee temp.txt",wait=wait)}else{system("./Colony2",wait=wait)}


#Remove the Colony2.exe and
	system(paste("rm",colonyexec))
		#
		if(file.exists("Colony2.sh")){system(paste("rm Colony2.sh"))}else{}

if(filename!="Colony2.DAT"){system("rm Colony2.DAT")}



}else{if(platform$OS.type=="windows"){

#THIS NEEDS TESTING

	#Windows commands
	shell(paste("copy",colonyexecpath,datadir,sep=" "))#Copy the colony exe file to the project directory
	if(filename!="Colony2.DAT"){shell(paste("rename",paste(datadir,filename,sep=""),paste(datadir,"Colony2.DAT",sep=""),sep=" "))}#Rename the colony dat file as Colony2.DAT
	shell.exec("Colony2.exe") #run colony2
	if(filename!="Colony2.DAT"){shell(paste("rename",paste(datadir,"Colony2.DAT",sep=""),paste(datadir,filename,sep=""),sep=" "))}#Rename the colony dat file to original file name.
	shell("del Colony2.exe") #tidy up, remove colony2.exe

	}else{stop(paste("This function is not correctly configured to run on",platform$OS.type,"systems."))
	}}

	#reset wd
 setwd(current.wd)

	}

