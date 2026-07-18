#' This function creates a data object from the Colony2 outputs.
#'
#' When provided with the directory name, and the name of the *.DAT colony
#' input file, this function creates a list object containing pertinent
#' information from the Colony output files.
#'
#'
#' @param datadir The path to the directory that contains the Colony output
#' files and the *.DAT Colony input file.
#' @param filename Optional. The name of the Colony input file. If this
#' argument is omitted, the command will search for a *.DAT file in the data
#' directory (datadir) and use that as the input file.
#' @param colonyVersion The version of Colony that was used (default is 2.0,
#' other options are 2.0.3, 2.0.6 and 2.0.7). Versions 2.0.6 and 2.0.7 cover
#' the current 2.0.6.x/2.0.7.x releases, which add extra parameter lines
#' (clone inference and full-sibship-size scaling) to the input file header.
#' @return A list, containing data extracted from the Colony output files
#' @author Owen R. Jones
#' @seealso \code{\link{run.colony}}, \code{\link{build.colony.input}}
#' @references \url{http://www.zoo.cam.ac.uk/ioz/software.htm#COLONY}
#' @keywords manip
#' @examples
#'
#' #Not run
#' #mynewdata<-get.colony.data("/Users/FredBlogs/Documents/Example/")
#' #mynewdata<-get.colony.data("/Users/FredBlogs/Documents/Example/","Example1.DAT")
#'
#' @export
get.colony.data <- function(datadir, filename = list.files(datadir, pattern = ".DAT", ignore.case=TRUE), colonyVersion = "2.0"){

    colony.object = NULL
    x = readLines(paste(datadir, filename, sep = ""))

    #Strip out empty rows, if there are any.
    if(length(which(x == "")) > 0){x = x[-which(x == ""|x == " ")]}

    #Extract the number of offspring from the dat file. This information is used for error checking later on.
    n = x[3]
    n = sub("^[\t\n\f\r ]*", "", n) #remove leading whitespace
    n = as.numeric(gsub("([A-Za-z0-9]*)([!0-9A-Za-z,/= ]*)", "\\1", n, perl = TRUE))

    #Extract the number of loci
    nLoci = x[4]
    nLoci = sub("^[\t\n\f\r ]*", "", nLoci) #remove leading whitespace
    nLoci = as.numeric(gsub("([A-Za-z0-9]*)([!0-9A-Za-z,/= ]*)", "\\1", nLoci, perl = TRUE))

    #Version check
    # The baseline is the line number (after blank lines have been stripped) at
    #which the offspring genotype block begins when population allele
    #frequencies are unknown. 'afLine' is the line holding the
    #unknown/known-allele-frequency flag. Later Colony versions insert extra
    #parameter lines into the input-file header (an inbreeding line, a
    #clone-inference line and a full-sibship-size scaling line), which pushes
    #both of these further down the file.
    if(colonyVersion == "2.0"){baseline = 22; afLine = 11}
    if(colonyVersion == "2.0.3"){baseline = 23; afLine = 11}
    #Colony 2.0.6.x / 2.0.7.x add the inbreeding, clone-inference and
    #full-sibship-size scaling lines relative to 2.0.3 (three extra header
    #lines, all before the allele-frequency flag), verified against a 2.0.7
    #-format input file.
    if(colonyVersion %in% c("2.0.6", "2.0.7")){baseline = 26; afLine = 14}
    if(!colonyVersion %in% c("2.0", "2.0.3", "2.0.6", "2.0.7")){stop("This function only works with Colony versions 2.0, 2.0.3, 2.0.6 or 2.0.7")}


    #Check whether allele frequency is known
    AFKnown = x[afLine]
    AFKnown = sub("^[\t\n\f\r ]*", "", AFKnown) #remove leading whitespace
    AFKnown = as.numeric(gsub("([A-Za-z0-9]*)([!0-9A-Za-z,/= ]*)", "\\1", AFKnown, perl = TRUE)) == 1
    if(AFKnown){
    	#If allele frequency is known then there is an extra row defining the
    	#number of alleles per locus (e.g. "12 13 14 15 16  !Number of alleles
    	#per locus"), followed by two rows per locus (allele identities and the
    	#corresponding frequencies). That is 1 + 2*nLoci extra lines before the
    	#offspring block.
    OFSStart = 2 * nLoci + (baseline + 1)
    }else{
    	OFSStart = baseline
    }

    ###################################################
    #Lists of offspring, fathers and mothers
    ###################################################
    # Starts at line 24 - 24+(n-1)
    offspring = x[OFSStart:(OFSStart + (n - 1))]

    nParents = x[OFSStart + (n+1)]
    nParents = sub("^[\t\n\f\r ]*", "", nParents) #remove leading whitespace

    nFathers = as.numeric(strsplit(nParents, split = " +")[[1]][1])
    nMothers = as.numeric(strsplit(nParents, split = " +")[[1]][2])

    #If the header offset for the specified colonyVersion does not match the
    #file, the line we expect to hold the candidate-parent counts will not be
    #two numbers. Fail loudly rather than silently returning garbage.
    if(is.na(nFathers) | is.na(nMothers)){
        stop("Could not locate the candidate-parent counts. The header offset for colonyVersion \"", colonyVersion, "\" does not match this file. Check that colonyVersion matches the Colony version that produced it.")
    }

    fathers = x[(OFSStart + (n + 2)):(OFSStart + (n + 1) + nFathers)]
    mothers = x[(OFSStart + (n + 2) + nFathers):(OFSStart + (n + 1) + nFathers + nMothers)]

    #Remove leading whitespace
    offspring = sub("^[\t\n\f\r ]*", "", offspring) #remove leading whitespace
    fathers = sub("^[\t\n\f\r ]*", "", fathers) #remove leading whitespace
    mothers = sub("^[\t\n\f\r ]*", "", mothers) #remove leading whitespace

    offspring = as.vector(sapply(offspring, function(x){gsub("([A-Za-z0-9]*)([!0-9A-Za-z,/= ]*)", "\\1", x, perl = TRUE)})) #extract names
    fathers = as.vector(sapply(fathers, function(x){gsub("([A-Za-z0-9]*)([!0-9A-Za-z,/= ]*)", "\\1", x, perl = TRUE)})) #extract names
    mothers = as.vector(sapply(mothers, function(x){gsub("([A-Za-z0-9]*)([!0-9A-Za-z,/= ]*)", "\\1", x, perl = TRUE)})) #extract names

    if(n != length(offspring)){stop("Wrong number of offspring. Check your files.")}

    #Assign numeric codes to fathers and mothers
    mothers = data.frame(motherID=mothers, mother.numID = as.numeric(as.factor(mothers)))
    mothers$motherID = as.character(mothers$motherID)
    mothers = rbind(mothers, c("Unknown", max(mothers$mother.numID) + 1))

    fathers = data.frame(fatherID=fathers, father.numID = as.numeric(as.factor(fathers)))
    fathers$fatherID = as.character(fathers$fatherID)
    fathers = rbind(fathers, c("Unknown",max(fathers$father.numID) + 1))

    offspring = data.frame(offspringID=offspring, offspring.numID = as.numeric(as.factor(offspring)))
    offspring$offspringID = as.character(offspring$offspringID)
    offspring = rbind(offspring, c("Unknown", max(offspring$offspring.numID) + 1))

    colony.object$fathers = fathers
    colony.object$mothers = mothers
    colony.object$offspring = offspring

    ###################################################
    #assigned parentage (nonpairwise)
    ###################################################
    mfiles = list.files(path = datadir, pattern = "\\.Maternity")
    if(length(mfiles) > 0){
        if(length(mfiles) > 1){
            warning("There are too many \"*.Maternity\" files in your project directory. \nYou should check them. \nUsing the first one.")
        }
        mfile = mfiles[1]
        maternity = utils::read.table(paste(datadir, mfile, sep=""), header = TRUE, fill = TRUE, na.strings = "")
        colony.object$maternity = maternity
    }

    pfiles = list.files(path = datadir, pattern = "\\.Paternity")
    if(length(pfiles) > 0){
        if(length(pfiles) > 1){
            warning("There are too many \"*.Paternity\" files in your project directory. \nYou should check them. \nUsing the first one.")
        }
        pfile = pfiles[1]
        paternity = utils::read.table(paste(datadir, pfile, sep=""), header = TRUE, fill = TRUE, na.strings = "")
        colony.object$paternity = paternity
    }

    ###################################################
    #assigned parentage (pairwise)
    ###################################################
    mfiles = list.files(path = datadir, pattern = "\\.PairwiseMaternity")
    if(length(mfiles) > 0){
        if(length(mfiles) > 1){
            warning("There are too many \"*.PairwiseMaternity\" files in your project directory. \nYou should check them. \nUsing the first one.")
        }
        mfile = mfiles[1]
        pairwise.maternity = utils::read.table(paste(datadir, mfile, sep = ""), header = TRUE, sep = ",")
        colony.object$pairwise.maternity = pairwise.maternity
    }

    pfiles = list.files(path = datadir, pattern = "\\.PairwisePaternity")
    if(length(pfiles) > 0){
        if(length(pfiles) > 1){
            warning("There are too many \"*.PairwisePaternity\" files in your project directory. \nYou should check them. \nUsing the first one.")
        }
        pfile = pfiles[1]
        pairwise.paternity = utils::read.table(paste(datadir, pfile, sep=""), header = TRUE, sep = ",")
        colony.object$pairwise.paternity = pairwise.paternity
    }

    ###################################################
    #Sibships (nonpairwise)
    ###################################################
    #Get full and half sibship data.
    full.file = list.files(path = datadir, pattern = "\\.FullSibDyad")[1]
    half.file = list.files(path = datadir, pattern = "\\.HalfSibDyad")[1]

    if(!is.na(full.file)){
    	fullsibs = utils::read.table(paste(datadir, full.file, sep = ""), header = TRUE, sep = ",")
    	fullsibs$type = rep("Full", dim(fullsibs)[1])
    	}else{
    		fullsibs = NA
    	}

    if(!is.na(half.file)){
    	halfsibs = utils::read.table(paste(datadir, half.file, sep = ""), header = TRUE, sep = ",")
        halfsibs$type = rep("Half", dim(halfsibs)[1])
    	}else{
    		halfsibs = NA
    	}

    #Combine whichever of the full/half sib files were present. If neither
    #exists, store NA rather than crashing on rbind(NA, NA).
    sib.parts = Filter(is.data.frame, list(fullsibs, halfsibs))
    if(length(sib.parts) > 0){
        sibs = do.call(rbind, sib.parts)
        sibs$type = as.factor(sibs$type)
    }else{
        sibs = NA
    }

    colony.object$sibs = sibs

    ###################################################
    #Sibships (pairwise)
    ###################################################

    #Get full and half sibship data.
    full.file = list.files(path = datadir, pattern = "\\.PairwiseFullSibDyad")[1]
    half.file = list.files(path = datadir, pattern = "\\.PairwiseHalfSibDyad")[1]

    if(!is.na(full.file)){
    	fullsibs = utils::read.table(paste(datadir, full.file, sep = ""), header = TRUE, sep = ",")
    	fullsibs$type = rep("Full", dim(fullsibs)[1])
    	}else{
    		fullsibs = NA
    	}

    if(!is.na(half.file)){
    	halfsibs = utils::read.table(paste(datadir, half.file, sep = ""), header = TRUE, sep = ",")
        halfsibs$type = rep("Half", dim(halfsibs)[1])
    	}else{
    		halfsibs = NA
    	}

    #Combine whichever of the pairwise full/half sib files were present. If
    #neither exists, store NA rather than crashing on rbind(NA, NA). This is
    #kept separate from the (non-pairwise) $sibs element, as plotsibs() reads
    #$pairwise.sibs for pairwise==TRUE.
    sib.parts = Filter(is.data.frame, list(fullsibs, halfsibs))
    if(length(sib.parts) > 0){
        sibs = do.call(rbind, sib.parts)
        sibs$type = as.factor(sibs$type)
    }else{
        sibs = NA
    }

    colony.object$pairwise.sibs = sibs

    return(colony.object)
    }

