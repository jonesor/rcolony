# Read a Colony *.Maternity / *.Paternity file. Colony 2.0.7.x writes these as
# comma-delimited files; earlier versions used whitespace. Detect which from
# the header line.
read.parentage.file <- function(path){
    header <- readLines(path, n = 1)
    delim <- if(grepl(",", header, fixed = TRUE)) "," else ""
    utils::read.table(path, header = TRUE, sep = delim, fill = TRUE, na.strings = "")
}

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
#' @param colonyVersion Optional. The version of Colony that produced the input
#' file. By default the layout of the input file (which changed between Colony
#' versions) is detected automatically from its structure, which is usually what
#' you want. Give a version (for example \code{"2.0.7.2"}) to insist on the
#' layout used by that version, in which case an error is given if the file does
#' not match. Colony 2.0.x versions are understood.
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
get.colony.data <- function(datadir, filename = list.files(datadir, pattern = "\\.DAT$", ignore.case=TRUE), colonyVersion = NULL){

    colony.object = NULL

    #Read the IDs of the offspring and candidate parents from the input file.
    #The layout of the file (which depends on the Colony version) is detected from
    #its structure unless colonyVersion says which one to expect; see
    #colony-format.R.
    layout <- if(is.null(colonyVersion)) NULL else colony.layout.name(colonyVersion)
    dat <- tryCatch(read.colony.dat(paste(datadir, filename, sep = ""), layout = layout),
                    error = function(e){
                      hint <- if(is.null(layout)) "" else "\nCheck that colonyVersion matches the Colony version that produced the file, or leave it out so that the layout is detected automatically."
                      stop(conditionMessage(e), hint, call. = FALSE)
                    })

    ids <- function(x) if(is.null(x)) character(0) else x[[1]]
    mothers <- ids(dat$mothers)
    fathers <- ids(dat$fathers)
    offspring <- ids(dat$offspring)

    #Assign numeric codes to fathers and mothers, with an extra "Unknown" parent
    #(The codes are character, as they have always been in this object.)
    with.unknown <- function(ids, id.name, num.name){
      codes <- as.numeric(as.factor(ids))
      data.frame(stats::setNames(list(c(ids, "Unknown"), as.character(c(codes, max(c(0, codes)) + 1))), c(id.name, num.name)),
                 stringsAsFactors = FALSE)
    }
    mothers = with.unknown(mothers, "motherID", "mother.numID")
    fathers = with.unknown(fathers, "fatherID", "father.numID")

    offspring = data.frame(offspringID=offspring, offspring.numID = as.numeric(as.factor(offspring)))
    offspring$offspringID = as.character(offspring$offspringID)

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
        maternity = read.parentage.file(paste(datadir, mfile, sep=""))
        colony.object$maternity = maternity
    }

    pfiles = list.files(path = datadir, pattern = "\\.Paternity")
    if(length(pfiles) > 0){
        if(length(pfiles) > 1){
            warning("There are too many \"*.Paternity\" files in your project directory. \nYou should check them. \nUsing the first one.")
        }
        pfile = pfiles[1]
        paternity = read.parentage.file(paste(datadir, pfile, sep=""))
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
        pairwise.maternity = utils::read.table(paste(datadir, mfile, sep = ""), header = TRUE, sep = ",", comment.char = "")
        colony.object$pairwise.maternity = pairwise.maternity
    }

    pfiles = list.files(path = datadir, pattern = "\\.PairwisePaternity")
    if(length(pfiles) > 0){
        if(length(pfiles) > 1){
            warning("There are too many \"*.PairwisePaternity\" files in your project directory. \nYou should check them. \nUsing the first one.")
        }
        pfile = pfiles[1]
        pairwise.paternity = utils::read.table(paste(datadir, pfile, sep=""), header = TRUE, sep = ",", comment.char = "")
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

