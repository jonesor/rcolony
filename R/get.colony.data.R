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
    if(colonyVersion == "2.0.3"){baseline = 23}
    if(colonyVersion == "2.0"){baseline = 22}
    if(!colonyVersion %in% c("2.0","2.0.3")){stop("This function only works with Colony version 2.0 or 2.0.3")}


    #Check whether allele frequency is known    
    AFKnown = x[11]
    AFKnown = sub("^[\t\n\f\r ]*", "", AFKnown) #remove leading whitespace
    AFKnown = as.numeric(gsub("([A-Za-z0-9]*)([!0-9A-Za-z,/= ]*)", "\\1", AFKnown, perl = TRUE)) == 1
    if(AFKnown){
    	#If allele frequency is known then there is an extra row defining the number of alleles per locus (e.g. "12 13 14 15 16  !Number of alleles per locus")
    	#Then there are a number of rows equal to the number of loci
    OFSStart = 2 * nLoci + (baseline + 2)
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
    mfile = list.files(path = datadir, pattern = "\\.Maternity")[1]
    if(!is.na(mfile)){
        if(length(mfile) > 1){
            warning("There are too many \"\ *.Maternity\" files in your project directory. \nYou should check them.")
        }
        maternity = read.table(paste(datadir, mfile, sep=""), header = TRUE, fill = TRUE, na.strings = "")
        colony.object$maternity = maternity
    }

    pfile = list.files(path = datadir, pattern = "\\.Paternity")[1]
    if(!is.na(pfile)){
        if(length(pfile) > 1){
            warning("There are too many \"\ *.Paternity\" files in your project directory. \nYou should check them.")
        }
        paternity = read.table(paste(datadir, pfile, sep=""), header = TRUE, fill = TRUE, na.strings = "")
        colony.object$paternity = paternity
    }
    
    ###################################################
    #assigned parentage (pairwise)
    ###################################################
    mfile = list.files(path = datadir, pattern = "\\.PairwiseMaternity")[1]
   
    if(!is.na(mfile)){
        if(length(mfile) > 1){
            warning("There are too many \"\ *.PairwiseMaternity\" files in your project directory. \nYou should check them. \nUsing the first one.")
        }
        pairwise.maternity = read.table(paste(datadir, mfile, sep = ""), header = TRUE, sep = ",")
        colony.object$pairwise.maternity = pairwise.maternity
    }

    pfile = list.files(path = datadir, pattern = "\\.PairwisePaternity")[1]
    if(!is.na(pfile)){
        if(length(mfile) > 1){
            warning("There are too many \"\ *.PairwisePaternity\" files in your project directory. \nYou should check them. \nUsing the first one.")
        }
        pairwise.paternity = read.table(paste(datadir, pfile, sep=""), header = TRUE, sep = ",")
        colony.object$pairwise.paternity = pairwise.paternity
    }
    
    ###################################################
    #Sibships (nonpairwise)
    ###################################################
    #Get full and half sibship data.
    full.file = list.files(path = datadir, pattern = "\\.FullSibDyad")[1]
    half.file = list.files(path = datadir, pattern = "\\.HalfSibDyad")[1]
    
    if(!is.na(full.file)){
    	fullsibs = read.table(paste(datadir, full.file, sep = ""), header = TRUE, sep = ",")
    	fullsibs$type = rep("Full", dim(fullsibs)[1])
    	}else{
    		fullsibs = NA
    	}

    if(!is.na(half.file)){
    	halfsibs = read.table(paste(datadir, half.file, sep = ""), header = TRUE, sep = ",")
        halfsibs$type = rep("Half", dim(halfsibs)[1])
    	}else{
    		halfsibs = NA
    	}
    
    sibs = na.omit(rbind(fullsibs, halfsibs))
    sibs$type = as.factor(sibs$type)
    
    colony.object$sibs = sibs
    
    ###################################################
    #Sibships (pairwise)
    ###################################################
    
    #Get full and half sibship data.
    full.file = list.files(path = datadir, pattern = "\\.PairwiseFullSibDyad")[1]
    half.file = list.files(path = datadir, pattern = "\\.PairwiseHalfSibDyad")[1]
    
    if(!is.na(full.file)){
    	fullsibs = read.table(paste(datadir, full.file, sep = ""), header = TRUE, sep = ",")
    	fullsibs$type = rep("Full", dim(fullsibs)[1])
    	}else{
    		fullsibs = NA
    	}

    if(!is.na(half.file)){
    	halfsibs = read.table(paste(datadir, half.file, sep = ""), header = TRUE, sep = ",")
        halfsibs$type = rep("Half", dim(halfsibs)[1])
    	}else{
    		halfsibs = NA
    	}
    
    sibs = na.omit(rbind(fullsibs, halfsibs))
    sibs$type = as.factor(sibs$type)
    
    colony.object$sibs = sibs
    
    return(colony.object)
    }

