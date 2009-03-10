read.bnd <- function(file, sorted=FALSE)
{
    ## disable warnings and revert to old settings afterwards
    oldOptions <- options(warn = -1)
    on.exit(options(oldOptions))    

    ## Liste wird erstellt: 1. Element enthält die 1. Spalte aus BND-Datei, 2. Element die 2. Spalte 
    data.raw <- scan(file, what = list("", ""), sep = ",", quote = "")
    
    ## Ursache für Warnungen: NAs werden bei jedem Regionsnamen und bei is.in erzeugt
    data.numeric <- list(as.numeric(data.raw[[1]]), as.numeric(data.raw[[2]]))
    
    is.in <- data.raw[[1]][which(data.raw[[1]]=="is.in")-1]
    contains <- data.raw[[2]][which(data.raw[[1]]=="is.in")]
    for(w in seq_along(is.in)){
        is.in[w] <- substring(is.in[w], 2, nchar(is.in[w]) - 1)
        contains[w] <- substring(contains[w], 2, nchar(contains[w]) - 1)
    }   
    anzkreise <- sum(is.na(data.numeric[[1]])) - length(is.in)
    cat("Note: map consists of", anzkreise, "polygons\n")
    cat("Reading map ...\n")

    map <- list()
    i <- 1
    
    for(k in 1:anzkreise) {
        j <- i
        npoints <- data.numeric[[2]][i]
        if(is.na(data.numeric[[1]][i + 1]) && is.na(data.numeric[[2]][i + 1]))
            i <- i + 1
        elem1 <- data.numeric[[1]][i+1:npoints]
        elem2 <- data.numeric[[2]][i+1:npoints]
        map[[k]] <- matrix(c(elem1, elem2), ncol = 2)
        names(map)[k] <- substring(data.raw[[1]][j], 2, nchar(data.raw[[1]][j]) - 1)
        i <- i + npoints + 1
    }
    
    if(sorted){
    	if(sum(is.na(as.numeric(names(map)))) == 0) {
            map <- map[order(as.numeric(names(map)))]
            cat("Note: regions sorted by number\n")
        }
        else {
            map <- map[order(names(map))]
            cat("Note: regions sorted by name\n")
        }      
    }

    ## Bestimmung des Höhe-Breiten-Verhältnisses
    minima <- sapply(map, function(x){apply(x,2,min)})
    maxima <- sapply(map, function(x){apply(x,2,max)})
    minimum <- apply(minima,1,min)
    maximum <- apply(maxima,1,max)
    x.range <- maximum[1] - minimum[1]
    y.range <- maximum[2] - minimum[2]
    height2width <- round(y.range/x.range, digits=2)

    count <- length(unique(names(map)))
    cat("Note: map consists of", count,"regions\n")
    attr(map, "is.in") <- is.in
    attr(map, "contains") <- contains
    attr(map, "height2width") <- height2width

    class(map) <- "bnd"
    return(map)
}

