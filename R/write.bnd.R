write.bnd <- function(map, file, replace=FALSE)
{
    if(! inherits(map,"bnd"))
        stop("Argument 'map' is not an object of class 'bnd'!")

    ## check whether the file exists
    if(replace & file.exists(file))
        test <- file.remove(file)
    if(!replace & file.exists(file))
        stop("Specified file already exists!")

    ## no. of regions
    regions <- names(map)
    S <- length(regions)
    
    is.in <- attr(map, "is.in")
    contains <- attr(map, "contains") 
    w <- 1
    
    for(i in 1:S){
        dat <- map[[i]]
        ind <- which(is.na(dat[,1]) | is.na(dat[,2]))
        if(length(ind)>0)
            dat <- dat[-ind,]
        
        temp <- paste("\"",regions[i],"\",",nrow(dat),sep="")
        write(temp,file,append=TRUE)
        
        if(regions[i] %in% is.in){
            con <- paste("is.in,","\"",contains[w],"\"", sep="")
            write(con,file,append=TRUE)
            w <- w + 1
        }
        write.table(dat,file,append=TRUE,col.names=FALSE,row.names=FALSE,sep=",",quote=FALSE)
    }

    return(invisible())
}

