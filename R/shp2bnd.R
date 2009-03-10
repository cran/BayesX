shp2bnd <- function(shpname, regionnames, check.is.in = TRUE)
{
    ## read the shapefile information
    shp<-shapefiles::read.shapefile(shpname)
    dbf<-shapefiles::read.dbf(paste(shpname,".dbf",sep=""))

    ## extract names of the regions
    if(is.character(regionnames))
    {
        regions<-dbf$dbf[regionnames][[1]]
    }
    else
    {
        regions <- regionnames
    }

    ## delete commas in names of the regions
    commas <- FALSE
    regions <- as.vector(regions)
    for(i in 1:length(regions)){
        comma.check <- regexpr(",",regions[i])
        if(attr(comma.check,"match.length")==1){
            commas <- TRUE
            parts <- unlist(strsplit(regions[i], "\\,"))
            new.name <- ""

            for(j in 1:length(parts)){
                new.name <- paste(new.name,parts[j],sep="")
            }

            regions[i] <- new.name
        } 
    }      
    if(commas)
        warning("Commas in names of the regions are deleted!")  

    ## split data into closed polygons
    regionsnew <- numeric(0)
    origind <- numeric(0)
    polynew <- list()
    k <- 1

    for(i in 1:length(regions))
    {
        temppoly <- shp$shp$shp[[i]]$points
        n <- nrow(temppoly)
        i1 <- 1
        i2 <- which(temppoly[,1]==temppoly[i1,1] & temppoly[,2]==temppoly[i1,2])[2]

        while(i2 < n)
        {
            tempname<-paste("\"",regions[i],"\",",i2,sep="")
            regionsnew <- c(regionsnew,tempname)
            origind <- c(origind,i)
            polynew[[k]] <- temppoly[i1:i2,]
            k <- k+1

            temppoly <- temppoly[-(i1:i2),]
            n <- nrow(temppoly)
            i1 <- 1
            i2 <- which(temppoly[,1]==temppoly[i1,1] & temppoly[,2]==temppoly[i1,2])[2]
        }

        tempname<-paste("\"",regions[i],"\",",i2,sep="")
        regionsnew <- c(regionsnew,tempname)
        origind <- c(origind,i)
        polynew[[k]] <- temppoly[i1:i2,]
        k <- k+1
    }

    ## check for regions contained in another region
    if(check.is.in)
    {
        dims <- rep(0,length(regionsnew))
        rmcheck <- rep(FALSE,length(regionsnew))
        for(i in 1:length(regionsnew))
        {
            dims[i] <- nrow(polynew[[i]])
        }
        for(i in 1:length(regionsnew))
        {
            dimshelp <- which(dims==dims[i]) # which polygons have same number of points as the current?
            if(length(dimshelp)>1)
            {
                for(j in 2:length(dimshelp))
                {
                    test <- (sum((polynew[[i]]-polynew[[dimshelp[j]]][nrow(polynew[[dimshelp[j]]]):1,])^2) < 10e-6)
                    if(test)
                    {
                        if(.ringDirxy(polynew[[dimshelp[j]]])<0)
                        {
                            rmcheck[dimshelp[j]] <- TRUE
                            regionsnew[i] <- paste(regionsnew[i],
                                                   "\nis.in,\"",
                                                   regions[origind[dimshelp[j]]],
                                                   "\"",
                                                   sep="")
                        }
                        else
                        {
                            rmcheck[i] <- TRUE
                            regionsnew[dimshelp[j]] <- paste(regionsnew[dimshelp[j]],
                                                             "\nis.in,\"",
                                                             regions[origind[i]],
                                                             "\"",
                                                             sep="")
                        }
                    }
                }
            }
        }

        regionsnew <- regionsnew[! rmcheck]
        polynew <- polynew[! rmcheck]
    }

    ## write to the specified temporary file
    bndname <- tempfile("bnd")
    for(i in 1:length(regionsnew))
    {
        write(regionsnew[i],bndname,append=T)
        write.table(polynew[[i]],bndname,append=T,col.names=FALSE,row.names=FALSE,sep=",",quote=FALSE)
    }

    bnd <- read.bnd(bndname)
    return(bnd)
}