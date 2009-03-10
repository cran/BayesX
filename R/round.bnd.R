smooth.bnd <- function(map, digits=2, scale=1)
{
    if(! inherits(map,"bnd"))
        stop("Argument 'map' is not an object of class 'bnd'!")

    nrpolys <- length(map)

    for (i in 1:nrpolys){
        map[[i]] <- round(map[[i]]*scale, digits)
    }

    class(map) <- "bnd"
    return(map)
}

