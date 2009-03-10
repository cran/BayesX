bnd2gra <- function(map)
{
   if(! inherits(map, "bnd"))
      stop("Argument 'map' is not an object of class 'bnd'!")

   S <- length(map)
   districts <- names(map)
   pmat <- matrix(0,S,S)

   for(i in 1:(S-1)){
      for(j in (i+1):S){
          
          region.i <- paste(map[[i]][, 1],
                            map[[i]][, 2],
                            sep="&")
          region.j <- paste(map[[j]][, 1],
                            map[[j]][, 2],
                            sep="&")

         if(sum(region.i %in% region.j) >= 2)
            pmat[i,j] <- pmat[j,i] <- -1   
      }
      if(i %% 25 == 0)
         cat(paste("progress: ",round(i/S * 100, 2),"%\n", sep=""))   
   }
   
   diag(pmat) <- -apply(pmat, 1, sum) 
   
   rownames(pmat) <- districts
   colnames(pmat) <- districts
   
   class(pmat) <- "gra"
   return(pmat)
}
     
