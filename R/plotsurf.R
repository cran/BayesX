plotsurf <- function(data, x=2, y=3, z=4, mode=1, ticktype="detailed", expand=0.75, d=100, theta=-30, phi=25, ...)
{
    if(!is.data.frame(data))
        data <- read.table(data, header=TRUE)

    if(is.character(x))
        x <- match(x, names(data))
    if(is.character(y))
        y <- match(y, names(data))
    if(is.character(z))
        z <- match(z, names(data))

    x <- data[, x]
    y <- data[, y]
    z <- data[, z]

    ## interp is from package akima, but this package does not have a namespace,
    ## so we have to load the whole package (see DESCRIPTION).
    data <- akima::interp(x, y, z)
    x <- data$x
    y <- data$y
    z <- data$z

    if(mode==1)
        persp(x, y, z, ticktype=ticktype, expand=expand, d=d, theta=theta, phi=phi, ...)
    else if(mode==2)
        image(x, y, z, ...)
    else if(mode==3)
        contour(x, y, z, ...)

    return(invisible())
}

