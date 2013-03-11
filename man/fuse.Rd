\name{fuse}
\alias{fuse}

\title{Combine Regions}

\description{
Combines a list of several regions of a map object in boundary format into a single region (not yet implemented).
}

\usage{
fuse(map, regions)
}

\arguments{
  \item{map}{Map object in boundary format that should be modified.}
  \item{regions}{Vector of regions to be combined}
}

\value{
Map object in boundary format with the specified regions combined.
}

\author{Felix Heinzl, Thomas Kneib}

\seealso{\code{\link{read.bnd}},\code{\link{write.bnd}}}

\keyword{spatial}
