\name{avatar}
\alias{avatar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve a Blog Avatar.
}
\description{
Get the url of a blog's avatar.
}
\usage{
avatar(base_hostname = NA, size = 64)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{base_hostname}{
The standard or custom blog hostname. See Details.
}
  \item{size}{
The size of the avatar (square, one value for both length and width). Must be one of the values: 16, 24, 30, 40, 48, 64, 96, 128, 512.
}
}
\details{
Each blog has a unique hostname. The hostname can be standard or custom.
Standard hostname: the blog short name + .tumblr.com.
Custom hostname: Anything at all, as determined by a DNS CNAME entry.
}
\value{
If the download succeeded, the url of the blog'avatar has returned, otherwise an error is encountered.
}
\references{
https://www.tumblr.com/docs/en/api/v2
}
\author{
Andrea Capozio
}

\examples{
\dontrun{
## you must specify a real blog for base_hostname

size <- 48
base_hostname <- "blogname.tumblr.com"

avatar(base_hostname = base_hostname, size = 48)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{avatar}
