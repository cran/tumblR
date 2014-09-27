\name{likes}
\alias{likes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve Blog's Likes
}
\description{
Retrieve the publicly exposed likes from a blog.
}
\usage{
likes(base_hostname = NA, limit = 20, offset = 0, api_key = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{base_hostname}{
The standard or custom blog hostname. See Details.
}
  \item{limit}{
The number of results to return: 1-20, inclusive.
}
  \item{offset}{
Liked post number to start at. 0 is the first post.
}
  \item{api_key}{
Your OAuth Consumer Key. See Details.
}
}
\details{
Each blog has a unique hostname. The hostname can be standard or custom.
Standard hostname: the blog short name + .tumblr.com.
Custom hostname: Anything at all, as determined by a DNS CNAME entry.
The API uses three different levels of authentication, depending on the method.
None: No authentication. Anybody can query the method.
API key: Requires an API key. Use your OAuth Consumer Key as your api_key.
OAuth: Requires a signed request that meets the OAuth 1.0a Protocol.
}
\value{
A list object with the following fields:
\item{liked_posts}{An Array. An array of post objects (posts liked by the user).}
\item{liked_count}{A number. Total number of liked posts.}
}
\references{
https://www.tumblr.com/docs/en/api/v2
}
\author{
Andrea Capozio
}
\examples{
\dontrun{	
## An example of an authenticated request,
## where api_key is fictitious.
## You can obtain your own at https://www.tumblr.com/oauth/apps
api_key <- "key"

## you must specify a real blog for base_hostname
base_hostname <- "blogname.tumblr.com"


likes(base_hostname = base_hostname, limit = 20, offset = 0, api_key = api_key)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{likes}