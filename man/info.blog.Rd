\name{info.blog}
\alias{info.blog}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve Blog's Info.
}
\description{
Returns general information about the blog, such as the title, number of posts, and other high-level data.
}
\usage{
info.blog(base_hostname = NA, api_key = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{base_hostname}{
The standard or custom blog hostname. See Details.
}
  \item{api_key}{
Your OAuth Consumer Key.
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
\item{title}{A string. The display title of the blog.}
\item{posts}{A number. The total number of posts to this blog.}
\item{name}{A string. The short blog name that appears before tumblr.com in a standard blog hostname (and before the domain in a custom blog hostname).}
\item{updated}{A number. The time of the most recent post, in seconds since the epoch.}
\item{description}{A string. The description of the blog.}
\item{ask}{A boolean. Indicates whether the blog allows questions.}
\item{ask_anon}{A boolean. Indicates whether the blog allows anonymous questions. Returned only if ask is TRUE.}
\item{likes}{A number. Number of likes for this user. Returned only if this is the primary blog of the user and sharing of likes is enabled.}
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

info.blog(base_hostname = base_hostname, api_key = api_key)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{info.blog}

