\name{followers}
\alias{followers}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve a Blog's Followers
}
\description{
Retrieve the followers of the user's blog.
}
\usage{
followers(base_hostname = NA, limit = 20, offset = 0, token = NA, 
			consumer_key = NA, consumer_secret = NA)
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
Result to start at. 0 is the first follower.
}
  \item{token}{
Represents the complete set of data needed for OAuth access: an app, an endpoint, cached credentials and parameters. See Details.
}
  \item{consumer_key}{
The consumer key provided by your application.
}
  \item{consumer_secret}{
The consumer secret provided by your application.
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

The API supports the OAuth 1.0a Protocol, accepting parameters via the Authorization header, with the HMAC-SHA1 signature method only.
}
\value{
A list object with the following fields:
\item{total_users}{A number. The number of users currently following the blog.}
\item{users}{An array. Each item is a follower, containing these fields:}
\item{name}{A string. The user's name on tumblr.}
\item{following}{A boolean. Whether the caller is following the user.}
\item{url}{A string. The URL of the user's primary blog.}
\item{updated}{A number. The time of the user's most recent post, in seconds since the epoch.}
}
\references{
https://www.tumblr.com/docs/en/api/v2
}
\author{
Andrea Capozio
}
\examples{
\dontrun{	
## An example of an authenticated request using the httr package,
## where consumer_key, consumer_secret, appname are fictitious.
## You can obtain your own at https://www.tumblr.com/oauth/apps
consumer_key <-'key'
consumer_secret <- 'secret'
appname <- Tumblr_App
tokenURL <- 'http://www.tumblr.com/oauth/request_token'
accessTokenURL <- 'http://www.tumblr.com/oauth/access_token'
authorizeURL <- 'http://www.tumblr.com/oauth/authorize'

app <- oauth_app(appname, consumer_key, consumer_secret)	
endpoint <- oauth_endpoint(tokenURL, authorizeURL, accessTokenURL)
token <- oauth1.0_token(endpoint, app)
sig <- sign_oauth1.0(app, 
token = token$credentials$oauth_token, 
token_secret = token$credentials$oauth_token_secret)

## you must specify a real blog for base_hostname
base_hostname <- "blogname.tumblr.com"

followers(base_hostname = base_hostname, limit = 20, offset = 0, token = token,
consumer_key = consumer_key, consumer_secret = consumer_secret)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{followers}