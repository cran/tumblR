\name{user.following}
\alias{user.following}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve the blogs a user is following.
}
\description{
Use this method to retrieve the blogs followed by the user whose OAuth credentials are submitted with the request.
}
\usage{
user.following(limit = 20, offset = 0, token = NA, consumer_key = NA, 
consumer_secret = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
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
The API supports the OAuth 1.0a Protocol, accepting parameters via the Authorization header, with the HMAC-SHA1 signature method only.
}
\value{
A serialized JSON object with the following fields:
\item{total_blogs}{A number. The number of blogs the user is following.}
\item{blogs}{An array. Each item is a blog that is being followed, containing these fields:}
\item{name}{A string. The user name attached the blog that is being followed.}
\item{url}{A string. The URL of the blog that is being followed.}
\item{updated}{A number. The time of the most recent post, in seconds since the epoch.}
\item{title}{A string. The title of the blog.}
\item{description}{A string. The description of the blog.
}
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

user.following(token = token, consumer_key = consumer_key, 
consumer_secret = consumer_secret)
}	
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{following}