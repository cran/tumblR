\name{user.likes}
\alias{user.likes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve a User's Likes.
}
\description{
Use this method to retrieve the liked posts that match the OAuth credentials submitted with the request.
}
\usage{
user.likes(limit = 20, offset = 0, token = NA, consumer_key = NA, 
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
\item{liked_posts}{An array. An array of posts objects(posts liked by the user).}
\item{liked_count}{A number.Total number of liked posts.}
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
	
	
user.likes(token = token, consumer_key = consumer_key, 
consumer_secret = consumer_secret)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{likes}