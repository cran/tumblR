\name{like.post}
\alias{like.post}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Like a Post.
}
\description{
This function allows to like a post of other Tumblr users.
}
\usage{
like.post(id = NA, reblog_key = NA, token = NA, consumer_key = NA, 
consumer_secret = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{id}{
The ID of the post to like.
}
  \item{reblog_key}{
  	The reblog key for the post id.
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
Returns 200: OK (post successfully liked) or a 404 (post id or reblog_key was not found).
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
tokenURL <- 'https://www.tumblr.com/oauth/request_token'
accessTokenURL <- 'https://www.tumblr.com/oauth/acces_token'
authorizeURL <- 'https://www.tumblr.com/oauth/authorize'

app <- oauth_app(appname, consumer_key, consumer_secret)	
endpoint <- oauth_endpoint(tokenURL, authorizeURL, accessTokenURL)
token <- oauth1.0_token(endpoint, app)
sig <- sign_oauth1.0(app, 
token = token$credentials$oauth_token, 
token_secret = token$credentials$oauth_token_secret)

id <- 7504154594
reblog_key <- "HNvqLd5G"

like.post(id = id, reblog_key = reblog_key, token = token,
consumer_key = consumer_key,consumer_secret = consumer_secret)	
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{like}
