\name{user.info}
\alias{user.info}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get a User's Information.
}
\description{
Use this method to retrieve the user's account information that matches the OAuth credentials submitted with the request.
}
\usage{
user.info(token = NA, consumer_key = NA, consumer_secret = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
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
A list object with the following fields:
\item{following}{A number. The number of blogs the user is following.}
\item{default_post_format}{A string. The default posting format -html, markdown.}
\item{name}{A string. The user's tumblr short name.}
\item{likes}{A number. The total count of the user's like.}
\item{blogs}{An array. Each item is a blog that is being followed, containing these fields:}
\item{name}{A string. The short name of the blog.}
\item{url}{A string. The URL of the blog.}
\item{primary}{A boolean. Indicates if this is the user's primary blog.}
\item{title}{A string. The title of the blog.}
\item{followers}{A number. Total count of followers for this blog.
}
\item{tweet}{A number. Indicates if posts are tweeted (auto, Y, N).
}
\item{facebook}{Indicates if posts are sent to Facebook	Y, N.
}
\item{type}{Indicates whether a blog is public or private.
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

user.info(token = token, consumer_key = consumer_key, 
consumer_secret = consumer_secret)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{user's info}