\name{dashboard}
\alias{dashboard}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve a User's Dashboard.
}
\description{
Use this method to retrieve the dashboard that matches the OAuth credentials submitted with the request.
}
\usage{
dashboard(limit = 20, offset = 0, type = NA, since_id = 0, reblog_info = FALSE, 
notes_info = FALSE, token = NA, consumer_key = NA, consumer_secret = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{limit}{
The number of results to return: 1-20, inclusive.
}
  \item{offset}{
Post number to start at. 0 is the first post.
}
  \item{type}{
The type of post to return. The available values are: text, photo, quote, link, chat, audio, video, answer. If no values are specified, all types are returned.
}
  \item{since_id}{
Return posts that have appeared after this ID.
}
  \item{reblog_info}{
Indicates whether to return reblog information (specify TRUE or FALSE). Returns the various reblogged_fields.
}
  \item{notes_info}{
Indicates whether to return notes information (specify TRUE or FALSE). Returns note count and note metadata.
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
\item{blog_name}{A string. The short name used to uniquely identify a blog.}
\item{id}{A number. The unique ID of the post.}
\item{post_url}{A string. The location of the post.}
\item{type}{A string. The type of post.}
\item{timestamp}{A number. The time of the post, in seconds since the epoch.}
\item{date}{A string. The GMT date and time of the post, as a string.}
\item{format}{A string. The post format: html or markdown.}
\item{reblog_key}{A string. The key used to reblog this post.}
\item{tags}{An array (string). Tags applied to the post.}
\item{bookmarklet}{A boolean. Indicates whether the post was created via the Tumblr bookmarklet. Exists only if true.}
\item{mobile}{A boolean. Indicates whether the post was created via mobile/email publishing. Exists only if true.}
\item{source_url}{A string. The URL for the source of the content for quotes, reblogs, etc.. Exists only if there is a content source.}
\item{source_title}{A string. The title of the source site. Exists only if there is a content source.}
\item{liked}{A boolean. Indicates if a user has already liked a post or not.Exists only if the request is fully authenticated with OAuth.}
\item{state}{A string. Indicates the current state of the post. States are: published, queued, draft and private.}
\item{total_posts}{A number. The total number of post available for this request, useful for paginating through results.}
}
\references{
https://www.tumblr.com/docs/en/api/v2#common-fields
}
\author{
Andrea Capozio
}
\examples{
\dontrun{
## An example of an authenticated request using the httr package,
## where consumer_key, consumer_secret and appname are fictitious.
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


dashboard(limit = 15, offset = 3, token = token,
consumer_key = consumer_key, consumer_secret = consumer_secret)

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dashboard}