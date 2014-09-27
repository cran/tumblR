\name{posts.submission}
\alias{posts.submission}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve Submission Posts.
}
\description{
This function retrieves submission posts.
}
\usage{
posts.submission(base_hostname = NA, offset = 0, filter = "HTML", token = NA, 
consumer_key = NA, consumer_secret = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{base_hostname}{
The standard or custom blog hostname. See Details.
}
  \item{offset}{
Result to start at. 0 is the first follower.
}
 \item{filter}{
Specifies the post format to return, other than HTML:
text - Plain text, no HTML;
raw - As entered by the user (no post-processing); if the user writes in Markdown, the Markdown will be returned rather than HTML. 
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
All post types have the following common response.
\item{blog_name}{A string. The short name used to uniquely identify a blog.}
\item{id}{A number. The post's unique ID.}
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

For a specified type, other response fields are returned. See References for more details.

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
base_hostname <- base_hostname					
					
posts.submission(base_hostname = base_hostname, filter = "HTML", token = token, 
consumer_key = consumer_key, consumer_secret = consumer_secret)
}				
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{posts}
\keyword{submission}