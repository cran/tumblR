\name{post}
\alias{post}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a New Blog Post
}
\description{
This function allows to create a blog post.
}
\usage{
post(base_hostname = NA, type = "text", state = "published", tags = NA, 
tweet = NA, date = as.character(Sys.time()), format = "html", slug = NA, 
title_text = NA, body = NA, caption_photo = NA, link = NA, source_photo = NA, 
data_photo = NA, quote = NA, source_quote = NA, url_link = NA, title_link = NA, 
description = NA, title_chat = NA, conversation = NA, external_url = NA, 
data_audio = NA, caption_audio = NA, embed = NA, data_video = NA, 
caption_video = NA, token = NA, consumer_key = NA, consumer_secret = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{}{All Post types have the following parameters:}
\item{base_hostname}{
The standard or custom blog hostname. See Details.
}
  \item{type}{
The type of post to create. Specify one of the following: text, photo, quote, link, chat, audio, video. The default is setted as text.
}
  \item{state}{
The state of the post. Specify one of the following: published, draft, queue, private. The default is setted as published.
}
  \item{tags}{
Comma-separated tags for this post.
}
  \item{tweet}{
Manages the autotweet (if enabled) for this post: set to off for no tweet, or enter text to override the default tweet.
}
  \item{date}{
The GMT date and time of the post, as a string.
}
  \item{format}{
Sets the format type of post. Supported formats are: html, markdown. The default is setted as html.
}
  \item{slug}{
Add a short text summary to the end of the post URL.
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

\bold{Text Posts}
  \item{title_text}{
The optional title of the post, HTML entities must be escaped.
}
  \item{body}{
The full post body, HTML allowed.
}

\bold{Photo Posts}
  \item{caption_photo}{
The user-supplied caption, HTML allowed.
}
  \item{link}{
The "click-through URL" for the photo.
}
  \item{source_photo}{
The photo source URL. (Either source_photo or data_photo)
}
  \item{data_photo}{
One or more image files (submit multiple times to create a slide show). (Either source_photo or data_photo)
}

\bold{Quote Posts}
  \item{quote}{
The full text of the quote, HTML entities must be escaped.
}
  \item{source_quote}{
Cited source, HTML allowed.
}

\bold{Link Posts}
  \item{url_link}{
The link.
}
  \item{title_link}{
The title of the page the link points to, HTML entities should be escaped.
}
  \item{description}{
A user-supplied description, HTML allowed.
}

\bold{Chat Posts}
  \item{title_chat}{
The title of the chat.
}
  \item{conversation}{
The text of the conversation/chat, with dialogue labels (no HTML).
}

\bold{Audio Posts}
  \item{external_url}{
The URL of the site that hosts the audio file (not tumblr). (Either external_url or data_audio)
}
  \item{data_audio}{
An audio file. (Either external_url or data_audio)
}
  \item{caption_audio}{
The user-supplied caption.
}

\bold{Video Posts}
  \item{embed}{
HTML embed code for the video. (Either embed or data_video)
}
  \item{data_video}{
A video file. (Either embed or data_video)
}
  \item{caption_video}{
The user-supplied caption.
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
Returns 201: Created or an error code.
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
					
post(base_hostname = base_hostname, type = "text", tags = "tumblr, api", body = "foo", 
token = token, consumer_key = consumer_key, consumer_secret = consumer_secret)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{post}