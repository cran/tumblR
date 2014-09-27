\name{posts}
\alias{posts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve Published Posts.
}
\description{
This function retrieves published posts.
}
\usage{
posts(base_hostname = NA, limit = 20, offset = 0, api_key = NA, type = NA, 
id = NA, tag = NA, reblog_info = FALSE, notes_info = FALSE, filter = "HTML")
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
  \item{api_key}{
The consumer secret provided by your application. See Details.
}
  \item{type}{
The type of post to return. The available values are: text, photo, quote, link, chat, audio, video, answer. If no values are specified, all types are returned.
}
  \item{id}{
A specific post ID. Returns the single post specified or (if not found) a 404 error.
}
  \item{tag}{
Limits the response to posts with the specified tag.
}
 \item{reblog_info}{
Indicates whether to return reblog information (specify TRUE or FALSE). Returns the various reblogged_fields. See Details.
}
  \item{notes_info}{
Indicates whether to return notes information (specify TRUE or FALSE). Returns note count and note metadata. See Details.
}

  \item{filter}{
Specifies the post format to return, other than HTML: 
text - Plain text, no HTML;
raw - As entered by the user (no post-processing); if the user writes in Markdown, the Markdown will be returned rather than HTML. 
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

reblog_info and notes_info are false by default because of the server impact involved in retrieving the data.
}
\value{
Each response includes a blog object that is equivalent of an info.blog response. Posts are returned as an array attached to the posts field.
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
## An example of an authenticated request,
## where api_key is fictitious.
## You can obtain your own at https://www.tumblr.com/oauth/apps
api_key <- "key"

## you must specify a real blog for base_hostname
base_hostname <- "blogname.tumblr.com"

posts(base_hostname = base_hostname, type = "text", api_key = api_key)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{posts}