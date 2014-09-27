\name{tagged}
\alias{tagged}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get Posts with Tag.
}
\description{
Retrieve the posts with a set of tags.
}
\usage{
tagged(api_key = NA, tag = NA, before = as.integer(Sys.time()), 
limit = 20, filter = "HTML")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{api_key}{
Your OAuth Consumer Key. See Details.
}
  \item{tag}{
The tag on the posts you'd like to retrieve.
}
  \item{before}{
The timestamp of when you'd like to see posts before. Current timestamp is the default.
}
  \item{limit}{
The number of results to return: 1-20, inclusive.
}
  \item{filter}{
Specifies the post format to return, other than HTML: 
text - plain text, no HTML; 
raw - as entered by the user(no post-processing); if the user writes in Markdown, the Markdown will be returned rather than HTML.
}
}
\details{
The API uses three different levels of authentication, depending on the method.
None: No authentication. Anybody can query the method.
API key: Requires an API key. Use your OAuth Consumer Key as your api_key.
OAuth: Requires a signed request that meets the OAuth 1.0a Protocol.
}
\value{
A list object with the following fields:
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
https://www.tumblr.com/docs/en/api/v2
}
\author{
Andrea Capozio
}
\examples{
## An example of an authenticated request,
## where api_key is fictitious.
## You can obtain your own at https://www.tumblr.com/oauth/apps	
api_key <- "key"

tag <- "api"

tagged(api_key = api_key, tag = tag)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{tagged}