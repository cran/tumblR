user.likes <-
function(limit=20,offset=0,token=NA,consumer_key=NA,consumer_secret=NA){
  
  if(!is.numeric(limit) || (limit<1 || limit>20) )
    stop("limit must be a numeric type beetwen 1 and 20 (inclusive)")
  
  if(!is.numeric(offset))
    stop("offset must be a numeric type")
  
  if(class(token)[1]!="Token1.0")
    stop("token must be a Token1.0 type")
  
  if(!is.character(consumer_key))
    stop("consumer_key must be a string")
  
  if(!is.character(consumer_secret))
    stop("consumer_secret must be a string")
  
  url<-"https://api.tumblr.com/v2/user/likes"
  connection="GET"
  
  Params<-list(limit=limit,offset=offset)
  
  len<-length(Params)
  s<-NULL
  for(i in 1:len){
    if(!is.na(Params[[i]][1]))
      s<-c(s,i)
  }
  
  bodyParams<-Params[s]
  
  res<-toJSON(http.connection(url,token,bodyParams,consumer_key,consumer_secret,connection))
  
  return(res)
}
