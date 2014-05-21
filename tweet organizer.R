EnsurePackage<-function(x)
{
  x<-as.character(x)
  if(!require(x,character.only=TRUE))
  {
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

PrepareTwitter<-function()
{
  EnsurePackage("bitops")
  EnsurePackage("RCurl")
  EnsurePackage("RJSONIO")
  EnsurePackage("twitteR")
}

TweetFrame<-function(searchTerm,maxTweets)
{
  twtList<-searchTwitter(searchTerm,n=maxTweets) 
  return(do.call("rbind",lapply(twtList,as.data.frame)))
}


CleanTweets<-function(tweets)
{
  require(stringr)
  tweets<-str_replace_all(tweets,"  "," ")
  tweets<-str_replace_all(tweets, "http://t.co/[a-z,A-Z,0-9]{8}","")
  tweets<-str_replace(tweets,"RT @[a-z,A-Z]*: ","")
  tweets<-str_replace_all(tweets,"#[a-z,A-Z]*","")
  tweets<-str_replace_all(tweets,"@[a-z,A-Z]*","")
  return(tweets)
}


pr1<-function(searchTerm)
{
  PrepareTwitter()
  require(wordcloud)
  require(tm)
  require(stringr)
  
  results<-TweetFrame(searchTerm,100)
  cleanText<-CleanTweets(results$text)
  tweetCorpus<-Corpus(VectorSource(cleanText))
  tweetCorpus<-tm_map(tweetCorpus, tolower)
  tweetCorpus<-tm_map(tweetCorpus, removePunctuation)
  tweetCorpus<-tm_map(tweetCorpus, removeWords, stopwords('english'))
  tweetTDM<-TermDocumentMatrix(tweetCorpus)
  tdMatrix<-as.matrix(tweetTDM)
  sortedMatrix<-sort(rowSums(tdMatrix),decreasing=TRUE)
  cloudFrame<-data.frame(word=names(sortedMatrix),freq=sortedMatrix)
  wordcloud(cloudFrame$word,cloudFrame$freq,scale=c(3,2),random.color=TRUE,rot.per=0.5,colors="black",use.r.layout=FALSE)
}
