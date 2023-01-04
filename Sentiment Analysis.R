#sentiment analsis
library(twitteR)
library(dplyr)
library(purrr)
library(plyr)
library(stringr)
library(tibble)
install.packages(c('RCurl','ROAuth'))
require('RCurl')
require('ROAuth')
score_value<-function(sentences,pos.words,neg.words,.progress='none')
{
  require(stringr)
  require(plyr)
  scores<-laply(sentences,function(sentence,pos.words,neg.words){
    sentence<-gsub('[[:punct:]]',"",sentence)
    sentence<-gsub('[[:cntrl:]]',"",sentence)
    sentence<-gsub('\\d+',"",sentence)
    sentence<-tolower(sentence)
    word.list<-str_split(sentence,'\\s+')
    word<-unlist(word.list)
    pos.matches<-match(word,pos.words)
    neg.matches<-match(word,neg.words)
    pos.matches<-!is.na(pos.matches)
    neg.matches<-!is.na(neg.matches)
    score<-sum(pos.matches) - sum(neg.matches)
    return(score)
  },pos.words,neg.words,.progress=.progress)
    scores_df<-data.frame(score=scores,text=sentences)
  return(scores_df)
}
pos.words<-scan("C:/Users/Vikra/OneDrive/Documents/Positive.txt",what = 'character',comment.char = ';')
neg.words<-scan("C:/Users/Vikra/OneDrive/Documents/Negative.txt",what = 'character',comment.char = ';')
View(neg.words)
getwd()




mykey<-"....................."
reqURL<-"AAAAAAAAAAAAAAAAAAAAAGKVdQEAAAAAW92ljPj%2BrXNYQzlEtUeXdz%2FmUS8%3D8xACziLljJjdiXZTNQzUNVz7cPYZDaCRV7Ew6ABOMmaCtgNVSG"
accessURL<-"AAAAAAAAAAAAAAAAAAAAAGKVdQEAAAAAW92ljPj%2BrXNYQzlEtUeXdz%2FmUS8%3D8xACziLljJjdiXZTNQzUNVz7cPYZDaCRV7Ew6ABOMmaCtgNVSG"
authURL<-"AAAAAAAAAAAAAAAAAAAAAGKVdQEAAAAAW92ljPj%2BrXNYQzlEtUeXdz%2FmUS8%3D8xACziLljJjdiXZTNQzUNVz7cPYZDaCRV7Ew6ABOMmaCtgNVSG"
consumerSecret<-"......................."
accessToken<-"1533399351112122368-ghZ4lk8aaz2WD7CyOCfoBNxIXcWdy4"
accessTokensecret<-"....................."
twitcred<-OAuthFactory$new(consumerKey=mykey,consumerSecret=consumerSecret,
                           requestURL=reqURL,accessURL=accessURL,authURL=authURL)
twitcred$handshake
setup_twitter_oauth(consumerkey,consumerSecret,accessToken,accessTokensecret)
tweet1<-userTimeline("@barcalona",n=100)
tweet2<-userTimeline("@realmadriden",n=100)
tweet_df<-tbl_df(map_df(tweet1,as.data.frame))
tweet2_df<-tbl_df(map_df(tweet2,as.data.frame))
tweet2_df$text
#main
bsscore<-score_value(tweet_df$text,pos.words,neg.words,.progress ="text" )
realscore<-score_value(tweet2_df$text,pos.words,neg.words,.progress = 'text')
hist(bsscore$score)
hist(realscore$score)


file.rename("Untitled5","Sentiment Analysis on Barcalona vs realmadriden")
