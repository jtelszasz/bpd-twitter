
library(tidyverse)
library(stringr)
library(rtweet)

source("R/config.R")

# Set API Keys

#setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
twitter_token <- create_token(app_name, api_key, api_secret)

# Get Tweets about BPD
tweets <- search_tweets('RecruitmentBPD', n=5000)

recruitmentBPD <- get_timeline(user = "RecruitmentBPD", n = 5000)

tweets.df <- twListToDF(tweets)

# what dates does this cover?
min(tweets.df$created)
max(tweets.df$created)

# how many are not retweets?
tweets.df %>% filter(isRetweet == F) %>% nrow

feed <- lapply(tweets, function(t) t$getText())

write_csv(tweets.df, "tweets.csv")

# Read in dictionary of positive and negative works
good.words <- scan('opinion-lexicon-English/positive-words.txt',
           what='character', comment.char=';')

bad.words <- scan('opinion-lexicon-English/negative-words.txt',
           what='character', comment.char=';')

score.sentiment <- function(sentences, good_text, bad_text, .progress='none')
{
        #https://www.r-bloggers.com/how-to-use-r-to-scrape-tweets-super-tuesday-2016/
        
        require(plyr)
        # we got a vector of sentences. plyr will handle a list
        # or a vector as an "l" for us
        # we want a simple array of scores back, so we use
        # "l" + "a" + "ply" = "laply":
        scores <- laply(sentences, function(sentence, good_text, bad_text) {
                
                # clean up sentences with R's regex-driven global substitute, gsub():
                sentence <- gsub('[[:punct:]]', '', sentence)
                sentence <- gsub('[[:cntrl:]]', '', sentence)
                sentence <- gsub('\\d+', '', sentence)
                
                #to remove emojis
                sentence <- iconv(sentence, 'UTF-8', 'ASCII')
                sentence <- tolower(sentence)
                
                # split into words. str_split is in the stringr package
                word.list <- str_split(sentence, '\\s+')
                # sometimes a list() is one level of hierarchy too much
                words <- unlist(word.list)
                
                # compare our words to the dictionaries of positive & negative terms
                pos.matches <- match(words, good_text)
                neg.matches <- match(words, bad_text)
                
                # match() returns the position of the matched term or NA
                # we just want a TRUE/FALSE:
                pos.matches <- !is.na(pos.matches)
                neg.matches <- !is.na(neg.matches)
                
                # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
                score <- sum(pos.matches) - sum(neg.matches)
                
                return(score)
        }, good_text, bad_text, .progress=.progress )
        
        scores.df = data.frame(score=scores, text=sentences)
        return(scores.df)
}

bpd <- score.sentiment(tweets.list.unique, good.words, bad.words, .progress='text') 

anno.y <- 1800
bpd.score.hist <- ggplot(bpd, aes(score)) +
        geom_histogram() +
        xlim(c(-4, 4)) +
        ggtitle("Sentiment Analysis of Tweets about Baltimore Police") +
        xlab("Sentiment Score") +
        theme_minimal() +
        annotate("text", x = 0.25, y = anno.y, label = "Positive", hjust = 0) +
        annotate("text", x = -0.25, y = anno.y, label = "Negative", hjust = 1) +
        annotate("segment", x = 1.5, xend = 2, y = anno.y, yend = anno.y, 
                 colour = "black", size = .5, arrow=arrow()) +
        annotate("segment", x = -1.5, xend = -2, y = anno.y, yend = anno.y, 
                 colour = "black", size = .5, arrow=arrow()) +
        ylim(c(0,2000))

bpd.score.hist


ggsave("bpd_score_histogram.png",
       bpd.score.hist,
       device = "png",
       width = 6,
       height = 4,
       units = "in",
       dpi = 200)


