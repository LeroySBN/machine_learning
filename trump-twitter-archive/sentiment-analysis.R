# We already see somewhat of a pattern in the types of words that are being tweeted more in one device versus the other
# we are not interested in specific words but rather in the tone.
# Vaziri's assertion is that the Android tweets are more hyperbolic
# So how can we check this with data? Hyperbolic is a hard sentiment to extract from words as it relies on interpreting phrases.
# However, words can be associated to more basic sentiment such as as anger, fear, joy and surprise.
# In this section we demonstrate basic sentiment analysis.

# IMPORTANT: The sentiments object used in this section has changed in a recent tidytext update and this code does not work - we are working on a replacement.
# You can explore a similar analysis with the bing or afinn lexicon instead of the nrc lexicon.
# In the meantime, please to proceed to the exercises to try some sentiment analysis with a lexicon that is still available.

# In sentiment analysis we assign a word to one or more "sentiment". Although this approach will miss context dependent sentiments, such as sarcasm, when performed on large numbers of words, summaries can provide insights.

# The first step in sentiment analysis is to assign a sentiment to each word
# The tidytext package includes several maps or lexicons in the object sentiments:
install.packages("Rtools")
install.packages("textdata")
install.packages("RSentiment")
library(textdata)
library(RSentiment)

table(sentiments$lexicon)

# The bing lexicon divides words into positive and negative.
# We can see this using the tidytext function get_sentiments:
get_sentiments("bing")

# The AFINN lexicon assigns a score between -5 and 5, with -5 the most negative and 5 the most positive.
get_sentiments("afinn")

# The loughran and nrc lexicons provide several different sentiments:
get_sentiments("loughran") %>% count(sentiment)
get_sentiments("nrc") %>% count(sentiment)

# For the analysis here we are interested in exploring the different sentiments of each tweet, so we will use the nrc lexicon:

nrc <- sentiments %>% filter(lexicon == "nrc") %>% select(word, sentiment)
nrc

# Combine the words and sentiments using inner_join, which will only keep words associated with a sentiment. 
# Here are 10 random words extracted from the tweets

tweet_words %>% inner_join(nrc, by = "word") %>% 
  select(source, word, sentiment) %>% sample_n(10)

# Quantitative analysis comparing Android and iPhone by comparing the sentiments of the tweets posted from each device.
# We could perform a tweet by tweet analysis, assigning a sentiment to each tweet
# However, this somewhat complex since each tweet will have several sentiments attached to it, one for each word appearing in the lexicon
# For illustrative purposes, we will perform a much simpler analysis: we will count and compare the frequencies of each sentiment appears for each device.

sentiment_counts <- tweet_words %>%
  left_join(nrc, by = "word") %>%
  count(source, sentiment) %>%
  spread(source, n) %>%
  mutate(sentiment = replace_na(sentiment, replace = "none"))

sentiment_counts

# Because more words were used on the Android than on the phone:

tweet_words %>% group_by(source) %>% summarize(n = n())

# For each sentiment we can compute the odds of being in the device: proportion of words with sentiment versus proportion of words without and then compute the odds ratio comparing the two devices:

sentiment_counts %>%
  mutate(Android = Android / (sum(Android) - Android) , 
         iPhone = iPhone / (sum(iPhone) - iPhone), 
         or = Android/iPhone) %>%
  arrange(desc(or))

# So we do see some difference and the order is interesting: the largest three sentiments are disgust, anger, and negative! But are they statistically significant? How does this compare if we are just assigning sentiments at random?

# To answer that question we can compute, for each sentiment, an odds ratio and confidence interval. We will add the two values we need to form a two-by-two table and the odds ratio:

library(broom)
log_or <- sentiment_counts %>%
  mutate( log_or = log( (Android / (sum(Android) - Android)) / (iPhone / (sum(iPhone) - iPhone))),
          se = sqrt( 1/Android + 1/(sum(Android) - Android) + 1/iPhone + 1/(sum(iPhone) - iPhone)),
          conf.low = log_or - qnorm(0.975)*se,
          conf.high = log_or + qnorm(0.975)*se) %>%
  arrange(desc(log_or))

log_or

# A graphical visualization shows some sentiments that are clearly overrepresented:

log_or %>%
  mutate(sentiment = reorder(sentiment, log_or)) %>%
  ggplot(aes(x = sentiment, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point(aes(sentiment, log_or)) +
  ylab("Log odds ratio for association between Android and sentiment") +
  coord_flip() 
ggsave("figs/sentiments-per-device-ratio.png")

# We see that the disgust, anger, negative sadness and fear sentiments are associated with the Android in a way that is hard to explain by chance alone
# Words not associated to a sentiment were strongly associated with the iPhone source, which is in agreement with the original claim about hyperbolic tweets.

# If we are interested in exploring which specific words are driving these differences, we can back to our android_iphone_or object:

android_iphone_or %>% inner_join(nrc) %>%
  filter(sentiment == "disgust" & Android + iPhone > 10) %>%
  arrange(desc(or))

# Make a graph

android_iphone_or %>% inner_join(nrc, by = "word") %>%
  mutate(sentiment = factor(sentiment, levels = log_or$sentiment)) %>%
  mutate(log_or = log(or)) %>%
  filter(Android + iPhone > 10 & abs(log_or)>1) %>%
  mutate(word = reorder(word, log_or)) %>%
  ggplot(aes(word, log_or, fill = log_or < 0)) +
  facet_wrap(~sentiment, scales = "free_x", nrow = 2) + 
  geom_bar(stat="identity", show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("figs/sentiments-per-device-ratio-2.png")






