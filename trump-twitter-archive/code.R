
# Project packages

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(dslabs)) install.packages("dslabs", repos = "http://cran.us.r-project.org")

set.seed(1)

# Data Prep

url <- 'http://www.trumptwitterarchive.com/data/realdonaldtrump/%s.json'

trump_tweets <- map(2009:2017, ~sprintf(url, .x)) %>%
  map_df(jsonlite::fromJSON, simplifyDataFrame = TRUE) %>%
  filter(!is_retweet & !str_detect(text, '^"')) %>%
  mutate(created_at = parse_date_time(created_at, orders = "a b! d! H!:M!:S! z!* Y!", tz="EST"))


data("trump_tweets")
head(trump_tweets)
names(trump_tweets)
nrow(trump_tweets)
trump_tweets[1,]


# View tweets
trump_tweets %>% select(text) %>% head

# source variable tells us the device that was used to compose and upload each tweet
trump_tweets %>% count(source) %>% arrange(desc(n))

# remove the Twitter for part of the source and filter out retweets

trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  count(source)

# We are interested in what happened during the campaign, so
# for the analysis here we will focus on what was tweeted between the day Trump announced his campaign and election day

campaign_tweets <- trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  filter(source %in% c("Android", "iPhone") & created_at >= ymd("2015-06-17") & created_at < ymd("2016-11-08")) %>%
  filter(!is_retweet) %>% arrange(created_at)

campaign_tweets

# Use data visualization to explore the possibility that two different groups were tweeting from these devices.
# For each tweet, we will extract the hour, in the east coast (EST), it was tweeted then compute the proportion of tweets tweeted at each hour for each device.
ds_theme_set()

campaign_tweets %>%
  mutate(hour = hour(with_tz(created_at, "EST"))) %>% count(source, hour) %>% group_by(source) %>%
  mutate(percent = n / sum(n)) %>% ungroup %>%
  ggplot(aes(hour, percent, color = source)) + geom_line() + geom_point() +
  scale_y_continuous(labels = percent_format()) + labs(x = "Hour of day (EST)", y = "% of tweets", color = "")

ggsave("figs/trump-campaign-tweets-grouped-twitter-client.png")

# We notice a big peak for the Android in early hours of the morning, between 6 and 8 AM.
# There seems to be a clear different in these patterns.
# We will therefore assume that two different entities are using these two devices.
# Now we will study how their tweets differ.

if(!require(tidytext)) install.packages("tidytext", repos = "http://cran.us.r-project.org")

i <- 3008

campaign_tweets$text[i]

campaign_tweets[i,] %>% 
  unnest_tokens(word, text) %>%
  select(word)

# Include mentions and hashtags
pattern <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"

campaign_tweets[i,] %>% 
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)

# Remove image links
campaign_tweets[i,] %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)

# extract the words for all our tweets.

tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern)

# What are the most commonly used words?

tweet_words %>% count(word) %>% arrange(desc(n))

# It is not surprising that these are the top words. The top words are not informative.
stop_words

# filter out rows representing stop words with filter(!word %in% stop_words$word):

tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word )

# Much more informative set of top 10 tweeted words

tweet_words %>% count(word) %>%
  top_n(10, n) %>% mutate(word = reorder(word, n)) %>% arrange(desc(n))

# Remove unwanted characters such as years (regex ^\d+$) and quotes that start with(')
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word &
           !str_detect(word, "^\\d+$")) %>%
  mutate(word = str_replace(word, "^'", ""))

# For each word we want to know if it is more likely to come from an Android tweet or an iPhone tweet using odds ratio
# We will have many proportions that are 0 so we use the 0.5 correction.
android_iphone_or <- tweet_words %>%
  count(word, source) %>% spread(source, n, fill = 0) %>%
  mutate(or = (Android + 0.5) / (sum(Android) - Android + 0.5) / 
           ( (iPhone + 0.5) / (sum(iPhone) - iPhone + 0.5)))

android_iphone_or %>% arrange(desc(or)) 

android_iphone_or %>% arrange(or)

# Given that several of these words are overall low frequency words we can impose a filter based on the total frequency
android_iphone_or %>% filter(Android+iPhone > 100) %>% arrange(desc(or))

android_iphone_or %>% filter(Android+iPhone > 100) %>% arrange(or)
