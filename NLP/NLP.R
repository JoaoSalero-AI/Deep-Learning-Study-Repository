# Text Analytics To Analyze Market Reaction To News From A Company - NLP

# Packages
library(tm)
# also try it:
# require(devtools)
# install_version("tm", version = "0.7-1", repos = "http://cran.us.r-project.org")
library(topicmodels)
library(textdata)
library(rvest)
library(tidyverse)
library(tidytext)
library(dplyr)
library(tidyr)
library(reshape2)
library(forcats)
library(scales)
library(stringr)
library(ggplot2)
library(wordcloud)
library(igraph)
library(ggraph)

# Web Scraping 

# Web Scraping from New York Times's website
nytimes_article <- read_html("https://www.nytimes.com/2021/02/01/business/gamestop-how-much-worth.html")
class(nytimes_article)
View(nytimes_article)

# Getting the article title
nytimes_article__title <- nytimes_article %>%
  html_nodes("title") %>%
  html_text()

# Title view
print(nytimes_article__title)

# Extracting article text
nytimes_article__text <- nytimes_article %>%
  html_nodes("p") %>%
  html_text()

# Text view
class(nytimes_article__text)
View(nytimes_article__text)


# Web Scraping from Yahoo Finance's website
yahoo_article <- read_html("https://finance.yahoo.com/news/gamestop-amc-reddit-investing-213609595.html")

# Getting the article title
yahoo_article_title <- yahoo_article %>%
  html_nodes("title") %>%
  html_text()

# Title view
print(yahoo_article_title)

# Extracting article text
yahoo_article_text <- yahoo_article %>%
  html_nodes("p")%>%
  html_text()

# Text view
View(yahoo_article_text)


# Web Scraping from Time Magazine's website
timemag_article <- read_html("https://time.com/5933242/gamestop-stock-gme/")

# Getting the article title
timemag_article_title <- timemag_article %>%
  html_nodes("title") %>%
  html_text()

# Title view
print(timemag_article_title)

# Extracting article text
timemag_article_text <- timemag_article %>%
  html_nodes("p") %>%
  html_text()

# Text view
View(timemag_article_text)


# Data Preparação

# New dataframe for the text of each article
df_nytimes <- data.frame(line = 1, text = nytimes_article__text, stringsAsFactors = FALSE)
class(df_nytimes)
View(df_nytimes)

df_yahoo <- data.frame(line = 1, text = yahoo_article__text, stringsAsFactors = FALSE)
View(df_yahoo)

df_timemag <- data.frame(line = 1, text = timemag_article__text, stringsAsFactors = FALSE)
View(df_timemag)


# Natural Language Processing 

# Tokenization
tokens_nytimes <- df_nytimes %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

View(tokens_nytimes)

tokens_yahoo <- df_yahoo %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

View(tokens_yahoo)

tokens_timemag <- df_timemag %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

View(tokens_timemag)


# Frequency Histogram - Checking High frequency words 
hist_nytimes <- df_nytimes %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  filter(n > 6) %>% 
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

print(hist_nytimes + ggtitle("New York Times Article Frequency Histogram"))

hist_yahoo <- df_yahoo %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  filter(n > 13) %>% 
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

print(hist_yahoo + ggtitle("Yahoo Finance Article Frequency Histogram"))

hist_timemag <- df_timemag %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  filter(n > 3) %>% 
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

print(hist_timemag + ggtitle("Time Magazine Article Frequency Histogram"))


# Removing Stop Words
nytimes_clean <- df_nytimes %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) 

yahoo_clean <- df_yahoo %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

timemag_clean <- df_timemag %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)


# Grouping dataframes

# Calculating frequency
frequency <- bind_rows(mutate(nytimes_clean, author = "NYTIMES"),
                        mutate(yahoo_clean, author = "YAHOO"),
                        mutate(timemag_clean, author = "TIME")) %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `TIME`, `NYTIMES`)

print(frequency)

# Comparing the articles plot
ggplot(frequency, aes(x = proportion, y = `YAHOO`, color = abs(`YAHOO`- proportion))) +
  geom_abline(color = "grey40", lty = 2) +
  geom_jitter(alpha = .1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels= percent_format()) +
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol=2) +
  theme(legend.position = "none") +
  labs(y = "Yahoo", x = NULL)

# Yahoo and NYTimes correlation
cor.test(data = frequency[frequency$author == "NYTIMES",], ~proportion + `YAHOO`)  

# Yahoo and Time correlation
cor.test(data = frequency[frequency$author == "TIME",], ~proportion + `YAHOO`)


# Feeling Analysis

# Feeling Lexicons (Sorry! I don't know this term in english)
afinn <- get_sentiments("afinn")
nrc <- get_sentiments("nrc")
bing <- get_sentiments("bing")

# From New York Times
# AFINN
nytimes_clean %>%
  inner_join(get_sentiments("afinn")) %>%
  summarise(sentiment = sum(value)) %>%
  mutate(method = "AFINN")

# Bing
nytimes_clean %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = T)

# NRC
sentiments_nytimes <- nytimes_clean %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = T) %>%
  ungroup()

# Plot 
sentiments_nytimes %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "New York Times Article Feelings", x = NULL) +
  coord_flip()

# Words most associated with positive feelings
sentiments_nytimes %>%
  group_by(sentiment) %>%
  filter(sentiment == "positive") %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) + ggtitle("Words most associated with positive feelings") +
  geom_col(show.legend = FALSE, fill = "palegreen2") +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Frequency", x = NULL) +
  coord_flip()

# Words most associated with negative feelings
sentimentos_nytimes %>%
  group_by(sentiment) %>%
  filter(sentiment == "negative") %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) + ggtitle("Words most associated with negative feelings") +
  geom_col(show.legend = FALSE, fill = "red3") +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Frequência", x = NULL) +
  coord_flip()


# Yahoo Finance
# AFINN
yahoo_clean %>%
  inner_join(get_sentiments("afinn")) %>%
  summarise(sentiment = sum(value)) %>%
  mutate(method = "AFINN")

# Bing
yahoo_clean %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = T)

# NRC
sentiments_yahoo <- yahoo_clean %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = T) %>%
  ungroup()

# Plot
sentiments_yahoo %>%
  group_by(sentiment) %>%
  top_n(8) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Yahoo Finance Feelings", x = NULL) +
  coord_flip()

# Words most associated with positive feelings
sentiments_yahoo %>%
  group_by(sentiment) %>%
  filter(sentiment == "positive") %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) + ggtitle("Words most associated with positive feelings")+
  geom_col(show.legend = FALSE, fill = "palegreen2") +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Frequency", x = NULL) +
  coord_flip()

# Words most associated with negative feelings
sentiments_yahoo %>%
  group_by(sentiment) %>%
  filter(sentiment == "negative") %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) + ggtitle("Words most associated with negative feelings") +
  geom_col(show.legend = FALSE, fill = "red3") +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Frequency", x = NULL) +
  coord_flip()


# Time Magazine
# AFINN
timemag_clean %>%
  inner_join(get_sentiments("afinn")) %>%
  summarise(sentiment = sum(value)) %>%
  mutate(method = "AFINN")

# Bing
timemag_clean %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = T)

# NRC
sentiments_timemag <- timemag_clean %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = T) %>%
  ungroup()

# Plot
sentiments_timemag %>%
  group_by(sentiment) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Time Magazine Feelings", x = NULL) +
  coord_flip()

# Words most associated with positive feelings
sentiments_timemag %>%
  group_by(sentiment) %>%
  filter(sentiment == "positive") %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) + ggtitle("Words most associated with positive feelings")+
  geom_col(show.legend = FALSE, fill = "palegreen2") +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Frequência", x = NULL) +
  coord_flip()

# Words most associated with negative feelings
sentiments_timemag %>%
  group_by(sentiment) %>%
  filter(sentiment == "negative")%>%
  top_n(5) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) + ggtitle("Words most associated with negative feelings") +
  geom_col(show.legend = FALSE, fill = "red3") +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Frequência", x = NULL) +
  coord_flip()


# TF-IDF - Finding the Most Common Words in Text Using

# Compiling the articles
df_comp <- bind_rows(mutate(df_nytimes, author = "Yahoo"),
                          mutate(df_yahoo, author = "TIME"),
                          mutate(df_timemag, author = "NYtimes"))

# Stop words removal and tokenization
df_comp_clean <- df_comp %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(author, word, sort = TRUE) %>%
  ungroup()

# Total Words
total_words <- df_comp_clean %>%
  group_by(author) %>%
  summarize(total = sum(n))

# Left join
df_comp_clean <- left_join(df_comp_clean, total_words)

# TF-IDF
df_tf_idf <- df_comp_clean %>%
  bind_tf_idf(word, author, n)

View(df_tf_idf)

# TF-IDF - Most Common Words Plot
df_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(author) %>%
  top_n(10) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = author)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~author, ncol = 2, scales = "free") +
  labs(x = "Most Frequency Words", y = NULL)


# Word Clouds

# Stop words removal and tokenization
df_comp_2 <- df_comp %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(author, word, sort = TRUE)

# NRC Plot
df_comp_2 %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"), max.words = 200)

# Plot with Bing
df_comp_2 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"), max.words = 200)


# Topic Modeling
# Document Term Matrix 
doc_term_matrix <- df_combi %>%
  unnest_tokens(word, text) %>%
  count(author, word) %>%
  cast_dtm(author, word, n)

print(doc_term_matrix)

dtm_articles <- df_combi_clean %>%
  cast_dtm(author, word, n)

# LDA (Latent Dirichlet Allocation)
model_lda <- LDA(dtm_articles, k = 3, control = list(seed = 123))
model_lda

# Topics Dataframe 
df_topics <- tidy(model_lda, matrix = "beta")
print(df_topics)

# Top terms
top_terms <- df_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Topics - Top Terms
top_terms %>%
  mutate(term = reorder(term, beta)) %>% 
  ggplot(aes(term, beta, fill = factor(topic))) + 
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") + 
  coord_flip()


# Conclusão: Há uma concisa neutralidade no tom adotado pelos vaículos de imprensa.  







