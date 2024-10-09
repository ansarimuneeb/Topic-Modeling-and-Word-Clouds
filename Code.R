#Preparing Data

reviews <- read.csv(file.choose())

summary(reviews)
str(reviews)
sum(is.na(reviews))

reviews_cleaned <- na.omit(reviews)
sum(is.na(reviews_cleaned))

library(sqldf)
average_score <- sqldf("SELECT ProductId, AVG(Score) AS AverageScore FROM reviews_cleaned GROUP BY ProductId ORDER BY AverageScore DESC")

most_reviews <- sqldf("SELECT ProductId, COUNT(*) AS RowCount
                      FROM reviews_cleaned
                      GROUP BY ProductId
                      ORDER BY RowCount DESC")

dog_treat <- sqldf("SELECT * FROM reviews_cleaned WHERE ProductId = 'B002QWP89S'")
dog_treat_avg_score <- sqldf("SELECT AVG(Score) FROM reviews_cleaned WHERE ProductId = 'B002QWP89S'")

write.csv(dog_treat, file = "dog_treat.csv", row.names = FALSE)

#Text Analysis

library(dplyr)
library(tidytext)

custom_stop_words <- tribble(
  ~word, ~lexicon,
  "dog", "CUSTOM",
  "dogs", "CUSTOM",
  "dog's", "CUSTOM",
  "greenies", "CUSTOM",
  "greenie", "CUSTOM",
  "br", "CUSTOM",
  "product", "CUSTOM",
  "treats", "CUSTOM",
  "treat", "CUSTOM",
  "amazon", "CUSTOM",
  "giving", "CUSTOM",
  "teenie", "CUSTOM",
  "teenie", "CUSTOM",
  "absolutely", "CUSTOM",
  "lot", "CUSTOM",
  "started", "CUSTOM",
  "doggie", "CUSTOM",
  "arrived", "CUSTOM",
  "makes", "CUSTOM",
  "job", "CUSTOM",
  "em", "CUSTOM",
  "chews", "CUSTOM"
)

stop_words2 <- stop_words %>%
  bind_rows(custom_stop_words)

tidy_review <- dog_treat %>%
  unnest_tokens(word,Text) %>%
  anti_join(stop_words2)

tidy_review %>%
  count(word) %>%
  arrange(desc(n))

#Visualization

library(rlang)
library(ggplot2)
library(forcats)

word_counts <- tidy_review %>%
  count(word) %>%
  filter(n > 30) %>%
  mutate(word2 = fct_reorder(word,n))

ggplot(
  word_counts, aes(x = word2, y = n)
) +
  geom_col(fill = "purple") +
  coord_flip() +
  ggtitle("Dog Treat Word Counts") +
  theme(plot.title = element_text(hjust = 0.4)) +
  labs(
    x = "Word",
    y = "Count"
  )

#Wordcloud

library(wordcloud)
wordcloud(
  words = word_counts$word,
  freq = word_counts$n,
  max.words = 24
)

#Sentiment Analysis

nrc <- read.csv(file.choose())


combined_df <- inner_join(tidy_review, nrc, by = "word")

combined_df_cleaned <- combined_df %>%
  select(word, sentiment)


count_of_sentiments <- sqldf("SELECT sentiment, COUNT(*) as count_of_sentiments
                             FROM combined_df_cleaned
                             GROUP BY sentiment
                             ORDER BY count_of_sentiments DESC")

count_of_sentiments$sentiment <- reorder(count_of_sentiments$sentiment, count_of_sentiments$count_of_sentiments)

ggplot(count_of_sentiments, aes(x = sentiment, y = count_of_sentiments, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Sentiment Word Counts",
    x = "Sentiments",
    y = "Counts"
  ) + 
  theme(plot.title = element_text(hjust = 0.4))
#_________________________________________________________________

combined_df_cleaned %>%
  count(sentiment) %>%
  arrange(desc(n))

combined_df_cleaned_2 <- combined_df_cleaned %>%
  filter(sentiment %in% c("positive", "negative"))

word_counts_2 <- combined_df_cleaned_2 %>%
  count(word, sentiment) %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>%
  ungroup() %>%
  mutate(
    word2 = fct_reorder(word, n)
  )
ggplot(word_counts_2, aes(x = word2, y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free") +
  coord_flip() +
  labs(
    title = "Positive and Negative",
    x = "Words",
    y = "Count"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

#___________________________________________________________

combined_df_cleaned %>%
  count(sentiment) %>%
  arrange(desc(n))

combined_df_cleaned_3 <- combined_df_cleaned %>%
  filter(sentiment %in% c("joy", "trust"))

word_counts_3 <- combined_df_cleaned_3 %>%
  count(word, sentiment) %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>%
  ungroup() %>%
  mutate(
    word2 = fct_reorder(word, n)
  )
ggplot(word_counts_3, aes(x = word2, y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free") +
  coord_flip() +
  labs(
    title = "Joy and Trust",
    x = "Words",
    y = "Count"
  ) +
  theme(plot.title = element_text(hjust = 0.5))
#________________________________________________________________________

combined_df_cleaned %>%
  count(sentiment) %>%
  arrange(desc(n))

combined_df_cleaned_4 <- combined_df_cleaned %>%
  filter(sentiment %in% c("anticipation", "sadness"))

word_counts_4 <- combined_df_cleaned_4 %>%
  count(word, sentiment) %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>%
  ungroup() %>%
  mutate(
    word2 = fct_reorder(word, n)
  )
ggplot(word_counts_4, aes(x = word2, y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free") +
  coord_flip() +
  labs(
    title = "Anticipation and Sadness",
    x = "Words",
    y = "Count"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

#___________________________________________________________________________

combined_df_cleaned %>%
  count(sentiment) %>%
  arrange(desc(n))

combined_df_cleaned_5 <- combined_df_cleaned %>%
  filter(sentiment %in% c("fear", "anger"))

word_counts_5 <- combined_df_cleaned_5 %>%
  count(word, sentiment) %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>%
  ungroup() %>%
  mutate(
    word2 = fct_reorder(word, n)
  )
ggplot(word_counts_5, aes(x = word2, y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free") +
  coord_flip() +
  labs(
    title = "Fear and Anger",
    x = "Words",
    y = "Count"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

#____________________________________________________________

combined_df_cleaned %>%
  count(sentiment) %>%
  arrange(desc(n))

combined_df_cleaned_6 <- combined_df_cleaned %>%
  filter(sentiment %in% c("surprise", "disgust"))

word_counts_6 <- combined_df_cleaned_6 %>%
  count(word, sentiment) %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>%
  ungroup() %>%
  mutate(
    word2 = fct_reorder(word, n)
  )
ggplot(word_counts_6, aes(x = word2, y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free") +
  coord_flip() +
  labs(
    title = "Disgust and Surprise",
    x = "Words",
    y = "Count"
  ) +
  theme(plot.title = element_text(hjust = 0.5))


#Topic Modeling

library(topicmodels)

dtm_review <- tidy_review %>% 
  count(word, UserId) %>% 
  cast_dtm(UserId, word, n)

lda_topics <- LDA(
  dtm_review,
  k = 2,
  method = "Gibbs",
  control = list(seed = 42)
) %>%
  tidy(matrix = "beta")
word_probs <- lda_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 15) %>%
  ungroup () %>%
  mutate(term2 = fct_reorder(term, beta))

ggplot(
  word_probs,
  aes(
    term2,
    beta,
    fill = as.factor(topic)
  )
) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  labs(title = "Topic Model", x = "Words", y = "Probability") +
  theme(plot.title = element_text(hjust = 0.5))
#_______________________________________________________________________
# Define a vector of colors for each topic
topic_colors <- c("#1f78b4", "#33a02c")  # You can replace these colors with your desired ones

# Plot with custom colors
ggplot(
  word_probs,
  aes(
    term2,
    beta,
    fill = as.factor(topic)
  )
) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = topic_colors) +  # Set the colors using scale_fill_manual
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  labs(title = "Topic Model", x = "Words", y = "Probability") +
  theme(plot.title = element_text(hjust = 0.5))