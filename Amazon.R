#Load packages
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(vioplot)
library(tm)
library(SnowballC)
library(stringr)

#Import dataset
amazon_original <- read.csv("Reviews2.csv", stringsAsFactors = F)

#Review data, confirmed no missing values present
summary(amazon_original)

#Create new rating_category variable
amazon_categories <- amazon_original %>% mutate(rating_category = case_when(
  .$Score == 5 ~ "Positive",
  .$Score == 4 ~ "Positive",
  .$Score == 3 ~ "Neutral",
  .$Score == 2 ~ "Negative",
  .$Score == 1 ~ "Negative"))

write.csv(amazon_categories, file = "amazon_clean.csv")

#Create a new column which contains count of characters from Text column
amazon_categories <- mutate(amazon_categories, TextChar = nchar(Text))

#histogram showing distribution of Score across all reviews
ggplot(amazon_categories, aes(x = Score)) +
  geom_histogram()

#Boxplot showing distribtion of # of characters by Score
boxplot(TextChar~Score, data = amazon_categories)



#Create a corpus for summary and another for text
corpus_summary = Corpus(VectorSource(amazon_categories$Summary))
corpus_text = Corpus(VectorSource(amazon_categories$Text))

#Preprocessing: Remove stopwords, and stem.
corpus_summary = tm_map(corpus_summary, removeWords, stopwords("english"))
corpus_text = tm_map(corpus_text, removeWords, stopwords("english"))
corpus_summary = tm_map(corpus_summary, stemDocument)
corpus_text = tm_map(corpus_text, stemDocument)

#Build document term matrices, remove sparse terms
dtm_summary = DocumentTermMatrix(corpus_summary)
dtm_summary = removeSparseTerms(dtm_summary, 0.99)

dtm_text = DocumentTermMatrix(corpus_text)
dtm_text = removeSparseTerms(dtm_text, 0.97)

#Create dataframe containing frequencies and append the dependent variable from original data set
Terms_summary = as.data.frame(as.matrix(dtm_summary))
Terms_text = as.data.frame(as.matrix(dtm_text))

Terms_summary$Score = amazon_categories$Score

#combine summary with text
combined = c(Terms_summary, Terms_text)

#Adding features

combined$Exclamation_summary = str_count(amazon_categories$Summary, pattern = "!") #count of exclamation marks
combined$CAPS_summary = str_count(amazon_categories$Summary, "\\b[A-Z]{2,}\\b") #count of words in CAPS
combined$summary_total = sapply(gregexpr("[[:alpha:]]+", amazon_categories$Summary), function(x) sum(x > 0)) #total # of words

combined$Exlamation_summary_proportion = combined$Exclamation_summary / combined$summary_total #proportion of exclamation points to total # of words
combined$CAPS_summary_proportion = combined$CAPS_summary / combined$summary_total #proportion of CAPS words to total # of words


combined$Exclamation_text = str_count(amazon_categories$Text, pattern = "!") #count of exclamation marks
combined$CAPS_text = str_count(amazon_categories$Text, "\\b[A-Z]{2,}\\b") #count of words in CAPS
combined$text_total = sapply(gregexpr("[[:alpha:]]+", amazon_categories$Text), function(x) sum(x > 0)) #total # of words

combined$Exlamation_text_proportion = combined$Exclamation_text / combined$text_total #proportion of exclamation points to total # of words
combined$CAPS_text_proportion = combined$CAPS_text / combined$summary_total #proportion of CAPS words to total # of words

