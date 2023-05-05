
title: "Soc492_Final_Project"
author: "Lucas Rubim"
format: html
---------------------------
  
  
  # install.packages("tidyverse")
  # install.packages("tidytext")
  # install.packages("widyr")
  # install.packages("igraph")
  # install.packages("ggraph")
  # install.packages("dplyr")
  # install.packages("readr")
  # install.packages("ggplot")
  # install.packages("corpus")
# install.packages("lsa") 
# install.packages("tm")
# install.packages("Hmisc")

library(usethis)
library(corrplot)
library(Hmisc)
library(tidytext)
library(widyr)
library(igraph)
library(ggraph)
# library(dplyr)
library(scales)
# library(ggplot2)
library(corpus)
library(lsa)
library(tm)
library(tidyverse)

#Read data
murray <- read_csv("Murray.txt")
unabomber <- read_csv("Unabomber.txt")
frank <- read_csv("Frank.txt")
ellul <- read_csv("Ellul.txt")

#Rename column name for both text datas
murraydata <- murray %>% rename("text" = "A. MURRAY HENRY")
unabomberdata <- unabomber %>% rename("text" = "INDUSTRIAL SOCIETY AND ITS FUTURE")
frankdata <- frank %>% rename("text" = "EXPERIMENTAL STUDIES OF PERSONAL PRESSURE AND")
elluldata <- ellul %>% rename("text" = "THE ARTIST IN THE TECHNOLOGICAL SOCIETY")
# Splitting into thirds due to long text and reinforce other conclusions
ted_1st3rd <- as_tibble(unabomberdata$text[1:323]) %>% rename("text" = "value")
ted_2st3rd <- as_tibble(unabomberdata$text[324:647]) %>% rename("text" = "value")
ted_3st3rd <- as_tibble(unabomberdata$text[648:971]) %>% rename("text" = "value")

###Merge both text file data into one data frame
texts <- bind_rows(unabomberdata %>%
                     mutate(author = "Ted"),
                   ted_1st3rd %>%
                     mutate(author = "ted_1.3rd"),
                   ted_2st3rd %>%
                     mutate(author = "ted_2.3rd"),
                   ted_3st3rd %>%
                     mutate(author = "ted_3.3rd"),
                   murraydata %>%
                     mutate(author = "Murray"),
                   frankdata %>%
                     mutate(author = "Frank"),
                   elluldata %>%
                     mutate(author = "Ellul"),
)

# Removing stop words
author_words <- texts %>% 
  unnest_tokens(output = word, input = text) %>%
  mutate(word1 = tolower(word)) %>% 
  anti_join(stop_words, by = "word") %>%
  filter(str_detect(word, "[:alpha:]"))

# Getting total counts of terms for each document
length(author_words$author[author_words$author == "Ted"]) # <-- TOTAL 13152 Terms
length(author_words$author[author_words$author == "ted_1.3rd"]) # <-- TOTAL 1461 Terms
length(author_words$author[author_words$author == "ted_2.3rd"]) # <-- TOTAL 1418 Terms
length(author_words$author[author_words$author == "ted_3.3rd"]) # <-- TOTAL 1389 Terms
length(author_words$author[author_words$author == "Murray"]) # <-- TOTAL 3901 Terms
length(author_words$author[author_words$author == "Frank"]) # <-- TOTAL 2809 Terms
length(author_words$author[author_words$author == "Ellul"]) # <-- TOTAL 2408 Terms

### Removing suffixes from the terms
author_words <- author_words %>% rename("text" = "word1")
tokenOutputList <- text_tokens(author_words, stemmer = 'en')
author_words_stemmed <- unlist(tokenOutputList)
author_words$text <- author_words_stemmed

## OPTION TO SEPERATE TWO SETS:
author_words_tokened <- author_words
author_words_tokened$text <- author_words_stemmed
author_words_tokened %>% arrange(word)

author_words <- author_words %>% rename("word1" = "text")
author_words_tokened <- author_words_tokened %>% rename("word1" = "text")

# Frequency analysis
frequency_1 <- author_words_tokened %>% 
  count(author, word, sort = TRUE) %>% 
  left_join(author_words %>% 
              count(author, name = "total")) %>%
  mutate(freq = n/total) %>% 
  arrange(desc(n))
# Create columns for each individual document (make table shorter)
frequency_2 <- frequency_1 %>% 
  select(author, word, freq, n) %>% 
  pivot_wider(names_from = author, values_from = freq) %>% 
  relocate("word", "n", "Ted", "ted_1.3rd", "ted_2.3rd", "ted_3.3rd", "Murray", "Ellul", "Frank")
# Rename and organize
frequency_3 <- frequency_2 %>% mutate(ted_freq = Ted, ted1.3_freq = ted_1.3rd, ted2.3_freq = ted_2.3rd,
                                      ted3.3_freq = ted_3.3rd, mur_freq = Murray, frank_freq = Frank,
                                      ellul_freq = Ellul) %>%
  select(-c(Ted, ted_1.3rd, ted_2.3rd, ted_3.3rd, Murray))

# Adding n (count) back to the table.  Frequency is count / total
frequency_3$Ted_count <- frequency_3$ted_freq * 13152  
frequency_3$Mur_count <- frequency_3$mur_freq * 3901
frequency_3$Frank_count <- frequency_3$frank_freq * 2809
frequency_3$Ellul_count <- frequency_3$ellul_freq * 2408
frequency_3$Ted1.3_count <- frequency_3$ted1.3_freq * 1461    
frequency_3$Ted2.3_count <- frequency_3$ted2.3_freq * 1418 
frequency_3$Ted3.3_count <- frequency_3$ted3.3_freq * 1389 

# Create table that ONLY has counts.
frequency_3_noFreqs <- frequency_3 %>% select(word, Ted_count, Mur_count, Frank_count, Ellul_count,
                                              Ted1.3_count, Ted2.3_count, Ted3.3_count) %>% 
  group_by(word) %>% dplyr::summarize(Ted_count = sum(Ted_count), Mur_count = sum(Mur_count),
                                      Frank_count = sum(Frank_count), Ellul_count = sum(Ellul_count),
                                      Ted1.3_count = sum(Ted1.3_count), Ted2.3_count = sum(Ted2.3_count),
                                      Ted3.3_count = sum(Ted3.3_count))

## Obtaining the tf-idf (term frequency according to inverse document frequency)
##   IN OTHER WORDS: - higher for words only in that document
##                   - lower for words in that document AND the others
frequency_inverseToFreq <- frequency_1 %>%
  count(author, word, sort = TRUE)  %>%  # aggregate to count n for each word
  bind_tf_idf(word, author, n) %>% 
  arrange(desc(tf_idf)) %>% 
  filter(!row_number() %in% c(1, 2))


### PRINT TOP 20 w/ tf-idf 

frequency_inverseToFreq %>% filter(grepl("Ted", author)) %>% arrange(desc(n)) %>% 
  head(20)
frequency_inverseToFreq %>% filter(grepl("Murray", author)) %>% arrange(desc(n)) %>% 
  head(20)
frequency_inverseToFreq %>% filter(grepl("Ellul", author)) %>% arrange(desc(n)) %>% 
  head(20)
frequency_inverseToFreq %>% filter(grepl("Frank", author)) %>% arrange(desc(n)) %>% 
  head(20) 




##### START CORRELATION ANALYSIS 


# Use only words that occur at least MORE THAN ONCE in one document.
frequency3_greaterThan1 <- frequency_3_noFreqs %>%
  filter(Ted_count > 1 | Mur_count > 1| Frank_count > 1|
           Ellul_count > 1| Ted1.3_count > 1| Ted2.3_count > 1 | Ted3.3_count > 1)

# Transform data with log, in preparation for Pearson Correlation.
frequency3_greaterThan1 <- frequency3_greaterThan1 %>%
  mutate(Ted_count = log(Ted_count), Mur_count = log(Mur_count),
         Frank_count = log(Frank_count), Ellul_count = log(Ellul_count),
         Ted1.3_count = log(Ted1.3_count), Ted2.3_count = log(Ted2.3_count),
         Ted3.3_count = log(Ted3.3_count))

# Remove nulls (AFTER log transformation to avoid strange values).
frequency3_greaterThan1_noNulls <- frequency3_greaterThan1
frequency3_greaterThan1_noNulls[is.na(frequency3_greaterThan1_noNulls)] <- 0
frequency_3_noFreqs_noNulls <- frequency_3_noFreqs
frequency_3_noFreqs_noNulls[is.na(frequency_3_noFreqs_noNulls)] <- 0

# Remove column defining which word rows pertain to.  (Only values for correlation.)
frequency3_greaterThan1_NOwords <- frequency3_greaterThan1_noNulls %>% select(-c(word))
frequency_3_noFreqs_NOwords <- frequency_3_noFreqs_noNulls %>% select(-c(word))

summary(frequency3_greaterThan1_NOwords)

# Run Pearson Correlation Matrix (with significance)
res2 <- rcorr(as.matrix(frequency3_greaterThan1_NOwords))
res2

# Plot correlation matrix.  
## Keeps giving a strange error but still manages to create graphic
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.05, insig = "blank")




##### START COSINE ANALYSIS 


# COSIGN1 = weighted values to frequencies of word appearance
# COSIGN2 = unweighted (all numbers over 1, are reverted to 1)

# To create cosign values table at end for export
names <- c()
namesGroup <- c()
cosine1Values <- c()
cosine2Values <- c()

# Turn NA values into zeros
frequency_3_noFreqs[is.na(frequency_3_noFreqs)] <- 0


# Murray
frequency3_Mur_Ted <- frequency_3_noFreqs %>% select(c(word, Mur_count, Ted_count))
frequency3_Mur_Ted <- frequency3_Mur_Ted %>% filter(Mur_count > 1 | Ted_count > 1)
length(frequency3_Mur_Ted$Ted_count)
cosign1 <- cosine(frequency3_Mur_Ted$Mur_count, frequency3_Mur_Ted$Ted_count)
frequency3_Mur_Ted$Mur_count[frequency3_Mur_Ted$Mur_count > 1] <- 1
frequency3_Mur_Ted$Ted_count[frequency3_Mur_Ted$Ted_count > 1] <- 1
cosign2 <- cosine(frequency3_Mur_Ted$Mur_count, frequency3_Mur_Ted$Ted_count)
namesGroup <- c(namesGroup, "Mur")
names <- c(names, "Ted Whole Doc")
cosine1Values <- c(cosine1Values, cosign1)
cosine2Values <- c(cosine2Values, cosign2)

frequency3_Mur_t1.3 <- frequency_3_noFreqs %>% select(c(word, Mur_count, Ted1.3_count))
frequency3_Mur_t1.3 <- frequency3_Mur_t1.3 %>% filter(Mur_count > 1 | Ted1.3_count > 1)
length(frequency3_Mur_t1.3$Ted1.3_count)
cosign1 <- cosine(frequency3_Mur_t1.3$Mur_count, frequency3_Mur_t1.3$Ted1.3_count)
frequency3_Mur_t1.3$Mur_count[frequency3_Mur_t1.3$Mur_count > 1] <- 1
frequency3_Mur_t1.3$Ted1.3_count[frequency3_Mur_t1.3$Ted1.3_count > 1] <- 1
cosign2 <- cosine(frequency3_Mur_t1.3$Mur_count, frequency3_Mur_t1.3$Ted1.3_count)
namesGroup <- c(namesGroup, "Mur")
names <- c(names, "Ted 1st Third")
cosine1Values <- c(cosine1Values, cosign1)
cosine2Values <- c(cosine2Values, cosign2)

frequency3_Mur_t2.3 <- frequency_3_noFreqs %>% select(c(word, Mur_count, Ted2.3_count))
frequency3_Mur_t2.3 <- frequency3_Mur_t2.3 %>% filter(Mur_count > 1 | Ted2.3_count > 1)
length(frequency3_Mur_t2.3$Ted2.3_count)
cosign1 <- cosine(frequency3_Mur_t2.3$Mur_count, frequency3_Mur_t2.3$Ted2.3_count)
frequency3_Mur_t2.3$Mur_count[frequency3_Mur_t2.3$Mur_count > 1] <- 1
frequency3_Mur_t2.3$Ted2.3_count[frequency3_Mur_t2.3$Ted2.3_count > 1] <- 1
cosign2 <- cosine(frequency3_Mur_t2.3$Mur_count, frequency3_Mur_t2.3$Ted2.3_count)
namesGroup <- c(namesGroup, "Mur")
names <- c(names, "Ted 2nd Third")
cosine1Values <- c(cosine1Values, cosign1)
cosine2Values <- c(cosine2Values, cosign2)

frequency3_Mur_t3.3 <- frequency_3_noFreqs %>% select(c(word, Mur_count, Ted3.3_count))
frequency3_Mur_t3.3 <- frequency3_Mur_t3.3 %>% filter(Mur_count > 1 | Ted3.3_count > 1)
length(frequency3_Mur_t3.3$Ted3.3_count)
cosign1 <- cosine(frequency3_Mur_t3.3$Mur_count, frequency3_Mur_t3.3$Ted3.3_count)
frequency3_Mur_t3.3$Mur_count[frequency3_Mur_t3.3$Mur_count > 1] <- 1
frequency3_Mur_t3.3$Ted3.3_count[frequency3_Mur_t3.3$Ted3.3_count > 1] <- 1
cosign2 <- cosine(frequency3_Mur_t3.3$Mur_count, frequency3_Mur_t3.3$Ted3.3_count)
namesGroup <- c(namesGroup, "Mur")
names <- c(names, "Ted 3rd Third")
cosine1Values <- c(cosine1Values, cosign1)
cosine2Values <- c(cosine2Values, cosign2)


# Ellul
frequency3_Ellul_Ted <- frequency_3_noFreqs %>% select(c(word, Ellul_count, Ted_count))
frequency3_Ellul_Ted <- frequency3_Ellul_Ted %>% filter(Ellul_count > 1 | Ted_count > 1)
length(frequency3_Ellul_Ted$Ted_count)
cosign1 <- cosine(frequency3_Ellul_Ted$Ellul_count, frequency3_Ellul_Ted$Ted_count)
frequency3_Ellul_Ted$Ellul_count[frequency3_Ellul_Ted$Ellul_count > 1] <- 1
frequency3_Ellul_Ted$Ted_count[frequency3_Ellul_Ted$Ted_count > 1] <- 1
cosign2 <- cosine(frequency3_Ellul_Ted$Ellul_count, frequency3_Ellul_Ted$Ted_count)
namesGroup <- c(namesGroup, "Ellul")
names <- c(names, "Ted Whole Doc")
cosine1Values <- c(cosine1Values, cosign1)
cosine2Values <- c(cosine2Values, cosign2)

frequency3_Ellul_t1.3 <- frequency_3_noFreqs %>% select(c(word, Ellul_count, Ted1.3_count))
frequency3_Ellul_t1.3 <- frequency3_Ellul_t1.3 %>% filter(Ellul_count > 1 | Ted1.3_count > 1)
length(frequency3_Ellul_t1.3$Ted1.3_count)
cosign1 <- cosine(frequency3_Ellul_t1.3$Ellul_count, frequency3_Ellul_t1.3$Ted1.3_count)
frequency3_Ellul_t1.3$Ellul_count[frequency3_Ellul_t1.3$Ellul_count > 1] <- 1
frequency3_Ellul_t1.3$Ted1.3_count[frequency3_Ellul_t1.3$Ted1.3_count > 1] <- 1
cosign2 <- cosine(frequency3_Ellul_t1.3$Ellul_count, frequency3_Ellul_t1.3$Ted1.3_count)
namesGroup <- c(namesGroup, "Ellul")
names <- c(names, "Ted 1st Third")
cosine1Values <- c(cosine1Values, cosign1)
cosine2Values <- c(cosine2Values, cosign2)

frequency3_Ellul_t2.3 <- frequency_3_noFreqs %>% select(c(word, Ellul_count, Ted2.3_count))
frequency3_Ellul_t2.3 <- frequency3_Ellul_t2.3 %>% filter(Ellul_count > 1 | Ted2.3_count > 1)
length(frequency3_Ellul_t2.3$Ted2.3_count)
cosign1 <- cosine(frequency3_Ellul_t2.3$Ellul_count, frequency3_Ellul_t2.3$Ted2.3_count)
frequency3_Ellul_t2.3$Ellul_count[frequency3_Ellul_t2.3$Ellul_count > 1] <- 1
frequency3_Ellul_t2.3$Ted2.3_count[frequency3_Ellul_t2.3$Ted2.3_count > 1] <- 1
cosign2 <- cosine(frequency3_Ellul_t2.3$Ellul_count, frequency3_Ellul_t2.3$Ted2.3_count)
namesGroup <- c(namesGroup, "Ellul")
names <- c(names, "Ted 2nd Third")
cosine1Values <- c(cosine1Values, cosign1)
cosine2Values <- c(cosine2Values, cosign2)

frequency3_Ellul_t3.3 <- frequency_3_noFreqs %>% select(c(word, Ellul_count, Ted3.3_count))
frequency3_Ellul_t3.3 <- frequency3_Ellul_t3.3 %>% filter(Ellul_count > 1 | Ted3.3_count > 1)
length(frequency3_Ellul_t3.3$Ted3.3_count)
cosign1 <- cosine(frequency3_Ellul_t3.3$Ellul_count, frequency3_Ellul_t3.3$Ted3.3_count)
frequency3_Ellul_t3.3$Ellul_count[frequency3_Ellul_t3.3$Ellul_count > 1] <- 1
frequency3_Ellul_t3.3$Ted3.3_count[frequency3_Ellul_t3.3$Ted3.3_count > 1] <- 1
cosign2 <- cosine(frequency3_Ellul_t3.3$Ellul_count, frequency3_Ellul_t3.3$Ted3.3_count)
namesGroup <- c(namesGroup, "Ellul")
names <- c(names, "Ted 3rd Third")
cosine1Values <- c(cosine1Values, cosign1)
cosine2Values <- c(cosine2Values, cosign2)


# Frank
frequency3_Frank_Ted <- frequency_3_noFreqs %>% select(c(word, Frank_count, Ted_count))
frequency3_Frank_Ted <- frequency3_Frank_Ted %>% filter(Frank_count > 1 | Ted_count > 1)
length(frequency3_Frank_Ted$Ted_count)
cosign1 <- cosine(frequency3_Frank_Ted$Frank_count, frequency3_Frank_Ted$Ted_count)
frequency3_Frank_Ted$Frank_count[frequency3_Frank_Ted$Frank_count > 1] <- 1
frequency3_Frank_Ted$Ted_count[frequency3_Frank_Ted$Ted_count > 1] <- 1
cosign2 <- cosine(frequency3_Frank_Ted$Frank_count, frequency3_Frank_Ted$Ted_count)
namesGroup <- c(namesGroup, "Frank")
names <- c(names, "Ted Whole Doc")
cosine1Values <- c(cosine1Values, cosign1)
cosine2Values <- c(cosine2Values, cosign2)

frequency3_Frank_t1.3 <- frequency_3_noFreqs %>% select(c(word, Frank_count, Ted1.3_count))
frequency3_Frank_t1.3 <- frequency3_Frank_t1.3 %>% filter(Frank_count > 1 | Ted1.3_count > 1)
length(frequency3_Frank_t1.3$Ted1.3_count)
cosign1 <- cosine(frequency3_Frank_t1.3$Frank_count, frequency3_Frank_t1.3$Ted1.3_count)
frequency3_Frank_t1.3$Frank_count[frequency3_Frank_t1.3$Frank_count > 1] <- 1
frequency3_Frank_t1.3$Ted1.3_count[frequency3_Frank_t1.3$Ted1.3_count > 1] <- 1
cosign2 <- cosine(frequency3_Frank_t1.3$Frank_count, frequency3_Frank_t1.3$Ted1.3_count)
namesGroup <- c(namesGroup, "Frank")
names <- c(names, "Ted 1st Third")
cosine1Values <- c(cosine1Values, cosign1)
cosine2Values <- c(cosine2Values, cosign2)

frequency3_Frank_t2.3 <- frequency_3_noFreqs %>% select(c(word, Frank_count, Ted2.3_count))
frequency3_Frank_t2.3 <- frequency3_Frank_t2.3 %>% filter(Frank_count > 1 | Ted2.3_count > 1)
length(frequency3_Frank_t2.3$Ted2.3_count)
cosign1 <- cosine(frequency3_Frank_t2.3$Frank_count, frequency3_Frank_t2.3$Ted2.3_count)
frequency3_Frank_t2.3$Frank_count[frequency3_Frank_t2.3$Frank_count > 1] <- 1
frequency3_Frank_t2.3$Ted2.3_count[frequency3_Frank_t2.3$Ted2.3_count > 1] <- 1
cosign2 <- cosine(frequency3_Frank_t2.3$Frank_count, frequency3_Frank_t2.3$Ted2.3_count)
namesGroup <- c(namesGroup, "Frank")
names <- c(names, "Ted 2nd Third")
cosine1Values <- c(cosine1Values, cosign1)
cosine2Values <- c(cosine2Values, cosign2)

frequency3_Frank_t3.3 <- frequency_3_noFreqs %>% select(c(word, Frank_count, Ted3.3_count))
frequency3_Frank_t3.3 <- frequency3_Frank_t3.3 %>% filter(Frank_count > 1 | Ted3.3_count > 1)
length(frequency3_Frank_t3.3$Ted3.3_count)
cosign1 <- cosine(frequency3_Frank_t3.3$Frank_count, frequency3_Frank_t3.3$Ted3.3_count)
frequency3_Frank_t3.3$Frank_count[frequency3_Frank_t3.3$Frank_count > 1] <- 1
frequency3_Frank_t3.3$Ted3.3_count[frequency3_Frank_t3.3$Ted3.3_count > 1] <- 1
cosign2 <- cosine(frequency3_Frank_t3.3$Frank_count, frequency3_Frank_t3.3$Ted3.3_count)
namesGroup <- c(namesGroup, "Frank")
names <- c(names, "Ted 3rd Third")
cosine1Values <- c(cosine1Values, cosign1)
cosine2Values <- c(cosine2Values, cosign2)

# Create Dataframe for export and ploting
cosignDF <- data.frame("namesGroup" = namesGroup, "Names" = names,
                       "Cosign1" = cosine1Values, "cosine2" = cosine2Values)

# Create Bar Graphs
#   COSINE1
ggplot(cosignDF, aes(x=namesGroup, y=Cosign1, fill=Names)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle("COSINE WORD FREQUENCY COMPARISON: \nAll Words Appearing More Than Once Weighted By Appearance") +
  ylab("Cosine Similarity") +
  xlab("Author Of Document Being Compared To The Manifesto") +
  labs(fill = "Portion of the  Manifesto") + 
  scale_fill_brewer(palette = "Dark2")
#   COSINE2
ggplot(cosignDF, aes(x=namesGroup, y=cosine2, fill=Names)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle("COSINE WORD FREQUENCY COMPARISON: \nAll Words Appearing More Than Once Weighted Evenly") +
  ylab("Cosine Similarity") +
  xlab("Author Of Document Being Compared To The Manifesto") +
  labs(fill = "Portion of the Manifesto") + 
  scale_fill_brewer(palette = "Dark2")



usethis::create_github_token()

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
