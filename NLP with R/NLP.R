library(quanteda)
library(wordcloud)
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidytext)
library(caret)
library(lattice)
library(quanteda.textmodels)
#DATA_PATH <- "C:\Desktop\NUIG assignments sem2\R\rt-polaritydata\rt-polaritydata"

# Load sentence in dataframes
df_neg <- data.frame(sentence = 
                       readLines("C:\\Desktop\\NUIG assignments sem2\\R\\rt-polaritydata\\rt-polaritydata\\rt-polarity.neg.txt"), 
                     stringsAsFactors = FALSE)
df_neg['sentiment'] <- "neg"

df_pos <- data.frame(sentence = 
                       readLines("C:\\Desktop\\NUIG assignments sem2\\R\\rt-polaritydata\\rt-polaritydata\\rt-polarity.pos.txt"), 
                     stringsAsFactors = FALSE)
df_pos['sentiment'] <- "pos"

# Create corpus from data frames
corp_movies_reviews <- corpus(rbind(df_neg, df_pos), text_field='sentence')


# Create tokens from corpus
moviescorpus_token <- tokens(corp_movies_reviews,remove_punct = TRUE,remove_numbers = TRUE, remove_symbols = TRUE,remove_separators = TRUE)

toks_movies_created <- tokens(corp_movies_reviews,remove_punct = TRUE,remove_numbers = TRUE, remove_symbols = TRUE,remove_separators = TRUE)

#Remove punctuations
toks_zeropunctuation <-tokens_select(toks_movies_created,pattern = stopwords('en'),selection = 'remove')

# Remove stopwords
toks_nostopwords <- tokens_remove(toks_zeropunctuation, 
                                 pattern = stopwords('en'), 
                                 padding = TRUE)


#Question1 a
dfmat_movies <- dfm(moviescorpus_token)

features_dfm_movies <- textstat_frequency(dfmat_movies, n = 15)

# Sort by reverse frequency order
#features_dfm_inaug$feature <- with(features_dfm_inaug, reorder(feature, -frequency))

ggplot(features_dfm_movies, aes(x = feature, y = frequency)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+scale_y_continuous(limits = c(0,1500),breaks = seq(0,1500,by=250))

ggplot(textstat_frequency(dfmat_movies, n = 15),
       aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal()

#Question1 b
textplot_wordcloud(dfmat_movies, min_size = 1, max_size = 8, max_words =50,color = "blue")
textplot_wordcloud(dfmat_movies,max_words = 50,max_size = 200)
install.packages("wordcloud2")
library(wordcloud2)
install.packages("wordcloud")
library(wordcloud)
install.packages("RColorBrewer")
library(RColorBrewer)
library(tm)
#Question 1 c

docvars(corp_movies_reviews, "dummy_english") <- 
  factor(ifelse(docvars(corp_movies_reviews, "sentiment") == "neg",
                "Negative",
                "Positive"))
dfmat_corp_language <- dfm(corp_movies_reviews, 
                           groups = "dummy_english")


set.seed(132)
textplot_wordcloud(corp_movies_reviews, comparison = TRUE,max_size = 15, max_words = 100)
#method2

names(docvars(corp_movies_reviews))
docvars(corp_movies_reviews, "sentiment") <- 
  factor(ifelse(docvars(corp_movies_reviews, "sentiment") == "neg",
                "Negative words",
                "Positive words"))



dfmat_corp_movies<- dfm(corp_movies_reviews, 
                        groups = "sentiment",remove = stopwords('en'),remove_punct=TRUE)
textplot_wordcloud(dfmat_corp_movies,
                   comparison = TRUE, max_words = 50,
                   color = c('red', 'blue'))
#Question 1 d
#lexical
# Lexical Diversity of Random 20 Sentances
moviescorpus_lexdiv<-textstat_lexdiv(dfmat_movies)
rand_lexdiv <- moviescorpus_lexdiv[sample(nrow(moviescorpus_lexdiv),20,replace = FALSE,prob = NULL),]
plot(rand_lexdiv$TTR, type = 'l', xaxt = 'n', xlab = NULL, ylab = "TTR")+grid()+axis(1,at=seq_len(nrow(rand_lexdiv)))



#Question 1 e


random_20_dendo <- dfmat_movies[sample(nrow(dfmat_movies),20,replace = FALSE,prob = NULL),]
#a<- textstat_dist(dfm(random_20))
distance <- as.dist(textstat_dist(random_20_dendo))
as.dist()
print(ra)
hc <- hclust(distance)
plot(hc,xlab = "Distance", ylab = NULL)
library(e1071)
#Question2
corp_movies_Ldict <- tokens_lookup(toks_nostopwords,dictionary = data_dictionary_LSD2015[1:2])
dfm_dict <- dfm(corp_movies_Ldict)
dfm_moviescorpus_set <- convert(dfm_dict, to = "data.frame")
dfm_moviescorpus_set$sentiment <- ifelse(dfm_moviescorpus_set$negative >= dfm_moviescorpus_set$positive,"negative","positive")
head(dfm_moviescorpus_set)
head(dfm_moviescorpus_token_lsd)
tab_class <- table(dfm_moviescorpus_set$sentiment,dfmat_movies@docvars$sentiment)
confusionMatrix(tab_class,mode= "everything")



# Create hierarchical clustering based on distance matrix
cluster <- hclust(tstat_distance)
plot(cluster, hang = -2, cex = 0.7, type = "triangle", xlab = "Distance")
