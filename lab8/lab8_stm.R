# clear all
#install.packages("rtools")
install.packages("quanteda")
install.packages("stm", dependencies=TRUE)
installed.packages("tidyverse")
#installed.packages("RColorBrewer")

rm(list = ls()) 
library(quanteda)
library(tidyverse)
library(RColorBrewer)
library(stm)

setwd("~/Dropbox/coursework/2018Spring/soc220_spring2018/Github_labs/lab8")

#######################################################################################
# READ FROM EXCEL MY DATA
library(readxl)
efr_tm <- read_excel("efr_tm.xlsx")

#######################################################################################
# PREPROCESSING FOR STM PACKAGE

# create a corpus object
corpus_object <- corpus(efr_tm)

doc_freq_matrix <- dfm(corpus_object,
           #stop words
           remove = c(stopwords("english")),
           #specify length of n-grams
           ngrams=1L,
           #stemmer? (porter)
           stem = F,
           #remove misc
           remove_numbers = TRUE, 
           remove_punct = TRUE,
           remove_symbols = TRUE)


#
trim_doc_freq_matrix <- dfm_trim(doc_freq_matrix, max_docfreq=70, min_docfreq = 2)
# lastly, trim that matrix to remove uncommon or too common words
#trim_doc_freq_matrix <- dfm_trim(doc_freq_matrix, max_docfreq=.90, min_docfreq = 2)

#structural topic model object from trimmed dtm
texts_stm <- convert(trim_doc_freq_matrix, to = 'stm')


#``````````````
#` prep documents object which takes three arguments:
#`1. documents 
#`2. the total vocabulary
#`3. meta data
#`4. lastly has another threshold if you wish to remove based on plot removed
#````
prep_docs_object <- prepDocuments(texts_stm$documents, texts_stm$vocab,
                           texts_stm$meta, lower.thresh = 10, verbose=TRUE)


#plot removed of how many words by minimal threshold count
plotRemoved(texts_stm$documents,lower.thresh = seq(1, 50, by = 1))

texts_stm$documents
texts_stm$vocab
texts_stm$meta

#######################################################################################
#' search for optimal number of topics

#`search for optimal number of topics. 
#`Takes in documents, vocab from structural topic object
#plots log-likelihood
ntopics <- searchK(texts_stm$documents,texts_stm$vocab, 
                   K = c(3,7,12,20), data = texts_stm$meta)
plot(ntopics)

#######################################################################################
# fit the number of topics

#fit stm, FINALLY!
fit_stm <- stm(documents = texts_stm$documents, 
               vocab = texts_stm$vocab,
               K = 7,
               #covariates
               prevalence = ~ monarch + war, 
               seed = 02138,
               data = texts_stm$meta
)

summary(fit_stm)

#let's take a look
labelTopics(fit_stm)

#``````````````
# FREX: are the words that are both frequent and exclusive, identifying words
#' that distinguish topic
#' 
#' Score is conditional on this topic, what are the most frequent words
#' 
#``````````````

# topic1: war with spain / france
# topic2: Catholics and the papists
# topic3: taxes
# topic4: social privileges
# topic5: EIC?

#plot frequent words for given topics
plot.STM(fit_stm, type = "labels") 
#plot topic proportions over whole corpus
plot.STM(fit_stm, type = "summary") 

#histogram of the topics
plot(fit_stm, type="hist")

#######################################################################################
#plots to compare pairs topics

par(mfrow = c(1,2))
plot(fit_stm, type = 'labels', topics = c(2, 4:5), labeltype = 'frex', main = 'FREX')
plot(fit_stm, type = 'labels', topics = c(2, 2:3), labeltype = 'score', main = 'score')

par(mfrow = c(1,1))
plot(fit_stm, type = 'perspectives',topics = c(1,2))
plot(fit_stm, type = 'perspectives',topics = c(2,3))
plot(fit_stm, type = 'perspectives',topics = c(3,4))


#######################################################################################
#install.packages('igraph')
#Network visualizer of the topics

topic_corr = topicCorr(fit_stm)
plot(topic_corr)

#######################################################################################
#' Estimating the effect of a given outcome on a topic.
#' THE STRUCTURAL PART of stm

#for a given outcome, we can plot differences in select topic proportions
#which topics are predicted by war
est_stm <- estimateEffect( ~ war, fit_stm, metadata = texts_stm$meta)
summary(est_stm)
plot(est_stm, covariate = 'war', topics = 1:7, model = fit_stm)

#######################################################################################
#######################################################################################
#' Selecting optimal model

efrSelectModel <- selectModel(texts_stm$documents,
                              texts_stm$vocab, 
                              K=10,
                              #other covariates
                              prevalence=~monarch+war,
                              max.em.its=75, 
                              data=texts_stm$meta,
                              runs=20,
                              seed=02138
)
# semantic coherence (like cosine similarity of words in topic)
#by exclusivility (only want certain words within topic)
plotModels(efrSelectModel)

# Choose model #1
fit_stm_model1 <- efrSelectModel$runout[[4]] 

#takes in formula, fit model, metadata
est_stm2 <- estimateEffect(1:10 ~ war + monarch, 
                           fit_stm_model1, 
                           metadata = texts_stm$meta)

summary(est_stm2)

#plot the estimate, choose covariate to especially examine
plot(est_stm2, topics=2:3,covariate = 'monarch', model = fit_stm_model1)


