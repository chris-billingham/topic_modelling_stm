options(scipen=999)

library(tidyverse)
library(stm)
library(textstem)
library(tidytext)
library(magrittr)
library(igraph)
library(nonameR)

# prep the data ####
# load in the data
gsr_reviews <- readRDS("data/gsr_reviews.rds")

# clean the text for non UTF-8 characters
gsr_reviews$review_scrub <- gsr_reviews$review_text %>% 
  iconv("UTF-8", "UTF-8",sub='')

# lower case it
gsr_reviews$review_scrub %<>% tolower()

# read in the brands file and then swap any instance of a brand name with "thisbrand"
brands <- read_csv("data/brands.csv")
gsr_reviews$review_scrub <- brands$brand %>% 
  paste(collapse = "|") %>% 
  gsub("thisbrand", gsr_reviews$review_scrub)

# lemmatize the strings, because stemming sucks
# note for future, maybe use/combine with pos tagging
gsr_reviews$lemma_text <- lemmatize_strings(gsr_reviews$review_scrub)

# split data set about 01/07/2017
train <- gsr_reviews %>% 
  filter(weekending < "2017-07-01")
test <- gsr_reviews %>% 
  filter(weekending >= "2017-07-01")

# begin stm modelling process ####
# run through the processed data step, stripping out html and leaving only characters
processed_data <- textProcessor(train$lemma_text, metadata = train, 
                                     striphtml = TRUE, 
                                     stem = FALSE,
                                     onlycharacter = TRUE)

# what's the effect of removing low frequency tokens
plotRemoved(processed_data$documents, seq(10, 1000, by=5))

# cool we're going to choose 100
# text processing, setting lower threshold to 100
# note if we've done this we'll just load it back in
if(file.exists("data/gsr_stm_out.rds")) {
  out <- readRDS("data/gsr_stm_out.rds")
} else {
  out <- prepDocuments(documents = processed_data$documents,
                            vocab = processed_data$vocab,
                            meta = processed_data$meta,
                            lower.thresh = 100)
  saveRDS(out, "data/gsr_stm_out.rds")
}

# seperate out the sections
docs <- out$documents
vocab <- out$vocab
meta <- out$meta

# reproducibility
set.seed(1979)

# run the model ####
if(file.exists("data/gsr_stm_model.rds")) {
  stm_fit <- readRDS("data/gsr_stm_model.rds")
} else {
  stm_fit <- stm(documents = out$documents,
                    vocab = out$vocab,
                    prevalence = ~ rating + brand,
                    K = 0,
                    max.em.its = 500,
                    LDAbeta = FALSE,
                    reportevery = 5,
                    data = out$meta)
  saveRDS(stm_fit, "data/gsr_stm_model.rds")
}

# review the model analyics #####
# extract K
k <- stm_fit$settings$dim$K

# create beta distribution - most likely words per topic
td_beta <- tidy(stm_fit, matrix = "beta", document_names = meta$review_id)

# create gamma distribution - what likelihood is each document each topic
td_gamma <- tidy(stm_fit, matrix = "gamma", document_names = meta$review_id)

# plot the word distributions for the topics
td_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  filter(between(topic, 11, 30)) %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# plot the document-likelihood distributions
td_gamma %>%
  ggplot(aes(gamma, fill = as.factor(topic))) + 
    geom_histogram(show.legend = FALSE, bins = 100) + 
    facet_wrap(~ topic) +
    scale_y_log10()

# run the estimate effect, if I've already got a copy just load that in.
if(file.exists("data/gsr_stm_prep.rds")) {
  prep <- readRDS("data/gsr_stm_prep.rds")
} else {
  prep <- estimateEffect(1:k ~ rating + brand, stm_fit, meta = meta, uncertainty = "Global")
  saveRDS(prep, "data/gsr_stm_prep.rds")
}

# this is the "formula" for each topic, here we've chose 25
summary(prep, topics=c(51))

# i think this is the influence of brand on something
plot(prep, "brand", model = stm_fit, method = "pointestimate", topics = c(51))

# the influence of rating on topic prevalence
plot(prep, "brand", model = stm_fit, method = "continuous", topics = c(51))

# this shows the top 3 words for each of the topics, i make the text smaller to get it in
plot.STM(stm_fit, type = "summary", text.cex = 0.5)

# this compares the top words between two different topics, looks cool
plot.STM(stm_fit, type = "perspectives", topics = c(17,24), text.cex = 1)

# run a correlation (don't use huge as you can adjust the cutoffs)
# this looks at the correlation plot between topics
corr <- topicCorr(stm_fit, method = c("simple"), cutoff = 0.15)

# here we make a customised topic correlation plot, need to load igraph to change the layout
plot(corr, vertex.color="white", 
     vlabels = topic_categorisation$top_level,
     vertex.label.cex = 0.5, 
     vertex.label.color = "blue", 
     vertex.size = 5, 
     layout = layout_nicely)

# plots semantic coherence and exclusivity for the topics, top and right is best
# right is how "clear" the topic is, up is how many exclusive words it has
topicQuality(model = stm_fit, documents = docs)

# terms per topic
labelTopics(stm_fit)

# sage terms per topic, i think this doesn't work unless LDABeta was FALSE in the model
sageLabels(stm_fit)

# highest rated documents for each topic, top 10
findThoughts(stm_fit, texts = meta$review_text, topics = c(21), n = 10)

# topics on the metadata ####

# create a dataframe of meta with the topic assigned to each one
trained <- make.dt(stm_fit, meta = meta)
topics <- colnames(trained[,2:(k+1)])[max.col(trained[,2:(k+1)], ties.method = 'first')] %>%
  as.tibble()
gsr_topics <- bind_cols(trained[,(k+2):length(trained)], topics)

# fitting new documents - using the test set #####
new_processed_data <- textProcessor(test$lemma_text, metadata = test, 
                                     striphtml = TRUE, 
                                     stem = FALSE,
                                     onlycharacter = TRUE)

# note you need both the old vocab (from the out) as well
new_docs <- alignCorpus(new = new_processed_data, old.vocab = vocab)

# note again the use of old and new and the model, also keep the prevalence the same
new_topic <- fitNewDocuments(stm_fit,
                             documents = new_docs$documents,
                             newData = new_docs$meta,
                             origData = meta,
                             prevalence = ~ rating + brand,
                             prevalencePrior = "Covariate")

# topics on the fit-new and combine into single dataframe ####

# split out the gamme/theta and add assigned topics against them
scored <- make.dt(new_topic, meta = new_docs$meta)
topics_new <- colnames(scored[,2:(k+1)])[max.col(scored[,2:(k+1)], ties.method = 'first')] %>%
  as.tibble()
gsr_new_topics <- bind_cols(scored[,(k+2):length(scored)], topics_new)


topic_categorisation <- read_csv("data/gsr_topic_categorisation.csv")

# combine into full data-set with some tags
gsr_all_1 <- gsr_topics %>% 
  inner_join(topic_categorisation, by = c("value" = "lookup")) %>% 
  mutate(source = "modelled-train")

gsr_all_2 <- gsr_new_topics %>% 
  inner_join(topic_categorisation, by = c("value" = "lookup")) %>% 
  mutate(source = "fit_new-test")

# done and save off
gsr_all <- bind_rows(gsr_all_1, gsr_all_2)
saveRDS(gsr_all, "data/gsr_stm_all.rds")

# summary plotting ####

# start to plot out the data
gsr_all %>% 
  group_by(source, weekending, top_level) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(weekending, n)) +
  geom_line(aes(colour = source)) + 
  facet_wrap(~ top_level, scales = "free") +
  theme(legend.position = "bottom") +
  ggtitle("stm topics over time, modelled and fit-new")

max(gsr_reviews$weekending)

# jesus christ it works ####
