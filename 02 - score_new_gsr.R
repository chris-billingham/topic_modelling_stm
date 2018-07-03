options(scipen=999)

library(tidyverse)
library(stm)
library(textstem)
library(tidytext)
library(magrittr)
library(occamsrazR)

# get username and password
td_user <- Sys.getenv("teradata_user")
td_pass <- Sys.getenv("teradata_pass")

start <- Sys.time()
# insert here
# find last date of gsr and get the latest
gsr_all <- readRDS("data/gsr_stm_all.rds")
from <- max(gsr_all$weekending)

# create and run query to get the latest data
sql <- paste0("sel * from marketing_temporary_database.IW_GSR_Full where weekending > date '", as.character(from), "'")
query <- td_download(sql, td_user, td_pass)

# sort out the columns we need
score <- query[,c(1, 26, 24, 15, 16, 17, 18)]

# find the max weekending from score, which is always like the weekend and get rid of it
max_we <- max(score$weekending)
score %<>% filter(weekending != max_we)

# fix up everything so we exactly match the modelled data
score$trading_title_code %<>% trimws()
score$trading_title_code <- factor(score$trading_title_code, levels = c("CGN", "DAL", "FPS", "FSP", "FW", "HOB", "JDW", "PMA"))
colnames(score) <- colnames(gsr_all[,1:7])
score$review_id %<>% as.character()
score$csat %<>% as.integer()
score$recommend <- ifelse(score$recommend == "Yes", 1, 0)
score$recommend %<>% as.logical()

# start the text transformation
score$review_scrub <- score$review_text %>% 
  iconv("UTF-8", "UTF-8",sub='')

# lower case it
score$review_scrub %<>% tolower()

# read in the brands file and then swap any instance of a brand name with "thisbrand"
brands <- read_csv("data/brands.csv")
score$review_scrub <- brands$brand %>% 
  paste(collapse = "|") %>% 
  gsub("thisbrand", score$review_scrub)

# lemmatize the strings
score$lemma_text <- lemmatize_strings(score$review_scrub)

# fitting new documents #####
new_processed_data <- textProcessor(score$lemma_text, metadata = score, 
                                    striphtml = TRUE, 
                                    stem = FALSE,
                                    onlycharacter = TRUE)

# read in the old data we need
out <- readRDS("data/gsr_stm_out.rds")
stm_fit <- readRDS("data/gsr_stm_model.rds")
docs <- out$documents
vocab <- out$vocab
meta <- out$meta
k <- stm_fit$settings$dim$K
topic_categorisation <- read_csv("data/gsr_topic_categorisation.csv")

# note you need both the old vocab (from the out) as well
new_docs <- alignCorpus(new = new_processed_data, old.vocab = vocab)

# note again the use of old and new and the model, also keep the prevalence the same
new_topic <- fitNewDocuments(stm_fit,
                             documents = new_docs$documents,
                             newData = new_docs$meta,
                             origData = meta,
                             prevalence = ~ rating + brand,
                             prevalencePrior = "Covariate")

# now seperate out the gamma/theta distributions and tag up each doc with topic
scored <- make.dt(new_topic, meta = new_docs$meta)
topics_new <- colnames(scored[,2:(k+1)])[max.col(scored[,2:(k+1)], ties.method = 'first')] %>%
  as.tibble()
gsr_new_topics <- bind_cols(scored[,(k+2):length(scored)], topics_new)

# then add download rolling and add to bottom of the list


gsr_all_new <- gsr_new_topics %>% 
  inner_join(topic_categorisation, by = c("value" = "lookup")) %>% 
  mutate(source = "fit_new-score")

Sys.time() - start

gsr_all <- bind_rows(gsr_all, gsr_all_new)
saveRDS(gsr_all, "data/gsr_stm_all.rds")

# start to plot out the data
gsr_all %>% 
  group_by(source, weekending, bottom_level) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(weekending, n)) +
  geom_line(aes(colour = source)) + 
  facet_wrap(~ bottom_level, scales = "free") +
  theme(legend.position = "bottom") +
  ggtitle("stm topics over time, modelled and fit-new")

gsr_all %>% 
  group_by(source, weekending, bottom_level) %>% 
  summarise(rating = mean(rating)) %>% 
  ggplot(aes(weekending, rating)) +
  geom_line(aes(colour = source)) + 
  facet_wrap(~ bottom_level, scales = "free") +
  theme(legend.position = "bottom") +
  ggtitle("stm mean rating over time, modelled and fit-new")
