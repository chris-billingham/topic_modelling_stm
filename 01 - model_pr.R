options(scipen=999)

library(tidyverse)
library(stm)
library(textstem)
library(tidytext)
library(magrittr)
library(igraph)
library(occamsrazR)

# prep the data ####
# load in the data
prod_reviews <- readRDS("data/prod_reviews.rds")
colnames(prod_reviews) <- tolower(colnames(prod_reviews))
prod_reviews %<>% filter(date_and_time_of_review >= '2015-07-01')
prod_reviews %<>% mutate_if(is.character, tolower(trimws(.)))

