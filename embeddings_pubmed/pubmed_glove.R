library(text2vec) # glove with RcppParallel backend
library(Rtsne) # tsne

#TODO: remove punctuation, numbers, etc.
#TODO: visualize with tensorboard

# create vocab ------------------------------------------------------------

tokens <- space_tokenizer(strings = pubmed_dfs$abstract)
it <- itoken(tokens, progressbar = TRUE)
vocab <- create_vocabulary(it)
vectorizer <- vocab %>% prune_vocabulary(term_count_min = 5L) %>% 
  vocab_vectorizer

# create context windows, i.e., build co-occurence matrix
tcm <- create_tcm(it, vectorizer, skip_gram_window = 5L)


# train glove and build word vectors --------------------------------------


# train GloVe
RcppParallel::setThreadOptions(numThreads = 16)
glove <- GlobalVectors$new(word_vectors_size = 200, vocabulary = vocab, x_max = 10)
wv_main <- glove$fit_transform(tcm, n_iter = 20, convergence_tol = 0.01)
dim(wv_main)
wv_context <- glove$components
dim(wv_context)

# build word vectors
word_vectors <- wv_main + t(wv_context)

# similarity
find_similar_words <- function(word, embedding_matrix, n = 5) {
  
  similarities <- embedding_matrix[word, , drop = FALSE] %>%
    sim2(embedding_matrix, y = ., method = "cosine")
  
  similarities[,1] %>% sort(decreasing = TRUE) %>% head(n)
  
}

find_similar_words("antiphagocytosis", wv_main)
find_similar_words("micropreparative", wv_main)


# projects, t-sne, pca ----------------------------------------------------


tsne <- Rtsne(word_vectors, perplexity = 50, pca = FALSE)

tsne_plot <- tsne$Y %>%
  as.data.frame() %>%
  mutate(word = row.names(word_vectors)[2:500]) %>%
  ggplot(aes(x = V1, y = V2, label = word)) + 
  geom_text(size = 3)
tsne_plot
