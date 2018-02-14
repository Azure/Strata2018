entropy <- function(p1){
  LITTLE_BIT <- 1e-12
  p1 <- (p1 + LITTLE_BIT)/( 1 + 2*LITTLE_BIT)
  H <- function(p){
    p[0>p] <- 0    ##??? p[0>p] <- NA
    p[0==p] <- 1e-100
    -p * log2(p)
  }
  H(p1) + H(1 - p1)
}

get_new_pseudolabelled_sample <- function(labelled_filenames){
  unlabelled_data_df[unlabelled_data_df$rev_id %in% labelled_filenames$rev_id, ]
}


fit_and_evaluate_model_rxLogisticRegression <- function(candidate_cases, form=FORM, 
                                                        test_set=TEST_SET, L1=L1_PENALTY, L2=L2_PENALTY){

  compute_roc <- function(pred_df){
    LITTLE_BIT <- 1e-12
    pred_df$Probability <- (pred_df$Probability + LITTLE_BIT)/( 1 + 2*LITTLE_BIT)
    rxRoc("flagged", "Probability", pred_df, numBreaks=1000)
  }
  
  prediction_logloss <- function(pred_df){
    LITTLE_BIT <- 1e-12
    pred_df$Probability <- (pred_df$Probability + LITTLE_BIT)/( 1 + 2*LITTLE_BIT)
    log_probs <- with(pred_df, log(Probability[flagged]))
    -sum(log_probs)/nrow(pred_df)
  }
  
  # for binary, NA means bad example
  training_set_new <- candidate_cases %>% filter(!is.na(flagged))

  progress_messages <- capture.output({
    fit_new <- rxLogisticRegression(form, training_set_new,
                                    type="binary", 
                                    l1Weight=L1, l2Weight=L2,
                                    reportProgress=0, verbose=0, optTol=1e-12)
    
    pred_test <- rxPredict(fit_new, test_set, extraVarsToWrite=c("rev_id", "flagged"))
    # names(pred_test) <- c("rev_id", "flagged", "PredictedLabel", "Score",  "Probability")
    
    roc_obj = compute_roc(pred_test)
    
    results <- list(
      model=fit_new,
      tss=nrow(training_set_new),
      test_predictions=pred_test,
      roc = roc_obj,
      performance = c(accuracy=with(pred_test, sum(flagged == PredictedLabel)/length(flagged)),
                      neg_logloss = -prediction_logloss(pred_test),
                      negentropy = -mean(entropy(pred_test$Probability)),
                      auc = rxAuc(roc_obj)
      ),
      confusion = with(pred_test, table(flagged, PredictedLabel))
    )
  })
  
  return(results)
}

plot_class_histograms <- function(results_df){
  # results_df$test_predictions %>% ggplot(aes(x=Probability)) + geom_density()
  with(results_df$test_predictions, hist(Probability, breaks=100))
}

# randomForest version
fit_and_evaluate_model_randomForest <- function(candidate_cases, form=FORM, test_set=TEST_SET){
  
  compute_roc <- function(pred_df){
    with(pred_df, roc(flagged, Probability))
  }
  
  prediction_logloss <- function(pred_df){
    LITTLE_BIT <- 1e-12
    pred_df$Probability <- (pred_df$Probability + LITTLE_BIT)/( 1 + 2*LITTLE_BIT)
    log_probs <- with(pred_df, log(Probability[flagged]))
    -sum(log_probs)/nrow(pred_df)
  }

  training_set_new <- candidate_cases %>% filter(!is.na(flagged))
  
  library(randomForest)
  
  training_set_new$flagged <- factor(training_set_new$flagged) # rf doesn't think binary is logical
  fit_new <- randomForest(form, training_set_new, ntree=501) # maxnodes=1024, sampsize=1000 ???
  
  pred_test <- tibble(
    rev_id = test_set$rev_id,
    flagged = test_set$flagged,
    PredictedLabel = predict(fit_new, test_set, type="response") %>% as.logical,
    Probability = predict(fit_new, test_set, type="prob")[,'TRUE']
  )

  roc_obj <- compute_roc(pred_test)
  
  results <- list(
    model=fit_new,
    tss=nrow(training_set_new),
    test_predictions=pred_test,
    roc = roc_obj,
    performance = c(accuracy=with(pred_test, sum(flagged == PredictedLabel)/length(flagged)),
                    neg_logloss = -prediction_logloss(pred_test),
                    negentropy = -mean(entropy(pred_test$Probability)),
                    auc = auc(roc_obj)
    ),
    confusion = with(pred_test, table(flagged, PredictedLabel))
  )

  return(results)
}

get_auc <- function(roc_obj){
  return(pROC::auc(roc_obj))  #####!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  library(pROC)
  if ('rxRoc' %in% class(roc_obj)){
    rxAuc(roc_obj)
  } else {
    pROC::auc(roc_obj)
  }
}

plot_roc_history_rxRoc <- function(initial_results, results_history){
  roc0 <- initial_results$roc
  with(roc0, plot(1 - specificity, sensitivity, type='l', col="black"))
  text(0.75, 0, sprintf("AUC: %0.3f", get_auc(roc0)), col="black", pos=3, cex=0.8)
  rbow <- rainbow(length(results_history), end=2/3, v=0.75)
  for (i in seq_along(results_history)){
    roc_N <- results_history[[i]]$roc
    with(roc_N, lines(1 - specificity, sensitivity, col=rbow[i]))
    text(0.75, i * 0.04, sprintf("AUC: %0.3f", get_auc(roc_N)), col=rbow[i], pos=3, cex=0.8)
  }
}

plot_roc_history_pROC <- function(initial_results, results_history){
  roc0 <- initial_results$roc
  plot(roc0, col="black")
  text(0.75, 0, sprintf("AUC: %0.3f", get_auc(roc0)), col="black", pos=3, cex=0.8)
  rbow <- rainbow(length(results_history), end=2/3, v=0.75)
  for (i in seq_along(results_history)){
    roc_N <- results_history[[i]]$roc
    lines(roc_N, col=rbow[i])
    text(0.75, i * 0.04, sprintf("AUC: %0.3f", get_auc(roc_N)), col=rbow[i], pos=3, cex=0.8)
  }
}
