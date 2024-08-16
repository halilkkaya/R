optimalCutoff <- function(actuals, predictedScores, optimizeFor = "misclasserror", returnDiagnostics = FALSE) {
  
  cutoff_values <- seq(0, 1, length.out = 100)
  
  metrics <- sapply(cutoff_values, function(cutoff) {
    predictedLabels <- ifelse(predictedScores >= cutoff, 1, 0)
    
    tp <- sum(actuals == 1 & predictedLabels == 1)
    tn <- sum(actuals == 0 & predictedLabels == 0)
    fp <- sum(actuals == 0 & predictedLabels == 1)
    fn <- sum(actuals == 1 & predictedLabels == 0)
    
    accuracy <- (tp + tn) / (tp + tn + fp + fn)
    sensitivity <- tp / (tp + fn) # TPR
    specificity <- tn / (tn + fp) # TNR
    misclasserror <- (fp + fn) / (tp + tn + fp + fn)
    youdensindex <- sensitivity + specificity - 1
    
    return(c(accuracy, sensitivity, specificity, misclasserror, youdensindex))
  })
  
  colnames(metrics) <- c("accuracy", "sensitivity", "specificity", "misclasserror", "youdensindex")
  rownames(metrics) <- cutoff_values
  
  if (optimizeFor == "accuracy") {
    optimal_index <- which.max(metrics["accuracy", ])
  } else if (optimizeFor == "sensitivity") {
    optimal_index <- which.max(metrics["sensitivity", ])
  } else if (optimizeFor == "specificity") {
    optimal_index <- which.max(metrics["specificity", ])
  } else if (optimizeFor == "misclasserror") {
    optimal_index <- which.min(metrics["misclasserror", ])
  } else if (optimizeFor == "youdensindex") {
    optimal_index <- which.max(metrics["youdensindex", ])
  } else {
    stop("Invalid optimization metric specified.")
  }
  
  optimal_cutoff <- cutoff_values[optimal_index]
  
  if (returnDiagnostics) {
    return(list(optimalCutoff = optimal_cutoff, metrics = metrics))
  } else {
    return(optimal_cutoff)
  }
}

