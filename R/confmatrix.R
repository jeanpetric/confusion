#' A confusion Matrix Function
#'
#' This function allows you to calculate prediction performance measures from a confusion matrix.
#' @param table A confusion matrix table
#' @keywords confustion matrix
#' @export
#' @examples
#' matrix = rbind(c(30,60),c(10,90))
#' confmatrix(matrix)

confmatrix <- function(table, revPositive = FALSE) {
  result = list()
  result$positive = colnames(table)[1]
  result$table = table
  tp = table[1,1]
  fp = table[1,2]
  fn = table[2,1]
  tn = table[2,2]
  
  accuracy = (tp+tn)/(tp+tn+fp+fn)
  errorRate = 1 - accuracy
  recall = pd = tp/(tp+fn)
  precision = tp/(tp+fp)
  specificity = tn/(tn+fp)
  pf = fp/(fp+tn)
  fMeasure = (2*recall*precision)/(recall + precision)
  balance = 1 - (sqrt( (0-pf)^2 + (1-pd)^2 ))/sqrt(2)
  mcc = (tp*tn-fp*fn)/sqrt( (tp+fp) * (tp+fn) * (tn+fp) * (tn+fn) )
  
  result$performance = c(accuracy,errorRate,recall,recall,recall,precision,specificity,pf,fMeasure,balance,mcc)
  names(result$performance) = c("accuracy","errorRate","recall","sensitivity","pd","precision","specificity","pf","fMeasure","balance","mcc")
  return(result)
}