source("pred/univariados.R")
source("pred/cubist_pred.R")
source("pred/lm_pred.R")
source("pred/mr_pred.R")
source("pred/plsr_pred.R")
source("pred/xgboost_pred.R")

predict <- function(metodo) {
  
  pred_stella <- paste(metodo,"_stella", sep = "")
  pred_bud <- paste(metodo,"_bud", sep = "")
  
  sales_pred1 <- get(pred_stella)()
  sales_pred2 <- get(pred_bud)()
  
  #sales_pred1 <- ksvm_stella()
  #sales_pred2 <- ksvm_bud()
  return(c(sales_pred1,sales_pred2))
}