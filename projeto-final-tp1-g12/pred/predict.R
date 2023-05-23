source("pred/univariados.R")

predict <- function(metodo) {
  
  pred_stella <- paste(metodo,"_stella", sep = "")
  pred_bud <- paste(metodo,"_bud", sep = "")
  
  sales_pred1 <- get(pred_stella)()
  sales_pred2 <- get(pred_bud)()
  
  return(c(sales_pred1,sales_pred2))
}