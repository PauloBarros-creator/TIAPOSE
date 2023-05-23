# explicacao breve da divisao entre ficheiros

# é necessário fazer "set as working directory" nesta pasta para funcionar!!!!! roda dentada na secção "Files"

source("eval.R")
source("repair.R")
#source("teste-predict.R")
source("getUpperLimit.R")


#sales_pred1 <- c()
#sales_pred2 <- c()

# UPLOAD DATA
# install.packages("openxlsx")
library(openxlsx)
db=read.xlsx(xlsxFile="bebidas.xlsx",sheet=1,skipEmptyRows=FALSE,colNames=TRUE,detectDates=TRUE)
db <- head(db, - 14) # Remove last 14 lines - too many zeroes

# Se desejar executar a interface, descomentar a seguinte linha:
#source('shiny/app.R')
