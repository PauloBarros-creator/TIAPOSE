# pred: previsao
# otim: otimizacao
# shiny: interface

# é necessário fazer "set as working directory" nesta pasta para funcionar!!!!! roda dentada na secção "Files"

source("eval.R")
source("repair.R")
source("getUpperLimit.R")

# UPLOAD DATA
# install.packages("openxlsx")
library(openxlsx)
db=read.xlsx(xlsxFile="bebidas.xlsx",sheet=1,skipEmptyRows=FALSE,colNames=TRUE,detectDates=TRUE)
db <- head(db, - 14) # Remove last 14 lines - too many zeroes

# Para executar a interface, ir à pasta shiny e ao ficheiro app.R e executar linha a linha (source ou run app podem não funcionar)