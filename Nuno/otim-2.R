#with help from ChatGPT

# assume your dataset is named "my_data" and has a date column named "date"
db <- db %>%
  mutate(week = (as.numeric(format(DATA, "%U")) - as.numeric(format(min(DATA), "%U"))) %% 52 + 1) %>%
  select(week, everything())

cat("Escolha o ano: ")
year_num <- as.numeric(readline())

cat("Escolha a semana a utilizar: ")
semana_escolhida <- as.numeric(readline())

# select rows that correspond to the selected year
my_year_data <- subset(db, format(DATA, "%Y") == year_num)

# subset the data for the selected week and convert to list
week_data <- subset(my_year_data, week == semana_escolhida)
week_list <- as.list(week_data)

# create vectors for all columns
my_week_vecs <- lapply(week_list, unlist)


#print(my_week_vecs)
sales_pred1 <- my_week_vecs$STELLA
sales_pred2 <- my_week_vecs$BUD

cat("Vendas previstas de STELLA: ",sales_pred1,"\n")
cat("   Vendas previstas de BUD: ",sales_pred2,"\n")