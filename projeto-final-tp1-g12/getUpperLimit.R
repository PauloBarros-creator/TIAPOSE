getUpperLimit <- function(prev) {
  
  # constante a multiplicar pelos valores previstos para definir o limite superior do espaço de otimização
  alpha <- 2
  
  # divisao do argumento input - e definicao do maximo das preparadas
  split_prev <- split(prev, rep(1:2, each = 7))
  max_p1 <- ceiling(alpha * split_prev[[1]])
  max_p2 <- ceiling(alpha * split_prev[[2]])
  
  # total de bebidas preparadas por dia
  max_total <- max_p1 + max_p2
  
  # recursos de armazenamento necessarios para satisfazer os maximos de bebidas - inteiro acima
  max_arm <- ceiling(max_total/72)
  
  # igual para os recursos de ditribuição
  max_v1 <- ceiling(max_total/60)
  max_v2 <- ceiling(max_total/90)
  max_v3 <- ceiling(max_total/120)
  
  # concatenar tudo
  upperLimit <- c(max_p1, max_p2, max_arm, max_v1, max_v2, max_v3)
  
  return (upperLimit)
}
