# Function to repair solution

repair <- function(s) {
  
  # splitting code by chatgpt
  split_s <- split(s, rep(1:6, each = 7))
  preparadas1 <- split_s[[1]]
  preparadas2 <- split_s[[2]]
  arm <- split_s[[3]]
  v1 <- split_s[[4]]
  v2 <- split_s[[5]]
  v3 <- split_s[[6]]
  
  # calculo dos maximos
  max_arm <- arm*72
  max_v <- v1*60 + v2*90 + v3*120
  
  #iniciacao das variaveis a usar
  total_p <- preparadas1 + preparadas2
  p1_a <- preparadas1
  p2_a <- preparadas2
  p1_v <- preparadas1
  p2_v <- preparadas2
  
  # verificacao
  for (i in 1:7) {
    
    # arm
    if (0 <= total_p[i] && max_arm[i] >= total_p[i]) {
      p1_a[i] <- p1_a[i]
      p2_a[i] <- p2_a[i]
    } else {
      cat("AVISO: Plano com valor ilegal encontrado nos recursos de armazém, no dia ",i,"- A resolver...\n")
      p1_a[i] <- max_arm[i] / 2
      p2_a[i] <- max_arm[i] / 2
    }
    
    # v
    if (0 <= total_p[i] && max_v[i] >= total_p[i]) {
      p1_v[i] <- p1_v[i]
      p2_v[i] <- p2_v[i]
    } else {
      cat("AVISO: Plano com valor ilegal encontrado nos recursos de distribuição, no dia ",i,"- A resolver...\n")
      p1_v[i] <- max_v[i] / 2
      p2_v[i] <- max_v[i] / 2
    }
    
  }
  
  # cria o vetor final com o minimo de cada elemento. de maneira a respeitar semrpe as duas
  p1_f <- pmin(p1_a,p1_v)
  p2_f <- pmin(p2_a,p2_v)
  
  return (c(p1_f,p2_f,arm,v1,v2,v3))
}