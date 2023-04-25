# por Paulo Cortez @ 2009
dados=1:10

# por Paulo Cortez @2016
# retorna quantos pares tem o vector x
pares=function(x)
{
  res=0; 
  for(i in x) if(i%%2==0) res=res+1;
  return(res)
}

print(pares(dados))