corandcov <- function(glsob,cov=T,...){
  corm <- corMatrix(glsob$modelStruct$corStruct)[[5]]
  print(corm)
  varstruct <- print(glsob$modelStruct$varStruct)  
  varests <- coef(varstruct, uncons=F, allCoef=T)
  covm <- corm*glsob$sigma^2*t(t(varests))%*%t(varests)
  return(covm)
}